open Format

let _ = Printexc.record_backtrace false
let print_backtrace = ref false

let set_print_backtrace (b : bool) : unit =
    print_backtrace := b

module Internal = struct
    type t =
    | ApplyOps

    let fmt (fmt : formatter) (self : t) : unit =
        match self with
        | ApplyOps -> fprintf fmt "ran into a `APPLY_OPS` instruction"
end

module Protocol = struct
    type t =
    | Failure of string
    | TooPoor of string * string * Int64.t
    | MutezOvrflw of string
    | Tezos of string

    let fmt (fmt : formatter) (self : t) : unit =
        match self with
        | Failure blah -> fprintf fmt "Failure on value %s" blah
        | Tezos blah -> fprintf fmt "%s" blah
        | MutezOvrflw blah -> fprintf fmt "mutez operation overflow: %s" blah
        | TooPoor (src, tgt, mutez) ->
            fprintf fmt "insufficient balance to process transaction from %s to %s of %s mutez"
                src tgt (Int64.to_string mutez)
end

type exc =
| Error of string list * exn option * Printexc.raw_backtrace
| Internal of Internal.t
| Protocol of Protocol.t

exception Exc of exc

module Throw = struct
    let apply_ops () : 'a =
        Exc (Internal (Internal.ApplyOps)) |> raise

    let failure (s : string) : 'a =
        Exc (Protocol (Protocol.Failure s)) |> raise

    let tezos (s : string) : 'a =
        Exc (Protocol (Protocol.Tezos s)) |> raise

    let too_poor
        ~(src : string)
        ~(tgt : string)
        ~(amount : Int64.t)
        : 'a
    =
        Exc (Protocol (Protocol.TooPoor (src, tgt, amount))) |> raise

    let mutez_overflow (blah : string) : 'a =
        Exc (Protocol (Protocol.MutezOvrflw blah)) |> raise
end

let rec fmt (fmtt : formatter) (e : exn) : unit =
    fprintf fmtt "@[<v 4>";
    (
        match e with
        | Exc (Error (trace, opt, backtrace)) ->
            fprintf fmtt "Error";
            trace |> List.iter (fprintf fmtt "@,@[%s@]");
            (
                match opt with
                | None -> ()
                | Some e -> (
                    fprintf fmtt "@,@[%a@]" fmt e;
                )
            );
            if !print_backtrace then (
                Printexc.raw_backtrace_to_string backtrace
                |> String.split_on_char '\n'
                |> List.iter (fprintf fmtt "@,%s")
            )
        | Exc (Internal i) ->
            fprintf fmtt "Uncaught internal exception@,%a" Internal.fmt i
        | Exc (Protocol p) ->
            fprintf fmtt "Tezos protocol error@,%a" Protocol.fmt p
        | e -> fprintf fmtt "Error@,%s" (Printexc.to_string e)
    );
    fprintf fmtt "@]"

let get_callstack () = Printexc.get_callstack 666

let throw (s : string) : 'a =
    Exc (Error ([s], None, get_callstack ())) |> raise
let throws (ss : string list) : 'a =
    Exc (Error (ss, None, get_callstack ())) |> raise

let erase_err (blah : unit -> string) (stuff : unit -> 'a) : 'a =
    try stuff () with
    | (Exc (Internal _) as e)
    | (Exc (Protocol _) as e) -> raise e
    | _ -> blah () |> throw

let chain_errs (blah : unit -> string list) (stuff : unit -> 'a) : 'a =
    try stuff () with
    | Exc (Error (trace, opt, backtrace)) ->
        Exc (Error (blah () @ trace, opt, backtrace)) |> raise
    | e -> Exc (Error (blah (), Some e, get_callstack ())) |> raise

let chain_err (blah : unit -> string) (stuff : unit -> 'a) : 'a =
    chain_errs (fun () -> [blah ()]) stuff

let catch_print (stuff : unit -> 'a) : 'a option =
    try Some (stuff ()) with
    | e ->
        printf "%a@." fmt e;
        None

let catch_fail (stuff : unit -> 'a) : 'a =
    match catch_print stuff with
    | None -> exit 2
    | Some a -> a

let unreachable () : 'a = throw "entered unreachable code, please notify the developers"
let unimplemented () : 'a = throw "triggered unimplemented feature"

let rec get_protocol (e : exn) : Protocol.t option =
    match e with
    | Exc ((Protocol e)) -> Some e
    | Exc (Internal _) -> None
    | Exc (Error (_, Some e, _)) -> get_protocol e
    | _ -> None

let rec get_internal (e : exn) : Internal.t option =
    match e with
    | Exc ((Protocol _)) -> None
    | Exc (Internal e) -> Some e
    | Exc (Error (_, Some e, _)) -> get_internal e
    | _ -> None
