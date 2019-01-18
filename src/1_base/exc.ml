open Format

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
    | Tezos of string

    let fmt (fmt : formatter) (self : t) : unit =
        match self with
        | Failure blah -> fprintf fmt "Failure on value %s" blah
        | Tezos blah -> fprintf fmt "%s" blah
end

type exc =
| Error of string list * exn option
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
end

let fmt (fmt : formatter) (e : exn) : unit =
    fprintf fmt "@[<v 4>";
    (
        match e with
        | Exc (Error (trace, opt)) ->
            fprintf fmt "Error";
            trace |> List.iter (fprintf fmt "@,@[%s@]");
            (
                match opt with
                | None -> ()
                | Some e ->
                    fprintf fmt "@,@[%s@]" (Printexc.to_string e)
            );
        | Exc (Internal i) ->
            fprintf fmt "Uncaught internal exception@,%a" Internal.fmt i
        | Exc (Protocol p) ->
            fprintf fmt "Tezos protocol error@,%a" Protocol.fmt p
        | e -> fprintf fmt "Error@,%s" (Printexc.to_string e)
    );
    fprintf fmt "@]"

let throw (s : string) : 'a = Exc (Error ([s], None)) |> raise
let throws (ss : string list) : 'a = Exc (Error (ss, None)) |> raise

let erase_err (blah : unit -> string) (stuff : unit -> 'a) : 'a =
    try stuff () with
    | (Exc (Internal _) as e)
    | (Exc (Protocol _) as e) -> raise e
    | _ -> blah () |> throw

let chain_errs (blah : unit -> string list) (stuff : unit -> 'a) : 'a =
    try stuff () with
    | (Exc (Internal _) as e)
    | (Exc (Protocol _) as e) -> raise e
    | Exc (Error (trace, opt)) -> Exc (Error (blah () @ trace, opt)) |> raise
    | e -> Exc (Error (blah (), Some e)) |> raise

let chain_err (blah : unit -> string) (stuff : unit -> 'a) : 'a =
    chain_errs (fun () -> [blah ()]) stuff

let catch_print (stuff : unit -> 'a) : 'a option =
    try Some (stuff ()) with
    | e ->
        printf "%a" fmt e;
        None

let catch_fail (stuff : unit -> 'a) : 'a =
    match catch_print stuff with
    | None -> exit 2
    | Some a -> a

let unreachable () : 'a = throw "entered unreachable code, please notify the developers"
let unimplemented () : 'a = throw "triggered unimplemented feature"