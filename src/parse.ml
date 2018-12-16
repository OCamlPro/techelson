(* Michelson parser. *)

open Common

let hd = List.head
let tl = List.tail
let len = List.length

module Token = struct
    let arity_check
        (desc : string)
        (expected : int)
        (blah : unit -> string)
        (args : 'b list)
        (arg_fmt : formatter -> 'b -> unit)
        : unit
    =
        let len = len args in
        if len != expected then throws [
            Fmt.plurify expected |> sprintf "%s takes %i %s%s" (blah ()) expected desc;
            Fmt.plurify len |> sprintf "found %i %s%s" len desc
        ]

    module DTyp = struct
        type t =
        (* Leaf datatype. *)
        | Leaf of Mic.DTyp.leaf
        (* Composite datatype. *)
        | List
        | Option
        | Set
        | Contract
        | Pair
        | Or
        | Map
        | BigMap

        let fmt (fmt : formatter) (token : t) = match token with
        | Leaf leaf -> Mic.DTyp.fmt_leaf fmt leaf
        | List -> "list"
        | Option -> "option"
        | Set -> "set"
        | Contract -> "contract"
        | Pair -> "pair"
        | Or -> "or"
        | Map -> "map"
        | BigMap -> "big_map"

        let arity_check
            (expected : int)
            (token : t)
            (args : Mic.DTyp.t list)
            : unit
        = arity_check "type argument" expected (
            fun () -> sprintf "%a" fmt token
        ) args Mic.DTyp.fmt

        let to_dtyp (token : t) (dtyps : Mic.DTyp.t list) : Mic.DTyp.t =
            let inner () =
                match token with
                | Leaf leaf ->
                    arity_check 0 token dtyps;
                    DTyp.Leaf leaf
                | List ->
                    arity_check 1 token dtyps;
                    DTyp.List (hd dtyps)
                | Option ->
                    arity_check 1 token dtyps;
                    DTyp.Option (hd dtyps)
                | Set ->
                    arity_check 1 token dtyps;
                    DTyp.Set (hd dtyps)
                | Contract ->
                    arity_check 1 token dtyps;
                    DTyp.Contract (hd dtyps)
                | Pair ->
                    arity_check 2 token dtyps;
                    DTyp.Pair (hd dtyps, tl dtyps |> hd)
                | Or ->
                    arity_check 2 token dtyps;
                    DTyp.Or (hd dtyps, tl dtyps |> hd)
                | Map ->
                    arity_check 2 token dtyps;
                    DTyp.Map (hd dtyps, tl dtyps |> hd)
                | BigMap ->
                    arity_check 2 token dtyps;
                    DTyp.BigMap (hd dtyps, tl dtyps |> hd)
            in
            Exc.chain_err (
                fun () -> sprintf "while parsing %a %a" DTyp.fmt_leaf leaf (
                    Fmt.fmt_list Fmt.sep_spc DTyp.fmt
                ) dtyps
            ) inner
    end

    module Ins = struct
        type t =
        | Leaf of leaf
        | EmptySet
        | EmptyMap
        | None
        | Left
        | Right
        | Nil
        | Leaf
        | Seq
        | If
        | Loop
        | LoopLeft
        | Dip
        | Push
        | Lambda
        | Iter
        | IfNone
        | IfLeft
        | IfRight
        | IfCons

        let fmt (fmt : formatter) (t : t) = match t with
        | Leaf leaf -> Mic.Ins.fmt_leaf fmt leaf
        | EmptySet -> fprintf fmt "EMPTY_SET"
        | EmptyMap -> fprintf fmt "EMPTY_MAP"
        | None -> fprintf fmt "NONE"
        | Left -> fprintf fmt "LEFT"
        | Right -> fprintf fmt "RIGHT"
        | Nil -> fprintf fmt "NIL"
        | Seq -> fprintf fmt "SEQ"
        | If -> fprintf fmt "IF"
        | Loop -> fprintf fmt "LOOP"
        | LoopLeft -> fprintf fmt "LOOP_LEFT"
        | Dip -> fprintf fmt "DIP"
        | Push -> fprintf fmt "PUSH"
        | Lambda -> fprintf fmt "LAMBDA"
        | Iter -> fprintf fmt "ITER"
        | IfNone -> fprintf fmt "IF_NONE"
        | IfLeft -> fprintf fmt "IF_LEFT"
        | IfRight -> fprintf fmt "IF_RIGHT"
        | IfCons -> fprintf fmt "IF_CONS"


        let to_ins
            (token : t)
            (dtyps: Mic.DTyp.t list)
            (args : Mic.Ins.t list)
            : Mic.Ins.t
        =
            let ty_arity_check (ty_expected : int) : unit =
                arity_check "type argument" ty_expected (
                    fun () -> sprintf "%a" fmt token
                ) dtyps Mic.DTyp.fmt
            in
            let arity_check (ty_expected : int) (ins_expected : int) : unit =
                ty_arity_check ty_expected;
                arity_check "instruction" ins_expected (
                    fun () -> sprintf "%a" fmt token
                ) args Mic.Ins.fmt
            in

            let inner () = match token with
            | Leaf leaf ->
                arity_check 0 0;
                Mic.Ins.Leaf leaf
            | EmptySet ->
                arity_check 1 0;
                Mic.Ins.EmptySet (hd dtyps)
            | EmptyMap ->
                arity_check 2 0;
                Mic.Ins.EmptyMap (hd dtyps, tl dtyps |> hd)
            | None ->
                arity_check 1 0;
                Mic.Ins.None (hd dtyps)
            | Left ->
                arity_check 1 0;
                Mic.Ins.Left (hd dtyps)
            | Right ->
                arity_check 1 0;
                Mic.Ins.Right (hd dtyps)
            | Nil ->
                arity_check 1 0;
                Mic.Ins.Nil (hd dtyps)
            | Seq ->
                ty_arity_check 0;
                Mic.Ins.Seq args
            | If ->
                arity_check 0 2;
                Mic.Ins.If (hd args, tl args |> hd)
            | IfNone ->
                arity_check 0 2;
                Mic.Ins.IfNone (hd args, tl args |> hd)
            | IfLeft ->
                arity_check 0 2;
                Mic.Ins.IfLeft (hd args, tl args |> hd)
            | IfRight ->
                arity_check 0 2;
                Mic.Ins.IfRight (hd args, tl args |> hd)
            | IfCons ->
                arity_check 0 2;
                Mic.Ins.IfCons (hd args, tl args |> hd)
            | Loop ->
                arity_check 0 1;
                Mic.Ins.Loop (hd args)
            | LoopLeft ->
                arity_check 0 1;
                Mic.Ins.LoopLeft (hd args)
            | Dip ->
                arity_check 0 1;
                Mic.Ins.Dip (hd args)
            | Push ->
                arity_check 1 1;
                Mic.Ins.Push (hd dtyps, hd args)
            | Lambda ->
                arity_check 2 1;
                Mic.Ins.Lambda (
                    hd dtyps,
                    tl dtyps |> hd,
                    hd 
                )
            | Iter ->
                arity_check 0 1;
                Mic.Ins.Iter (hd dtyps)
            in
            inner ()
    end
end