(* Macro parsing. *)

(* Recognizes strings corresponding to macro operators. *)
let op (token : string) : Base.Mic.Macro.op option = match token with
| "EQ" -> Some Eq
| "NEQ" -> Some Neq
| "LT" -> Some Lt
| "LE" -> Some Le
| "GE" -> Some Ge
| "GT" -> Some Gt
| _ -> None

(* Parses a prefix and then an operator. *)
let prefixed_op
    (err : unit -> string list)
    (pref : string)
    (build : Base.Mic.Macro.op -> 'a)
    (token : string)
    : 'a option
= match Utils.tail_of_pref ~pref token with
| None -> None
| Some tail -> (
    match op tail with
    | Some op -> Some (build op)
    | None -> err () |> Base.Exc.throws
)

let pair_op (c : char) : Base.Mic.Macro.pair_op option = match c with
| 'P' -> Some P
| 'A' -> Some A
| 'I' -> Some I
| _ -> None

let pair_ops (token : string) : (Base.Mic.Macro.pair_op list * string) =
    Utils.sequence pair_op token

let unpair_op (c : char) : Base.Mic.Macro.unpair_op option = match c with
| 'A' -> Some A
| 'D' -> Some D
| _ -> None

let unpair_ops (token : string) : (Base.Mic.Macro.unpair_op list * string) =
    Utils.sequence unpair_op token
