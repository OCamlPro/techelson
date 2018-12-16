(* Michelson instructions. *)

open Common

module DTyp = struct
    type leaf =
    | Str
    | Nat
    | Int
    | Bytes
    | Bool
    | Unit
    | Mutez
    | Address
    | Operation
    | Key
    | KeyH
    | Signature
    | Timestamp

    let fmt_leaf (fmt : formatter) (typ : leaf) = match typ with
    | Str -> fprintf fmt "string"
    | Nat -> fprintf fmt "nat"
    | Int -> fprintf fmt "int"
    | Bytes -> fprintf fmt "bytes"
    | Bool -> fprintf fmt "bool"
    | Unit -> fprintf fmt "unit"
    | Mutez -> fprintf fmt "mutez"
    | Address -> fprintf fmt "address"
    | Operation -> fprintf fmt "operation"
    | Key -> fprintf fmt "key"
    | KeyH -> fprintf fmt "key_hash"
    | Signature -> fprintf fmt "signature"
    | Timestamp -> fprintf fmt "timestamp"

    type t =
    | Leaf of leaf

    | List of t
    | Option of t
    | Set of t
    | Contract of t

    | Pair of t * t
    | Or of t * t
    | Map of t * t
    | BigMap of t * t

    let str = Leaf Str
    let int = Leaf Int
    let nat = Leaf Nat
    let bytes = Leaf Bytes
    let bool = Leaf Bool
    let unit = Leaf Unit
    let mutez = Leaf Mutez
    let address = Leaf Address
    let operation = Leaf Operation
    let key = Leaf Key
    let key_hash = Leaf KeyH
    let signature = Leaf Signature
    let timestamp = Leaf Timestamp

    let fmt (fmt : formatter) (typ : t) =
        let rec loop (stack : ( (Fmt.sep * t) list * Fmt.seq_end) list) : unit = match stack with
        | [] -> ()
        | ([], seq_end) :: tail ->
            seq_end ();
            loop tail
        | (((sep, to_do) :: to_do_tail), seq_end) :: tail ->
            sep ();
            let tail = (to_do_tail, seq_end) :: tail in
            let tail = match to_do with
                | Leaf leaf ->
                    fmt_leaf fmt leaf;
                    tail

                | List sub ->
                    fprintf fmt "list (";

                    ([(ignore, sub)], Fmt.cls_paren fmt) :: tail
                | Option sub ->
                    fprintf fmt "option (";

                    ([(ignore, sub)], Fmt.cls_paren fmt) :: tail
                | Set sub ->
                    fprintf fmt "set (";

                    ([(ignore, sub)], Fmt.cls_paren fmt) :: tail
                | Contract sub ->
                    fprintf fmt "contract (";

                    ([(ignore, sub)], Fmt.cls_paren fmt) :: tail

                | Or (lft, rgt) ->
                    fprintf fmt "or @[(";

                    (
                        [ (ignore, lft) ; ((fun () -> fprintf fmt ")@ ("), rgt) ],
                        Fmt.cls_of fmt ")"
                    ) :: tail

                | Pair (lft, rgt) ->
                    fprintf fmt "pair @[(";

                    (
                        [ (ignore, lft) ; ((fun () -> fprintf fmt ")@ ("), rgt) ],
                        Fmt.cls_of fmt ")"
                    ) :: tail

                | Map (k, v) ->
                    fprintf fmt "map @[(";

                    (
                        [ (ignore, k) ; ((fun () -> fprintf fmt ")@ ("), v) ],
                        Fmt.cls_of fmt ")"
                    ) :: tail

                | BigMap (k, v) ->
                    fprintf fmt "big_map @[(";

                    (
                        [ (ignore, k) ; ((fun () -> fprintf fmt ")@ ("), v) ],
                        Fmt.cls_of fmt ")"
                    ) :: tail
            in
            loop tail
        in
        fprintf fmt "@[";
        loop [ [ignore, typ], ignore ];
        fprintf fmt "@]"

end


module Macro = struct
    type op =
    | Eq | Neq | Lt | Le | Ge | Gt

    let fmt_op (fmt : formatter) (op : op) : unit = match op with
    | Eq -> fprintf fmt "EQ"
    | Neq -> fprintf fmt "NEQ"
    | Lt -> fprintf fmt "LT"
    | Le -> fprintf fmt "LE"
    | Ge -> fprintf fmt "GE"
    | Gt -> fprintf fmt "GT"

    type pair_op =
    | P | A | I

    let fmt_pair_op (fmt : formatter) (op : pair_op) : unit = match op with
    | P -> fprintf fmt "P"
    | A -> fprintf fmt "A"
    | I -> fprintf fmt "I"

    type unpair_op =
    | A | D

    let fmt_unpair_op (fmt : formatter) (op : unpair_op) : unit = match op with
    | A -> fprintf fmt "A"
    | D -> fprintf fmt "D"

    type 'ins t =
    | Cmp of op
    | If of op
    | IfCmp of op
    | Fail
    | Assert
    | Assert_ of op
    | AssertCmp of op
    | AssertNone
    | AssertSome
    | AssertLeft
    | AssertRight
    | Dip of (int * 'ins)
    | Dup of (int * 'ins)
    | P of pair_op list
    | Unp of pair_op list
    | CadR of unpair_op list
    | IfSome of 'ins * 'ins
    | SetCadr of unpair_op list
    | MapCadr of unpair_op list * 'ins

    let fmt
        (fmt_ins : formatter -> 'ins -> unit)
        (fmt : formatter)
        (t : 'ins t)
        : unit
    = match t with
    | Cmp op -> fprintf fmt "CMP%a" fmt_op op
    | If op -> fprintf fmt "IF%a" fmt_op op
    | IfCmp op -> fprintf fmt "IFCMP%a" fmt_op op

    | Fail -> fprintf fmt "FAIL"
    | Assert -> fprintf fmt "ASSERT"
    | Assert_ op -> fprintf fmt "ASSERT_%a" fmt_op op
    | AssertCmp op -> fprintf fmt "ASSERT_CMP%a" fmt_op op
    | AssertNone -> fprintf fmt "ASSERT_NONE"
    | AssertSome -> fprintf fmt "ASSERT_SOME"
    | AssertLeft -> fprintf fmt "ASSERT_LEFT"
    | AssertRight -> fprintf fmt "ASSERT_RIGHT"
    | Dip (n, ins) ->
        fprintf fmt "DI";
        for _ = 1 to n do
            fprintf fmt "I"
        done;
        fprintf fmt "P";
        fmt_ins fmt ins
    | Dup (n, ins) ->
        fprintf fmt "DU";
        for _ = 1 to n do
            fprintf fmt "U"
        done;
        fprintf fmt "P";
        fmt_ins fmt ins
    | P ops ->
        fprintf fmt "P%aR" (Fmt.fmt_list Fmt.sep_non fmt_pair_op) ops
    | Unp ops ->
        fprintf fmt "UNP%aR" (Fmt.fmt_list Fmt.sep_non fmt_pair_op) ops
    | CadR ops ->
        fprintf fmt "C%aR" (Fmt.fmt_list Fmt.sep_non fmt_unpair_op) ops
    | IfSome (ins_1, ins_2) ->
        fprintf fmt "IF_SOME @[%a@ %a@]" fmt_ins ins_1 fmt_ins ins_2
    | SetCadr ops ->
        fprintf fmt "SET_C%aR" (Fmt.fmt_list Fmt.sep_non fmt_unpair_op) ops
    | MapCadr (ops, ins) ->
        fprintf fmt "MAP_C%aR %a" (Fmt.fmt_list Fmt.sep_non fmt_unpair_op) ops fmt_ins ins
end


module Ins = struct
    type leaf =
    | Failwith
    | Exec
    | Drop
    | Dup
    | Swap
    | Unit
    | Eq
    | Neq
    | Lt | Le | Gt | Ge
    | Or | And | Xor | Not
    | Neg | Abs | Add | Sub | Mul | EDiv
    | Lsl | Lsr
    | Compare
    | Concat
    | Size
    | Pair
    | Car
    | Cdr
    | Get
    | Mem
    | Update
    | Some
    | Cons
    | CreateContract
    | CreateAccount
    | TransferTokens
    | SetDelegate
    | Balance
    | Contract
    | Source
    | Sender
    | Self
    | Amount
    | ImplicitAccount
    | StepsToQuota
    | Now
    | Pack
    | Unpack
    | Slice
    | HashKey
    | Blake2B
    | Sha256
    | Sha512
    | CheckSignature

    let fmt_leaf (fmt : formatter) (leaf : leaf) : unit = match leaf with
    | Failwith -> fprintf fmt "FAILWITH"
    | Exec -> fprintf fmt "EXEC"
    | Drop -> fprintf fmt "DROP"
    | Dup -> fprintf fmt "DUP"
    | Swap -> fprintf fmt "SWAP"
    | Unit -> fprintf fmt "UNIT"
    | Eq -> fprintf fmt "EQ"
    | Neq -> fprintf fmt "NEQ"
    | Lt -> fprintf fmt "LT"
    | Le -> fprintf fmt "LE"
    | Gt -> fprintf fmt "GT"
    | Ge -> fprintf fmt "GE"
    | Or -> fprintf fmt "OR"
    | And -> fprintf fmt "AND"
    | Xor -> fprintf fmt "XOR"
    | Not -> fprintf fmt "NOT"
    | Neg -> fprintf fmt "NEG"
    | Abs -> fprintf fmt "ABS"
    | Add -> fprintf fmt "ADD"
    | Sub -> fprintf fmt "SUB"
    | Mul -> fprintf fmt "MUL"
    | EDiv -> fprintf fmt "EDIV"
    | Lsl -> fprintf fmt "LSL"
    | Lsr -> fprintf fmt "LSR"
    | Compare -> fprintf fmt "COMPARE"
    | Concat -> fprintf fmt "CONCAT"
    | Size -> fprintf fmt "SIZE"
    | Pair -> fprintf fmt "PAIR"
    | Car -> fprintf fmt "CAR"
    | Cdr -> fprintf fmt "CDR"
    | Get -> fprintf fmt "GET"
    | Mem -> fprintf fmt "MEM"
    | Update -> fprintf fmt "UPDATE"
    | Some -> fprintf fmt "SOME"
    | Cons -> fprintf fmt "CONS"
    | CreateContract -> fprintf fmt "CREATE_CONTRACT"
    | CreateAccount -> fprintf fmt "CREATE_ACCOUNT"
    | TransferTokens -> fprintf fmt "TRANSFER_TOKENS"
    | SetDelegate -> fprintf fmt "SET_DELEGATE"
    | Balance -> fprintf fmt "BALANCE"
    | Contract -> fprintf fmt "CONTRACT"
    | Source -> fprintf fmt "SOURCE"
    | Sender -> fprintf fmt "SENDER"
    | Self -> fprintf fmt "SELF"
    | Amount -> fprintf fmt "AMOUNT"
    | ImplicitAccount -> fprintf fmt "IMPLICIT_ACCOUNT"
    | StepsToQuota -> fprintf fmt "STEPS_TO_QUOTA"
    | Now -> fprintf fmt "NOW"
    | Pack -> fprintf fmt "PACK"
    | Unpack -> fprintf fmt "UNPACK"
    | Slice -> fprintf fmt "SLICE"
    | HashKey -> fprintf fmt "HASH_KEY"
    | Blake2B -> fprintf fmt "BLAKE2B"
    | Sha256 -> fprintf fmt "SHA256"
    | Sha512 -> fprintf fmt "SHA512"
    | CheckSignature -> fprintf fmt "CHECK_SIGNATURE"

    type t =
    | Leaf of leaf
    | EmptySet of DTyp.t
    | EmptyMap of DTyp.t * DTyp.t
    | None of DTyp.t
    | Left of DTyp.t
    | Right of DTyp.t
    | Nil of DTyp.t
    | Seq of t list
    | If of t * t
    | Loop of t
    | LoopLeft of t
    | Dip of t
    | Push of DTyp.t * t
    | Lambda of DTyp.t * DTyp.t * t
    | Iter of t
    | IfNone of t * t
    | IfLeft of t * t
    | IfRight of t * t
    | IfCons of t * t
    | Macro of t list * t Macro.t

    (* Formats an IF-like instruction. *)
    let fmt_if_like
        (fmt : formatter)
        (key : string)
        (bt : t)
        (bf : t)
        (stack : ( (Fmt.sep * t) list * Fmt.seq_end) list)
    =
        fprintf fmt "%s @[<v>" key;
        (
            [ (ignore, bt) ; (Fmt.sep_brk fmt, bf) ],
            Fmt.cls fmt
        ) :: stack

    (* Formats instructions.

        Strictly speaking, this function is not tailrec. Macro formatting takes the instruction
        formatter as a parameter, hence when printing macros we need to pass the `fmt` below so
        that it knows how to format instructions. Hence this function consumes stack whenever it
        goes down a macro.
    *)
    let rec fmt (fmtt : formatter) (t : t) : unit =
        let rec loop (stack : ( (Fmt.sep * t) list * Fmt.seq_end) list) : unit = match stack with
        | [] -> ()
        | ( [], seq_end) :: tail ->
            seq_end ();
            loop tail
        | ( ((sep, to_do) :: to_do_tail, seq_end) :: tail ) ->
            sep ();
            let tail = (to_do_tail, seq_end) :: tail in
            let tail =
                match to_do with
                | EmptySet dtyp ->
                    fprintf fmtt "EMPTY_SET (%a)" DTyp.fmt dtyp;
                    tail
                | EmptyMap (k, v) ->
                    fprintf fmtt "EMPTY_MAP (%a) (%a)" DTyp.fmt k DTyp.fmt v;
                    tail
                | None dtyp ->
                    fprintf fmtt "NONE (%a)" DTyp.fmt dtyp;
                    tail
                | Left dtyp ->
                    fprintf fmtt "LEFT (%a)" DTyp.fmt dtyp;
                    tail
                | Right dtyp ->
                    fprintf fmtt "RIGHT (%a)" DTyp.fmt dtyp;
                    tail
                | Nil dtyp ->
                    fprintf fmtt "NIL (%a)" DTyp.fmt dtyp;
                    tail
                | Leaf leaf ->
                    fmt_leaf fmtt leaf;
                    tail

                | Seq ts -> (
                    match ts with
                    | [] ->
                        fprintf fmtt "{}";
                        tail
                    | _ ->
                        fprintf fmtt "@[@[<v 2>{";
                        (
                            ts |> List.map (
                                fun to_do -> Fmt.sep_brk fmtt, to_do
                            ),
                            fun () -> fprintf fmtt "@]@,}@]"
                        ) :: tail
                )

                | Loop t ->
                    fprintf fmtt "@[<v 2>LOOP@,";
                    ([ignore, t], Fmt.cls fmtt) :: tail

                | LoopLeft t ->
                    fprintf fmtt "@[<v 2>LOOP_LEFT@,";
                    ([ignore, t], Fmt.cls fmtt) :: tail

                | If (bt, bf) -> fmt_if_like fmtt "IF" bt bf tail
                | IfNone (bt, bf) -> fmt_if_like fmtt "IF_NONE" bt bf tail
                | IfLeft (bt, bf) -> fmt_if_like fmtt "IF_LEFT" bt bf tail
                | IfRight (bt, bf) -> fmt_if_like fmtt "IF_RIGHT" bt bf tail
                | IfCons (bt, bf) -> fmt_if_like fmtt "IF_CONS" bt bf tail

                | Dip t ->
                    fprintf fmtt "DIP ";
                    ([ignore, t], ignore) :: tail

                | Iter t ->
                    fprintf fmtt "ITER ";
                    ([ignore, t], ignore) :: tail

                | Push (dtyp, t) ->
                    fprintf fmtt "PUSH @[<v>(@[%a@])@,(" DTyp.fmt dtyp;
                    (
                        [ignore, t], Fmt.cls_of fmtt ")"
                    ) :: tail

                | Lambda (dom, codom, t) ->
                    fprintf fmtt "LAMBDA @[<v>(@[%a@])@,(@[%a@])@,(" DTyp.fmt dom DTyp.fmt codom;
                    (
                        [ignore, t], Fmt.cls_of fmtt ")"
                    ) :: tail

                | Macro (ts, macro) -> (
                    match ts with
                    | [] ->
                        fprintf fmtt "@[{ # Expansion of `%a`@,}@]" (Macro.fmt fmt) macro;
                        tail
                    | _ ->
                        fprintf fmtt "@[@[<v 2>{ # Expansion of `%a`" (Macro.fmt fmt) macro;
                        (
                            ts |> List.map (
                                fun to_do -> Fmt.sep_brk fmtt, to_do
                            ),
                            fun () -> fprintf fmtt "@]@,}@]"
                        ) :: tail
                )
            in
            loop tail
        in
        loop [ [ignore, t], ignore ]
end