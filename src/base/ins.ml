(* Instructions types and helpers. *)

open Common

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
| Som
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
| Som -> fprintf fmt "SOME"
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

let leaf_of_string (token : string) : leaf option = match token with
| "FAILWITH" -> Some Failwith
| "EXEC" -> Some Exec
| "DROP" -> Some Drop
| "DUP" -> Some Dup
| "SWAP" -> Some Swap
| "UNIT" -> Some Unit
| "EQ" -> Some Eq
| "NEQ" -> Some Neq
| "LT" -> Some Lt
| "LE" -> Some Le
| "GT" -> Some Gt
| "GE" -> Some Ge
| "OR" -> Some Or
| "AND" -> Some And
| "XOR" -> Some Xor
| "NOT" -> Some Not
| "NEG" -> Some Neg
| "ABS" -> Some Abs
| "ADD" -> Some Add
| "SUB" -> Some Sub
| "MUL" -> Some Mul
| "EDIV" -> Some EDiv
| "LSL" -> Some Lsl
| "LSR" -> Some Lsr
| "COMPARE" -> Some Compare
| "CONCAT" -> Some Concat
| "SIZE" -> Some Size
| "PAIR" -> Some Pair
| "CAR" -> Some Car
| "CDR" -> Some Cdr
| "GET" -> Some Get
| "MEM" -> Some Mem
| "UPDATE" -> Some Update
| "SOME" -> Some Som
| "CONS" -> Some Cons
| "CREATE_CONTRACT" -> Some CreateContract
| "CREATE_ACCOUNT" -> Some CreateAccount
| "TRANSFER_TOKENS" -> Some TransferTokens
| "SET_DELEGATE" -> Some SetDelegate
| "BALANCE" -> Some Balance
| "CONTRACT" -> Some Contract
| "SOURCE" -> Some Source
| "SENDER" -> Some Sender
| "SELF" -> Some Self
| "AMOUNT" -> Some Amount
| "IMPLICIT_ACCOUNT" -> Some ImplicitAccount
| "STEPS_TO_QUOTA" -> Some StepsToQuota
| "NOW" -> Some Now
| "PACK" -> Some Pack
| "UNPACK" -> Some Unpack
| "SLICE" -> Some Slice
| "HASH_KEY" -> Some HashKey
| "BLAKE2B" -> Some Blake2B
| "SHA256" -> Some Sha256
| "SHA512" -> Some Sha512
| "CHECK_SIGNATURE" -> Some CheckSignature
| _ -> None

let var_arity_of_leaf (leaf : leaf) : int = match leaf with
| Failwith
| Swap
| Drop -> 0

| Exec
| Dup
| Unit
| Eq
| Neq
| Lt
| Le
| Gt
| Ge
| Or
| And
| Xor
| Not
| Neg
| Abs
| Add
| Sub
| Mul
| EDiv
| Lsl
| Lsr
| Compare
| Concat
| Size
| Pair
| Car
| Cdr
| Get
| Mem
| Update
| Som
| Cons
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
| CheckSignature -> 1

| CreateContract
| CreateAccount -> 2

type t =
| Leaf of leaf
| EmptySet of Dtyp.t
| EmptyMap of Dtyp.t * Dtyp.t
| Non of Dtyp.t
| Left of Dtyp.t
| Right of Dtyp.t
| Nil of Dtyp.t
| Seq of t list
| If of t * t
| Loop of t
| LoopLeft of t
| Dip of t
| Push of Dtyp.t * t
| Lambda of Dtyp.t * Dtyp.t * t
| Iter of t
| IfNone of t * t
| IfLeft of t * t
| IfRight of t * t
| IfCons of t * t
| Macro of t list * t Macro.t

let mk_seq (seq : t list) : t = Seq seq

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
                fprintf fmtt "EMPTY_SET (%a)" Dtyp.fmt dtyp;
                tail
            | EmptyMap (k, v) ->
                fprintf fmtt "EMPTY_MAP (%a) (%a)" Dtyp.fmt k Dtyp.fmt v;
                tail
            | Non dtyp ->
                fprintf fmtt "NONE (%a)" Dtyp.fmt dtyp;
                tail
            | Left dtyp ->
                fprintf fmtt "LEFT (%a)" Dtyp.fmt dtyp;
                tail
            | Right dtyp ->
                fprintf fmtt "RIGHT (%a)" Dtyp.fmt dtyp;
                tail
            | Nil dtyp ->
                fprintf fmtt "NIL (%a)" Dtyp.fmt dtyp;
                tail
            | Leaf leaf ->
                fmt_leaf fmtt leaf;
                tail

            | Seq ts -> (
                match ts with
                | [] ->
                    fprintf fmtt "{}";
                    tail
                | hd :: tl ->
                    fprintf fmtt "@[<v>@[<v 2>{";
                    (
                        (Fmt.sep_brk fmtt, hd) ::  (
                            tl |> List.map (
                                fun to_do -> (fun () -> fprintf fmtt " ;@,"), to_do
                            )
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
                fprintf fmtt "PUSH @[<v>(@[%a@])@,(" Dtyp.fmt dtyp;
                (
                    [ignore, t], Fmt.cls_of fmtt ")"
                ) :: tail

            | Lambda (dom, codom, t) ->
                fprintf fmtt "LAMBDA @[<v>(@[%a@])@,(@[%a@])@,(" Dtyp.fmt dom Dtyp.fmt codom;
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

(* Instruction parser. *)
let parse
    (token : string)
    (dtyps: Dtyp.t list)
    (args : t list)
    (annots : string list)
    : t
=
    let ty_arity_check (ty_expected : int) : unit =
        Check.arity "type argument" ty_expected (
            fun () -> asprintf "%s" token
        ) dtyps
    in
    let annot_arity_check (expected : int) : unit =
        Check.arity_le "variable annotation" expected (
            fun () -> asprintf "%s" token
        ) annots
    in
    let arity_check (ty_expected : int) (ins_expected : int) (annot_expected : int) : unit =
        ty_arity_check ty_expected;
        Check.arity "instruction" ins_expected (
            fun () -> asprintf "%s" token
        ) args;
        annot_arity_check annot_expected
    in

    let inner () = match token with
    | "EMPTY_SET" ->
        arity_check 1 0 1;
        EmptySet (List.hd dtyps)
    | "EMPTY_MAP" ->
        arity_check 2 0 1;
        EmptyMap (List.hd dtyps, List.tl dtyps |> List.hd)
    | "NONE" ->
        arity_check 1 0 1;
        Non (List.hd dtyps)
    | "LEFT" ->
        arity_check 1 0 1;
        Left (List.hd dtyps)
    | "RIGHT" ->
        arity_check 1 0 1;
        Right (List.hd dtyps)
    | "NIL" ->
        arity_check 1 0 1;
        Nil (List.hd dtyps)
    | "IF" ->
        arity_check 0 2 0;
        If (List.hd args, List.tl args |> List.hd)
    | "IF_NONE" ->
        arity_check 0 2 0;
        IfNone (List.hd args, List.tl args |> List.hd)
    | "IF_LEFT" ->
        arity_check 0 2 0;
        IfLeft (List.hd args, List.tl args |> List.hd)
    | "IF_RIGHT" ->
        arity_check 0 2 0;
        IfRight (List.hd args, List.tl args |> List.hd)
    | "IF_CONS" ->
        arity_check 0 2 0;
        IfCons (List.hd args, List.tl args |> List.hd)
    | "LOOP" ->
        arity_check 0 1 0;
        Loop (List.hd args)
    | "LOOP_LEFT" ->
        arity_check 0 1 0;
        LoopLeft (List.hd args)
    | "DIP" ->
        arity_check 0 1 0;
        Dip (List.hd args)
    | "PUSH" ->
        arity_check 1 1 1;
        Push (List.hd dtyps, List.hd args)
    | "LAMBDA" ->
        arity_check 2 1 1;
        Lambda (List.hd dtyps, List.tl dtyps |> List.hd, List.hd args)
    | "ITER" ->
        arity_check 0 1 0;
        Iter (List.hd args)
    | _ -> (
        match leaf_of_string token with
        | None ->
            sprintf "unknown instruction `%s`" token
            |> Exc.throw
        | Some leaf ->
            var_arity_of_leaf leaf |> arity_check 0 0;
            Leaf leaf
    )
    in
    Exc.chain_err (
        fun () ->
            let dtyps =
                if dtyps = [] then ""
                else asprintf " %a" (Fmt.fmt_list Fmt.sep_spc (Fmt.fmt_paren Dtyp.fmt)) dtyps
            in
            let args =
                if args = [] then ""
                else asprintf " %a" (Fmt.fmt_list Fmt.sep_spc fmt) args
            in
            sprintf "while parsing \"%s%s%s\"" token dtyps args
    ) inner