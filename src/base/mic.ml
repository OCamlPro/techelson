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
    | If of (op * 'ins * 'ins)
    | IfCmp of (op * 'ins * 'ins)
    | Fail
    | Assert
    | Assert_ of op
    | AssertCmp of op
    | AssertNone
    | AssertSome
    | AssertLeft
    | AssertRight
    | Dip of (int * 'ins)
    | Dup of int
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
    | If (op, _, _) ->
        fprintf fmt "IF%a ..." fmt_op op
    | IfCmp (op, _, _) ->
        fprintf fmt "IFCMP%a ..." fmt_op op
    | IfSome _ ->
        fprintf fmt "IF_SOME ..."

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
    | Dup n ->
        fprintf fmt "DU";
        for _ = 1 to n do
            fprintf fmt "U"
        done;
        fprintf fmt "P"
    | P ops ->
        fprintf fmt "P%aR" (Fmt.fmt_list Fmt.sep_non fmt_pair_op) ops
    | Unp ops ->
        fprintf fmt "UNP%aR" (Fmt.fmt_list Fmt.sep_non fmt_pair_op) ops
    | CadR ops ->
        fprintf fmt "C%aR" (Fmt.fmt_list Fmt.sep_non fmt_unpair_op) ops
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
| Rename

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
| Rename -> fprintf fmt "RENAME"

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
| "RENAME" -> Some Rename
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
| CreateAccount
| CheckSignature
| Rename -> 1

let field_arity_of_leaf (leaf : leaf) : int = match leaf with
| Pair
| Som
| Car
| Cdr -> 1

| Failwith
| Swap
| Drop
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
| Get
| Mem
| Update
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
| CheckSignature
| CreateAccount
| Rename -> 0

type 'sub ins =
| Leaf of leaf
| EmptySet of Dtyp.t
| EmptyMap of Dtyp.t * Dtyp.t
| Non of Dtyp.t
| Left of Dtyp.t
| Right of Dtyp.t
| Nil of Dtyp.t
| Seq of 'sub list
| If of 'sub * 'sub
| Loop of 'sub
| LoopLeft of 'sub
| Dip of 'sub
| Push of Dtyp.t * const
| Lambda of Dtyp.t * Dtyp.t * 'sub
| Iter of 'sub
| IfNone of 'sub * 'sub
| IfLeft of 'sub * 'sub
| IfRight of 'sub * 'sub
| IfCons of 'sub * 'sub
| CreateContract of contract option
| Macro of 'sub list * 'sub Macro.t

and const =
| Unit

| Bool of bool
| Int of string
| Str of string

| Contract of contract

| Lft of const
| Rgt of const

| No
| So of const

and contract = {
    storage : Dtyp.t ;
    param : Dtyp.t ;
    entry : t ;
}

and t = {
    ins : t ins ;
    typs : Annot.typs ;
    vars : Annot.vars ;
    fields : Annot.fields ;
}

let mk
    ?vars:(vars=[])
    ?fields:(fields=[])
    ?typs:(typs=[])
    (ins : t ins)
    : t
= { ins ; vars ; fields ; typs }

let mk_contract ~(storage : Dtyp.t) ~(param : Dtyp.t) (entry : t) : contract =
    { storage ; param ; entry }

let invisible_get_one (desc : string) (l : 'a list) : 'a =
    if l = [] then (
        sprintf "no \"%s\" field found" desc |> Exc.throw
    ) else if List.length l > 1 then (
        sprintf "more than one \"%s\" field found" desc |> Exc.throw
    );
    List.hd l
let mk_contract_of_lists ~(storage : Dtyp.t list) ~(param : Dtyp.t list) (entry : t list) : contract =
    {
        storage = invisible_get_one "storage type" storage ;
        param = invisible_get_one "entry parameter" param ;
        entry = invisible_get_one "entry code" entry
    }

let mk_leaf
    ?vars:(vars=[])
    ?fields:(fields=[])
    ?typs:(typs=[])
    (leaf : leaf)
    : t
= mk ~vars:(vars) ~fields:(fields) ~typs:(typs) (Leaf leaf)

let mk_seq (seq : t list) : t =
    match seq with
    | [ins] -> ins
    | _ -> Seq seq |> mk

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

(* Formats contracts. *)
let rec fmt_contract (fmtt : formatter) ({ storage ; param ; entry } : contract) : unit =
    fprintf
        fmtt "@[@[<v 4>{@ storage %a ;@ parameter %a ;@ code @[%a@]@]@,}@]"
        Dtyp.fmt storage Dtyp.fmt param fmt entry

(* Formats constants. *)
and fmt_const (fmtt : formatter) (c : const) : unit =
    match c with
    | Unit -> fprintf fmtt "Unit"
    | Bool b -> fprintf fmtt (if b then "True" else "False")
    | Int n -> fprintf fmtt "%s" n
    | Str s -> fprintf fmtt "\"%s\"" s
    | Contract c -> fmt_contract fmtt c
    | Lft c -> fprintf fmtt "(Left %a)" fmt_const c
    | Rgt c -> fprintf fmtt "(Right %a)" fmt_const c
    | No -> fprintf fmtt "None"
    | So c -> fprintf fmtt "(Some %a)" fmt_const c

(* Formats instructions.

    Strictly speaking, this function is not tailrec. Macro formatting takes the instruction
    formatter as a parameter, hence when printing macros we need to pass the `fmt` below so
    that it knows how to format instructions. Hence this function consumes stack whenever it
    goes down a macro.
*)
and fmt (fmtt : formatter) (t : t) : unit =
    let rec loop (stack : ( (Fmt.sep * t) list * Fmt.seq_end) list) : unit = match stack with
    | [] -> ()
    | ( [], seq_end) :: tail ->
        seq_end ();
        loop tail
    | ( ((sep, to_do) :: to_do_tail, seq_end) :: tail ) ->
        sep ();
        let tail = (to_do_tail, seq_end) :: tail in
        let { ins = to_do ; vars ; fields ; typs } = to_do in
        let fmt_annots () : unit =
            Annot.fmt_typs fmtt typs;
            Annot.fmt_vars fmtt vars;
            Annot.fmt_fields fmtt fields
        in
        let tail =
            match to_do with
            | EmptySet dtyp ->
                fprintf fmtt "EMPTY_SET (%a)" Dtyp.fmt dtyp;
                fmt_annots ();
                tail
            | EmptyMap (k, v) ->
                fprintf fmtt "EMPTY_MAP %a %a" Dtyp.fmt k Dtyp.fmt v;
                fmt_annots ();
                tail
            | Non dtyp ->
                fprintf fmtt "NONE %a" Dtyp.fmt dtyp;
                fmt_annots ();
                tail
            | Left dtyp ->
                fprintf fmtt "LEFT %a" Dtyp.fmt dtyp;
                fmt_annots ();
                tail
            | Right dtyp ->
                fprintf fmtt "RIGHT %a" Dtyp.fmt dtyp;
                fmt_annots ();
                tail
            | Nil dtyp ->
                fprintf fmtt "NIL %a" Dtyp.fmt dtyp;
                fmt_annots ();
                tail
            | Leaf leaf ->
                fmt_leaf fmtt leaf;
                fmt_annots ();
                tail

            | Seq ts -> (
                assert (vars = []);
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
                assert (vars = []);
                fprintf fmtt "@[<v 2>LOOP@,";
                ([ignore, t], Fmt.cls fmtt) :: tail

            | LoopLeft t ->
                assert (vars = []);
                fprintf fmtt "@[<v 2>LOOP_LEFT@,";
                ([ignore, t], Fmt.cls fmtt) :: tail

            | If (bt, bf) ->
                assert (vars = []);
                fmt_if_like fmtt "IF" bt bf tail
            | IfNone (bt, bf) ->
                assert (vars = []);
                fmt_if_like fmtt "IF_NONE" bt bf tail
            | IfLeft (bt, bf) ->
                assert (vars = []);
                fmt_if_like fmtt "IF_LEFT" bt bf tail
            | IfRight (bt, bf) ->
                assert (vars = []);
                fmt_if_like fmtt "IF_RIGHT" bt bf tail
            | IfCons (bt, bf) ->
                assert (vars = []);
                fmt_if_like fmtt "IF_CONS" bt bf tail

            | Dip t ->
                fprintf fmtt "DIP ";
                ([ignore, t], fmt_annots) :: tail

            | Iter t ->
                assert (vars = []);
                fprintf fmtt "ITER ";
                ([ignore, t], ignore) :: tail

            | Push (dtyp, c) ->
                fprintf fmtt "PUSH @[<v>(@[%a@])@,%a@]" Dtyp.fmt dtyp fmt_const c;
                tail

            | Lambda (dom, codom, t) ->
                fprintf fmtt "LAMBDA @[<v>(@[%a@])@,(@[%a@])@,(" Dtyp.fmt dom Dtyp.fmt codom;
                (
                    [ignore, t], Fmt.unit_combine (Fmt.cls_of fmtt ")") fmt_annots
                ) :: tail

            | CreateContract c -> (
                fprintf fmtt "@[@[<v 4>CREATE_CONTRACT";
                match c with
                | None ->
                    fprintf fmtt "@]@]";
                    tail
                | Some c ->
                    fprintf
                        fmtt " {@,storage %a;@,parameter %a;@,code "
                        Dtyp.fmt c.storage Dtyp.fmt c.param;
                    (
                        [ignore, c.entry], fun () -> fprintf fmtt "@]}@]"
                    ) :: tail
            )

            | Macro (ts, macro) -> (
                match ts with
                | [] ->
                    fprintf fmtt "@[{ # Expansion of `%a`@,}@]" (Macro.fmt fmt) macro;
                    tail
                | hd :: tl ->
                    fprintf fmtt "@[<v>@[<v 2>{ # Expansion of `%a`" (Macro.fmt fmt) macro;
                    (
                        ((fun () -> fprintf fmtt "@,"), hd)
                        :: (
                            tl |> List.map (
                                fun to_do -> (fun () -> fprintf fmtt " ;@,"), to_do
                            )
                        ),
                        fun () -> fprintf fmtt "@]@,}@]"
                    ) :: tail
            )
        in
        loop tail
    in
    loop [ [ignore, t], ignore ]
