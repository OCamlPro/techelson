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
    | If (op, bt, bf) ->
        fprintf fmt "IF%a @[<hov>%a@ %a@]" fmt_op op fmt_ins bt fmt_ins bf
    | IfCmp (op, bt, bf) ->
        fprintf fmt "IFCMP%a @[<hov>%a@ %a@]" fmt_op op fmt_ins bt fmt_ins bf
    | IfSome (ins_1, ins_2) ->
        fprintf fmt "IF_SOME @[<hov>%a@ %a@]" fmt_ins ins_1 fmt_ins ins_2

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

    module Parse = struct
        (* If `token` starts with `pref`, returns what's after the `pref` part.
        
            Used to parse macros.
        *)
        let tail_of_pref (pref : string) (token : string) : string option =
            let pref_len = String.length pref in
            let token_len = String.length token in
            if pref_len <= token_len then (
                let token_pref = String.sub token 0 pref_len in
                if token_pref = pref then (
                    Some (
                        String.sub token pref_len (token_len - pref_len)
                    )
                ) else (
                    None
                )
            ) else (
                None
            )

        (* Recognizes strings corresponding to macro operators. *)
        let op (token : string) : op option = match token with
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
            (build : op -> 'a)
            (token : string)
            : 'a option
        = match tail_of_pref pref token with
        | None -> None
        | Some tail -> (
            match op tail with
            | Some op -> Some (build op)
            | None -> err () |> Exc.throws
        )

        (* Recognizes sequences of characters.

            Input function says whether some character is recognized. The function stops parsing
            when it returs `None`.
        
            Keeps on parsing as long as it is successful. Returns the tail of the token, the part
            that was not parsed.
        *)
        let sequence (f : char -> 'a option) (token : string) : ('a list * string) =
            let token_len = String.length token in
            (* Input `n` is the index of the character in the string we're currently dealing with. *)
            let rec loop lst n =
                let op =
                    if n < token_len then (
                        String.get token n |> f
                    ) else (
                        None
                    )
                in
                match op with
                | Some op -> loop (op :: lst) (n + 1)
                | None -> List.rev lst, String.sub token n (token_len - n)
            in
            loop [] 0

        let pair_op (c : char) : pair_op option = match c with
        | 'P' -> Some P
        | 'A' -> Some A
        | 'I' -> Some I
        | _ -> None

        let pair_ops (token : string) : (pair_op list * string) = sequence pair_op token

        let unpair_op (c : char) : unpair_op option = match c with
        | 'A' -> Some A
        | 'D' -> Some D
        | _ -> None

        let unpair_ops (token : string) : (unpair_op list * string) = sequence unpair_op token
    end
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
| Push of Dtyp.t * 'sub
| Lambda of Dtyp.t * Dtyp.t * 'sub
| Iter of 'sub
| IfNone of 'sub * 'sub
| IfLeft of 'sub * 'sub
| IfRight of 'sub * 'sub
| IfCons of 'sub * 'sub
| Macro of 'sub list * 'sub Macro.t

type var = string

type t = {
    ins : t ins ;
    vars : var list ;
}

let fmt_var (fmt : formatter) (var : var) : unit =
    fprintf fmt "@%s" var
let fmt_vars (pref : string) (fmt : formatter) (vars : var list) : unit =
    if vars <> [] then (
        fprintf fmt "%s%a" pref (Fmt.fmt_list Fmt.sep_spc fmt_var) vars
    )

let mk ?vars:(vars=[]) (ins : t ins) : t = { ins ; vars }
let mk_leaf ?vars:(vars=[]) (leaf : leaf) : t = mk ~vars:(vars) (Leaf leaf)
let mk_seq (seq : t list) : t = Seq seq |> mk

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
        let { ins = to_do ; vars } = to_do in
        let fmt_vars () : unit =
            fmt_vars " " fmtt vars
        in
        let tail =
            match to_do with
            | EmptySet dtyp ->
                fprintf fmtt "EMPTY_SET (%a)" Dtyp.fmt dtyp;
                fmt_vars ();
                tail
            | EmptyMap (k, v) ->
                fprintf fmtt "EMPTY_MAP %a %a" Dtyp.fmt k Dtyp.fmt v;
                fmt_vars ();
                tail
            | Non dtyp ->
                fprintf fmtt "NONE %a" Dtyp.fmt dtyp;
                fmt_vars ();
                tail
            | Left dtyp ->
                fprintf fmtt "LEFT %a" Dtyp.fmt dtyp;
                fmt_vars ();
                tail
            | Right dtyp ->
                fprintf fmtt "RIGHT %a" Dtyp.fmt dtyp;
                fmt_vars ();
                tail
            | Nil dtyp ->
                fprintf fmtt "NIL %a" Dtyp.fmt dtyp;
                fmt_vars ();
                tail
            | Leaf leaf ->
                fmt_leaf fmtt leaf;
                fmt_vars ();
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
                ([ignore, t], fmt_vars) :: tail

            | Iter t ->
                assert (vars = []);
                fprintf fmtt "ITER ";
                ([ignore, t], ignore) :: tail

            | Push (dtyp, t) ->
                fprintf fmtt "PUSH @[<v>(@[%a@])@,(" Dtyp.fmt dtyp;
                (
                    [ignore, t], Fmt.unit_combine (Fmt.cls_of fmtt ")") fmt_vars
                ) :: tail

            | Lambda (dom, codom, t) ->
                fprintf fmtt "LAMBDA @[<v>(@[%a@])@,(@[%a@])@,(" Dtyp.fmt dom Dtyp.fmt codom;
                (
                    [ignore, t], Fmt.unit_combine (Fmt.cls_of fmtt ")") fmt_vars
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

module Expand = struct
    let op_to_ins (op : Macro.op) : t =
        let leaf : leaf =
            match op with
            | Macro.Eq -> Eq
            | Macro.Neq -> Neq
            | Macro.Lt -> Lt
            | Macro.Le -> Le
            | Macro.Ge -> Ge
            | Macro.Gt -> Gt
        in
        mk (Leaf leaf)

    let macro_cmp (op : Macro.op) : t list = [
        mk_leaf Compare ;
        op_to_ins op ;
    ]
    let macro_if (op : Macro.op) (i_1 : t) (i_2 : t) : t list = [
        op_to_ins op ;
        If (i_1, i_2) |> mk ;
    ]
    let macro_if_cmp (op : Macro.op) (i_1 : t) (i_2 : t) : t list = [
        mk_leaf Compare ;
        op_to_ins op ;
        If (i_1, i_2) |> mk ;
    ]

    let macro_fail : t list = [
        mk_leaf Unit ;
        mk_leaf Failwith ;
    ]

    let macro_assert : t list = [
        If (mk_seq [],  mk_seq macro_fail) |> mk ;
    ]
    let macro_assert_ (op : Macro.op) : t list =
        macro_if op (mk_seq []) (mk_seq macro_fail)
    let macro_assert_cmp (op : Macro.op) : t list =
        macro_if_cmp op (mk_seq []) (mk_seq macro_fail)
    let macro_assert_none : t list = [
        IfNone (mk_seq [], mk_seq macro_fail) |> mk ;
    ]
    let macro_assert_some : t list = [
        IfNone (mk_seq macro_fail, mk_seq []) |> mk ;
    ]
    let macro_assert_left : t list = [
        IfLeft (mk_seq [], mk_seq macro_fail) |> mk ;
    ]
    let macro_assert_right : t list = [
        IfLeft (mk_seq macro_fail, mk_seq []) |> mk ;
    ]
    let macro_dip (n : int) (i : t) : t list =
        assert (n > 0);
        (* Note that `DIIP <code>` expands to `DIP (DIP <code>)`.
            Hence we're generating `n + 1` nested `DIP`s.
        *)
        let rec loop (acc : t) (count : int) : t =
            if count > 0 then loop (Dip acc |> mk) (n - 1)
            else acc
        in
        [ loop (Dip i |> mk) n ]
    let macro_dup (annot : string list) (n : int) : t list =
        assert (n > 0);
        (* Note that `DUUP <code>` expands to `DIP (DUP <code>)`.
            Hence we're generating `n + 1` nested `DIP`s.
        *)
        let rec loop (acc : t) (count : int) : t =
            if count > 0 then loop (Dip acc |> mk) (n - 1)
            else acc
        in
        [ loop (Dup |> mk_leaf ~vars:annot) n ]
end

(* Instruction parser. *)
let rec parse
    (token : string)
    (dtyps: Dtyp.t list)
    (args : t list)
    (annots : string list)
    : t
=
    let full_arity_check =
        (* This function is defined below. *)
        full_arity_check token dtyps args annots
    in
    let inner () =
        match token with
        | "EMPTY_SET" ->
            full_arity_check 1 0 1;
            EmptySet (List.hd dtyps)
        | "EMPTY_MAP" ->
            full_arity_check 2 0 1;
            EmptyMap (List.hd dtyps, List.tl dtyps |> List.hd)
        | "NONE" ->
            full_arity_check 1 0 1;
            Non (List.hd dtyps)
        | "LEFT" ->
            full_arity_check 1 0 1;
            Left (List.hd dtyps)
        | "RIGHT" ->
            full_arity_check 1 0 1;
            Right (List.hd dtyps)
        | "NIL" ->
            full_arity_check 1 0 1;
            Nil (List.hd dtyps)
        | "IF" ->
            full_arity_check 0 2 0;
            If (List.hd args, List.tl args |> List.hd)
        | "IF_NONE" ->
            full_arity_check 0 2 0;
            IfNone (List.hd args, List.tl args |> List.hd)
        | "IF_LEFT" ->
            full_arity_check 0 2 0;
            IfLeft (List.hd args, List.tl args |> List.hd)
        | "IF_RIGHT" ->
            full_arity_check 0 2 0;
            IfRight (List.hd args, List.tl args |> List.hd)
        | "IF_CONS" ->
            full_arity_check 0 2 0;
            IfCons (List.hd args, List.tl args |> List.hd)
        | "LOOP" ->
            full_arity_check 0 1 0;
            Loop (List.hd args)
        | "LOOP_LEFT" ->
            full_arity_check 0 1 0;
            LoopLeft (List.hd args)
        | "DIP" ->
            full_arity_check 0 1 0;
            Dip (List.hd args)
        | "PUSH" ->
            full_arity_check 1 1 1;
            Push (List.hd dtyps, List.hd args)
        | "LAMBDA" ->
            full_arity_check 2 1 1;
            Lambda (List.hd dtyps, List.tl dtyps |> List.hd, List.hd args)
        | "ITER" ->
            full_arity_check 0 1 0;
            Iter (List.hd args)
        | _ -> (
            match leaf_of_string token with
            | Some leaf ->
                var_arity_of_leaf leaf |> full_arity_check 0 0;
                Leaf leaf
            | None -> (
                match parse_macro token dtyps args annots with
                | Some ins -> ins
                | None -> sprintf "unknown instruction `%s`" token |> Exc.throw
            )
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
    |> fun ins -> { ins ; vars = annots }


and ty_arity_check (token : string) (dtyps : Dtyp.t list) (ty_expected : int) : unit =
    Check.arity "type argument" ty_expected (
        fun () -> token
    ) dtyps

and args_arity_check (token : string) (args : t list) (expected : int) : unit =
    Check.arity "instruction" expected (
        fun () -> token
    ) args

and annot_arity_check (token : string) (annots : string list) (expected : int) : unit =
    Check.arity_le "variable annotation" expected (
        fun () -> token
    ) annots

and full_arity_check
    (token : string)
    (dtyps : Dtyp.t list)
    (args : t list)
    (annots : string list)
    (ty_expected : int)
    (ins_expected : int)
    (annot_expected : int)
    : unit
=
    ty_arity_check token dtyps ty_expected;
    args_arity_check token args ins_expected;
    annot_arity_check token annots annot_expected

and parse_macro_cmp
    (token : string)
    (dtyps: Dtyp.t list)
    (args : t list)
    (annots : string list)
    : t ins option
=
    Macro.Parse.prefixed_op
        (
            fun () -> [
                sprintf "token `%s` looks like a CMP-like macro, but is not" token ;
                sprintf "please refer to https://tezos.gitlab.io/master/whitedoc/michelson.html#compare" ;
            ]
        )
        "CMP"
        (fun op ->
            full_arity_check token dtyps args annots 0 0 0;
            let expanded = Expand.macro_cmp op in
            let macro = Macro.Cmp op in
            Macro (expanded, macro)
        )
        token

and parse_macro_if
    (token : string)
    (dtyps: Dtyp.t list)
    (args : t list)
    (annots : string list)
    : t ins option
=
    Macro.Parse.prefixed_op
        (
            fun () -> [
                sprintf "token `%s` looks like an IF-like macro, but is not" token ;
                sprintf "please refer to https://tezos.gitlab.io/master/whitedoc/michelson.html#compare" ;
            ]
        )
        "IF"
        (fun op ->
            full_arity_check token dtyps args annots 0 2 0;
            let (bt, bf) = (List.hd args, List.tl args |> List.hd) in
            let expanded = Expand.macro_if op bt bf in
            let macro = Macro.If (op, bt, bf) in
            Macro (expanded, macro)
        )
        token

and parse_macro_if_cmp
    (token : string)
    (dtyps: Dtyp.t list)
    (args : t list)
    (annots : string list)
    : t ins option
=
    Macro.Parse.prefixed_op
        (
            fun () -> [
                sprintf "token `%s` looks like an IFCMP-like macro, but is not" token ;
                sprintf "please refer to https://tezos.gitlab.io/master/whitedoc/michelson.html#compare" ;
            ]
        )
        "IFCMP"
        (fun op ->
            full_arity_check token dtyps args annots 0 2 0;
            let (bt, bf) = (List.hd args, List.tl args |> List.hd) in
            let expanded = Expand.macro_if op bt bf in
            let macro = Macro.IfCmp (op, bt, bf) in
            Macro (expanded, macro)
        )
        token

and parse_macro_fail
    (token : string)
    (dtyps: Dtyp.t list)
    (args : t list)
    (annots : string list)
    : t ins option
=
    if token = "FAIL" then (
        full_arity_check token dtyps args annots 0 0 0;
        Some (Macro (Expand.macro_fail, Macro.Fail))
    ) else None

(* Parses (variations of) the assert macro. *)
and parse_macro_assert
    (token : string)
    (dtyps: Dtyp.t list)
    (args : t list)
    (annots : string list)
    : t ins option
=
    (* Parameter is the kind of assert the token looks like: `ASSERT`, `ASSERT_CMP` ... *)
    let bail (branch : string) : 'a =
        [
            sprintf "token `%s` looks like an %s-like macro, but is not" token branch ;
            sprintf "please refer to https://tezos.gitlab.io/master/whitedoc/michelson.html#assertion-macros" ;
        ] |> Exc.throws
    in
    (* Calls `Macro.Parse.op` on `s` minus the `n` first characters. *)
    let op_of_suff (s : string) (n : int) : Macro.op option =
        String.sub s n ((String.length s) - n) |> Macro.Parse.op
    in
    (* Performs arity checks and wraps into `Some`. *)
    let terminate (expansion : t list) (res : t Macro.t) : t ins option =
        full_arity_check token dtyps args annots 0 0 0;
        Some (Macro (expansion, res))
    in
    match Macro.Parse.tail_of_pref "ASSERT" token with
    | None -> None
    | Some "" -> terminate Expand.macro_assert Macro.Assert
    | Some tail -> (
        if String.get tail 0 <> '_' then (
            bail "ASSERT" |> ignore
        );
        let tail = String.sub tail 1 ((String.length tail) - 1) in
        match Macro.Parse.op tail with
        | Some op -> Macro.Assert_ op |> terminate (Expand.macro_assert_ op)
        | None -> (
            let tail_len = String.length tail in
            let (expansion, macro) : (t list * t Macro.t) =
                if tail_len < 3 then (
                    bail "ASSERT_"
                ) else if "CMP" = String.sub tail 0 3 then (
                    match op_of_suff tail 3 with
                    | Some op ->
                        Expand.macro_assert_cmp op, Macro.AssertCmp op
                    | None -> bail "ASSERT_CMP{CMP}"
                ) else if tail_len < 4 then (
                    bail "ASSERT_"
                ) else if "NONE" = String.sub tail 0 4 then (
                    if tail = "NONE" then
                        Expand.macro_assert_none, Macro.AssertNone
                    else bail "ASSERT_NONE"
                ) else if "SOME" = String.sub tail 0 4 then (
                    if tail = "SOME" then
                        Expand.macro_assert_some, Macro.AssertSome
                    else bail "ASSERT_SOME"
                ) else if "LEFT" = String.sub tail 0 4 then (
                    if tail = "LEFT" then
                        Expand.macro_assert_left, Macro.AssertLeft
                    else bail "ASSERT_LEFT"
                ) else if tail_len < 5 then (
                    bail "ASSERT_"
                ) else if "RIGHT" = String.sub tail 0 5 then (
                    if tail = "RIGHT" then
                        Expand.macro_assert_right, Macro.AssertRight
                    else bail "ASSERT_RIGHT"
                ) else bail "ASSERT_"
            in
            terminate expansion macro
        )
    )

(* Factors common code between `parse_macro_dip` and `parse_macro_dup`.

    Parses strings of the form `<pref><rep>*<finish>`. Returns the number of times `rep` appears in
    the token.
*)
and parse_macro_diup
    (pref : string)
    (rep : char)
    (finish : char)
    (token : string)
    : int option
=
    let bail () : int =
        [
            sprintf "token `%s` looks like a %s%c+%c-like macro, but is not" token pref rep finish ;
            sprintf "please refer to https://tezos.gitlab.io/master/whitedoc/michelson.html#syntactic-conveniences" ;
        ] |> Exc.throws
    in
    match Macro.Parse.tail_of_pref pref token with
    | None -> None
    | Some tail -> (
        let tail_len = String.length tail in
        let rec count (n : int) : int =
            if n < tail_len then (
                let next = String.get tail n in
                if next = rep then (
                    n + 1 |> count
                ) else if next = finish then (
                    (* Anything else in the string? *)
                    if n + 1 = tail_len then (
                        n
                    ) else (
                        bail ()
                    )
                ) else (
                    bail ()
                )
            ) else (
                bail ()
            )
        in
        Some (count 0)
    )

and parse_macro_dip
    (token : string)
    (dtyps: Dtyp.t list)
    (args : t list)
    (annots : string list)
    : t ins option
=
    match parse_macro_diup "DI" 'I' 'P' token with
    | None -> None
    | Some i_count -> (
        if i_count = 0 then (
            Exc.throw "entered unreachable code: encountered `DIP` while parsing a `DII+P`"
        );
        full_arity_check token dtyps args annots 0 1 0;
        let ins = List.hd args in
        let expanded = Expand.macro_dip i_count ins in
        let macro = Macro.Dip (i_count, ins) in
        Some (Macro (expanded, macro))
    )

and parse_macro_dup
    (token : string)
    (dtyps: Dtyp.t list)
    (args : t list)
    (annots : string list)
    : t ins option
=
    match parse_macro_diup "DU" 'U' 'P' token with
    | None -> None
    | Some u_count -> (
        if u_count = 0 then (
            Exc.throw "entered unreachable code: encountered `DUP` while parsing a `DUU+P`"
        );
        full_arity_check token dtyps args annots 0 0 1;
        let expanded = Expand.macro_dup annots u_count in
        let macro = Macro.Dup u_count in
        Some (Macro (expanded, macro))
    )

and parse_macro_pair
    (token : string)
    (dtyps: Dtyp.t list)
    (args : t list)
    (annots : string list)
    : t ins option
=
    match Macro.Parse.tail_of_pref "P" token with
    | None -> None
    | Some tail -> (
        match Macro.Parse.pair_ops tail with
        | ops, "R" -> (
            full_arity_check token dtyps args annots 0 0 1;
            let macro = Macro.P ops in
            Some (Macro ([], macro))
        )
        | _ ->
            [
                sprintf "token `%s` looks like a P[API]+R-like macro, but is not" token ;
                sprintf "please refer to https://tezos.gitlab.io/master/whitedoc/michelson.html#syntactic-conveniences" ;
            ] |> Exc.throws
    )

and parse_macro_unpair
    (token : string)
    (dtyps: Dtyp.t list)
    (args : t list)
    (annots : string list)
    : t ins option
=
    match Macro.Parse.tail_of_pref "UNP" token with
    | None -> None
    | Some tail -> (
        match Macro.Parse.pair_ops tail with
        | ops, "R" -> (
            full_arity_check token dtyps args annots 0 0 1;
            let macro = Macro.Unp ops in
            Some (Macro ([], macro))
        )
        | _ ->
            [
                sprintf "token `%s` looks like a UNP[API]+R-like macro, but is not" token ;
                sprintf "please refer to https://tezos.gitlab.io/master/whitedoc/michelson.html#syntactic-conveniences" ;
            ] |> Exc.throws
    )

and parse_macro_cadr
    (token : string)
    (dtyps: Dtyp.t list)
    (args : t list)
    (annots : string list)
    : t ins option
=
    match Macro.Parse.tail_of_pref "C" token with
    | None -> None
    | Some tail -> (
        match Macro.Parse.unpair_ops tail with
        | ops, "R" -> (
            full_arity_check token dtyps args annots 0 0 1;
            let macro = Macro.CadR ops in
            Some (Macro ([], macro))
        )
        | _ ->
            [
                sprintf "token `%s` looks like a C[AD]+R-like macro, but is not" token ;
                sprintf "please refer to https://tezos.gitlab.io/master/whitedoc/michelson.html#syntactic-conveniences" ;
            ] |> Exc.throws
    )

and parse_macro_set_cadr
    (token : string)
    (dtyps: Dtyp.t list)
    (args : t list)
    (annots : string list)
    : t ins option
=
    match Macro.Parse.tail_of_pref "SET_C" token with
    | None -> None
    | Some tail -> (
        match Macro.Parse.unpair_ops tail with
        | ops, "R" -> (
            full_arity_check token dtyps args annots 0 0 1;
            let macro = Macro.SetCadr ops in
            Some (Macro ([], macro))
        )
        | _ ->
            [
                sprintf "token `%s` looks like a SET_C[AD]+R-like macro, but is not" token ;
                sprintf "please refer to https://tezos.gitlab.io/master/whitedoc/michelson.html#syntactic-conveniences" ;
            ] |> Exc.throws
    )

and parse_macro_map_cadr
    (token : string)
    (dtyps: Dtyp.t list)
    (args : t list)
    (annots : string list)
    : t ins option
=
    match Macro.Parse.tail_of_pref "MAP_C" token with
    | None -> None
    | Some tail -> (
        match Macro.Parse.unpair_ops tail with
        | ((_ :: _) as ops), "R" -> (
            full_arity_check token dtyps args annots 0 0 1;
            let macro = Macro.SetCadr ops in
            Some (Macro ([], macro))
        )
        | _ ->
            [
                sprintf "token `%s` looks like a MAP_C[AD]+R-like macro, but is not" token ;
                sprintf "please refer to https://tezos.gitlab.io/master/whitedoc/michelson.html#syntactic-conveniences" ;
            ] |> Exc.throws
    )

and parse_macro_if_some
    (token : string)
    (dtyps: Dtyp.t list)
    (args : t list)
    (annots : string list)
    : t ins option
=
    if token = "IF_SOME" then (
        full_arity_check token dtyps args annots 0 2 0;
        let macro = Macro.IfSome (List.hd args, List.tl args |> List.hd) in
        Some (Macro ([], macro))
    ) else (
        None
    )

and parse_macro_if_none
    (token : string)
    (dtyps: Dtyp.t list)
    (args : t list)
    (annots : string list)
    : t ins option
=
    if token = "IF_NONE" then (
        full_arity_check token dtyps args annots 0 2 0;
        let macro = Macro.IfSome (List.hd args, List.tl args |> List.hd) in
        Some (Macro ([], macro))
    ) else (
        None
    )

(* Macro parser. *)
and parse_macro
    (token : string)
    (dtyps: Dtyp.t list)
    (args : t list)
    (annots : string list)
    : t ins option
=
    let all : (unit -> t ins option) list =
        [
            (fun () -> parse_macro_cmp token dtyps args annots) ;
            (fun () -> parse_macro_if_some token dtyps args annots) ;
            (fun () -> parse_macro_if_none token dtyps args annots) ;
            (fun () -> parse_macro_if_cmp token dtyps args annots) ;
            (fun () -> parse_macro_if token dtyps args annots) ;
            (fun () -> parse_macro_fail token dtyps args annots) ;
            (fun () -> parse_macro_assert token dtyps args annots) ;
            (fun () -> parse_macro_dip token dtyps args annots) ;
            (fun () -> parse_macro_dup token dtyps args annots) ;
            (fun () -> parse_macro_pair token dtyps args annots) ;
            (fun () -> parse_macro_unpair token dtyps args annots) ;
            (fun () -> parse_macro_cadr token dtyps args annots) ;
            (fun () -> parse_macro_set_cadr token dtyps args annots) ;
            (fun () -> parse_macro_map_cadr token dtyps args annots) ;
        ]
    in
    let rec loop = function
        | [] -> None
        | parse :: tail -> (
            match parse () with
            | Some macro ->
                log_1 "parsed a macro: @[%a@]@." fmt { ins = macro ; vars = annots };
                Some macro
            | None -> loop tail
        )
    in
    loop all
