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
    | IfSome of 'ins * 'ins
    | Int
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
    | SetCadr of unpair_op list
    | MapCadr of unpair_op list * 'ins

    let fmt
        (fmt_ins : (formatter -> 'ins -> unit) option)
        (fmt : formatter)
        (t : 'ins t)
        : unit
    =
        let fmt_ins =
            match fmt_ins with
            | Some fmt_ins -> fmt_ins
            | None -> fun fmt _ -> fprintf fmt "..."
        in
        match t with
        | Cmp op -> fprintf fmt "CMP%a" fmt_op op
        | If (op, _, _) ->
            fprintf fmt "IF%a ..." fmt_op op
        | IfCmp (op, _, _) ->
            fprintf fmt "IFCMP%a ..." fmt_op op
        | IfSome _ ->
            fprintf fmt "IF_SOME ..."

        | Int -> fprintf fmt "INT"
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
            fprintf fmt "P ";
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

type hash_fun =
| B58Check
| Blake2B
| Sha256
| Sha512

let fmt_hash_fun (fmt : formatter) (h : hash_fun) : unit =
    match h with
    | B58Check -> fprintf fmt "HASH_KEY"
    | Blake2B -> fprintf fmt "BLAKE2B"
    | Sha256 -> fprintf fmt "SHA256"
    | Sha512 -> fprintf fmt "SHA512"

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
| Source
| Sender
| Self
| Amount
| ImplicitAccount
| StepsToQuota
| Now
| Pack
| Slice
| Address
| Hash of hash_fun
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
| Source -> fprintf fmt "SOURCE"
| Sender -> fprintf fmt "SENDER"
| Self -> fprintf fmt "SELF"
| Amount -> fprintf fmt "AMOUNT"
| ImplicitAccount -> fprintf fmt "IMPLICIT_ACCOUNT"
| StepsToQuota -> fprintf fmt "STEPS_TO_QUOTA"
| Now -> fprintf fmt "NOW"
| Pack -> fprintf fmt "PACK"
| Slice -> fprintf fmt "SLICE"
| Address -> fprintf fmt "ADDRESS"
| Hash h -> fmt_hash_fun fmt h
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
| "SOURCE" -> Some Source
| "SENDER" -> Some Sender
| "SELF" -> Some Self
| "AMOUNT" -> Some Amount
| "IMPLICIT_ACCOUNT" -> Some ImplicitAccount
| "STEPS_TO_QUOTA" -> Some StepsToQuota
| "NOW" -> Some Now
| "PACK" -> Some Pack
| "SLICE" -> Some Slice
| "HASH_KEY" -> Some (Hash B58Check)
| "BLAKE2B" -> Some (Hash Blake2B)
| "SHA256" -> Some (Hash Sha256)
| "SHA512" -> Some (Hash Sha512)
| "CHECK_SIGNATURE" -> Some CheckSignature
| "RENAME" -> Some Rename
| _ -> None

let annot_arity_of_leaf (leaf : leaf) : (int * int * int) = match leaf with
(* Supports nothing. *)
| Failwith
| Swap
| Drop -> (0, 0, 0)

(* One variable annotation, one field annotation. *)
| Car
| Cdr
| Cons
| Som
| Address -> (0, 1, 1)

(* One variable annotation, two field annotations. *)
| Pair -> (0, 1, 2)

(* Others. *)
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
| TransferTokens
| SetDelegate
| Balance
| Source
| Sender
| Self
| Amount
| ImplicitAccount
| StepsToQuota
| Now
| Pack
| Slice
| Hash _
| CreateAccount
| CheckSignature
| Rename -> (0, 1, 0)

type extension =
| GetStorage of Dtyp.t
| GetBalance
| ApplyOps
| PrintStack
| MustFail of Dtyp.t
| Step of string option
| SetSource of t
| SpawnContract of Dtyp.t

and 'sub ins =
| Leaf of leaf
| Cast of Dtyp.t
| EmptySet of Dtyp.t
| EmptyMap of Dtyp.t * Dtyp.t
| Unpack of Dtyp.t
| Non of Dtyp.t
| Left of Dtyp.t
| Right of Dtyp.t
| Nil of Dtyp.t
| Contract of Dtyp.t
| Seq of 'sub list
| If of 'sub * 'sub
| Loop of 'sub
| LoopLeft of 'sub
| Dip of 'sub
| Push of Dtyp.t * const
| Lambda of Dtyp.t * Dtyp.t * 'sub
| Iter of 'sub
| Map of Dtyp.t * 'sub
| IfNone of 'sub * 'sub
| IfLeft of 'sub * 'sub
| IfRight of 'sub * 'sub
| IfCons of 'sub * 'sub
| CreateContract of (contract option, string) Either.t
| Macro of 'sub list * 'sub Macro.t
| Extension of extension

and const =
| U

| Bool of bool
| Int of string
| Str of string
| Bytes of string

| Cont of contract

| Lft of const
| Rgt of const

| No
| So of const

| Pr of const * const
| Lst of const list
| Mapping of (const * const) list

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
    comments : string list ;
}

let mk_str_const (s : string) : const =
    let rec loop (acc : string list) (ss : string list) : string =
        match ss with
        | [] -> List.rev acc |> String.concat ""
        | "" :: tail -> loop ("\\" :: acc) tail
        | head :: tail -> loop (head :: acc) tail
    in
    Str (
        String.split_on_char '\\' s
        |> (
            function
            | "" :: tail -> loop [] tail
            | head :: tail -> loop [head] tail
            | [] -> ""
        )
    )

let nu_mk ?annot:(annot=None) ?comments:(comments=[]) (ins : t ins) : t =
    let vars, fields, typs =
        match annot with
        | None -> [], [], []
        | Some annot -> annot.Annot.vars, annot.Annot.fields, annot.Annot.typs
    in
    { vars ; fields ; typs ; ins ; comments }

let mk
    ?vars:(vars=[])
    ?fields:(fields=[])
    ?typs:(typs=[])
    ?comments:(comments=[])
    (ins : t ins)
    : t
= { ins ; vars ; fields ; typs ; comments }

let mk_contract ~(storage : Dtyp.t) ~(param : Dtyp.t) (entry : t) : contract =
    { storage ; param ; entry }

let invisible_get_one (desc : string) (l : 'a list) : 'a =
    if l = [] then (
        sprintf "no `%s` field found" desc |> Exc.throw
    ) else if List.length l > 1 then (
        sprintf "more than one `%s` field found" desc |> Exc.throw
    );
    List.hd l
let mk_contract_of_lists
    ~(storage : Dtyp.t list)
    ~(param : Dtyp.t list)
    (entry : t list)
    : contract
=
    {
        storage = invisible_get_one "storage type" storage ;
        param = invisible_get_one "entry parameter" param ;
        entry = invisible_get_one "entry code" entry
    }

let nu_mk_leaf ?annot:(annot=None) ?comments:(comments=[]) (leaf : leaf) : t =
    Leaf leaf |> nu_mk ~annot ~comments

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

let unit_contract : contract =
    let dtyp = Dtyp.mk_leaf Dtyp.Unit in
    let entry =
        mk_seq [
            Drop |> mk_leaf ;
            Unit |> mk_leaf ;
            Nil (Dtyp.mk_leaf Dtyp.Operation) |> mk ;
            Pair |> mk_leaf ;
        ]
    in
    mk_contract ~storage:(dtyp) ~param:(dtyp) entry

(* Formats an IF-like instruction. *)
let fmt_if_like
    (fmt : formatter)
    (key : string)
    (bt : t)
    (bf : t)
    (stack : ( (Fmt.sep * t * bool) list * Fmt.seq_end) list)
=
    fprintf fmt "%s @[<v>" key;
    (
        [ (ignore, bt, true) ; (Fmt.sep_brk fmt, bf, true) ],
        Fmt.cls fmt
    ) :: stack

(* True if the instruction needs parens when used as an argument. *)
let needs_parens (t : t) : bool =
    match t.ins with
    | Leaf _ -> t.typs <> [] || t.vars <> [] || t.fields <> []
    | Seq _ -> false
    | _ -> true

let rec fmt_extension
    ?annots:(annots = fun (_ : formatter) () -> ())
    (fmtt : formatter)
    (e : extension)
    : unit
=
    match e with
    | GetStorage dtyp -> fprintf fmtt "GET_STORAGE%a %a" annots () Dtyp.fmt dtyp
    | GetBalance -> fprintf fmtt "GET_BALANCE%a" annots ()
    | ApplyOps -> fprintf fmtt "APPLY_OPERATIONS%a" annots ()
    | PrintStack -> fprintf fmtt  "PRINT_STACK%a" annots ()
    | MustFail dtyp -> fprintf fmtt "MUST_FAIL%a %a" annots () Dtyp.fmt dtyp
    | Step opt -> unwrap_or "<none>" opt |> fprintf fmtt "STEP %s"
    | SetSource code -> fprintf fmtt "SET_SOURCE%a %a" annots () fmt code
    | SpawnContract dtyp -> fprintf fmtt "SPAWN_CONTRACT%a %a" annots () Dtyp.fmt dtyp

(* Formats contracts. *)
and fmt_contract (fmtt : formatter) ({ storage ; param ; entry } : contract) : unit =
    fprintf
        fmtt "@[@[<v 4>{@ storage %a ;@ parameter %a ;@ code @[%a@] ;@]@,}@]"
        Dtyp.fmt storage Dtyp.fmt param fmt entry

(*  Formats constants.

    # TODO

    - stackless
*)
and fmt_const (fmtt : formatter) (c : const) : unit =
    match c with
    | U -> fprintf fmtt "Unit"
    | Bool b -> fprintf fmtt (if b then "True" else "False")
    | Int n -> fprintf fmtt "%s" n
    | Str s -> fprintf fmtt "%S" s
    | Bytes s -> fprintf fmtt "0x%s" s
    | Cont c -> fmt_contract fmtt c
    | Lft c -> fprintf fmtt "(Left %a)" fmt_const c
    | Rgt c -> fprintf fmtt "(Right %a)" fmt_const c
    | No -> fprintf fmtt "None"
    | So c -> fprintf fmtt "(Some %a)" fmt_const c
    | Pr (fst, snd) -> fprintf fmtt "(Pair %a %a)" fmt_const fst fmt_const snd
    | Lst l -> (
        fprintf fmtt "{";
        l |> List.iter (fprintf fmtt " %a ;" fmt_const);
        fprintf fmtt " }"
    )
    | Mapping map -> (
        fprintf fmtt "{";
        map |> List.iter (fun (k, v) -> fprintf fmtt " Elt %a %a ;" fmt_const k fmt_const v);
        fprintf fmtt " }"
    )

(* Formats instructions.

    Strictly speaking, this function is not tailrec. Macro formatting takes the instruction
    formatter as a parameter, hence when printing macros we need to pass the `fmt` below so
    that it knows how to format instructions. Hence this function consumes stack whenever it
    goes down a macro.
*)
and fmt (fmtt : formatter) (t : t) : unit =
    let rec loop (stack : ( (Fmt.sep * t * bool) list * Fmt.seq_end) list) : unit =
        match stack with
        | [] -> ()
        | ( [], seq_end) :: tail ->
            seq_end ();
            loop tail
        | ( ((sep, to_do, sub) :: to_do_tail, seq_end) :: tail ) ->
            sep ();
            let tail = (to_do_tail, seq_end) :: tail in

            let paren = sub && needs_parens to_do in
            let tail =
                if paren then (
                    fprintf fmtt "(";
                    ([], Fmt.cls_paren fmtt) :: tail
                ) else tail
            in

            let { ins = to_do ; vars ; fields ; typs ; comments } = to_do in
            let _ =
                fprintf fmtt "@[<v>";
                comments |> List.iter (
                    fprintf fmtt "# %s@,"
                );
                fprintf fmtt "@]"
            in
            let fmt_annots fmtt () : unit =
                Annot.fmt_typs fmtt typs;
                Annot.fmt_vars fmtt vars;
                Annot.fmt_fields fmtt fields
            in
            let tail =
                match to_do with

                | Cast dtyp ->
                    fprintf fmtt "CAST%a %a" fmt_annots () Dtyp.fmt dtyp;
                    tail
                | EmptySet dtyp ->
                    fprintf fmtt "EMPTY_SET%a %a" fmt_annots () Dtyp.fmt dtyp;
                    tail
                | EmptyMap (k, v) ->
                    fprintf fmtt "EMPTY_MAP%a %a %a" fmt_annots () Dtyp.fmt k Dtyp.fmt v;
                    tail
                | Unpack dtyp ->
                    fprintf fmtt "UNPACK%a %a" fmt_annots () Dtyp.fmt dtyp;
                    tail
                | Non dtyp ->
                    fprintf fmtt "NONE%a %a" fmt_annots () Dtyp.fmt dtyp;
                    tail
                | Left dtyp ->
                    fprintf fmtt "LEFT%a %a" fmt_annots () Dtyp.fmt dtyp;
                    tail
                | Right dtyp ->
                    fprintf fmtt "RIGHT%a %a" fmt_annots () Dtyp.fmt dtyp;
                    tail
                | Nil dtyp ->
                    fprintf fmtt "NIL%a %a" fmt_annots () Dtyp.fmt dtyp;
                    tail
                | Contract dtyp ->
                    fprintf fmtt "CONTRACT %a" Dtyp.fmt dtyp;
                    tail

                | Leaf leaf ->
                    fmt_leaf fmtt leaf;
                    fmt_annots fmtt ();
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
                            (Fmt.sep_brk fmtt, hd, false) ::  (
                                tl |> List.map (
                                    fun to_do -> (fun () -> fprintf fmtt " ;@,"), to_do, false
                                )
                            ),
                            fun () -> fprintf fmtt "@]@,}@]"
                        ) :: tail
                )

                | Loop t ->
                    assert (vars = []);
                    fprintf fmtt "@[<v 2>LOOP@,";
                    ([ignore, t, true], Fmt.cls fmtt) :: tail

                | LoopLeft t ->
                    assert (vars = []);
                    fprintf fmtt "@[<v 2>LOOP_LEFT@,";
                    ([ignore, t, true], Fmt.cls fmtt) :: tail

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
                    fprintf fmtt "@[<hv 2>DIP%a@ " fmt_annots ();
                    ([ignore, t, true], fun () -> fprintf fmtt "@]") :: tail

                | Iter t ->
                    assert (vars = []);
                    fprintf fmtt "ITER ";
                    ([ignore, t, true], ignore) :: tail

                | Map (_, t) ->
                    assert (vars = []);
                    fprintf fmtt "MAP ";
                    ([ignore, t, true], ignore) :: tail

                | Push (dtyp, c) ->
                    fprintf
                        fmtt "PUSH%a @[<hov>@[%a@]@ %a@]"
                        fmt_annots () Dtyp.fmt dtyp fmt_const c;
                    tail

                | Lambda (dom, codom, t) ->
                    fprintf
                        fmtt "LAMBDA%a @[<v>@[%a@]@,(@[%a@])@,("
                        fmt_annots () Dtyp.fmt dom Dtyp.fmt codom;
                    (
                        [ignore, t, true], fun () -> fprintf fmtt ")@]"
                    ) :: tail

                | CreateContract c -> (
                    fprintf fmtt "@[@[<v 4>CREATE_CONTRACT%a" fmt_annots ();
                    match c with
                    | Either.Lft None ->
                        fprintf fmtt "@]@]";
                        tail
                    | Either.Lft (Some c) ->
                        fprintf
                            fmtt " {@ storage %a;@ parameter %a;@ code "
                            Dtyp.fmt c.storage Dtyp.fmt c.param;
                        (
                            [ignore, c.entry, true], fun () -> fprintf fmtt ";@]@,}@]"
                        ) :: tail
                    | Either.Rgt name ->
                        fprintf fmtt " \"%s\"@]@]" name;
                        tail
                )

                | Macro (ts, macro) -> (
                    match ts with
                    | [] ->
                        fprintf fmtt "@[{ # Expansion of `%a%a`@,}@]"
                            (Macro.fmt None) macro fmt_annots ();
                        tail
                    | hd :: tl ->
                        fprintf fmtt "@[<v>@[<v 2>{ # Expansion of `%a%a`@,"
                            (Macro.fmt None) macro fmt_annots ();
                        (
                            (ignore, hd, false)
                            :: (
                                tl |> List.map (
                                    fun to_do -> (fun () -> fprintf fmtt " ;@,"), to_do, false
                                )
                            ),
                            fun () -> fprintf fmtt "@]@,}@]"
                        ) :: tail
                )

                | Extension ext ->
                    fmt_extension ~annots:fmt_annots fmtt ext;
                    tail
            in
            loop tail
    in
    loop [ [ignore, t, false], ignore ]

let typ_of_contract ?alias:(alias = None) (c : contract) : Dtyp.t =
    Dtyp.Contract c.param |> Dtyp.mk ~alias

let comments (blah : string list) (self : t) : t =
    { self with comments = self.comments @ blah }
