(** Michelson Parser. *)

%{
    module ParseMic = Mic
    module ParseDtyp = Dtyp

    open Base
    open Common

    let named_dtyp
        ?alias:(alias=None)
        ?name:(name=None)
        (dtyp : Dtyp.dtyp)
        : Dtyp.named
    =
        Dtyp.mk ~alias dtyp |> Dtyp.mk_named name

    (** Fails if the name of the named datatype is not none. *)
    let strip_dtyp_name (dtyp : Dtyp.named) : Dtyp.t =
        match dtyp.name with
        | None -> dtyp.inner
        | Some annot -> (
            asprintf
                "unexpected field annotation %a on datatype %a"
                Annot.Field.fmt annot Dtyp.fmt dtyp.inner
            |> Exc.throw
        )

    (** Converts a `[AIP]+` string in a list of pair operations. *)
    let aip_to_ops (s : string) : Mic.Macro.pair_op list =
        let ops : Mic.Macro.pair_op list ref = ref [] in
        s |> String.iter (
            function
            | 'P' -> ops := Mic.Macro.P :: !ops
            | 'A' -> ops := Mic.Macro.A :: !ops
            | 'I' -> ops := Mic.Macro.I :: !ops
            | c ->
                sprintf "illegal character '%c' found in `(UN)P[AIP]+R` macro" c
                |> Exc.throw
        );
        Mic.Macro.P :: List.rev !ops

    (** Converts a `[AD]+` string in a list of unpair operations. *)
    let ad_to_ops (s : string) : Mic.Macro.unpair_op list =
        let ops : Mic.Macro.unpair_op list ref = ref [] in
        s |> String.iter (
            function
            | 'A' -> ops := Mic.Macro.A :: !ops
            | 'D' -> ops := Mic.Macro.D :: !ops
            | c ->
                sprintf "illegal character '%c' found in `(SET|MAP)C[AD]+R` macro" c
                |> Exc.throw
        );
        List.rev !ops
%}

%token <Base.Annot.Typ.t> COLANNOT
%token <Base.Annot.Var.t> ATANNOT
%token <Base.Annot.Field.t> PERANNOT
%token OCURL CCURL
%token OPAR CPAR
%token SEMICOL
%token PARAMETER STORAGE CODE
%token EOF

(* Tokens for datatypes. *)
%token
    T_STR T_NAT T_INT T_BYTES T_BOOL T_UNIT T_MUTEZ T_ADDRESS
    T_OPERATION T_KEY T_KEYH T_SIG T_TSTAMP
    T_PAIR T_OR T_OPTION T_LIST T_SET T_CONTRACT T_MAP T_BIGMAP T_LAMBDA

(* Tokens for constants. *)
%token C_UNIT
%token <bool> C_BOOL
%token <string> C_INT C_STR C_BYTES
%token C_NONE C_SOME C_LEFT C_RIGHT C_PAIR

(* Tokens for instructions. *)
%token
    I_FAILWITH I_EXEC I_DROP I_DUP I_SWAP I_UNIT I_EQ I_NEQ I_LT I_LE I_GT I_GE I_OR I_AND I_XOR
    I_NOT I_NEG I_ABS I_ADD I_SUB I_MUL I_EDIV I_LSL I_LSR I_COMPARE I_CONCAT I_SIZE I_PAIR I_CAR
    I_CDR I_GET I_MEM I_UPDATE I_SOME I_CONS I_CREATE_ACCOUNT I_ADDRESS I_TRANSFER_TOKENS
    I_SET_DELEGATE I_BALANCE I_SOURCE I_SENDER I_SELF I_AMOUNT I_IMPLICIT_ACCOUNT I_STEPS_TO_QUOTA
    I_NOW I_PACK I_UNPACK I_SLICE I_CHECK_SIGNATURE I_RENAME

    I_INT I_NAT I_CAST I_EMPTY_SET I_EMPTY_MAP I_NONE I_LEFT I_RIGHT I_NIL I_CONTRACT I_IF I_LOOP
    I_LOOP_LEFT I_DIP I_PUSH I_LAMBDA I_ITER I_MAP I_IF_NONE I_IF_LEFT I_IF_RIGHT I_IF_CONS
    I_CREATE_CONTRACT

    I_STORAGE_OF I_BALANCE_OF I_APPLY_OPERATIONS I_PRINT_STACK I_MUST_FAIL I_STEP I_SET_SOURCE
%token <Base.Mic.hash_fun> I_HASH

(* Tokens for macros. *)

%token
    I_M_ASSERT_NONE I_M_ASSERT_SOME I_M_ASSERT_LEFT I_M_ASSERT_RIGHT I_M_INT I_M_FAIL I_M_ASSERT
    I_M_IF_SOME
%token <int> I_M_DUUP I_M_DIIP
%token <Base.Mic.Macro.op> I_M_ASSERT_ I_M_ASSERT_CMP I_M_CMP I_M_IF I_M_IF_CMP
%token <string> I_M_PAIR I_M_UNPAIR I_M_CADR I_M_SET_CADR I_M_MAP_CADR

%start <string -> Base.Common.Source.t -> Base.Contract.t> just_contract
%start <Base.Mic.t> just_mic
%%

just_contract :
    | c = contract
    ; EOF {
        fun (name : string) (source : Common.Source.t) ->
            Contract.mk
                ~storage:(c.Mic.storage)
                ~entry_param:(c.Mic.param)
                name
                (Some source)
                c.Mic.entry
                None
    }

just_mic :
    | ins = instruction
    ; EOF { ins }

contract :
    | sub = contract_sub {
        let (s, p, e) = sub in
        Mic.mk_contract_of_lists ~storage:(s) ~param:(p) e
    }
;

contract_sub :
    | sub = contract_sub
    ; STORAGE
    ; storage = datatype
    ; SEMICOL {
        let (s, p ,e) = sub in
        (storage :: s), p, e
    }
    | sub = contract_sub
    ; PARAMETER
    ; param = datatype
    ; SEMICOL {
        let (s, p ,e) = sub in
        s, (param :: p), e
    }
    | sub = contract_sub
    ; CODE
    ; entry = instruction
    ; SEMICOL {
        let (s, p ,e) = sub in
        s, p, (entry :: e)
    }
    | { [], [], [] }

instructions_semicol :
    | i = instruction
    ; SEMICOL
    ; tail = instructions_semicol {
        i :: tail
    }
    | i = instruction {
        [i]
    }
    | SEMICOL {
        []
    }
    | { [] }
;

instruction :
    (* Sequence of instructions. *)
    | OCURL
    ; mic = instructions_semicol
    ; CCURL {
        Mic.Seq mic |> Mic.mk
    }

    (* Leaf instructions need nothing. *)
    | leaf = instruction_leaf
    ; annots = annotations {
        let { Annot.typs ; Annot.vars ; Annot.fields } = annots in
        Mic.mk ~typs ~vars ~fields leaf
    }

    (* Instructions that take one datatype argument. *)
    | mic_of_dtyp = instruction_dtyp
    ; annots = annotations
    ; dtyp = datatype {
        let { Annot.typs ; Annot.vars ; Annot.fields } = annots in
        mic_of_dtyp dtyp |> Mic.mk ~typs ~vars ~fields
    }

    (* Instructions that take two datatype arguments. *)
    | mic_of_dtyps = instruction_dtyp_2
    ; annots = annotations
    ; dtyp_1 = datatype
    ; dtyp_2 = datatype {
        let { Annot.typs ; Annot.vars ; Annot.fields } = annots in
        mic_of_dtyps dtyp_1 dtyp_2 |> Mic.mk ~typs ~vars ~fields
    }

    (* Instructions that take two datatype arguments and one code argument. *)
    | mic_of_dtyps_code = instruction_dtyp_2_code
    ; annots = annotations
    ; dtyp_1 = datatype
    ; dtyp_2 = datatype
    ; code = instruction {
        let { Annot.typs ; Annot.vars ; Annot.fields } = annots in
        mic_of_dtyps_code dtyp_1 dtyp_2 code |> Mic.mk ~typs ~vars ~fields
    }

    (* Instructions that take a datatype and a constant. *)
    | mic_of_dtyp_const = instruction_dtyp_const
    ; annots = annotations
    ; dtyp = datatype
    ; c = const {
        let { Annot.typs ; Annot.vars ; Annot.fields } = annots in
        mic_of_dtyp_const dtyp c |> Mic.mk ~typs ~vars ~fields
    }

    (* Instructions that take one code argument. *)
    | mic_of_code = instruction_code
    ; annots = annotations
    ; code = instruction {
        let { Annot.typs ; Annot.vars ; Annot.fields } = annots in
        mic_of_code code |> Mic.mk ~typs ~vars ~fields
    }

    (* Instructions that take two code arguments. *)
    | mic_of_code_2 = instruction_code_2
    ; annots = annotations
    ; code_1 = instruction
    ; code_2 = instruction {
        let { Annot.typs ; Annot.vars ; Annot.fields } = annots in
        mic_of_code_2 code_1 code_2 |> Mic.mk ~typs ~vars ~fields
    }

    (* Multi-sig instructions. *)
    | I_CREATE_CONTRACT
    ; annots = annotations
    ; arg = create_contract_arg {
        let { Annot.typs ; Annot.vars ; Annot.fields } = annots in
        Mic.CreateContract arg |> Mic.mk ~typs ~vars ~fields
    }

    (* Macros. *)
    | macro = instruction_macro_leaf
    ; annots = annotations {
        ParseMic.macro annots macro
    }
    | macro = instruction_macro_code
    ; annots = annotations
    ; code = instruction {
        macro code |> ParseMic.macro annots
    }
    | macro = instruction_macro_code_2
    ; annots = annotations
    ; code_1 = instruction
    ; code_2 = instruction {
        macro code_1 code_2 |> ParseMic.macro annots
    }
;

instruction_macro_leaf :
    | I_M_INT { Mic.Macro.Int }
    | I_M_FAIL { Mic.Macro.Fail }
    | I_M_ASSERT { Mic.Macro.Assert }
    | I_M_ASSERT_NONE { Mic.Macro.AssertNone }
    | I_M_ASSERT_SOME { Mic.Macro.AssertSome }
    | I_M_ASSERT_LEFT { Mic.Macro.AssertLeft }
    | I_M_ASSERT_RIGHT { Mic.Macro.AssertRight }
    | count = I_M_DUUP { Mic.Macro.Dup count }
    | op = I_M_CMP { Mic.Macro.Cmp op }
    | op = I_M_ASSERT_ { Mic.Macro.Assert_ op }
    | op = I_M_ASSERT_CMP { Mic.Macro.AssertCmp op }
    | aip = I_M_PAIR { Mic.Macro.P (aip_to_ops aip) }
    | aip = I_M_UNPAIR { Mic.Macro.Unp (aip_to_ops aip) }
    | ad = I_M_CADR { Mic.Macro.CadR (ad_to_ops ad) }
    | ad = I_M_SET_CADR { Mic.Macro.SetCadr (ad_to_ops ad) }

instruction_macro_code :
    | count = I_M_DIIP {
        fun (code : Mic.t) -> Mic.Macro.Dip (count, code)
    }
    | ad = I_M_MAP_CADR {
        fun (code : Mic.t) -> Mic.Macro.MapCadr (ad_to_ops ad, code)
    }

instruction_macro_code_2 :
    | op = I_M_IF {
        fun (code_1 : Mic.t) (code_2 : Mic.t) -> Mic.Macro.If (op, code_1, code_2)
    }
    | op = I_M_IF_CMP {
        fun (code_1 : Mic.t) (code_2 : Mic.t) -> Mic.Macro.IfCmp (op, code_1, code_2)
    }
    | I_M_IF_SOME {
        fun (code_1 : Mic.t) (code_2 : Mic.t) -> Mic.Macro.IfSome (code_1, code_2)
    }

create_contract_arg :
    | ident = C_STR { Either.Rgt ident }
    | OCURL ; c = contract ; CCURL { Either.Lft (Some c) }
    | { Either.Lft None }

instruction_leaf :
    | I_FAILWITH { Mic.Leaf Mic.Failwith }
    | I_EXEC { Mic.Leaf Mic.Exec }
    | I_DROP { Mic.Leaf Mic.Drop }
    | I_DUP { Mic.Leaf Mic.Dup }
    | I_SWAP { Mic.Leaf Mic.Swap }
    | I_UNIT { Mic.Leaf Mic.Unit }
    | I_EQ { Mic.Leaf Mic.Eq }
    | I_NEQ { Mic.Leaf Mic.Neq }
    | I_LT { Mic.Leaf Mic.Lt }
    | I_LE { Mic.Leaf Mic.Le }
    | I_GT { Mic.Leaf Mic.Gt }
    | I_GE { Mic.Leaf Mic.Ge }
    | I_OR { Mic.Leaf Mic.Or }
    | I_AND { Mic.Leaf Mic.And }
    | I_XOR { Mic.Leaf Mic.Xor }
    | I_NOT { Mic.Leaf Mic.Not }
    | I_NEG { Mic.Leaf Mic.Neg }
    | I_ABS { Mic.Leaf Mic.Abs }
    | I_ADD { Mic.Leaf Mic.Add }
    | I_SUB { Mic.Leaf Mic.Sub }
    | I_MUL { Mic.Leaf Mic.Mul }
    | I_EDIV { Mic.Leaf Mic.EDiv }
    | I_LSL { Mic.Leaf Mic.Lsl }
    | I_LSR { Mic.Leaf Mic.Lsr }
    | I_COMPARE { Mic.Leaf Mic.Compare }
    | I_CONCAT { Mic.Leaf Mic.Concat }
    | I_SIZE { Mic.Leaf Mic.Size }
    | I_PAIR { Mic.Leaf Mic.Pair }
    | I_CAR { Mic.Leaf Mic.Car }
    | I_CDR { Mic.Leaf Mic.Cdr }
    | I_GET { Mic.Leaf Mic.Get }
    | I_MEM { Mic.Leaf Mic.Mem }
    | I_UPDATE { Mic.Leaf Mic.Update }
    | I_SOME { Mic.Leaf Mic.Som }
    | I_CONS { Mic.Leaf Mic.Cons }
    | I_CREATE_ACCOUNT { Mic.Leaf Mic.CreateAccount }
    | I_ADDRESS { Mic.Leaf Mic.Address }
    | I_TRANSFER_TOKENS { Mic.Leaf Mic.TransferTokens }
    | I_SET_DELEGATE { Mic.Leaf Mic.SetDelegate }
    | I_BALANCE { Mic.Leaf Mic.Balance }
    | I_SOURCE { Mic.Leaf Mic.Source }
    | I_SENDER { Mic.Leaf Mic.Sender }
    | I_SELF { Mic.Leaf Mic.Self }
    | I_AMOUNT { Mic.Leaf Mic.Amount }
    | I_IMPLICIT_ACCOUNT { Mic.Leaf Mic.ImplicitAccount }
    | I_STEPS_TO_QUOTA { Mic.Leaf Mic.StepsToQuota }
    | I_NOW { Mic.Leaf Mic.Now }
    | I_PACK { Mic.Leaf Mic.Pack }
    | I_SLICE { Mic.Leaf Mic.Slice }
    | I_CHECK_SIGNATURE { Mic.Leaf Mic.CheckSignature }
    | I_RENAME { Mic.Leaf Mic.Rename }
    | I_INT { Mic.Cast (Dtyp.Int |> Dtyp.mk_leaf) }
    | I_NAT { Mic.Cast (Dtyp.Nat |> Dtyp.mk_leaf) }

    | I_APPLY_OPERATIONS { Mic.Extension Mic.ApplyOps }
    | I_BALANCE_OF { Mic.Extension Mic.BalanceOf }
    | I_MUST_FAIL { Mic.Extension Mic.MustFail }
    | I_PRINT_STACK { Mic.Extension Mic.PrintStack }
    | I_SET_SOURCE { Mic.Extension Mic.SetSource }
    | I_STEP
    ; blah = C_STR { Mic.Extension (Mic.Step (Some blah)) }
    | I_STEP { Mic.Extension (Mic.Step None) }

    | hash = I_HASH { Mic.Leaf (Mic.Hash hash) }

instruction_dtyp :
    | I_CAST {
        fun (dtyp : Dtyp.t) -> Mic.Cast dtyp
    }
    | I_EMPTY_SET {
        fun (dtyp : Dtyp.t) -> Mic.EmptySet dtyp
    }
    | I_UNPACK {
        fun (dtyp : Dtyp.t) -> Mic.Unpack dtyp
    }
    | I_NONE {
        fun (dtyp : Dtyp.t) -> Mic.Non dtyp
    }
    | I_LEFT {
        fun (dtyp : Dtyp.t) -> Mic.Left dtyp
    }
    | I_RIGHT {
        fun (dtyp : Dtyp.t) -> Mic.Right dtyp
    }
    | I_NIL {
        fun (dtyp : Dtyp.t) -> Mic.Nil dtyp
    }
    | I_CONTRACT {
        fun (dtyp : Dtyp.t) -> Mic.Contract dtyp
    }
    | I_STORAGE_OF {
        fun (dtyp : Dtyp.t) -> Mic.Extension (Mic.StorageOf dtyp)
    }

instruction_dtyp_2 :
    | I_EMPTY_MAP {
        fun (keys : Dtyp.t) (vals : Dtyp.t) -> Mic.EmptyMap (keys, vals)
    }

instruction_dtyp_2_code :
    | I_LAMBDA {
        fun (dom : Dtyp.t) (codom : Dtyp.t) (code : Mic.t) ->
            Mic.Lambda (dom, codom, code)
    }

instruction_dtyp_const :
    | I_PUSH {
        fun (dtyp : Dtyp.t) (c : Mic.const) -> Mic.Push (dtyp, c)
    }

instruction_code :
    | I_LOOP {
        fun (code : Mic.t) -> Mic.Loop code
    }
    | I_LOOP_LEFT {
        fun (code : Mic.t) -> Mic.LoopLeft code
    }
    | I_DIP {
        fun (code : Mic.t) -> Mic.Dip code
    }
    | I_ITER {
        fun (code : Mic.t) -> Mic.Iter code
    }
    | I_MAP {
        fun (code : Mic.t) -> Mic.Map (Dtyp.mk_var (), code)
    }

instruction_code_2 :
    | I_IF {
        fun (code_1 : Mic.t) (code_2 : Mic.t) -> Mic.If (code_1, code_2)
    }
    | I_IF_NONE {
        fun (code_1 : Mic.t) (code_2 : Mic.t) -> Mic.IfNone (code_1, code_2)
    }
    | I_IF_LEFT {
        fun (code_1 : Mic.t) (code_2 : Mic.t) -> Mic.IfLeft (code_1, code_2)
    }
    | I_IF_RIGHT {
        fun (code_1 : Mic.t) (code_2 : Mic.t) -> Mic.IfRight (code_1, code_2)
    }
    | I_IF_CONS {
        fun (code_1 : Mic.t) (code_2 : Mic.t) -> Mic.IfCons (code_1, code_2)
    }

annotations :
    | annots = rev_annotations {
        Annot.rev annots
    }
rev_annotations :
    | annots = rev_annotations
    ; typ = COLANNOT {
        Annot.cons_typ typ annots
    }

    | annots = rev_annotations
    ; var = ATANNOT {
        Annot.cons_var var annots
    }

    | annots = rev_annotations
    ; field = PERANNOT {
        Annot.cons_field field annots
    }

    | { Annot.empty }
;

datatype :
    | dtyp = named_datatype {
        strip_dtyp_name dtyp
    }

named_datatype_leaf :
    | T_STR { Dtyp.Str }
    | T_NAT { Dtyp.Nat }
    | T_INT { Dtyp.Int }
    | T_BYTES { Dtyp.Bytes }
    | T_BOOL { Dtyp.Bool }
    | T_UNIT { Dtyp.Unit }
    | T_MUTEZ { Dtyp.Mutez }
    | T_ADDRESS { Dtyp.Address }
    | T_OPERATION { Dtyp.Operation }
    | T_KEY { Dtyp.Key }
    | T_KEYH { Dtyp.KeyH }
    | T_SIG { Dtyp.Signature }
    | T_TSTAMP { Dtyp.Timestamp }
;

named_datatype :
    | dtyp = named_datatype_leaf { Dtyp.Leaf dtyp |> named_dtyp }
    | OPAR
    ; dtyp = named_datatype_leaf
    ; annots = annotations
    ; CPAR {
        let alias, name = Annot.to_dtyp_annots annots in
        Dtyp.Leaf dtyp |> named_dtyp ~alias ~name
    }

    | OPAR
    ; T_PAIR
    ; annots = annotations
    ; lft = named_datatype
    ; rgt = named_datatype
    ; CPAR {
        let alias, name = Annot.to_dtyp_annots annots in
        Dtyp.Pair (lft, rgt) |> named_dtyp ~alias ~name
    }
    | OPAR
    ; T_OR
    ; annots = annotations
    ; lft = named_datatype
    ; rgt = named_datatype
    ; CPAR {
        let alias, name = Annot.to_dtyp_annots annots in
        Dtyp.Or (lft, rgt) |> named_dtyp ~alias ~name
    }
    | OPAR
    ; T_OPTION
    ; annots = annotations
    ; sub = named_datatype
    ; CPAR {
        let alias, name = Annot.to_dtyp_annots annots in
        Dtyp.Option sub |> named_dtyp ~alias ~name
    }

    | OPAR
    ; T_LIST
    ; annots = annotations
    ; sub = datatype
    ; CPAR {
        let alias, name = Annot.to_dtyp_annots annots in
        Dtyp.List sub |> named_dtyp ~alias ~name
    }
    | OPAR
    ; T_SET
    ; annots = annotations
    ; sub = datatype
    ; CPAR {
        let alias, name = Annot.to_dtyp_annots annots in
        Dtyp.Set sub |> named_dtyp ~alias ~name
    }
    | OPAR
    ; T_CONTRACT
    ; annots = annotations
    ; sub = datatype
    ; CPAR {
        let alias, name = Annot.to_dtyp_annots annots in
        Dtyp.Contract sub |> named_dtyp ~alias ~name
    }

    | OPAR
    ; T_MAP
    ; annots = annotations
    ; keys = datatype
    ; vals = datatype
    ; CPAR {
        let alias, name = Annot.to_dtyp_annots annots in
        Dtyp.Map (keys, vals) |> named_dtyp ~alias ~name
    }
    | OPAR
    ; T_BIGMAP
    ; annots = annotations
    ; keys = datatype
    ; vals = datatype
    ; CPAR {
        let alias, name = Annot.to_dtyp_annots annots in
        Dtyp.BigMap (keys, vals) |> named_dtyp ~alias ~name
    }
    | OPAR
    ; T_LAMBDA
    ; annots = annotations
    ; dom = datatype
    ; codom = datatype
    ; CPAR {
        let alias, name = Annot.to_dtyp_annots annots in
        Dtyp.Lambda (dom, codom) |> named_dtyp ~alias ~name
    }
;

const :
    | C_UNIT {
        Mic.U
    }
    | C_NONE {
        Mic.No
    }

    | b = C_BOOL {
        Mic.Bool b
    }
    | i = C_INT {
        Mic.Int i
    }
    | s = C_STR {
        Mic.mk_str_const s
    }
    | s = C_BYTES {
        Mic.Bytes s
    }

    | OPAR
    ; C_SOME
    ; c = const
    ; CPAR {
        Mic.So c
    }
    | OPAR
    ; C_LEFT
    ; c = const
    ; CPAR {
        Mic.Lft c
    }
    | OPAR
    ; C_RIGHT
    ; c = const
    ; CPAR {
        Mic.Rgt c
    }
    | OPAR
    ; C_PAIR
    ; fst = const
    ; snd = const
    ; CPAR {
        Mic.Pr (fst, snd)
    }

    | OCURL
    ; c = contract
    ; CCURL {
        Mic.Cont c
    }

    | OCURL
    ; lst = const_list
    ; CCURL {
        Mic.Lst lst
    }
;

const_list :
    | c = const
    ; SEMICOL
    ; tail = const_list {
        c :: tail
    }
    | c = const {
        [c]
    }