(** Michelson Parser. *)

%token UNIT
%token <bool> BOOL
%token <string> INT STR BYTES
%token <string> TYPTKN INSTKN CONSTRTKN
%token <Base.Annot.Typ.t> COLANNOT
%token <Base.Annot.Var.t> ATANNOT
%token <Base.Annot.Field.t> PERANNOT
%token OCURL CCURL
%token OPAR CPAR
%token SEMICOL
%token CONTRACT TEST PARAMETER STORAGE CODE
%token EOF

%start <string -> Base.Common.Source.t -> Base.Contract.t> just_contract
%start <Base.Common.Source.t -> Base.Contract.t> named_contract
%start <Base.Common.Source.t -> (Base.Contract.t list * Base.Testcase.t list)> scenario
%start <Base.Mic.t> just_mic
%%

scenario :
    | res = scenario_inner
    ; EOF {
        let contracts, tests = res in
        fun (src : Base.Common.Source.t) ->
            (contracts |> List.rev |> List.map (fun mk -> mk src)),
            (tests |> List.rev |> List.map (fun mk -> mk src))
    }

scenario_inner :
    | res = scenario_inner
    ; contract = named_contract {
        let contracts, tests = res in
        (contract :: contracts), tests
    }
    | res = scenario_inner
    ; test = named_test {
        let contracts, tests = res in
        contracts, (test :: tests)
    }
    | { [], [] }

named_contract :
    | CONTRACT
    ; name = CONSTRTKN
    ; OCURL
    ; c = contract
    ; CCURL
    ; SEMICOL {
        fun (source : Base.Common.Source.t) ->
            Base.Contract.mk
                ~storage:(c.Base.Mic.storage)
                ~entry_param:(c.Base.Mic.param)
                name
                (Some source)
                c.Base.Mic.entry
                None
    }

named_test :
    | TEST
    ; name = CONSTRTKN
    ; code = instruction
    ; SEMICOL {
        fun (source : Base.Common.Source.t) ->
            Base.Testcase.mk name source code
    }

just_contract :
    | c = contract
    ; EOF {
        fun (name : string) (source : Base.Common.Source.t) ->
            Base.Contract.mk
                ~storage:(c.Base.Mic.storage)
                ~entry_param:(c.Base.Mic.param)
                name
                (Some source)
                c.Base.Mic.entry
                None
    }

just_mic :
    | ins = instruction
    ; EOF { ins }

contract :
    | sub = contract_sub {
        let (s, p, e) = sub in
        Base.Mic.mk_contract_of_lists ~storage:(s) ~param:(p) e
    }
;

contract_sub :
    | sub = contract_sub
    ; STORAGE
    ; storage = top_datatype
    ; SEMICOL {
        let (s, p ,e) = sub in
        (storage :: s), p, e
    }
    | sub = contract_sub
    ; PARAMETER
    ; param = top_datatype
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

instruction :
    | OCURL
    ; inss = instructions_semicol
    ; CCURL {
        Base.Mic.mk_seq inss
    }

    | token = INSTKN
    ; annots = annotations
    ; dtyps = datatypes
    ; args = instructions_space {
        let dtyps =
            dtyps |> List.map (
                fun (dtyp, annot) ->
                    annot |> Base.Common.if_let_some (
                        fun a -> [
                            Format.asprintf
                                "illegal field annotation `%a` in top-level datatype" Base.Annot.Field.fmt a ;
                            Format.asprintf
                                "while parsing instruction `%s%a`" token Base.Annot.fmt annots
                        ] |> Base.Exc.throws
                    );
                    dtyp
            )
        in
        Mic.parse token annots dtyps args
    }
;

instructions_semicol :
    | inss = rev_instructions_semicol {
        List.rev inss
    }

rev_instructions_semicol :
    | i = instruction { [i] }
    | is = rev_instructions_semicol ; SEMICOL ; i = instruction { i :: is }
    | { [] }
;

instruction_arg :
    | token = INSTKN {
        Mic.parse token Base.Annot.empty [] []
        |> Base.Common.Either.lft
    }

    | c = const {
        Base.Common.Either.rgt c
    }

    | OPAR
    ; i = instruction
    ; CPAR {
        Base.Common.Either.lft i
    }

    | OCURL
    ; is = instructions_semicol
    ; CCURL {
        Base.Mic.mk_seq is |> Base.Common.Either.lft
    }

    | OCURL
    ; CCURL {
        Base.Mic.mk_seq [] |> Base.Common.Either.lft
    }

    | OCURL
    ; c = contract
    ; CCURL {
        Base.Mic.Contract c |> Base.Common.Either.rgt
    }
;

instructions_space :
    | inss = rev_instructions_space {
        List.rev inss
    }

rev_instructions_space :
    | is = rev_instructions_space ; i = instruction_arg {
        i :: is
    }
    | i = instruction_arg {
        [i]
    }
    | {
        []
    }
;

top_datatype :
    | dtyp = datatype {
        let (dtyp, field) = dtyp in
        match field with
        | Some f ->
            Format.asprintf
                "illegal field annotation `%a` in top-level datatype" Base.Annot.Field.fmt f
            |> Base.Exc.throw
        | None  -> dtyp
    }
;

datatype :
    (* Leaf type. *)
    | token = TYPTKN {
        Dtyp.parse token Base.Annot.empty []
    }

    (* Compound type. *)
    | OPAR
    ; token = TYPTKN
    ; annots = annotations
    ; dtyps = datatypes
    ; CPAR {
        Dtyp.parse token annots dtyps
    }

    (* Contract type. *)
    | OPAR
    ; CONTRACT
    ; annots = annotations
    ; dtyps = datatypes
    ; CPAR {
        Dtyp.parse "contract" annots dtyps
    }
;

datatypes :
    | dtyps = rev_datatypes {
        List.rev dtyps
    }

rev_datatypes :
    | dtyps = rev_datatypes
    ; dtyp = datatype {
        dtyp :: dtyps
    }
    | { [] }
;

annotations :
    | annots = rev_annotations {
        let t, v, f = annots in
        let t, v, f = List.rev t, List.rev v, List.rev f in
        Base.Annot.mk t v f
    }
rev_annotations :
    | annots = rev_annotations
    ; annot = COLANNOT {
        let typs, vars, fields = annots in
        (annot :: typs), vars, fields
    }

    | annots = rev_annotations
    ; annot = ATANNOT {
        let typs, vars, fields = annots in
        typs, (annot :: vars), fields
    }

    | annots = rev_annotations
    ; annot = PERANNOT {
        let typs, vars, fields = annots in
        typs, vars, (annot :: fields)
    }

    | { [], [], [] }
;

const :
    | UNIT {
        Base.Mic.Unit
    }
    | b = BOOL {
        Base.Mic.Bool b
    }
    | i = INT {
        Base.Mic.Int i
    }
    | s = STR {
        Base.Mic.mk_str_const s
    }
    | s = BYTES {
        Base.Mic.Bytes s
    }
    | OCURL
    ; c = contract
    ; CCURL {
        Base.Mic.Contract c
    }
    | OPAR
    ; token = CONSTRTKN
    ; subs = consts
    ; CPAR {
        Const.parse token subs
    }
;

consts :
    | cs = rev_consts {
        List.rev cs
    }

rev_consts :
    | tail = rev_consts
    ; head = const {
        head :: tail
    }
    | { [] }
;
