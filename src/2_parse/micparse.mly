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
                name source
                ~storage:(c.Base.Mic.storage)
                ~entry_param:(c.Base.Mic.param)
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
                name source
                ~storage:(c.Base.Mic.storage)
                ~entry_param:(c.Base.Mic.param)
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
    ; storage = top_datatype {
        let (s, p ,e) = sub in
        (storage :: s), p, e
    }
    | sub = contract_sub
    ; PARAMETER
    ; param = top_datatype {
        let (s, p ,e) = sub in
        s, (param :: p), e
    }
    | sub = contract_sub
    ; CODE
    ; entry = instruction {
        let (s, p ,e) = sub in
        s, p, (entry :: e)
    }
    | sub = contract_sub
    ; SEMICOL {
        sub
    }
    | { [], [], [] }

instruction :
    | OCURL
    ; inss = rev_instructions_semicol
    ; CCURL {
        List.rev inss |> Base.Mic.mk_seq
    }

    | token = INSTKN
    ; annots = rev_annotations
    ; dtyps = rev_datatypes
    ; args = rev_instructions_space {
        let typs, vars, fields = annots in
        let typs, vars, fields =
            List.rev typs, List.rev vars, List.rev fields
        in
        let args = List.rev args in
        let dtyps = List.rev dtyps |> List.map fst in
        Mic.parse token typs vars fields dtyps args
    }
;

rev_instructions_semicol :
    | i = instruction { [i] }
    | is = rev_instructions_semicol ; SEMICOL ; i = instruction { i :: is }
    | { [] }
;

instruction_arg :
    | token = INSTKN {
        Mic.parse token [] [] [] [] []
        |> Base.Common.Either.lft
    }

    | c = const {
        Base.Common.Either.rgt c
    }

    | OPAR
    ; i = instruction
    ; CPAR {
        (* Format.printf "parsed %a@." Base.Mic.fmt i ; *)
        Base.Common.Either.lft i
    }

    | OCURL
    ; is = rev_instructions_semicol
    ; CCURL {
        let i = List.rev is |> Base.Mic.mk_seq in
        (* Format.printf "parsed %a@." Base.Mic.fmt i ; *)
        Base.Common.Either.lft i
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

rev_instructions_space :
    | is = rev_instructions_space ; i = instruction_arg {
        let i : (Base.Mic.t, Base.Mic.const) Base.Common.Either.t = i in
        (* Format.printf "ins spc %a@." (Base.Common.Either.fmt Base.Mic.fmt Base.Mic.fmt_const) i ; *)
        i :: is
    }
    | i = instruction_arg {
        [i]
    }
    | {
        (* Format.printf "empty ins spc@." ; *)
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
        (Dtyp.parse token None [], None)
    }

    (* Annotated/aliased type. *)
    | OPAR
    ; token = TYPTKN
    ; annot = COLANNOT
    ; dtyps = rev_datatypes
    ; CPAR {
        let dtyps = List.rev dtyps in
        (Dtyp.parse token (Some annot) dtyps, None)
    }

    (* Field-annotated type. *)
    | OPAR
    ; token = TYPTKN
    ; annot = PERANNOT
    ; dtyps = rev_datatypes
    ; CPAR {
        let dtyps = List.rev dtyps in
        (Dtyp.parse token None dtyps, Some annot)
    }

    (* Non-annotated compound types. *)
    | OPAR
    ; token = TYPTKN
    ; dtyps = rev_datatypes
    ; CPAR {
        let dtyps = List.rev dtyps in
        (Dtyp.parse token None dtyps, None)
    }
;

rev_datatypes :
    | dtyps = rev_datatypes
    ; dtyp = datatype {
        dtyp :: dtyps
    }
    | { [] }
;

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
    ; subs = rev_consts
    ; CPAR {
        List.rev subs |> Const.parse token
    }
;

rev_consts :
    | tail = rev_consts
    ; head = const {
        head :: tail
    }
    | { [] }
;