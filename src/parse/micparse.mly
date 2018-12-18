(** Michelson Parser. *)

%token <int> INT
%token <string> STR
%token <string> TYPTKN INSTKN COLANNOT ATANNOT PERANNOT
%token OCURL CCURL
%token OPAR CPAR
%token SEMICOL COL
%token CONTRACT
%token PARAMETER STORAGE CODE
%token EOF

%start <unit> mic
%start <string -> Base.Contract.t> just_contract
%start <Base.Ins.t> just_mic
%%

mic :
    | EOF { Format.printf "eof@." }

    | ctr = contract {
        let ctr = ctr "unnamed" in
        Format.printf "parsed a contract @[%a@]@.@." (Base.Contract.fmt true) ctr
    }

    | ins = instruction
    ; CPAR {
        Format.printf "parsed an instruction: @[%a@]@.@." Base.Ins.fmt ins
    }
    | dtyp = datatype
    ; CPAR {
        Format.printf "parsed a datatype: @[%a@]@.@." Base.Dtyp.fmt dtyp
    }
;

just_mic :
    | ins = instruction
    ; EOF { ins }

just_contract :
    | c = contract { let c : string -> Base.Contract.t = c in c }
    /* ; EOF { c } */

contract :
    | PARAMETER
    ; entry_param = datatype
    ; SEMICOL
    ; STORAGE
    ; storage = datatype
    ; SEMICOL
    ; CODE
    ; entry = instruction
    ; SEMICOL {
        fun (name : string) -> Base.Contract.mk name ~storage ~entry_param entry None
    }


instruction :
    | OCURL
    ; inss = rev_instructions_semicol
    ; CCURL {
        List.rev inss |> Base.Ins.mk_seq
    }

    | token = INSTKN
    ; dtyps = rev_datatypes
    ; inss = rev_instructions_space
    ; annots = rev_var_annots {
        let inss = List.rev inss in
        let dtyps = List.rev dtyps |> List.map fst in
        let annots = List.rev annots in
        Base.Ins.parse token dtyps inss annots
    }
;

rev_instructions_semicol :
    | i = instruction { [i] }
    | is = rev_instructions_semicol ; SEMICOL ; i = instruction { i :: is }
    | { [] }
;

rev_instructions_space :
    | i = instruction { [i] }
    | is = rev_instructions_space ; i = instruction { i :: is }
    | { [] }
;

rev_var_annots :
    | annots = rev_var_annots
    ; annot = ATANNOT {
        annot :: annots
    }
    | { [] }
;

datatype :
    (* Leaf type. *)
    | token = TYPTKN {
        Base.Dtyp.parse token None []
    }

    (* Annotated type. *)
    | OPAR
    ; token = TYPTKN
    ; annot = COLANNOT
    ; dtyps = rev_datatypes
    ; CPAR {
        let dtyps = List.rev dtyps in
        Base.Dtyp.parse token (Some annot) dtyps
    }

    (* Non-annotated compound types. *)
    | OPAR
    ; token = TYPTKN
    ; dtyps = rev_datatypes
    ; CPAR {
        let dtyps = List.rev dtyps in
        Base.Dtyp.parse token None dtyps
    }
;

rev_datatypes :
    | dtyps = rev_datatypes
    ; OPAR
    ; dtyp = datatype
    ; name = PERANNOT
    ; CPAR {
        (dtyp, (Some name)) :: dtyps
    }
    | dtyps = rev_datatypes
    ; dtyp = datatype {
        (dtyp, None) :: dtyps
    }
    | { [] }
;