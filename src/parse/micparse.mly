(** Michelson Parser. *)

%token <int> INT
%token <string> STR
%token <string> TKN
%token OCURL CCURL
%token OPAR CPAR
%token SEMICOL
%token CONTRACT
%token EOF

%start <unit> mic
%%

mic :
    | EOF { Format.printf "eof@." }

    | ctr = contract {
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

contract :
    | CONTRACT
    ; name = TKN
    ; OPAR
    ; storage = datatype
    ; CPAR
    ; OPAR
    ; entry_param = datatype
    ; CPAR
    ; entry = instruction
    ; CPAR {
        Base.Contract.mk name ~storage ~entry_param entry None
    }


instruction :
    | OCURL
    ; inss = rev_instructions_semicol
    ; CCURL {
        List.rev inss |> Base.Ins.mk_seq
    }
    | token = TKN ; dtyps = rev_datatypes ; inss = rev_instructions_space {
        let inss = List.rev inss in
        let dtyps = List.rev dtyps in
        Base.Ins.parse token dtyps inss
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

datatype :
    | token = TKN
    ; dtyps = rev_datatypes {
        let dtyps = List.rev dtyps in
        Base.Dtyp.parse token dtyps
    }
;

rev_datatypes :
    | dtyps = rev_datatypes
    ; OPAR
    ; dtyp = datatype
    ; CPAR { dtyp :: dtyps }
    | { [] }
;