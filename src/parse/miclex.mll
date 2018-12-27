(** Michelson lexer.

    # TODO
    
    - escape sequences in strings
    - bytes *)

{
    open Micparse
}

rule token = parse
| [' ' '\t' '\n'] { token lexbuf }

| "parameter" { PARAMETER }
| "storage" { STORAGE }
| "code" { CODE }
| "Unit" { UNIT }

| ':' (['a'-'z' '_' 'A'-'Z']['a'-'z' 'A'-'Z' '_' '.' '0'-'9']* as str) {
    COLANNOT (Base.Annot.Typ.of_string str)
}
| '@' (['a'-'z' '_' 'A'-'Z']['a'-'z' 'A'-'Z' '_' '.' '0'-'9']* as str) {
    ATANNOT (Base.Annot.Var.of_string str)
}
| '%' (['a'-'z' '_' 'A'-'Z']['a'-'z' 'A'-'Z' '_' '.' '0'-'9']* as str) {
    PERANNOT (Base.Annot.Field.of_string str)
}

| ':' { COLANNOT (Base.Annot.Typ.of_string "") }
| '@' { ATANNOT (Base.Annot.Var.of_string "") }
| '%' { PERANNOT (Base.Annot.Field.of_string "") }

| ['A'-'Z' '_']+ as str { INSTKN str }
| ['a'-'z' '_']+ as str { TYPTKN str }

| "True"  { BOOL true }
| "False" { BOOL false }
| ['A'-'Z' 'a'-'z' '_']+ as str { CONSTRTKN str }

| '"' [^'"']* '"' as str {
    STR (String.sub str 1 ((String.length str) - 2))
}

| ['0'-'9']+ as lxm { INT lxm }
| '{' { OCURL }
| '}' { CCURL }
| '(' { OPAR }
| ')' { CPAR }
| ';' { SEMICOL }
| eof { EOF }