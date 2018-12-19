(** Michelson lexer. *)

{
    open Micparse
}

rule token = parse
| [' ' '\t' '\n'] { token lexbuf }

| "CONTRACT" { CONTRACT }
| "parameter" { PARAMETER }
| "storage" { STORAGE }
| "code" { CODE }

| ':' (['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '_' '0'-'9']* as lxm) {
    COLANNOT lxm
}
| '@' (['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '_' '0'-'9']* as lxm) {
    ATANNOT lxm
}
| '%' (['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '_' '0'-'9']* as lxm) {
    PERANNOT lxm
}

| ['A'-'Z' '_']* as str { INSTKN str }
| ['a'-'z' '_']* as str { TYPTKN str }

| '"' [^'"']* '"' as str { STR str }

| ['0'-'9']+ as lxm { INT (int_of_string lxm) }
| '{' { OCURL }
| '}' { CCURL }
| '(' { OPAR }
| ')' { CPAR }
| ';' { SEMICOL }
| eof { EOF }