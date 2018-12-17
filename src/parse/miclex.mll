(** Michelson lexer. *)

{
    open Micparse
}

rule token = parse
| "CONTRACT" { CONTRACT }
| [' ' '\t' '\n'] { token lexbuf }
| ['0'-'9']+ as lxm { INT (int_of_string lxm) }
| ['A'-'Z' 'a'-'z' '_']+ as str { TKN str }
| '"' [^'"']* '"' as str { STR str }
| '{' { OCURL }
| '}' { CCURL }
| '(' { OPAR }
| ')' { CPAR }
| ';' { SEMICOL }
| eof { EOF }