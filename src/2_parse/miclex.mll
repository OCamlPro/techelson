(** Michelson lexer. *)

{
    open Micparse

    let found_newline ({ Lexing.lex_curr_p ; _ } as lexbuf) =
        lexbuf.Lexing.lex_curr_p <- {
            lex_curr_p with
            pos_lnum = lex_curr_p.Lexing.pos_lnum + 1;
            pos_bol = lex_curr_p.Lexing.pos_cnum;
        }

    let mk_hashtbl init =
        let tbl = List.length init |> Hashtbl.create in
        init |> List.iter (fun (k, v) -> Hashtbl.add tbl k v) ;
        tbl

    let keyword_table = mk_hashtbl [
        (* Datatypes. *)
        "string", T_STR ;
        "nat", T_NAT ;
        "int", T_INT ;
        "bytes", T_BYTES ;
        "bool", T_BOOL ;
        "unit", T_UNIT ;
        "mutez", T_MUTEZ ;
        "address", T_ADDRESS ;
        "operation", T_OPERATION ;
        "key", T_KEY ;
        "key_hash", T_KEYH ;
        "signature", T_SIG ;
        "timestapm", T_TSTAMP ;
        "pair", T_PAIR ;
        "or", T_OR ;
        "option", T_OPTION ;
        "list", T_LIST ;
        "set", T_SET ;
        "contract", T_CONTRACT ;
        "map", T_MAP ;
        "big_map", T_BIGMAP ;
        "lambda", T_LAMBDA ;

        (* Instructions. *)
        "FAILWITH", I_FAILWITH ;
        "EXEC", I_EXEC ;
        "DROP", I_DROP ;
        "DUP", I_DUP ;
        "SWAP", I_SWAP ;
        "UNIT", I_UNIT ;
        "EQ", I_EQ ;
        "NEQ", I_NEQ ;
        "LT", I_LT ;
        "LE", I_LE ;
        "GT", I_GT ;
        "GE", I_GE ;
        "OR", I_OR ;
        "AND", I_AND ;
        "XOR", I_XOR ;
        "NOT", I_NOT ;
        "NEG", I_NEG ;
        "ABS", I_ABS ;
        "ADD", I_ADD ;
        "SUB", I_SUB ;
        "MUL", I_MUL ;
        "EDIV", I_EDIV ;
        "LSL", I_LSL ;
        "LSR", I_LSR ;
        "COMPARE", I_COMPARE ;
        "CONCAT", I_CONCAT ;
        "SIZE", I_SIZE ;
        "PAIR", I_PAIR ;
        "CAR", I_CAR ;
        "CDR", I_CDR ;
        "GET", I_GET ;
        "MEM", I_MEM ;
        "UPDATE", I_UPDATE ;
        "SOME", I_SOME ;
        "CONS", I_CONS ;
        "CREATE_ACCOUNT", I_CREATE_ACCOUNT ;
        "TRANSFER_TOKENS", I_TRANSFER_TOKENS ;
        "SET_DELEGATE", I_SET_DELEGATE ;
        "BALANCE", I_BALANCE ;
        "SOURCE", I_SOURCE ;
        "SENDER", I_SENDER ;
        "SELF", I_SELF ;
        "AMOUNT", I_AMOUNT ;
        "IMPLICIT_ACCOUNT", I_IMPLICIT_ACCOUNT ;
        "STEPS_TO_QUOTA", I_STEPS_TO_QUOTA ;
        "NOW", I_NOW ;
        "PACK", I_PACK ;
        "UNPACK", I_UNPACK ;
        "SLICE", I_SLICE ;
        "CHECK_SIGNATURE", I_CHECK_SIGNATURE ;
        "RENAME", I_RENAME ;

        "HASH", I_HASH Base.Mic.B58Check ;
        "BLAKE2B", I_HASH Base.Mic.Blake2B ;
        "SHA256", I_HASH Base.Mic.Sha256 ;
        "SHA512", I_HASH Base.Mic.Sha512 ;

        "CAST", I_CAST ;
        "EMPTY_SET", I_EMPTY_SET ;
        "EMPTY_MAP", I_EMPTY_MAP ;
        "NONE", I_NONE ;
        "LEFT", I_LEFT ;
        "RIGHT", I_RIGHT ;
        "NIL", I_NIL ;
        "CONTRACT", I_CONTRACT ;
        "IF", I_IF ;
        "LOOP", I_LOOP ;
        "LOOP_LEFT", I_LOOP_LEFT ;
        "DIP", I_DIP ;
        "PUSH", I_PUSH ;
        "LAMBDA", I_LAMBDA ;
        "ITER", I_ITER ;
        "IF_NONE", I_IF_NONE ;
        "IF_LEFT", I_IF_LEFT ;
        "IF_RIGHT", I_IF_RIGHT ;
        "IF_CONS", I_IF_CONS ;
        "CREATE_CONTRACT", I_CREATE_CONTRACT ;

        "ASSERT_NONE", I_M_ASSERT_NONE ;
        "ASSERT_SOME", I_M_ASSERT_SOME ;
        "ASSERT_LEFT", I_M_ASSERT_LEFT ;
        "ASSERT_RIGHT", I_M_ASSERT_RIGHT ;
        "IF_SOME", I_M_IF_SOME ;

        "ASSERT_EQ", I_M_ASSERT_ Base.Mic.Macro.Eq ;
        "ASSERT_NEQ", I_M_ASSERT_ Base.Mic.Macro.Neq ;
        "ASSERT_LT", I_M_ASSERT_ Base.Mic.Macro.Lt ;
        "ASSERT_LE", I_M_ASSERT_ Base.Mic.Macro.Le ;
        "ASSERT_GE", I_M_ASSERT_ Base.Mic.Macro.Ge ;
        "ASSERT_GT", I_M_ASSERT_ Base.Mic.Macro.Gt ;

        "ASSERT_CMPEQ", I_M_ASSERT_CMP Base.Mic.Macro.Eq ;
        "ASSERT_CMPNEQ", I_M_ASSERT_CMP Base.Mic.Macro.Neq ;
        "ASSERT_CMPLT", I_M_ASSERT_CMP Base.Mic.Macro.Lt ;
        "ASSERT_CMPLE", I_M_ASSERT_CMP Base.Mic.Macro.Le ;
        "ASSERT_CMPGE", I_M_ASSERT_CMP Base.Mic.Macro.Ge ;
        "ASSERT_CMPGT", I_M_ASSERT_CMP Base.Mic.Macro.Gt ;

        "CMPEQ", I_M_CMP Base.Mic.Macro.Eq ;
        "CMPNEQ", I_M_CMP Base.Mic.Macro.Neq ;
        "CMPLT", I_M_CMP Base.Mic.Macro.Lt ;
        "CMPLE", I_M_CMP Base.Mic.Macro.Le ;
        "CMPGE", I_M_CMP Base.Mic.Macro.Ge ;
        "CMPGT", I_M_CMP Base.Mic.Macro.Gt ;

        "IFEQ", I_M_IF Base.Mic.Macro.Eq ;
        "IFNEQ", I_M_IF Base.Mic.Macro.Neq ;
        "IFLT", I_M_IF Base.Mic.Macro.Lt ;
        "IFLE", I_M_IF Base.Mic.Macro.Le ;
        "IFGE", I_M_IF Base.Mic.Macro.Ge ;
        "IFGT", I_M_IF Base.Mic.Macro.Gt ;

        "IFCMPEQ", I_M_IF_CMP Base.Mic.Macro.Eq ;
        "IFCMPNEQ", I_M_IF_CMP Base.Mic.Macro.Neq ;
        "IFCMPLT", I_M_IF_CMP Base.Mic.Macro.Lt ;
        "IFCMPLE", I_M_IF_CMP Base.Mic.Macro.Le ;
        "IFCMPGE", I_M_IF_CMP Base.Mic.Macro.Ge ;
        "IFCMPGT", I_M_IF_CMP Base.Mic.Macro.Gt ;

        "STORAGE_OF", I_STORAGE_OF ;
        "BALANCE_OF", I_BALANCE_OF ;
        "APPLY_OPERATIONS", I_APPLY_OPERATIONS ;
        "PRINT_STACK", I_PRINT_STACK ;
        "MUST_FAIL", I_MUST_FAIL ;

        (* Constants. *)
        "Unit", C_UNIT ;
        "True", C_BOOL true ;
        "False", C_BOOL false ;
        "None", C_NONE ;
        "Some", C_SOME ;
        "Left", C_LEFT ;
        "Right", C_RIGHT ;

        (* Contract's fields. *)
        "parameter", PARAMETER ;
        "storage", STORAGE ;
        "code", CODE ;
    ]
}

let lf = '\010'
let lf_cr = ['\010' '\013']
let dos_newline = "\013\010"
let whitespace = [' ' '\009' '\012']

rule token = parse
| whitespace { token lexbuf }
| lf | lf_cr | dos_newline { found_newline lexbuf ; token lexbuf }

| "#" [^'\n']* "\n" { found_newline lexbuf ; token lexbuf }

| '{' { OCURL }
| '}' { CCURL }
| '(' { OPAR }
| ')' { CPAR }
| ';' { SEMICOL }


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

| ['0'-'9']+ as str { C_INT str }
| "0x" (['0'-'9' 'A'-'Z' 'a'-'z']+ as bytes) {
    C_BYTES bytes
}
| '"' (([^'"'] | "\\\"")* as str) '"' {
    C_STR str
}

(* Macros. *)
| 'D' 'U' (['U']+ as str) 'P' {
    I_M_DUUP (String.length str)
}
| 'D' 'I' (['I']+ as str) 'P' {
    I_M_DIIP (String.length str)
}
| 'P' (['A' 'I' 'P']['A' 'I' 'P']['A' 'I' 'P']+ as str) 'R' {
    I_M_PAIR str
}
| "UNP" (['A' 'I' 'P']['A' 'I' 'P']['A' 'I' 'P']+ as str) 'R' {
    I_M_UNPAIR str
}
| 'C' (['A' 'D']+ as str) 'R' {
    I_M_CADR str
}
| "SET_C" (['A' 'D']+ as str) 'R' {
    I_M_SET_CADR str
}
| "MAP_C" (['A' 'D']+ as str) 'R' {
    I_M_MAP_CADR str
}

(* Keywords. *)
| ['A'-'Z' 'a'-'z']['A'-'Z' 'a'-'z' '_' '0'-'9']* as str {
    try Hashtbl.find keyword_table str with
    | Not_found -> IDENT str
}


| eof { EOF }