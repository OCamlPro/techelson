(** Parsing-related helpers. *)

(** Returns what's after some prefix in a token.

    Used to parse macros. First parameter is the prefix, second is the token.
*)
val tail_of_pref : pref:string -> string -> string option

(** Recognizes sequences of characters in a token.

    Input function says whether some character is recognized. The function stops parsing
    when it returs `None`.

    Keeps on parsing as long as it is successful. Returns the tail of the token, the part
    that was not parsed.
*)
val sequence : (char -> 'a option) -> string -> ('a list * string)