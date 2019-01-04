(** Datatype parsing. *)

open Base

(** Parses a datatype from a token. *)
val parse :
    string ->
    Annot.t ->
    (Dtyp.t * Annot.Field.t option) list ->
    (Dtyp.t * Annot.Field.t option)