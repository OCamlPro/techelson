(** Datatype parsing. *)

open Base

(** Parses a datatype from a token.

    `parse token name dtyps`
    - `token`: token for the type constructor
    - `name`: type annotation for the type constructed
    - `dtyps`: argument datatypes with optional field annotations *)
val parse :
    string ->
    Annot.Typ.t option ->
    (Dtyp.t * Annot.Field.t option) list ->
    Dtyp.t