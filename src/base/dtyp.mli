(** Datatype types and helpers. *)

open Common

(** Nullary datatypes. *)
type leaf =
| Str
| Nat
| Int
| Bytes
| Bool
| Unit
| Mutez
| Address
| Operation
| Key
| KeyH
| Signature
| Timestamp

(** Formatter for nullary datatypes. *)
val fmt_leaf : formatter -> leaf -> unit

(** String to leaf conversion. *)
val leaf_of_string : string -> leaf option

(** Wraps a datatype with a name. *)
type 'sub named = {
  typ : 'sub ;
  name : string option ;
}

(** Nameless datatype. *)
type 'sub dtyp =
| Leaf of leaf

| List of 'sub
| Option of 'sub
| Set of 'sub
| Contract of 'sub

| Pair of (t named) list
| Or of (t named) list
| Map of 'sub * 'sub
| BigMap of 'sub * 'sub

(** Datatypes. *)
and t = {
  typ : t dtyp ;
  alias : string option ;
}

(** Named datatype constructor. *)
val mk : string option -> t dtyp -> t

(** Formatter for datatypes. *)
val fmt : formatter -> t -> unit

(** Datatype parser. *)
val parse : string -> string option -> (t * string option) list -> t