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

(** Datatypes. *)
type t =
| Leaf of leaf

| List of t
| Option of t
| Set of t
| Contract of t

| Pair of t * t
| Or of t * t
| Map of t * t
| BigMap of t * t

(** String datatype. *)
val str : t

(** Integer datatype. *)
val int : t

(** Natural datatype. *)
val nat : t

(** Bytes datatype. *)
val bytes : t

(** Bool datatype. *)
val bool : t

(** Unit datatype. *)
val unit : t

(** Mutez datatype. *)
val mutez : t

(** Address datatype. *)
val address : t

(** Operation datatype. *)
val operation : t

(** Key datatype. *)
val key : t

(** Key hash datatype. *)
val key_hash : t

(** Signature datatype. *)
val signature : t

(** Timestamp datatype. *)
val timestamp : t

(** Formatter for datatypes. *)
val fmt : formatter -> t -> unit

(** Datatype parser. *)
val parse : string -> t list -> t