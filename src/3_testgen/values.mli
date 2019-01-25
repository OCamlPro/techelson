(** Generates michelson instructions that construct random values. *)

open Base

(** Generates `Mic.const`s. *)
module Const : sig
    (** Generates unit. *)
    val unit : Mic.const

    (** Generates a boolean. *)
    val bool : unit -> Mic.const

    (** Generates an integer. *)
    val int : unit -> Mic.const

    (** Generates a string. *)
    val string : unit -> Mic.const

    (** Generates some bytes. *)
    val bytes : unit -> Mic.const

    (** Generates `Left` of something. *)
    val lft : Mic.const -> Mic.const

    (** Generates `Right` of something. *)
    val rgt : Mic.const -> Mic.const

    (** Generates `None`. *)
    val none : Mic.const

    (** Generates `Some`. *)
    val some : Mic.const -> Mic.const
end

(** Generates a unit value. *)
val unit : Dtyp.t -> Mic.t

(** Generates a random boolean. *)
val bool : Dtyp.t -> Mic.t

(** Generates a random integer. *)
val int : Dtyp.t -> Mic.t

(** Generates a random natural. *)
val nat : Dtyp.t -> Mic.t

(** Generates a random string. *)
val str : Dtyp.t -> Mic.t

(** Generates a random byte sequence. *)
val bytes : Dtyp.t -> Mic.t

(** Generates a random mutez amount. *)
val mutez : Dtyp.t -> Mic.t

(** Generates a random key. *)
val key : Dtyp.t -> Mic.t

(** Generates a random key hash from a random key. *)
val key_hash : Dtyp.t -> Mic.t list

(** Type of contract generators.

    The input datatype is for the parameter.
*)
type contract_generator = Dtyp.t -> Mic.t list

(** Generates michelson that constructs a random value for a datatype. *)
val from : contract_generator -> Dtyp.t -> Mic.t list
