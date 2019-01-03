(** Types and helpers for test cases.

    A test case is essentially a block of code. This block of code if a function from nothing to a
    list of operations. The operations are expected to be creation of/calls to contracts that are
    part of the test context. *)

open Common

(** A test case. *)
type t = {
    name : string ;
    (** Name of the test case. *)
    source : Source.t ;
    (** Source of the test case. *)
    code : Mic.t ;
    (** Code of the test case. *)
}

(** Constructor. *)
val mk : string -> Source.t -> Mic.t -> t

(** Changes the name of a test case. *)
val rename : string -> t -> t

(** Formats a test case. *)
val fmt : full : bool -> formatter -> t -> unit