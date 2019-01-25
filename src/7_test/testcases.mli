(** A context containing user-provided named contracts and testcases. *)

open Base
open Common

(** Type of contexts. *)
type t

(** Context constructor. *)
val mk : Contract.t StrMap.t -> Testcase.t list -> t

(** Adds tests to a context. *)
val add_tests : Testcase.t list -> t -> t

(** Constructor that takes raw data. *)
val of_raw : Contract.t list -> Testcase.t list -> t

(** Retrieves a contract from the context.

    Fails if the contract is unknown. *)
val get_contract : string -> t -> Contract.t

(** Retrieves all the contracts from a context. *)
val get_contracts : t -> Contract.t list

(** Retrieves the testcases of a context. *)
val get_tests : t -> Testcase.t list

(** Formats a context. *)
val fmt : full : bool -> formatter -> t -> unit
