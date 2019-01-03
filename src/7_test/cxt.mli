(** A context is the result of loading contracts and test cases. *)

open Base
open Common

(** Type of contexts. *)
type t

(** Context constructor. *)
val mk : Contract.t StrMap.t -> Testcase.t list -> t

(** Constructor that takes raw data. *)
val of_raw : Contract.t list -> Testcase.t list -> t

(** Retrieves a contract from the context.

    Fails if the contract is unknown. *)
val get_contract : string -> t -> Contract.t

(** Retrieves the testcases of a context. *)
val get_tests : t -> Testcase.t list

(** Formats a context. *)
val fmt : full : bool -> formatter -> t -> unit