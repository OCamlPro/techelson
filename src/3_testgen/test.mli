(** Testcase generation for a contract. *)

open Base

(** Generates a testcase for a contract. *)
val generate : Contract.t -> string -> Testcase.t