(** Types and helpers to load contracts and test cases. *)

open Base
open Common

(** Number of errors found during loading.

    Functions returning this metric print the error. *)
type error_count = int

(** Loads some files. *)
val of_source : Source.t list -> in_channel list * error_count

(** Loads a contract.

    `contract name source chan`
*)
val contract : string -> Source.t -> in_channel -> Contract.t

(** Loads a testcase.

    `test name source chan`
*)
val test : string -> Source.t -> in_channel -> Testcase.t

(** Loads some contracts.

    `contracts files constructor`
*)
val contracts : Conf.contract list -> Contract.t list * error_count

(** Loads some tests.

    `tests files constructor`
*)
val tests : string list -> Testcase.t list * error_count

(** Loads a ful context. *)
val context :
    contract_files : Conf.contract list ->
    test_files : string list ->
    Testcases.t * error_count