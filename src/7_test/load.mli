(** Types and helpers to load contracts and test cases. *)

open Base
open Common

(** A list of errors in the form of exceptions. *)
type error_list = Exc.exc list

(** Formats a list of errors as a vertical block. *)
val fmt_error_list : formatter -> error_list -> unit

(** Errors during contract/testcase loading. *)
type errors = {
    contracts : error_list ;
    (** Errors during contract loading. *)
    testcases : error_list ;
    (** Errors during testcase loading. *)
}

(** True if there is one error or more. *)
val has_errors : errors -> bool

(** Formats load errors.

    The output may have line breaks `@ `, but the surrounding block is left to the caller.
*)
val fmt_errors : formatter -> errors -> unit

(** Number of errors. *)
val error_count : errors -> int

(** Loads some files. *)
val of_source : Source.t list -> in_channel list * error_list

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
val contracts : Conf.contract list -> Contract.t list * error_list

(** Loads some tests.

    `tests files constructor`
*)
val tests : string list -> Testcase.t list * error_list

(** Loads a full context: contracts and testcases. *)
val context :
    contract_files : Conf.contract list ->
    test_files : string list ->
    Testcases.t * errors
