(** Base exception and helper functions. *)

open Format

(** If true, then the backtrace will be included when raising exceptions. *)
val set_print_backtrace : bool -> unit

(** Internal exception encode special behavior.

    These exception should always be caught internally.
*)
module Internal : sig
    (** Internal exception encode special behavior. *)
    type t =
    | ApplyOps
    (** Thrown when executing a test case and running into `APPLY_OPERATIONS`. *)

    (** Formatter. *)
    val fmt : formatter -> t -> unit
end

(** Tezos *protocol* exceptions.

    Encodes protocl errors and operation application problems.
*)
module Protocol : sig
    (** Tezos *protocol* exceptions. *)
    type t =
    | Failure of string
    (** Ran into the `FAILWITH` instruction. *)
    | TooPoor of string * string * Int64.t
    (** Insufficient amount to process transaction.

        First string is the name of the sender, second is the name of the target, last is the amount of the transaction that triggered this beharior.
    *)
    | MutezOvrflw of string
    (** Mutez overflow.

        Can happen either during a transfer or during operations on mutez.
    *)
    | MutezUdrflw of string
    (** Mutez underflow.

        Can happen either during a transfer or during operations on mutez.
    *)
    | DivZero of string
    (** Division by zero. *)
    | Tezos of string
    (** Something went wrong in the protocol.

        Trying to run exactly the same operation twice throws this kind of exception for instance.
    *)

    (** Formatter. *)
    val fmt : formatter -> t -> unit
end

(** Aggregates all theory-agnostic errors. *)
type exc =
| Error of string list * exn option * Printexc.raw_backtrace
(** Thrown when something bad happens internally. *)
| Internal of Internal.t
(** Internal exception that encodes special behavior. *)
| Protocol of Protocol.t
(** Something was rejected by the protocol itself. *)

(** Techelson exceptions. *)
exception Exc of exc

(** Formats an exception. *)
val fmt : formatter -> exn -> unit

(** Aggregates functions that raise exceptions. *)
module Throw : sig
    (** Raises `ApplyOps`. *)
    val apply_ops : unit -> 'a

    (** Raises a failure. *)
    val failure : string -> 'a

    (** Raises a tezos protocol error. *)
    val tezos : string -> 'a

    (** Raises an insufficient amount error. *)
    val too_poor : src : string -> tgt : string -> amount : Int64.t -> 'a

    (** Raises a mutez overflow error. *)
    val mutez_overflow : string -> 'a

    (** Raises a mutez underflow error. *)
    val mutez_underflow : string -> 'a

    (** Raises a division by zero error. *)
    val div_zero : string -> 'a
end

(** Raises an exception from a single trace frame. *)
val throw : string -> 'a

(** Raises an exception from a trace. *)
val throws : string list -> 'a

(** Replaces an exception.

    If evaluation of the second argument fails, generates the exception composed of the first
    argument. *)
val erase_err : (unit -> string) -> (unit -> 'a) -> 'a

(** Chains an error to a trace of errors. *)
val chain_err : (unit -> string) -> (unit -> 'a) -> 'a

(** Chains some errors to a trace of errors. *)
val chain_errs : (unit -> string list) -> (unit -> 'a) -> 'a

(** Catches an exception and prints it.

    First parameter is the indentation of the error.
*)
val catch_print : (unit -> 'a) -> 'a option

(** Catches an exception, prints it, and exits with code `2` *)
val catch_fail : (unit -> 'a) -> 'a

(** Fails by saying unreachable code was reached. *)
val unreachable : unit -> 'a

(** Fails by saying an unimplemented feature was triggered. *)
val unimplemented : unit -> 'a

(** If the exception was originally a protocol error, returns that error. *)
val get_protocol : exn -> Protocol.t option

(** If the exception was originally an internal error, returns that error. *)
val get_internal : exn -> Internal.t option
