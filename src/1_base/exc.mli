(** Base exception and helper functions. *)

(** Base exception, used everywhere. *)
exception Exc of string list

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

(** Catches an exception and prints it.

    First parameter is the indentation of the error.
*)
val catch_print : int -> (unit -> 'a) -> 'a option

(** Catches an exception, prints it, and exits with code `2` *)
val catch_fail : (unit -> 'a) -> 'a

(** Fails by saying unreachable code was reached. *)
val unreachable : unit -> 'a