(** Configuration stuff, built from CLAs. *)

open Format

(** Stores a contract file and its optional initializer. *)
type contract = {
    file : string ;
    (** Contract file. *)

    init : string option ;
    (** Initializer. *)
}

(** Contract constructor. *)
val mk_contract : string -> string option -> contract

(** Testgen mode, stores options for test generation. *)
type testgen_mode = {
    mutable count : int ;
    (** Number of testcases to generate. *)
}

(** Formats testgen options. *)
val fmt_testgen_mode : formatter -> testgen_mode -> unit

(** Default testgen mode. *)
val default_testgen_mode : testgen_mode

(** Lists the modes available. *)
type mode =
| Testgen of testgen_mode
(** Test generation. *)
| Inactive
(** No mode is active. *)

(** Formats a mode. *)
val fmt_mode : formatter -> mode -> unit

(** Stores the configuration from CLAs. *)
type t = {
    mutable verb : int;
    (** Verbosity level. *)

    mutable step : bool;
    (** Lockstep evaluation. *)

    mutable contracts : contract list;
    (** Contracts of the run. *)

    mutable args : string list;
    (** Special field storing values not passed as options. *)

    mutable mode : mode;
    (** Active mode, if any. *)
}

(** Default configuration. *)
val default : t

(** Formatter. *)
val fmt : formatter -> t -> unit

(** Acts on the testgen mode.

    Fails if the mode is not the testgen mode.
*)
val map_testgen_mode : (testgen_mode -> unit) -> t -> unit

(** Sets the mode of the configuration.

    Fails if the mode is not `Inactive`.
*)
val set_mode : mode -> t -> unit
