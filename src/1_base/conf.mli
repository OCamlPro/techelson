(** Configuration stuff, built from CLAs. *)

(** Stores a contract file and its optional initializer. *)
type contract = {
    file : string ;
    (** Contract file. *)

    init : string option ;
    (** Initializer. *)
}

(** Contract constructor. *)
val mk_contract : string -> string option -> contract

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
}

(** Default configuration. *)
val default : t

(** Formatter. *)
val fmt : Format.formatter -> t -> unit