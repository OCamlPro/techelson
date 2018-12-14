(** Configuration stuff, built from CLAs. *)

(** Stores the configuration from CLAs. *)
type t = {
    mutable verb : int;
    (** Verbosity level. *)

    mutable args : string list;
    (** Special field storing values not passed as options. *)
}

(** Default configuration. *)
val default : t

(** Formatter. *)
val fmt : Format.formatter -> t -> unit