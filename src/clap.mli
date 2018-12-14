(** CLAP stuff.

    Flags such as `-v` can **never** take any arguments, since they're flags. Options such as `--verb` should always take arguments, although this is not mandatory.
*)

(** CLAs, without the first useless string (program name). *)
val args : string list

(** Runs CLAP on custom arguments. *)
val run_on : string list -> Conf.t

(** Runs CLAP and produces a configuration. *)
val run : unit -> Conf.t