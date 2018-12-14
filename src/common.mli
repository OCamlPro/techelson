(** Common types and functions used by the whole program. *)

include (module type of Format)

(** Configuration, built by CLAP. *)
val conf : Conf.t