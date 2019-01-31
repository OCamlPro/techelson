open Common

(** Type checking context. Stores type constraints. *)
type t

(** Context formatter. *)
val fmt : formatter -> t -> unit

(** Creates an empty type-checking context. *)
val empty : unit -> t

(** Unifies two types.

    The first type cannot contain type variables.
*)
val unify : t -> Dtyp.t -> Dtyp.t -> unit
