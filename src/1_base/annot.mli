(** Types and helpers for annotations. *)

open Common

(** Variable annotations. *)
module Var : sig
    (** Type of variable annotations. *)
    type t

    (** Creates a variable annotation from a string. *)
    val of_string : string -> t

    (** The wildcard variable annotation. *)
    val wild : t

    (** Variable annotation formatter. *)
    val fmt : formatter -> t -> unit
end

(** Field annotations. *)
module Field : sig
    (** Type of field annotations. *)
    type t

    (** Creates a field annotation from a string. *)
    val of_string : string -> t

    (** The wildcard field annotation. *)
    val wild : t

    (** Field annotation formatter. *)
    val fmt : formatter -> t -> unit
end

(** Type annotations. *)
module Typ : sig
    (** Type of type annotations. *)
    type t

    (** Creates a type annotation from a string. *)
    val of_string : string -> t

    (** The wildcard type annotation. *)
    val wild : t

    (** Type annotation formatter. *)
    val fmt : formatter -> t -> unit
end

(** A list of type annotations. *)
type typs = Typ.t list

(** Formats a list of type annotations.

    If the list's not empty, inserts a space `" "` before printing the list.
*)
val fmt_typs : formatter -> typs -> unit

(** A list of variable annotations. *)
type vars = Var.t list

(** Formats a list of variable annotations.

    If the list's not empty, inserts a space `" "` before printing the list.
*)
val fmt_vars : formatter -> vars -> unit

(** A list of field annotations. *)
type fields = Field.t list

(** Formats a list of field annotations.

    If the list's not empty, inserts a space `" "` before printing the list.
*)
val fmt_fields : formatter -> fields -> unit
