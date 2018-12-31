(** Contract types and helpers. *)

open Common

(** Contract signature. *)
type signature = {
    name : string option ;
    (** Name of the signature (optional). *)
    entry_param : Dtyp.t ;
    (** Type of the entry parameter. *)
}

(** Signature formatter. *)
val fmt_sig : formatter -> signature -> unit

(** A contract. *)
type t = {
    name : string ;
    (** Name of the contract. *)
    storage : Dtyp.t ;
    (** Type of the contract's storage. *)
    entry_param : Dtyp.t ;
    (** Type of the entry parameter. *)
    entry : Mic.t ;
    (** Code of the entry point. *)
    init : Mic.t option ;
    (** Initializer (optional). *)
}

(** Contract constructor. *)
val mk : string -> storage:Dtyp.t -> entry_param:Dtyp.t -> Mic.t -> Mic.t option -> t

(** Contract formatter. *)
val fmt : bool -> formatter -> t -> unit

(** Name of a contract from the name of the file it was loaded from.

    - drops everything befor the last `'/'`
    - drops everything after the first `'.'`
    - capitalizes the first letter.
*)
val name_of_file : string -> string