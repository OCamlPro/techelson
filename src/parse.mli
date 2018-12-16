(** Michelson parser. *)

open Common

(** Tokens for instruction and datatype parsing. *)
module Token : sig
    (** Datatype tokens related stuff. *)
    module DTyp : sig
        (** Datatype tokens. *)
        type t =
        (** Leaf datatype. *)
        | Leaf of Mic.DTyp.leaf
        (** List (composite). *)
        | List
        (** Option (composite). *)
        | Option
        (** Set (composite). *)
        | Set
        (** Contract (composite). *)
        | Contract
        (** Pair (composite). *)
        | Pair
        (** Or (composite). *)
        | Or
        (** Map (composite). *)
        | Map
        (** BigMap (composite). *)
        | BigMap

        (** Datatype token formatter. *)
        val fmt : formatter -> token -> unit

        (** Turns a datatype token in a datatype given some type paramaters. *)
        val to_dtyp : token -> Mic.DTyp.t list -> Mic.DTyp.t
    end
end