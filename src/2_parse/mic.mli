(** Instruction parsing. *)

open Base
open Base.Common

type arg = (Mic.t, Mic.const) Either.t
type args = arg list

(** Parses instructions. *)
val parse : string -> Annot.t -> Dtyp.t list -> args -> Mic.t

(** Parses a macro. *)
val macro : Annot.t -> Mic.t Mic.Macro.t -> Mic.t