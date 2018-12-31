(** Instruction parsing. *)

open Base
open Base.Common

type arg = (Mic.t, Mic.const) Either.t
type args = arg list

(** Parses instructions. *)
val parse : string -> Annot.typs -> Annot.vars -> Annot.fields -> Dtyp.t list -> args -> Mic.t