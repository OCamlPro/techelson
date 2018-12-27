(** Types and helpers for macro expansion.

    Macros expand to lists of instructions `Mic.t list`.
*)

(** Helpful expansion-related functions. *)
module Utils : sig
    (** Turns a macro comparison operator in an instruction. *)
    val macro_op_to_mic : Mic.Macro.op -> Mic.t
end

(** Expands a `CMP<OP>` macro. *)
val macro_cmp : Mic.Macro.op -> Mic.t list

(** Expands a `IF<OP>` macro. *)
val macro_if : Mic.Macro.op -> Mic.t -> Mic.t -> Mic.t list

(** Expands a `IF_CMP<OP> macro. *)
val macro_if_cmp : Mic.Macro.op -> Mic.t -> Mic.t -> Mic.t list

(** Expansion of a `FAIL` macro. *)
val macro_fail : Mic.t list

(** Expansion of a `ASSERT` macro. *)
val macro_assert : Mic.t list

(** Expands a `ASSERT_<OP>` macro. *)
val macro_assert_ : Mic.Macro.op -> Mic.t list

(** Expands a `ASSERT_CMP<OP>` macro. *)
val macro_assert_cmp : Mic.Macro.op -> Mic.t list

(** Expansion of a `ASSERT_NONE` macro. *)
val macro_assert_none : Mic.t list

(** Expansion of a `ASSERT_SOME` macro. *)
val macro_assert_some : Mic.t list

(** Expansion of a `ASSERT_LEFT` macro. *)
val macro_assert_left : Mic.t list

(** Expansion of a `ASSERT_RIGHT` macro. *)
val macro_assert_right : Mic.t list

(** Expansion of a `DII+P` macro.

    Input int is the number of `I` repeated **after the first one**. So `DIIIP` is `2`.
*)
val macro_dip : int -> Mic.t -> Mic.t list

(** Expansion of a `DUU+P` macro.

    Input int is the number of `U` repeated **after the first one**. So `DUUUP` is `2`.
*)
val macro_dup : Annot.vars -> int -> Mic.t list

(** Expands a `P[PAI]*R` macro. *)
val macro_pair : Mic.Macro.pair_op list -> Mic.t list

(** Expands a `UNP[PAI]*R` macro. *)
val macro_unpair : Mic.Macro.pair_op list -> Mic.t list

(** Expands a `C[AD]*R` macro. *)
val macro_cadr : Mic.Macro.unpair_op list -> Mic.t list

(** Expands a `SET_C[AD]*R` macro. *)
val macro_set_cadr : Mic.Macro.unpair_op list -> Mic.t list

(** Expands a `MAP_C[AD]*R` macro. *)
val macro_map_cadr : Mic.Macro.unpair_op list -> Mic.t -> Mic.t list

(** Expands a `IF_SOME` macro. *)
val macro_if_some : Mic.t -> Mic.t -> Mic.t list
