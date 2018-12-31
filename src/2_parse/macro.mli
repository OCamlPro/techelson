(** Macro parsing. *)

(** Recognizes strings corresponding to macro operators. *)
val op : string -> Base.Mic.Macro.op option

(** Parses a prefix and then an operator. *)
val prefixed_op :
    (unit -> string list) ->
    string ->
    (Base.Mic.Macro.op -> 'a) ->
    string ->
    'a option

(** Parses a pair operator `[AIP]`. *)
val pair_op : char -> Base.Mic.Macro.pair_op option

(** Parses a sequence of pair operators.

    Returns the tail of the input string: the part that was not parsed. *)
val pair_ops : string -> Base.Mic.Macro.pair_op list * string

(** Parses an unpair operator `[AD]`. *)
val unpair_op : char -> Base.Mic.Macro.unpair_op option

(** Parses a sequence of unpair opoerators.

    Returns the tail of the input string: the part that was not parsed. *)
val unpair_ops : string -> Base.Mic.Macro.unpair_op list * string
