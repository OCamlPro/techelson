(** Common types and functions used by the whole program. *)

include (module type of Format)

(** Iterator over options. *)
val if_let_some : ('a -> unit) -> 'a option -> unit

(** Formatting-related stuff. *)
module Fmt : sig
    (** Type of separator printers by side-effect. *)
    type sep = unit -> unit

    (** Type of sequence termination printers by side-effect. *)
    type seq_end = unit -> unit

    (** List formatter. *)
    val fmt_list :
        (formatter -> unit -> unit) ->
        (formatter -> 'a -> unit) ->
        formatter ->
        'a list ->
        unit

    (** Closes a formatting block. *)
    val cls : formatter -> seq_end

    (** Closes two formatting blocks. *)
    val cls2 : formatter -> seq_end

    (** Outputs a string and closes a formatting block. *)
    val cls_of : formatter -> string -> seq_end

    (** Outputs a string and closes two formatting blocks. *)
    val cls2_of : formatter -> string -> seq_end

    (** Closing parenthesis. *)
    val cls_paren : formatter -> seq_end

    (** Outputs a break `@,`. *)
    val sep_brk : formatter -> sep

    (** Outputs a space ` `. *)
    val sep_spc : formatter -> sep

    (** Outputs nothing. *)
    val sep_non : formatter -> sep

    (** Returns the empty string in the input is `1`, `"s"` otherwise. *)
    val plurify : int -> string
end

(** Configuration, built by CLAP. *)
val conf : Conf.t