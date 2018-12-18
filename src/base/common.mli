(** Common types and functions used by the whole program. *)

include (module type of Format)

(** Iterator over options. *)
val if_let_some : ('a -> unit) -> 'a option -> unit

(** Getter on options with a default value. *)
val unwrap_or : 'a -> 'a option -> 'a

(** Getter on options with a lazy default value. *)
val unwrap_or_else : (unit -> 'a) -> 'a option -> 'a

(** Tries to open a file. *)
val open_file : string -> in_channel

(** List helpers. *)
module Lst : sig
    (** Head of a list. *)
    val hd : 'a list -> 'a option

    (** Tail of a list. *)
    val tl : 'a list -> 'a list option

    (** Length of a list. *)
    val len : 'a list -> int

    (** Fold over lists. *)
    val fold : ('acc -> 'a -> 'acc) -> 'acc -> 'a list -> 'acc
end

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

    (** Wraps something between parens. *)
    val fmt_paren :
        (formatter -> 'a -> unit) ->
        formatter -> 'a -> unit

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

    (** Runs two function in sequence. *)
    val unit_combine : (unit -> unit) -> (unit -> unit) -> unit -> unit

    (** Returns the empty string in the input is `1`, `"s"` otherwise. *)
    val plurify : int -> string
end

(** Helpers to check things. *)
module Check : sig
    (** Checks that an arity constraint is respected.

        Most parameters deal with reporting the error properly if any.
    
        Parameters:
        - `desc` description of the elements of the list
        - `expected` number of arguments expected
        - `blah` description of the constructor we're checking the arity of
        - `args` list of arguments

        This function fails (throws an exception) whenever `expected` is different from the length
        of `args`.

        # Examples

        ```ocaml
        open Common
        let token = "list" in
        let sub = Dtyp.string () in
        (* Following does not fail. *)
        Check.arity "type argument" 1 (
            fun () -> sprintf "type constructor `%s`" token
        ) [ sub ];
        (* Following fails. *)
        Check.arity "type argument" 2 (
            fun () -> sprintf "type constructor `%s`" token
        ) [ sub ];
        ```
    *)
    val arity : string -> int -> (unit -> string) -> 'a list -> unit

    (** Same as `arity` but with a greater than or equal to constraint. *)
    val arity_ge : string -> int -> (unit -> string) -> 'a list -> unit

    (** Same as `arity` but with a less than or equal to constraint. *)
    val arity_le : string -> int -> (unit -> string) -> 'a list -> unit
end

(** Handles information about where tests came from. *)
module Source : sig
    (** Either stdin or a file. *)
    type t =
    | Stdin
    | File of string

    (** Source formatter. *)
    val fmt : formatter -> t -> unit

    (** Turns a source in an input channel. *)
    val to_channel : t -> in_channel
end

(** Sets the configuration. *)
val set_conf : Conf.t -> unit

(** Configuration, built by CLAP. *)
val conf : unit -> Conf.t

(** Logs something at some log level.

    Actually output if the configuration's log level is `>=` the level. *)
val log : int -> ('a, formatter, unit) format -> 'a

(** Logs something at log level 0 (always active). *)
val log_0 : ('a, Format.formatter, unit) format -> 'a

(** Logs something at log level 1. *)
val log_1 : ('a, Format.formatter, unit) format -> 'a

(** Logs something at log level 2. *)
val log_2 : ('a, Format.formatter, unit) format -> 'a

(** Logs something at log level 3. *)
val log_3 : ('a, Format.formatter, unit) format -> 'a