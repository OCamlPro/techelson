(** Common types and functions used by the whole program. *)

include (module type of Format)

(** Iterator over options. *)
val if_let_some : ('a -> unit) -> 'a option -> unit

(** Getter on options with a default value. *)
val unwrap_or : 'a -> 'a option -> 'a

(** Getter on options with a lazy default value. *)
val unwrap_or_else : (unit -> 'a) -> 'a option -> 'a

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
end

(** Sets the configuration. *)
val set_conf : Conf.t -> unit

(** Configuration, built by CLAP. *)
val conf : unit -> Conf.t