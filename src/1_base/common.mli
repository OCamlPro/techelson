(** Common types and functions used by the whole program. *)

include (module type of Format)

(** Identity function. *)
val id : 'a -> 'a

(** Iterator over options. *)
val if_let_some : ('a -> unit) -> 'a option -> unit

(** True if the option is `Some` of something. *)
val is_some : 'a option -> bool

(** Unwraps an option. Fails if `None`. *)
val unwrap : 'a option -> 'a

(** Getter on options with a default value. *)
val unwrap_or : 'a -> 'a option -> 'a

(** Getter on options with a lazy default value. *)
val unwrap_or_else : (unit -> 'a) -> 'a option -> 'a

(** Tries to open a file. *)
val open_file : string -> in_channel

(** Option type. *)
module Opt : sig
    (** Map over options. *)
    val map : ('a -> 'b) -> 'a option -> 'b option
end

(** Union type. *)
module Either : sig
    (** Either of two types. *)
    type ('l, 'r) t = | Lft of 'l | Rgt of 'r

    (** Left constructor. *)
    val lft : 'l -> ('l, 'r) t

    (** Right constructor. *)
    val rgt : 'r -> ('l, 'r) t

    (** Formatter. *)
    val fmt : (formatter -> 'l -> unit) -> (formatter -> 'r -> unit) -> formatter -> ('l, 'r) t -> unit

    (** Map over the left part of a disjunction. *)
    val map_lft : ('l -> 'lft) -> ('l, 'r) t -> ('lft, 'r) t

    (** Map over the right part of a disjunction. *)
    val map_rgt : ('r -> 'rgt) -> ('l, 'r) t -> ('l, 'rgt) t

    (** Transparent formatter. *)
    val fmt_through : (formatter -> 'l -> unit) -> (formatter -> 'r -> unit) -> formatter -> ('l, 'r) t -> unit
end

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

    (** Turns an option into a list. *)
    val of_opt : 'a option -> 'a list
end

(** Formatting-related stuff. *)
module Fmt : sig
    (** String formatter. *)
    val fmt_str : formatter -> string -> unit

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

    (** Outputs a space break `@ `. *)
    val sep_spc_brk : formatter -> sep

    (** Outputs a space ` `. *)
    val sep_spc : formatter -> sep

    (** Outputs nothing. *)
    val sep_non : formatter -> sep

    (** Runs two function in sequence. *)
    val unit_combine : (unit -> unit) -> (unit -> unit) -> unit -> unit

    (** Returns the empty string in the input is `1`, `"s"` otherwise. *)
    val plurify : int -> string
end

(** Int set. *)
module IntSet : sig
    (** Type of int sets. *)
    type t

    (** Creates an empty set. *)
    val empty : unit -> t

    (** Clones a set. *)
    val clone : t -> t

    (** Adds an element to the set.

        Returns true if the element was not there (`is_new`).
    *)
    val add : int -> t -> bool
end

(** String maps. *)
module StrMap : sig
    type 'a t
    type key = string
    val find : key -> 'a t -> 'a
    val add : key -> 'a -> 'a t -> 'a t
    val empty : 'a t
    val iter : (key -> 'a -> unit) -> 'a t -> unit

    val get : key -> 'a t -> 'a option
    val insert : key -> 'a -> 'a t -> 'a t * 'a option

    val values : 'a t -> 'a list
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

(** Catches an exception. *)
val catch_exn : (unit -> 'a) -> ('a, exn) Either.t

(** Catches protocol errors. *)
val catch_protocol_exn : (unit -> 'a) -> ('a, Exc.Protocol.t) Either.t

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

(** Logs something at log level 4. *)
val log_4 : ('a, Format.formatter, unit) format -> 'a