(** Helpers to check things. *)

open Common

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
val arity : string -> (unit -> string) -> int -> 'a list -> unit

(** Same as `arity` but with a greater than or equal to constraint. *)
val arity_ge : string -> (unit -> string) -> int -> 'a list -> unit

(** Same as `arity` but with a less than or equal to constraint. *)
val arity_le : string -> (unit -> string) -> int -> 'a list -> unit

(** Checks that all elements in a list are distinct.

    `distinct desc fmt lst`
    - `desc`: description of the elements of the list
    - `fmt`: formatter for the elements of the list
    - `lst`: list of elements to check
    
    The first two arguments are only used if two elements are the same.
*)
val distinct : string -> (formatter -> 'a -> unit) -> 'a list -> unit

(** Annotation checks. *)
module Annots : sig
    (** Checks that there are `expected` *distinct* type annotations or less. *)
    val typ_arity_le : (unit -> string) -> int -> Annot.typs -> unit

    (** Checks that there are `expected` *distinct* variable annotations or less. *)
    val var_arity_le : (unit -> string) -> int -> Annot.vars -> unit

    (** Checks that there are `expected` *distinct* field annotations or less. *)
    val field_arity_le : (unit -> string) -> int -> Annot.fields -> unit

    (** Checks type, variable and field annotations arity constraints. *)
    val arity_le :
        (unit -> string) ->
        (int * Annot.typs) ->
        (int * Annot.vars) ->
        (int * Annot.fields) ->
        unit
end

(** Checks that there are exactly `expected` type arguments. *)
val typ_arity : (unit -> string) -> int -> Dtyp.t list -> unit

(** Checks that there are exactly `expected` instruction arguments. *)
val args_arity : (unit -> string) -> int -> 'a list -> unit

(** Checks type and argument arity constraints. *)
val param_arity :
    (unit -> string) ->
    (int * Dtyp.t list) ->
    (int * 'a list) ->
    unit

(** Checks annotation and parameter arity constraints. *)
val full_arity :
    (unit -> string) ->
    (int * Annot.typs) ->
    (int * Annot.vars) ->
    (int * Annot.fields) ->
    (int * Dtyp.t list) ->
    (int * 'a list) ->
    unit