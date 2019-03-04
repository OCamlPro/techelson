(** Instruction types and helpers. *)

open Common

(** Macro-related types and helpers. *)
module Macro : sig
    (** Comparison operators used in macros. *)
    type op =
    | Eq | Neq | Lt | Le | Ge | Gt

    (** Formatter for comparison operators. *)
    val fmt_op : formatter -> op -> unit

    (** Pair construction operators used in macros. *)
    type pair_op =
    | P | A | I

    (** Formatter for pair construction operators. *)
    val fmt_pair_op : formatter -> pair_op -> unit

    (** Pair destruction operators used in macros. *)
    type unpair_op =
    | A | D

    (** Formatter for pair destruction operators. *)
    val fmt_unpair_op : formatter -> unpair_op -> unit

    (** Macros, polymorphic over instructions. *)
    type 'ins t =
    | Cmp of op
    | If of (op * 'ins * 'ins)
    | IfCmp of (op * 'ins * 'ins)
    | IfSome of 'ins * 'ins
    | Int
    | Fail
    | Assert
    | Assert_ of op
    | AssertCmp of op
    | AssertNone
    | AssertSome
    | AssertLeft
    | AssertRight
    | Dip of (int * 'ins)
    | Dup of int
    | P of pair_op list
    | Unp of pair_op list
    | CadR of unpair_op list
    | SetCadr of unpair_op list
    | MapCadr of unpair_op list * 'ins

    (** Formatter for macros.

        First parameter is the instruction formatter. If it is `None`, instructions will be printed
        as `...`. *)
    val fmt : (formatter -> 'ins -> unit) option -> formatter -> 'ins t -> unit
end

(** Enum of the hash functions. *)
type hash_fun =
| B58Check
| Blake2B
| Sha256
| Sha512

(** Nullary instructions. *)
type leaf =
| Failwith
| Exec
| Drop
| Dup
| Swap
| Unit
| Eq
| Neq
| Lt | Le | Gt | Ge
| Or | And | Xor | Not
| Neg | Abs | Add | Sub | Mul | EDiv
| Lsl | Lsr
| Compare
| Concat
| Size
| Pair
| Car
| Cdr
| Get
| Mem
| Update
| Som
| Cons
| CreateAccount
| TransferTokens
| SetDelegate
| Balance
| Source
| Sender
| Self
| Amount
| ImplicitAccount
| StepsToQuota
| Now
| Pack
| Slice
| Address
| Hash of hash_fun
| CheckSignature
| Rename

(** Leaf formatter. *)
val fmt_leaf : formatter -> leaf -> unit

(** String to leaf conversion. *)
val leaf_of_string : string -> leaf option

(** Returns the maximum number of annotations supported by a leaf.

    - type annot arity
    - var annot arity
    - field annot arity
*)
val annot_arity_of_leaf : leaf -> int * int * int

(** Extensions. *)
type extension =
| GetStorage of Dtyp.t
| GetBalance
| ApplyOps
| PrintStack
| MustFail of Dtyp.t
| Step of string option
| SetSource of t
| SpawnContract of Dtyp.t
| SetTimestamp

(** Instructions. *)
and 'sub ins =
| Leaf of leaf
| Cast of Dtyp.t
| EmptySet of Dtyp.t
| EmptyMap of Dtyp.t * Dtyp.t
| Unpack of Dtyp.t
| Non of Dtyp.t
| Left of Dtyp.t
| Right of Dtyp.t
| Nil of Dtyp.t
| Contract of Dtyp.t
| Seq of 'sub list
| If of 'sub * 'sub
| Loop of 'sub
| LoopLeft of 'sub
| Dip of 'sub
| Push of Dtyp.t * const
| Lambda of Dtyp.t * Dtyp.t * 'sub
| Iter of 'sub
| Map of Dtyp.t * 'sub
(** Map over maps.

    - type of the codomain of the lambda
    - lambda to apply
*)
| IfNone of 'sub * 'sub
| IfLeft of 'sub * 'sub
| IfRight of 'sub * 'sub
| IfCons of 'sub * 'sub
| CreateContract of (contract option, string) Either.t
| Macro of 'sub list * 'sub Macro.t
| Extension of extension

and const =
| U

| Bool of bool
| Int of string
| Str of string
| Bytes of string

| Cont of contract

| Lft of const
| Rgt of const

| No
| So of const

| Pr of const * const
| Lst of const list
| Mapping of (const * const) list

and contract = {
    storage : Dtyp.t ;
    (** Type of the storage. *)
    param : Dtyp.t ;
    (** Type of the parameter of the entry point. *)
    entry : t ;
    (** Code for the entry point. *)
}

(** Instruction with variable bindings. *)
and t = {
    ins : t ins ;
    typs : Annot.typs ;
    vars : Annot.vars ;
    fields : Annot.fields ;
    comments : string list ;
}

(** Type of a contract. *)
val typ_of_contract : ?alias : Annot.Typ.t option -> contract -> Dtyp.t

(** The unit contract. *)
val unit_contract : contract

(** Creates a string constant.

    De-escapes escape characters. *)
val mk_str_const : string -> const

(** Creates an instruction. *)
val mk :
    ?vars: Annot.vars ->
    ?fields: Annot.fields ->
    ?typs: Annot.typs ->
    ?comments: string list ->
    t ins ->
    t

(** Creates an instruction. *)
val nu_mk : ?annot : Annot.t option -> ?comments : string list -> t ins -> t

(** Creates a contract. *)
val mk_contract : storage : Dtyp.t -> param : Dtyp.t -> t -> contract

(** Creates a contract from a list of parameters.
    
    Used by parsing. This function fails if any of the input lists are not singletons. *)
val mk_contract_of_lists : storage : Dtyp.t list -> param : Dtyp.t list -> t list -> contract

(** Creates an instruction from a leaf. *)
val mk_leaf :
    ?vars: Annot.vars ->
    ?fields: Annot.fields ->
    ?typs: Annot.typs ->
    leaf ->
    t

(** Creates an instruction from a leaf. *)
val nu_mk_leaf : ?annot : Annot.t option -> ?comments : string list -> leaf -> t

(** Creates a sequence instruction. *)
val mk_seq : t list -> t

(** Extension formatter. *)
val fmt_extension :
    ?annots : (formatter -> unit -> unit) ->
    formatter ->
    extension ->
    unit

(** Contract formatter.

    If `full` is true, the code of the contract will be printed, otherwise it will appear as `...`.
*)
val fmt_contract : full : bool -> formatter -> contract -> unit

(** Constant formatter. *)
val fmt_const : formatter -> const -> unit

(** Instruction formatter. *)
val fmt : formatter -> t -> unit

(** Adds comments to an instruction. *)
val comments : string list -> t -> t