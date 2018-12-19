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
    | IfSome of 'ins * 'ins
    | SetCadr of unpair_op list
    | MapCadr of unpair_op list * 'ins

    (** Formatter for macros.

        First parameter is the instruction formatter. *)
    val fmt : (formatter -> 'ins -> unit) -> formatter -> 'ins t -> unit
end

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
| CreateContract
| CreateAccount
| TransferTokens
| SetDelegate
| Balance
| Contract
| Source
| Sender
| Self
| Amount
| ImplicitAccount
| StepsToQuota
| Now
| Pack
| Unpack
| Slice
| HashKey
| Blake2B
| Sha256
| Sha512
| CheckSignature

(** Leaf formatter. *)
val fmt_leaf : formatter -> leaf -> unit

(** String to leaf conversion. *)
val leaf_of_string : string -> leaf option

(** Returns the maximum number of variable annotations supported by a leaf. *)
val var_arity_of_leaf : leaf -> int

(** Instructions. *)
type 'sub ins =
| Leaf of leaf
| EmptySet of Dtyp.t
| EmptyMap of Dtyp.t * Dtyp.t
| Non of Dtyp.t
| Left of Dtyp.t
| Right of Dtyp.t
| Nil of Dtyp.t
| Seq of 'sub list
| If of 'sub * 'sub
| Loop of 'sub
| LoopLeft of 'sub
| Dip of 'sub
| Push of Dtyp.t * 'sub
| Lambda of Dtyp.t * Dtyp.t * 'sub
| Iter of 'sub
| IfNone of 'sub * 'sub
| IfLeft of 'sub * 'sub
| IfRight of 'sub * 'sub
| IfCons of 'sub * 'sub
| Macro of 'sub list * 'sub Macro.t

(** Type of variables. *)
type var = string

(** Instruction with variable bindings. *)
type t = {
    ins : t ins ;
    vars : var list ;
}

(** Creates an instruction. *)
val mk : ?vars: var list -> t ins -> t

(** Creates an instruction from a leaf. *)
val mk_leaf : ?vars: var list -> leaf -> t

(** Creates a sequence instruction. *)
val mk_seq : t list -> t

(** Instruction formatter. *)
val fmt : formatter -> t -> unit

(** Instruction parser. *)
val parse : string -> Dtyp.t list -> t list -> string list -> t
