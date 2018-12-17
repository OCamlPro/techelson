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
    | If of op
    | IfCmp of op
    | Fail
    | Assert
    | Assert_ of op
    | AssertCmp of op
    | AssertNone
    | AssertSome
    | AssertLeft
    | AssertRight
    | Dip of (int * 'ins)
    | Dup of (int * 'ins)
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

    (** Instructions. *)
    type t =
    | Leaf of leaf
    | EmptySet of Dtyp.t
    | EmptyMap of Dtyp.t * Dtyp.t
    | Non of Dtyp.t
    | Left of Dtyp.t
    | Right of Dtyp.t
    | Nil of Dtyp.t
    | Seq of t list
    | If of t * t
    | Loop of t
    | LoopLeft of t
    | Dip of t
    | Push of Dtyp.t * t
    | Lambda of Dtyp.t * Dtyp.t * t
    | Iter of t
    | IfNone of t * t
    | IfLeft of t * t
    | IfRight of t * t
    | IfCons of t * t
    | Macro of t list * t Macro.t

    (** Creates a sequence instruction. *)
    val mk_seq : t list -> t

    (** Instruction formatter. *)
    val fmt : formatter -> t -> unit

    (** Instruction parser. *)
    val parse : string -> Dtyp.t list -> t list -> t