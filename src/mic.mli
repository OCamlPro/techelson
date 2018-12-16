(** Types and helpers for michelson stuff. *)

open Common

(** Datatypes. *)
module DTyp : sig
    (** Nullary datatypes. *)
    type leaf =
    | Str
    | Nat
    | Int
    | Bytes
    | Bool
    | Unit
    | Mutez
    | Address
    | Operation
    | Key
    | KeyH
    | Signature
    | Timestamp

    (** Formatter for nullary datatypes. *)
    val fmt_leaf : formatter -> leaf -> unit

    (** Datatypes. *)
    type t =
    | Leaf of leaf

    | List of t
    | Option of t
    | Set of t
    | Contract of t

    | Pair of t * t
    | Or of t * t
    | Map of t * t
    | BigMap of t * t

    (** String datatype. *)
    val str : t

    (** Integer datatype. *)
    val int : t

    (** Natural datatype. *)
    val nat : t

    (** Bytes datatype. *)
    val bytes : t

    (** Bool datatype. *)
    val bool : t

    (** Unit datatype. *)
    val unit : t

    (** Mutez datatype. *)
    val mutez : t

    (** Address datatype. *)
    val address : t

    (** Operation datatype. *)
    val operation : t

    (** Key datatype. *)
    val key : t

    (** Key hash datatype. *)
    val key_hash : t

    (** Signature datatype. *)
    val signature : t

    (** Timestamp datatype. *)
    val timestamp : t

    (** Formatter for datatypes. *)
    val fmt : formatter -> t -> unit
end

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

(** Instruction-related types and helpers. *)
module Ins : sig
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
    | Some
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

    (** Instructions. *)
    type t =
    | Leaf of leaf
    | EmptySet of DTyp.t
    | EmptyMap of DTyp.t * DTyp.t
    | None of DTyp.t
    | Left of DTyp.t
    | Right of DTyp.t
    | Nil of DTyp.t
    | Seq of t list
    | If of t * t
    | Loop of t
    | LoopLeft of t
    | Dip of t
    | Push of DTyp.t * t
    | Lambda of DTyp.t * DTyp.t * t
    | Iter of t
    | IfNone of t * t
    | IfLeft of t * t
    | IfRight of t * t
    | IfCons of t * t
    | Macro of t list * t Macro.t

    (** Instruction formatter. *)
    val fmt : formatter -> t -> unit
end