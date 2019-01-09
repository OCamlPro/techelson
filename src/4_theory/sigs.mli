(** Input/output signatures for the functors that create evaluators. *)

open Base
open Base.Common

module type SigT = sig
    type t
end
module type SigTFmt = sig
    include SigT
    val fmt : formatter -> t -> unit
    val to_str : t -> string
end

module type SigArith = sig
    include SigTFmt

    val of_str : string -> t
    val of_native : int -> t
    val to_native : t -> int

    val add : t -> t -> t
    val mul : t -> t -> t
    val div : t -> t -> t
    val compare : t -> t -> int
    val zero : t
end

module type SigInt = sig
    include SigArith
    val sub : t -> t -> t
end

module type SigTez = sig
    include SigTFmt
    type nat

    val of_str : string -> t
    val of_native : int -> t
    val to_native : t -> int

    val add : t -> t -> t
    val sub : t -> t -> t
    val mul_nat : t -> nat -> t
    val div_nat : t -> nat -> t
    val div : t -> t -> t
    val compare : t -> t -> int
    val zero : t

    val to_nat : t -> nat
    val of_nat : nat -> t
end

module type SigNatConv = sig
    type int
    type nat
    val int_to_nat : int -> nat option
    val nat_to_int : nat -> int
    val nat_sub : nat -> nat -> int
end

module type SigStr = sig
    include SigTFmt

    val of_str : string -> t
    val concat : t -> t -> t
    val fmt : formatter -> t -> unit
end

module type SigStrConv = sig
    type str
    type nat
    type int
    val size : str -> nat
    val slice : nat -> nat -> str -> str
    val compare : str -> str -> int
end

module type SigTStamp = sig
    include SigTFmt

    val of_str : string -> t
    val now : unit -> t
    val compare : t -> t -> int
end

module type SigTStampConv = sig
    type t_stamp
    type int

    val add : t_stamp -> int -> t_stamp
    val sub_int : t_stamp -> int -> t_stamp
    val sub : t_stamp -> t_stamp -> int
end

module type SigKey = sig
    include SigTFmt
    val of_str : string -> t
end

module type SigKeyH = sig
    include SigTFmt

    val compare : t -> t -> int
end

module type SigKeyHConv = sig
    type key_h
    type key

    val b58check : key -> key_h
    val blake2b : key -> key_h
    val sha256 : key -> key_h
    val sha512 : key -> key_h
end

module type SigAddress = sig
    type t

    val fresh : Annot.Var.t option -> t
    val fmt : formatter -> t -> unit
    val equal : t -> t -> bool
    val uid : t -> int
end

module type SigCmp = sig
    module Int : SigInt

    module Nat : sig
        include SigArith
    end
    module NatConv : SigNatConv with type int = Int.t and type nat = Nat.t

    module Str : sig
        include SigStr
    end
    module StrConv : SigStrConv with type str = Str.t and type int = Int.t and type nat = Nat.t

    module Bytes : sig
        include SigStr
        val size : t -> Nat.t
        val slice : Nat.t -> Nat.t -> t -> t
        val compare : t -> t -> Int.t
    end
    module BytesConv : SigStrConv with type str = Bytes.t and type int = Int.t and type nat = Nat.t

    module TStamp : SigTStamp
    module TStampConv : SigTStampConv with type t_stamp = TStamp.t and type int = Int.t

    module Key : SigKey
    module KeyH : SigKeyH
    module KeyHConv : SigKeyHConv with type key = Key.t and type key_h = KeyH.t
end

module type SigTheory = sig
    module Address : SigAddress

    module Cmp : sig
        include SigCmp

        module Tez : SigTez with type nat = Nat.t

        type t =
        | B of bool
        | I of Int.t
        | N of Nat.t
        | S of Str.t
        | By of Bytes.t
        | Ts of TStamp.t
        | Tz of Tez.t
        | KeyH of KeyH.t

        val cmp : t -> t -> int
        val fmt : formatter -> t -> unit
        val dtyp : t -> Dtyp.t

        val cast : Dtyp.t -> t -> t
    end

    module Int : sig
        include SigArith with type t = Cmp.Int.t
        val sub : t -> t -> t
        val to_nat : t -> Cmp.Nat.t option
        val of_nat : Cmp.Nat.t -> t
    end

    module Nat : sig
        include SigArith with type t = Cmp.Nat.t
        val sub : t -> t -> Int.t
        val of_int : Int.t -> t option
        val to_int : t -> Int.t
    end

    module Tez : SigTez with type t = Cmp.Tez.t and type nat = Nat.t

    module Str : sig
        include SigStr with type t = Cmp.Str.t
        val size : t -> Nat.t
        val slice : Nat.t -> Nat.t -> t -> t
        val compare : t -> t -> Int.t
    end

    module Bytes : sig
        include SigStr with type t = Cmp.Bytes.t
        val size : t -> Nat.t
        val slice : Nat.t -> Nat.t -> t -> t
        val compare : t -> t -> Int.t
    end

    module TStamp : sig
        include SigTStamp with type t = Cmp.TStamp.t
        val add : t -> Int.t -> t
        val sub_int : t -> Int.t -> t
        val sub : t -> t -> Int.t
    end

    module Key : sig
        include SigKey with type t = Cmp.Key.t
        val b58check : t -> Cmp.KeyH.t
        val blake2b : t -> Cmp.KeyH.t
        val sha256 : t -> Cmp.KeyH.t
        val sha512 : t -> Cmp.KeyH.t
    end
    module KeyH : SigKeyH with type t = Cmp.KeyH.t

    module Unwrap : sig
        val bool : Cmp.t -> bool
        val int : Cmp.t -> Cmp.Int.t
        val nat : Cmp.t -> Cmp.Nat.t
        val str : Cmp.t -> Cmp.Str.t
        val bytes : Cmp.t -> Cmp.Bytes.t
    end

    module Set : sig
        type t
        type elm = Cmp.t
        val empty : t
        val mem : elm -> t -> bool
        val update : elm -> bool -> t -> t
        val fold : ('acc -> elm -> 'acc) -> 'acc -> t -> 'acc
        val fold_as : (elm -> 'a) -> ('acc -> 'a -> 'acc) -> 'acc -> t -> 'acc
        val size : t -> Cmp.Nat.t
        val fmt : formatter -> t -> unit
    end

    module Map : sig
        type 'a t
        type key = Cmp.t
        val empty : 'a t
        val get : key -> 'a t -> 'a option
        val mem : key -> 'a t -> bool
        val update : key -> 'a option -> 'a t -> 'a t
        val map : (key -> 'a -> 'b) -> 'a t -> 'b t
        val map_as : (key -> 'k) -> ('a -> 'b) -> ('k -> 'b -> 'c) -> 'a t -> 'c t
        val fold : ('acc -> key -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
        val fold_as : (key -> 'k) -> ('a -> 'b) -> ('acc -> 'k -> 'b -> 'acc) -> 'acc -> 'a t -> 'acc
        val size : 'a t -> Cmp.Nat.t
        val fmt : (formatter -> 'a -> unit) -> formatter -> 'a t -> unit
    end

    module BigMap : sig
        type 'a t
        type key = Cmp.t
        val empty : 'a t
        val get : key -> 'a t -> 'a option
        val mem : key -> 'a t -> bool
        val update : key -> 'a option -> 'a t -> 'a t
        val map : (key -> 'a -> 'b) -> 'a t -> 'b t
        val map_as : (key -> 'k) -> ('a -> 'b) -> ('k -> 'b -> 'c) -> 'a t -> 'c t
        val fold : ('acc -> key -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
        val fold_as : (key -> 'k) -> ('a -> 'b) -> ('acc -> 'k -> 'b -> 'acc) -> 'acc -> 'a t -> 'acc
        val size : 'a t -> Cmp.Nat.t
        val fmt : (formatter -> 'a -> unit) -> formatter -> 'a t -> unit
    end

    module Either : sig
        type ('l, 'r) t =
        | Lft of 'l
        | Rgt of 'r

        val fmt : (formatter -> 'l -> unit) -> (formatter -> 'r -> unit) -> formatter -> ('l, 'r) t -> unit
    end

    module Option : sig
        type 'a t = 'a option

        val fmt : (formatter -> 'a -> unit) -> formatter -> 'a t -> unit
    end

    module Lst : sig
        type 'a t
        val nil : 'a t
        val cons : 'a -> 'a t -> 'a t
        val is_nil : 'a t -> bool
        val map : ('a -> 'b) -> 'a t -> 'b t
        val fold : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
        val size : 'a t -> Cmp.Nat.t
        val fmt : (formatter -> 'a -> unit) -> formatter -> 'a t -> unit

        val head : 'a t -> ('a * 'a t) option
    end

    type value = private
    | U
    | C of Cmp.t
    | Key of Key.t
    | Set of Set.t
    | Map of value Map.t
    | BigMap of value BigMap.t
    | Either of (value, value) Either.t
    | Option of value Option.t
    | Lst of value Lst.t
    | Pair of value * value
    | Contract of Mic.contract
    | Operation of operation
    | Address of Address.t

    and contract_params = private {
        address : Address.t ;
        manager : KeyH.t ;
        delegate : KeyH.t option ;
        spendable : bool ;
        delegatable : bool ;
        tez : Tez.t ;
        value : value ;
    }

    and operation =
    | Create of contract_params * Mic.contract
    | CreateNamed of contract_params * Contract.t
    | InitNamed of contract_params * value * string

    val mk_contract_params :
        spendable : bool ->
        delegatable : bool ->
        KeyH.t ->
        KeyH.t option ->
        Cmp.Tez.t ->
        Address.t ->
        value ->
        contract_params

    val fmt : formatter -> value -> unit
    val fmt_contract_params : formatter -> contract_params -> unit
    val fmt_operation : formatter -> operation -> unit
    val cast : Dtyp.t -> value -> value

    module Of : sig
        val int : Int.t -> value
        val nat : Nat.t -> value
        val str : Str.t -> value
        val bytes : Bytes.t -> value
        val timestamp : TStamp.t -> value
        val key : Key.t -> value
        val key_h : KeyH.t -> value

        val address : Address.t -> value

        val primitive_str : Dtyp.t -> string -> value

        val unit : value
        val const : Mic.const -> value
        val cmp : Cmp.t -> value
        val set : Set.t -> value
        val map : value Map.t -> value
        val big_map : value BigMap.t -> value
        val either : (value, value) Either.t -> value
        val list : value Lst.t -> value
        val option : value Option.t -> value
        val pair : value -> value -> value
        val contract : Mic.contract -> value

        module Operation : sig
            val create : contract_params -> Mic.contract -> value
            val create_named : contract_params -> Contract.t -> value
            val init_named : contract_params -> value -> string -> value
        end
    end

    module Inspect : sig
        val list : value -> value Lst.t
        val key : value -> Key.t
    end

    val cons : value -> value -> value
end
