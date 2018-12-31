(** Input/output signatures for the functors that create evaluators. *)

open Base.Common

module type SigT = sig
    type t
end
module type SigTFmt = sig
    include SigT
    val fmt : formatter -> t -> unit
end

module type SigArith = sig
    include SigTFmt

    val of_str : string -> t
    val add : t -> t -> t
    val sub : t -> t -> t
    val mul : t -> t -> t
    val div : t -> t -> t
    val compare : t -> t -> int
end

module type SigStr = sig
    include SigTFmt

    val of_str : string -> t
    val concat : t -> t -> t
    val fmt : formatter -> t -> unit
end

module type SigCmp = sig
    module Int : sig
        include SigArith

        val of_int : int -> t
    end

    module Nat : sig
        include SigArith

        val to_int : t -> Int.t
        val of_int : Int.t -> t option
    end

    module Str : sig
        include SigStr
        val size : t -> Nat.t
        val slice : Nat.t -> Nat.t -> t -> t
        val compare : t -> t -> Int.t
    end

    module Bytes : sig
        include SigStr
        val size : t -> Nat.t
        val slice : Nat.t -> Nat.t -> t -> t
        val compare : t -> t -> Int.t
    end
end

module type SigEval = sig
    module Cmp : sig
        include SigCmp
        type t =
        | B of bool
        | I of Int.t
        | N of Nat.t
        | S of Str.t
        | By of Bytes.t

        val cmp : t -> t -> int
        val fmt : formatter -> t -> unit
    end

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
    end

    module Either : sig
        type ('l, 'r) t =
        | Lft of 'l
        | Rgt of 'r
    end

    module Lst : sig
        type 'a t
        val nil : 'a t
        val cons : 'a -> 'a t -> 'a t
        val is_nil : 'a t -> bool
        val map : ('a -> 'b) -> 'a t -> 'b t
        val fold : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
        val size : 'a t -> Cmp.Nat.t
    end

    type value = private
    | C of Cmp.t
    | Set of Set.t
    | Map of value Map.t
    | BigMap of value BigMap.t
    | Either of (value, value) Either.t
    | Lst of value Lst.t
    | Pair of value * value

    module Of : sig
        val cmp : Cmp.t -> value
        val set : Set.t -> value
        val map : value Map.t -> value
        val big_map : value BigMap.t -> value
        val either : (value, value) Either.t -> value
        val list : value Lst.t -> value
        val pair : value -> value -> value
    end
end