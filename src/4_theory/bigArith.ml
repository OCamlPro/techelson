open Base
open Common

(* Factors Int/Nat stuff. *)
module BigInt = struct
    type t = Z.t

    let fmt = Z.pp_print
    let to_string = Z.to_string

    let of_string = Z.of_string
    let of_native = Z.of_int
    let to_native = Z.to_int

    let add = Z.add
    let mul = Z.mul
    let div = Z.div

    let compare = Z.compare
    let zero = Z.zero
end

module BInt = struct
    include BigInt
    let sub = Z.sub
end

module BNat = struct
    include BigInt

    let of_string (s : string) : t =
        let nat = Z.of_string s in
        if Z.geq nat Z.zero then nat
        else sprintf "illegal string for `Nat.of_string \"%s\"`" s |> Exc.throw
    let of_native (n : int) : t =
        let nat = Z.of_int n in
        if Z.geq nat Z.zero then nat
        else sprintf "illegal int for `Nat.of_native \"%i\"`" n |> Exc.throw

    let lshift_lft (n : t) (shift : t) : t =
        to_native shift |> Z.shift_left n
    let lshift_rgt (n : t) (shift : t) : t =
        to_native shift |> Z.shift_right n
    let xor = Z.logxor
end

module BNatConv = struct
    type int = BInt.t
    type nat = BNat.t

    let int_to_nat (n : int) : nat option =
        if Z.geq n Z.zero then Some n else None
    let nat_to_int (n : nat) : int = n
    let nat_sub (n_1 : nat) (n_2 : nat) : int = Z.sub n_1 n_2
    let int_abs (i_1 : int) : nat = Z.abs i_1
    let ediv (i_1 : int) (i_2 : int) : (int * nat) option =
        if Z.equal i_2 Z.zero then None else (
            try (
                let q, r = Z.div_rem i_1 i_2 in
                let q, r =
                    if Z.lt r Z.zero && Z.geq q Z.zero then
                        Z.add q Z.one, Z.sub r i_2
                    else if Z.lt r Z.zero && Z.lt q Z.zero then
                        Z.sub q Z.one, Z.add r i_2
                    else Z.sub q Z.one, Z.add i_2 r
                in
                Some (q, r)
            ) with
            | Division_by_zero -> None
        )
end

module NaiveStrConv = struct
    type str = Naive.Str.t
    type nat = BNat.t
    type int = BInt.t

    let size (str : str) : nat =
        String.length str |> BNat.of_native
    let slice (start : nat) (len : nat) (str : str) : str =
        String.sub str (BNat.to_native start) (BNat.to_native len)
    let compare (s_1 : str) (s_2 : str) : int =
        String.compare s_1 s_2 |> BNat.of_native
end


module NaiveTStampConv = struct
    type t_stamp = Naive.TStamp.t
    type int = BInt.t

    let add (t : t_stamp) (i : int) : t_stamp =
        let i = BInt.to_native i in
        t + (i * 100)
    let sub_int (t : t_stamp) (i : int) : t_stamp =
        let i = BInt.to_native i in
        t - (i * 100)
    let sub (t_1 : t_stamp) (t_2 : t_stamp) : int =
        t_1 - t_2 |> BInt.of_native
end

module BigNaivePrimitive = struct
    module Int = BInt
    module Nat = BNat
    module NatConv = BNatConv
    module Str = Naive.Str
    module StrConv = NaiveStrConv
    module Bytes = Naive.Bytes
    module BytesConv = NaiveStrConv
    module TStamp = Naive.TStamp
    module TStampConv = NaiveTStampConv
    module Key = Naive.Key
    module KeyH = Naive.KeyH
    module KeyHConv = Naive.KeyHConv
    module Address = Naive.Address
end

module Theory = Make.Theory (BigNaivePrimitive)