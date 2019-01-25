(** Theory modules for big integers. *)

module BInt : Sigs.Int

module BNat : Sigs.Nat

module BNatConv : Sigs.NatConv with
    type int = BInt.t and
    type nat = BNat.t

module NaiveStrConv : Sigs.StrConv with
    type str = Naive.Str.t and
    type nat = BNat.t and
    type int = BInt.t

module NaiveTStampConv : Sigs.TStampConv with
    type t_stamp = Naive.TStamp.t and
    type int = BInt.t

module BigNaivePrimitive : Sigs.Primitive

module Theory : Sigs.Theory