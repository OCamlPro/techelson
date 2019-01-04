open Base
open Base.Common

module Int : Sigs.SigInt with type t = int = struct
    type t = int

    let of_str (s : string) : t =
        try int_of_string s with
        | e -> [
            sprintf "failed to convert string `%s` to int" s ;
            sprintf "%s" (Printexc.to_string e)
        ] |> Exc.throws

    let compare (t_1 : t) (t_2 : t) : int = compare t_1 t_2
    let zero : t = 0

    let add (t_1 : t) (t_2 : t) : t = t_1 + t_2
    let sub (t_1 : t) (t_2 : t) : t = t_1 - t_2
    let mul (t_1 : t) (t_2 : t) : t = t_1 * t_2
    let div (t_1 : t) (t_2 : t) : t = t_1 / t_2

    let fmt (fmt : formatter) (t : t) : unit =
        fprintf fmt "%i" t

    let of_native (n : int) : t = n
    let to_native (t : t) : int = t
end

module Nat : Sigs.SigArith with type t = int = struct
    type t = int

    let of_native (n : int) : t = n
    let to_native (t : t) : int = t

    let of_str s =
        let bail_msg () = sprintf "failed to convert string `%s` to nat" s in
        let i =
            try int_of_string s with
            | e -> [
                bail_msg () ;
                sprintf "%s" (Printexc.to_string e)
            ] |> Exc.throws
        in
        of_native i
    let compare (t_1 : t) (t_2 : t) : int = compare t_1 t_2
    let zero : t = 0

    let add (t_1 : t) (t_2 : t) : t = t_1 + t_2
    let mul (t_1 : t) (t_2 : t) : t = t_1 * t_2
    let div (t_1 : t) (t_2 : t) : t = t_1 / t_2

    let fmt (fmt : formatter) (t : t) : unit =
        fprintf fmt "%ip" t
end

module Str : sig
    include Sigs.SigStr with type t = string
end = struct
    type t = string
    let of_str (s : string) : t = s

    let concat (t_1 : t) (t_2 : t) : t = t_1 ^ t_2

    let fmt (fmt : formatter) (t : t) : unit =
        fprintf fmt "\"%s\"" t
end

module Bytes : sig
    include Sigs.SigStr with type t = string
    val size : t -> Nat.t
    val slice : Nat.t -> Nat.t -> t -> t
    val compare : t -> t -> Int.t
end = struct
    type t = string
    let of_str (s : string) : t = s

    let size (t : t) : Nat.t = String.length t |> sprintf "%i" |> Nat.of_str
    let slice (start : Nat.t) (len : Nat.t) (t : t) : t =
        String.sub t (Nat.to_native start) (Nat.to_native len)
    let compare (t_1 : t) (t_2 : t) : Int.t = compare t_1 t_2 |> Int.of_native

    let concat (t_1 : t) (t_2 : t) : t = t_1 ^ t_2

    let fmt (fmt : formatter) (t : t) : unit =
        fprintf fmt "\"%s\"" t
end

module TStamp : Sigs.SigTStamp with type t = int = struct
    type t = int

    let now () : t = 42

    let compare (t_1 : t) (t_2 : t) : Int.t = compare t_1 t_2 |> Int.of_native

    let fmt (fmt : formatter) (t : t) : unit =
        fprintf fmt "\"%i\"" t
end

module NatConv : Sigs.SigNatConv with type int = Int.t and type nat = Nat.t = struct
    type int = Int.t
    type nat = Nat.t
    let nat_to_int (t : Nat.t) : Int.t = t
    let int_to_nat (i : Int.t) : Nat.t option = if i >= 0 then Some i else None
    let nat_sub (t_1 : Nat.t) (t_2 : Nat.t) : Int.t = t_1 - t_2
end

module StrConv
    : Sigs.SigStrConv with type str = Str.t and type int = Int.t and type nat = Nat.t
= struct
    type str = Str.t
    type int = Int.t
    type nat = Nat.t
    let size (t : Str.t) : Nat.t = String.length t |> sprintf "%i" |> Nat.of_str
    let slice (start : Nat.t) (len : Nat.t) (t : Str.t) : Str.t =
        String.sub t (Nat.to_native start) (Nat.to_native len)
    let compare (t_1 : Str.t) (t_2 : Str.t) : Int.t = compare t_1 t_2 |> Int.of_native
end

module TStampConv
    : Sigs.SigTStampConv with type t_stamp = TStamp.t and type int = Int.t
= struct
    type t_stamp = TStamp.t
    type int = Int.t

    let add (t : t_stamp) (i : int) : t_stamp = t + i
    let sub_int (t : t_stamp) (i : int) : t_stamp = t - i |> Int.of_native
    let sub (t_1 : t_stamp) (t_2 : t_stamp) : int = t_1 - t_2 |> Int.of_native
end

module Cmp : Sigs.SigCmp = struct
    module Int = Int

    module Nat = Nat
    module NatConv = NatConv

    module Str = Str
    module StrConv = StrConv

    module Bytes = Bytes
    module BytesConv = StrConv

    module TStamp = TStamp
    module TStampConv = TStampConv
end


module Theory : Sigs.SigTheory = Make.Colls(Cmp)