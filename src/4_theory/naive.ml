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
    let to_str (t : t) : string =
        sprintf "%i" t

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
    let to_str (t : t) : string =
        sprintf "%i" t

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
    let to_str (t : t) : string = t

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
    let to_str (t : t) : string = t

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

    let to_str (t : t) : string =
        sprintf "%i" t
    let of_str (s : string) : t =
        (fun () -> int_of_string s)
        |> Exc.chain_err (
            fun () -> asprintf "illegal timestamp `%s`" s
        )

    let now () : t = 42. +. (100. *. Sys.time ()) |> Float.floor |> Float.to_int

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

    let add (t : t_stamp) (i : int) : t_stamp = t + (i * 100)
    let sub_int (t : t_stamp) (i : int) : t_stamp = t - (i * 100) |> Int.of_native
    let sub (t_1 : t_stamp) (t_2 : t_stamp) : int = t_1 - t_2 |> Int.of_native
end

module Key : Sigs.SigKey with type t = string = struct
    type t = string
    let fmt (fmt : formatter) (t : t) : unit = fprintf fmt "\"%s\"" t
    let of_str (s : string) : t = s
    let to_str (t : t) : string = t
end

module KeyH : Sigs.SigKeyH with type t = string = struct
    include Key
    let compare (t_1 : t) (t_2 : t) : int = compare t_1 t_2
end

module KeyHConv : Sigs.SigKeyHConv with type key = Key.t and type key_h = KeyH.t = struct
    type key = Key.t
    type key_h = KeyH.t

    let b58check (key : key) : key_h =
        "b58check:" ^ key
    let blake2b (key : key) : key_h =
        "blake2b:" ^ key
    let sha256 (key : key) : key_h =
        "sha256:" ^ key
    let sha512 (key : key) : key_h =
        "sha512:" ^ key
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

    module Key = Key
    module KeyH = KeyH
    module KeyHConv = KeyHConv
end



module Address : Sigs.SigAddress = struct
    type t = {
        uid : int ;
        tag : Annot.Var.t option ;
    }
    let cnt : int ref = ref 0

    let fresh (tag : Annot.Var.t option) : t =
        let uid = !cnt in
        cnt := !cnt + 1;
        { uid ; tag }
    let fmt (fmt : formatter) (self : t) : unit =
        fprintf fmt "address[%i]" self.uid;
        self.tag |> if_let_some (fprintf fmt "%a" Annot.Var.fmt)
    let equal (self : t) (other : t) : bool =
        self.uid = other.uid
end


module Theory : Sigs.SigTheory = Make.Colls (Cmp) (Address)