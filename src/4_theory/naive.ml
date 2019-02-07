open Base
open Base.Common

module Int : Sigs.Int with type t = int = struct
    type t = int

    let of_string (s : string) : t =
        try int_of_string s with
        | e -> [
            sprintf "failed to convert string `%s` to int" s ;
            sprintf "%s" (Printexc.to_string e)
        ] |> Exc.throws
    let to_string (t : t) : string =
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

module Nat : Sigs.Nat with type t = int = struct
    type t = int

    let of_native (n : int) : t = n
    let to_native (t : t) : int = t

    let of_string s =
        let bail_msg () = sprintf "failed to convert string `%s` to nat" s in
        let i =
            try int_of_string s with
            | e -> [
                bail_msg () ;
                sprintf "%s" (Printexc.to_string e)
            ] |> Exc.throws
        in
        of_native i
    let to_string (t : t) : string =
        sprintf "%i" t

    let compare (t_1 : t) (t_2 : t) : int = compare t_1 t_2
    let zero : t = 0

    let add (t_1 : t) (t_2 : t) : t = t_1 + t_2
    let mul (t_1 : t) (t_2 : t) : t = t_1 * t_2
    let div (t_1 : t) (t_2 : t) : t = t_1 / t_2
    
    let lshift_lft (n : t) (m : t) : t = n lsl m
    let lshift_rgt (n : t) (m : t) : t = n lsr m

    let xor (n : t) (m : t) : t = n lxor m

    let fmt (fmt : formatter) (t : t) : unit =
        fprintf fmt "%ip" t
end

module Str : sig
    include Sigs.Str with type t = string
end = struct
    type t = string

    let of_native (s : string) : t = s
    let to_string (t : t) : string = t

    let concat (t_1 : t) (t_2 : t) : t = t_1 ^ t_2

    let fmt (fmt : formatter) (t : t) : unit =
        fprintf fmt "\"%s\"" t
end

module Bytes : Sigs.Str with type t = string = struct
    include Str
end

module TStamp : Sigs.TStamp with type t = int = struct
    type t = int

    let now () : t = 42. +. (100. *. Sys.time ()) |> Float.floor |> Float.to_int

    let to_string (t : t) : string =
        sprintf "%i" t
    let of_native (s : string) : t =
        (fun () -> int_of_string s)
        |> Exc.chain_err (
            fun () -> asprintf "illegal timestamp `%s`" s
        )

    let compare (t_1 : t) (t_2 : t) : Int.t = compare t_1 t_2 |> Int.of_native

    let fmt (fmt : formatter) (t : t) : unit =
        fprintf fmt "\"%i\"" t
end

module NatConv : Sigs.NatConv with type int = Int.t and type nat = Nat.t = struct
    type int = Int.t
    type nat = Nat.t
    let nat_to_int (t : Nat.t) : Int.t = t
    let int_to_nat (i : Int.t) : Nat.t option = if i >= 0 then Some i else None
    let nat_sub (t_1 : Nat.t) (t_2 : Nat.t) : Int.t = t_1 - t_2
    let int_abs (i : Int.t) : Nat.t =
        match int_to_nat i with
        | None -> (
            match int_to_nat (Int.sub Int.zero i) with
            | None -> asprintf "could not retrieve absolute value of %a" Int.fmt i |> Exc.throw
            | Some res -> res
        )
        | Some res -> res
    let ediv (i_1 : Int.t) (i_2 : Int.t) : (Int.t * Nat.t) option =
        try (
            let q = i_1 / i_2 in
            let r = i_1 mod i_2 in
            let q, r =
                if r < 0 && q >= 0 then
                    q + 1, r - i_2
                else if r < 0 && q < 0 then
                    q - 1, r + i_2
                else q - 1, i_2 + r
            in
            Some (Int.of_native q, Nat.of_native r)
        ) with
        | Division_by_zero -> None
end

module StrConv
    : Sigs.StrConv with type str = Str.t and type int = Int.t and type nat = Nat.t
= struct
    type str = Str.t
    type int = Int.t
    type nat = Nat.t
    let size (t : Str.t) : Nat.t = String.length t |> sprintf "%i" |> Nat.of_string
    let slice (start : Nat.t) (len : Nat.t) (t : Str.t) : Str.t =
        String.sub t (Nat.to_native start) (Nat.to_native len)
    let compare (t_1 : Str.t) (t_2 : Str.t) : Int.t = compare t_1 t_2 |> Int.of_native
end

module TStampConv
    : Sigs.TStampConv with type t_stamp = TStamp.t and type int = Int.t
= struct
    type t_stamp = TStamp.t
    type int = Int.t

    let int_to_tstamp (i : int) : t_stamp = i * 100
    let add (t : t_stamp) (i : int) : t_stamp = t + (i * 100)
    let sub_int (t : t_stamp) (i : int) : t_stamp = t - (i * 100) |> Int.of_native
    let sub (t_1 : t_stamp) (t_2 : t_stamp) : int = t_1 - t_2 |> Int.of_native
end

module Key : Sigs.Key with type t = string = struct
    type t = string

    let fmt (fmt : formatter) (t : t) : unit = fprintf fmt "\"%s\"" t
    let of_native (s : string) : t = s
    let to_string (t : t) : string = t
end

module KeyH : Sigs.KeyH with type t = string = struct
    include Key
    let compare (t_1 : t) (t_2 : t) : int = compare t_1 t_2
end

module KeyHConv : Sigs.KeyHConv with type key = Key.t and type key_h = KeyH.t = struct
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

module Address : Sigs.Address = struct
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

    let uid (self : t) : int = self.uid
end

module Prims : Sigs.Primitive = struct
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
    module Address = Address
end


module Theory : Sigs.Theory = Make.Theory (Prims)
