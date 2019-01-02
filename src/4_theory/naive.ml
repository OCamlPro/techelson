open Base
open Base.Common

module Int : sig
    include Sigs.SigArith
    val of_int : int -> t
    val to_int : t -> int
    val sub : t -> t -> t
end = struct
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
    
    let of_int (n : int) : t = n
    let to_int (t : t) : int = t
end

module Nat : sig
    include Sigs.SigArith
    val of_int : Int.t -> t option
    val to_int : t -> Int.t
    val sub : t -> t -> Int.t
end = struct
    type t = int

    let to_int (t : t) : Int.t = Int.of_int t
    let of_int (i : Int.t) : t option =
        let i = Int.to_int i in
        if i < 0 then None else Some i

    let of_str s =
        let bail_msg () = sprintf "failed to convert string `%s` to nat" s in
        let i =
            Exc.erase_err bail_msg (fun () -> Int.of_str s)
        in
        match of_int i with
        | Some i -> i
        | None ->  bail_msg () |> Exc.throw
    let compare (t_1 : t) (t_2 : t) : int = compare t_1 t_2
    let zero : t = 0

    let add (t_1 : t) (t_2 : t) : t = t_1 + t_2
    let sub (t_1 : t) (t_2 : t) : Int.t = t_1 - t_2 |> to_int
    let mul (t_1 : t) (t_2 : t) : t = t_1 * t_2
    let div (t_1 : t) (t_2 : t) : t = t_1 / t_2

    let fmt (fmt : formatter) (t : t) : unit =
        fprintf fmt "%ip" t
end

module Str : sig
    include Sigs.SigStr
    val size : t -> Nat.t
    val slice : Nat.t -> Nat.t -> t -> t
    val compare : t -> t -> Int.t
end = struct
    type t = string
    let of_str (s : string) : t = s

    let size (t : t) : Nat.t = String.length t |> sprintf "%i" |> Nat.of_str
    let slice (start : Nat.t) (len : Nat.t) (t : t) : t =
        String.sub t (Nat.to_int start |> Int.to_int) (Nat.to_int len |> Int.to_int)
    let compare (t_1 : t) (t_2 : t) : Int.t = compare t_1 t_2 |> Int.of_int

    let concat (t_1 : t) (t_2 : t) : t = t_1 ^ t_2

    let fmt (fmt : formatter) (t : t) : unit =
        fprintf fmt "\"%s\"" t
end

module Bytes : sig
    include Sigs.SigStr
    val size : t -> Nat.t
    val slice : Nat.t -> Nat.t -> t -> t
    val compare : t -> t -> Int.t
end = struct
    type t = string
    let of_str (s : string) : t = s

    let size (t : t) : Nat.t = String.length t |> sprintf "%i" |> Nat.of_str
    let slice (start : Nat.t) (len : Nat.t) (t : t) : t =
        String.sub t (Nat.to_int start |> Int.to_int) (Nat.to_int len |> Int.to_int)
    let compare (t_1 : t) (t_2 : t) : Int.t = compare t_1 t_2 |> Int.of_int

    let concat (t_1 : t) (t_2 : t) : t = t_1 ^ t_2

    let fmt (fmt : formatter) (t : t) : unit =
        fprintf fmt "\"%s\"" t
end

module Cmp : Sigs.SigCmp = struct
    module Int = Int
    module Nat = Nat
    module Str = Str
    module Bytes = Bytes
end


module Theory : Sigs.SigTheory = Make.Colls(Cmp)