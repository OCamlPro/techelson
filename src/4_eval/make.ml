(** Functors combining evaluators. *)

open Base
open Base.Common

module Colls (C : Signatures.SigCmp) : Signatures.SigEval = struct

    module Cmp = struct
        include C
        type t =
        | B of bool
        | I of Int.t
        | N of Nat.t
        | S of Str.t
        | By of Bytes.t

        let fmt (fmt : formatter) (c : t) : unit =
            match c with
            | B b -> if b then fprintf fmt "True" else fprintf fmt "False"
            | I i -> Int.fmt fmt i
            | N n -> Nat.fmt fmt n
            | S s -> Str.fmt fmt s
            | By by -> Bytes.fmt fmt by

        let cmp (lft : t) (rgt : t) : int =
            match (lft, rgt) with
            | (B b_1), (B b_2) -> compare b_1 b_2
            | (B _), _ ->
                asprintf "cannot compare values %a and %a" fmt lft fmt rgt
                |> Exc.throw
            | _ -> failwith "aaa"
    end

    module Unwrap = struct
        let bool (c : Cmp.t) : bool =
            match c with
            | B b -> b
            | _ -> asprintf "unwrapping %a as bool" Cmp.fmt c |> Exc.throw
        let int (c : Cmp.t) : Cmp.Int.t =
            match c with
            | I i -> i
            | _ -> asprintf "unwrapping %a as int" Cmp.fmt c |> Exc.throw
        let nat (c : Cmp.t) : Cmp.Nat.t =
            match c with
            | N n -> n
            | _ -> asprintf "unwrapping %a as nat" Cmp.fmt c |> Exc.throw
        let str (c : Cmp.t) : Cmp.Str.t =
            match c with
            | S s -> s
            | _ -> asprintf "unwrapping %a as string" Cmp.fmt c |> Exc.throw
        let bytes (c : Cmp.t) : Cmp.Bytes.t =
            match c with
            | By by -> by
            | _ -> asprintf "unwrapping %a as bytes" Cmp.fmt c |> Exc.throw
    end

    module Set = struct
        module Inner = Set.Make(
            struct
                type t = Cmp.t
                let compare = Cmp.cmp
            end
        )
        type t = Inner.t
        type elm = Cmp.t
        let empty = Inner.empty
        let mem = Inner.mem
        let update (e : elm) (add : bool) = if add then Inner.add e else Inner.remove e
        let fold (f : 'acc -> elm -> 'acc) (acc : 'acc) (set : t) : 'acc =
            Inner.fold (fun a acc -> f acc a) set acc
        let fold_as (pre : elm -> 'a) (f : 'acc -> 'a -> 'acc) (acc : 'acc) (set : t) : 'acc =
            fold (fun acc a -> f acc (pre a)) acc set
        let size (t : t) : Cmp.Nat.t =
            Inner.cardinal t |> sprintf "%i" |> Cmp.Nat.of_str
    end

    module Map = struct
        module Inner = Map.Make(
            struct
                type t = Cmp.t
                let compare = Cmp.cmp
            end
        )
        type 'a t = 'a Inner.t
        type key = Cmp.t
        let empty = Inner.empty
        let mem = Inner.mem
        let get (k : key) (map : 'a t) : 'a option =
            try Some (Inner.find k map) with
            | Not_found -> None
        let update (e : key) (v : 'a option) (map : 'a t) : 'a t =
            Inner.update e (fun _ -> v) map
        let fold (f : 'acc -> key -> 'a -> 'acc) (acc : 'acc) (map : 'a t) : 'acc =
            Inner.fold (fun k a acc -> f acc k a) map acc
        let fold_as (f_k : key -> 'k) (f_v : 'a -> 'b) (f : 'acc -> 'k -> 'b -> 'acc) (acc : 'acc) (map : 'a t) : 'acc =
            fold (fun acc k v -> f acc (f_k k) (f_v v)) acc map
        let map (f : key -> 'a -> 'b) (t : 'a t) : 'b t = Inner.mapi f t
        let map_as (f_k : key -> 'k) (f_v : 'a -> 'b) (f : 'k -> 'b -> 'c) (t : 'a t) : 'c t =
            map (
                fun k v -> f (f_k k) (f_v v)
            ) t
        let size (t : 'a t) : Cmp.Nat.t =
            Inner.cardinal t |> sprintf "%i" |> Cmp.Nat.of_str
    end

    module BigMap = Map

    module Either = struct
        type ('l, 'r) t =
        | Lft of 'l
        | Rgt of 'r
    end

    module Lst = struct
        type 'a t = 'a list
        let size (l : 'a t) : Cmp.Nat.t =
            List.length l |> sprintf "%i" |> Cmp.Nat.of_str
        let fold = List.fold_left
        let map = List.map
        let is_nil lst = lst = []
        let cons hd tl = hd :: tl
        let nil = []
    end

    type value =
    | C of Cmp.t
    | Set of Set.t
    | Map of value Map.t
    | BigMap of value BigMap.t
    | Either of (value, value) Either.t
    | Lst of value Lst.t
    | Pair of value * value

    module Of = struct
        let cmp (cmp : Cmp.t) : value = C cmp
        let set (set : Set.t) : value = Set set
        let map (map : value Map.t) : value = Map map
        let big_map (map : value BigMap.t) : value = BigMap map
        let either (e : (value, value) Either.t) : value = Either e
        let list (l : value Lst.t) : value = Lst l
        let pair (lft : value) (rgt : value) : value = Pair (lft, rgt)
    end
end