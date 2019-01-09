(** Functors combining evaluators. *)

open Base
open Base.Common

module Colls (
    C : Sigs.SigCmp
) (
    A : Sigs.SigAddress
) : Sigs.SigTheory = struct
    module Address = A

    module Cmp = struct
        include C

        module Tez = struct
            type t = Int64.t
            type nat = C.Nat.t

            let to_str (t : t) : string =
                Int64.to_string t

            let fmt (fmt : formatter) (t : t) : unit =
                Int64.to_string t |> fprintf fmt "%stz"

            let to_nat (t : t) : nat =
                (fun () -> Int64.to_string t |> C.Nat.of_str)
                |> Exc.erase_err (
                    fun () -> asprintf "cannot convert %a to nat" fmt t
                )
            let of_nat (n : nat) : t =
                (fun () -> C.Nat.to_str n |> Int64.of_string)
                |> Exc.chain_err (
                    fun () -> asprintf "cannot convert %a to mutez" C.Nat.fmt n
                )

            let of_str (s : string) : t =
                (fun () -> Int64.of_string s)
                |> Exc.erase_err (
                    fun () -> sprintf "cannot convert string `%s` to tezos" s
                )
            let of_native (n : int) : t = Int64.of_int n
            let to_native (t : t) : int = Int64.to_int t

            let add (t_1 : t) (t_2 : t) : t =
                Int64.add t_1 t_2
            let sub (t_1 : t) (t_2 : t) : t =
                if t_1 < t_2 then (
                    sprintf
                        "underflow on thezos subtraction `%s - %s`"
                        (Int64.to_string t_1) (Int64.to_string t_2)
                    |> Exc.throw
                ) else Int64.sub t_1 t_2
            let mul_nat (t : t) (n : nat) : t =
                (fun () -> of_nat n |> Int64.mul t)
                |> Exc.erase_err (
                    fun () -> asprintf "while evaluating `%a * %a`" fmt t C.Nat.fmt n
                )
            let div_nat (t : t) (n : nat) : t =
                (fun () -> of_nat n |> Int64.mul t)
                |> Exc.erase_err (
                    fun () -> asprintf "while evaluating `%a / %a`" fmt t C.Nat.fmt n
                )
            let div (t_1 : t) (t_2 : t) : t =
                if t_2 = Int64.zero then (
                    asprintf "cannot divide by zero in `%a / %a`" fmt t_1 fmt t_2
                    |> Exc.throw
                );
                Int64.div t_1 t_2
            let compare (t_1 : t) (t_2 : t) : int =
                compare t_1 t_2
            let zero : t = Int64.zero
        end

        type t =
        | B of bool
        | I of Int.t
        | N of Nat.t
        | S of Str.t
        | By of Bytes.t
        | Ts of TStamp.t
        | Tz of Tez.t
        | KeyH of KeyH.t

        let fmt (fmt : formatter) (c : t) : unit =
            match c with
            | B b -> if b then fprintf fmt "True" else fprintf fmt "False"
            | I i -> Int.fmt fmt i
            | N n -> Nat.fmt fmt n
            | S s -> Str.fmt fmt s
            | By by -> Bytes.fmt fmt by
            | Ts ts -> TStamp.fmt fmt ts
            | Tz tz -> Tez.fmt fmt tz
            | KeyH kh -> KeyH.fmt fmt kh

        let cmp (lft : t) (rgt : t) : int =
            match (lft, rgt) with
            | (B b_1), (B b_2) -> compare b_1 b_2
            | (I i_1), (I i_2) -> compare i_1 i_2
            | (N n_1), (N n_2) -> compare n_1 n_2
            | (S s_1), (S s_2) -> compare s_1 s_2
            | (By by_1), (By by_2) -> compare by_1 by_2
            | (Ts ts_1), (Ts ts_2) -> compare ts_1 ts_2
            | (Tz tz_1), (Tz tz_2) -> compare tz_1 tz_2
            | (KeyH kh_1), (KeyH kh_2) -> compare kh_1 kh_2
            | (B _), _
            | (I _), _
            | (N _), _
            | (S _), _
            | (By _), _
            | (Ts _), _
            | (Tz _), _
            | (KeyH _), _ ->
                asprintf "cannot compare values %a and %a" fmt lft fmt rgt
                |> Exc.throw

        let dtyp (t : t) : Dtyp.t =
            match t with
            | B _ -> Dtyp.mk_leaf Dtyp.Bool
            | I _ -> Dtyp.mk_leaf Dtyp.Int
            | N _ -> Dtyp.mk_leaf Dtyp.Nat
            | S _ -> Dtyp.mk_leaf Dtyp.Str
            | By _ -> Dtyp.mk_leaf Dtyp.Bytes
            | Ts _ -> Dtyp.mk_leaf Dtyp.Timestamp
            | Tz _ -> Dtyp.mk_leaf Dtyp.Mutez
            | KeyH _ -> Dtyp.mk_leaf Dtyp.KeyH

        let cast (dtyp : Dtyp.t) (t : t) : t =
            let bail () =
                asprintf "cannot cast value `%a` to type `%a`" fmt t Dtyp.fmt dtyp |> Exc.throw
            in

            match t, dtyp.typ with

            | I _, Leaf Int -> t
            | I i, Leaf Nat -> (
                match NatConv.int_to_nat i with
                | Some n -> N n
                | None -> bail ()
            )
            | I i, Leaf Mutez -> (
                match NatConv.int_to_nat i with
                | Some n -> Tz (Tez.of_nat n)
                | None -> bail ()
            )

            | N _, Leaf Nat -> t
            | N n, Leaf Int -> I (NatConv.nat_to_int n)
            | N n, Leaf Mutez -> Tz (Tez.of_nat n)

            | B _, Leaf Bool -> t

            | S _, Leaf Str -> t

            | By _, Leaf Bytes -> t

            | Ts _, Leaf Timestamp -> t

            | KeyH _, Leaf KeyH -> t

            | _ -> bail ()

    end

    module Tez = Cmp.Tez

    module Int = struct
        include Cmp.Int
        let to_nat : t -> Cmp.Nat.t option = Cmp.NatConv.int_to_nat
        let of_nat : Cmp.Nat.t -> t = Cmp.NatConv.nat_to_int
    end

    module Nat = struct
        include Cmp.Nat
        let sub : t -> t -> Int.t = Cmp.NatConv.nat_sub
        let of_int : Int.t -> t option = Cmp.NatConv.int_to_nat
        let to_int : t -> Int.t = Cmp.NatConv.nat_to_int
    end

    module Str = struct
        include Cmp.Str
        let size : t -> Nat.t = Cmp.StrConv.size
        let slice : Nat.t -> Nat.t -> t -> t = Cmp.StrConv.slice
        let compare : t -> t -> Int.t = Cmp.StrConv.compare
    end

    module Bytes = struct
        include Cmp.Bytes
        let size : t -> Nat.t = Cmp.BytesConv.size
        let slice : Nat.t -> Nat.t -> t -> t = Cmp.BytesConv.slice
        let compare : t -> t -> Int.t = Cmp.BytesConv.compare
    end

    module TStamp = struct
        include Cmp.TStamp
        let add : t -> Int.t -> t = Cmp.TStampConv.add
        let sub_int : t -> Int.t -> t = Cmp.TStampConv.sub_int
        let sub : t -> t -> Int.t = Cmp.TStampConv.sub
    end

    module Key = struct
        include Cmp.Key
        let b58check : t -> Cmp.KeyH.t = Cmp.KeyHConv.b58check
        let blake2b : t -> Cmp.KeyH.t = Cmp.KeyHConv.blake2b
        let sha256 : t -> Cmp.KeyH.t = Cmp.KeyHConv.sha256
        let sha512 : t -> Cmp.KeyH.t = Cmp.KeyHConv.sha512
    end
    module KeyH = Cmp.KeyH

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
        let fmt (fmt : formatter) (set : t) : unit =
            fprintf fmt "@[@[<hv 2>{";
            fold (
                fun () c -> fprintf fmt "@ %a," Cmp.fmt c
            ) () set |> ignore;
            fprintf fmt "}@]@]"
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
        let fmt (fmt_a : formatter -> 'a -> unit) (fmt : formatter) (map : 'a t) : unit =
            fprintf fmt "@[@[<hv 2>{";
            fold (
                fun () k v -> fprintf fmt "@ %a -> %a," Cmp.fmt k fmt_a v
            ) () map |> ignore;
            fprintf fmt "@]";
            if Cmp.Nat.compare Cmp.Nat.zero (size map) > 0 then ( 
                fprintf fmt "@ "
            );
            fprintf fmt "}@]"
    end

    module BigMap = Map

    module Either = struct
        type ('l, 'r) t =
        | Lft of 'l
        | Rgt of 'r

        let fmt
            (fmt_l : formatter -> 'l -> unit)
            (fmt_r : formatter -> 'r -> unit)
            (fmt : formatter)
            (either : ('l, 'r) t)
            : unit
        =
            match either with
            | Lft l -> fprintf fmt "(Left %a)" fmt_l l
            | Rgt r  -> fprintf fmt "(Right %a)" fmt_r r
    end

    module Option = struct
        type 'a t = 'a option

        let fmt
            (fmt_a : formatter -> 'a -> unit)
            (fmt : formatter)
            (opt : 'a t)
            : unit
        =
            match opt with
            | Some a -> fprintf fmt "(Some %a)" fmt_a a
            | None  -> fprintf fmt "None"
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

        let fmt (fmt_a : formatter -> 'a -> unit) (fmt : formatter) (lst : 'a t) : unit =
            fprintf fmt "@[@[<hv 2>[";
            fold (
                fun () a -> fprintf fmt "@ `%a`," fmt_a a
            ) () lst |> ignore;
            fprintf fmt "@]";
            if lst <> [] then (
                fprintf fmt "@ "
            );
            fprintf fmt "]@]"
        
        let head (lst : 'a t) : ('a * 'a t) option =
            match lst with
            | [] -> None
            | hd :: tl -> Some (hd, tl)
    end

    type value =
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

    and contract_params = {
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

    let mk_contract_params
        ~(spendable : bool)
        ~(delegatable : bool)
        (manager : KeyH.t)
        (delegate : KeyH.t option)
        (tez : Cmp.Tez.t)
        (address : Address.t)
        (value : value)
        : contract_params
    =
        { address ; manager ; delegate ; spendable ; delegatable ; tez ; value }

    let rec fmt_contract_params (fmtt : formatter) (params : contract_params) : unit =
        fprintf fmtt "(@%a, %a, %a, %b, %b, %a)"
            Address.fmt params.address
            KeyH.fmt params.manager
            (Option.fmt KeyH.fmt) params.delegate
            params.spendable params.delegatable
            Tez.fmt params.tez

    and fmt_operation (fmtt : formatter) (op : operation) : unit =
        match op with
        | Create (params, contract) ->
            fprintf fmtt "@[<hv 4>CREATE %a %a@]" fmt_contract_params params Mic.fmt_contract contract
        | CreateNamed (params, contract) ->
            fprintf fmtt "@[<hv 4>CREATE %a \"%s\"@]" fmt_contract_params params contract.name
        | InitNamed (params, value, name) ->
            fprintf fmtt "@[<hv 4>CREATE %a %a %s@]" fmt_contract_params params fmt value name
        

    and fmt (fmt : formatter) (v : value) : unit =

        let rec go_down (stack : ((string * value) list * string) list) (v : value) : unit =
            match v with
            | U ->
                fprintf fmt "Unit";
                go_up stack
            | C c ->
                Cmp.fmt fmt c;
                go_up stack
            | Key k ->
                Cmp.Key.fmt fmt k;
                go_up stack
            | Address a ->
                Address.fmt fmt a;
                go_up stack

            | Set set ->
                fprintf fmt "Set {";
                let _, elms =
                    Set.fold (
                        fun (is_first, acc) value ->
                            let pref = if is_first then " " else ", " in
                            false, (pref, C value) :: acc
                    ) (true, []) set
                in
                go_up ((elms, " }") :: stack)
            | Map map ->
                fprintf fmt "Map {";
                let _, elms =
                    Map.fold (
                        fun (is_first, acc) key value ->
                            let pref = if is_first then " " else ", " in
                            false, (pref, C key) :: (" -> ", value) :: acc
                    ) (true, []) map
                in
                go_up ((elms, " }") :: stack)
            | BigMap map ->
                fprintf fmt "BigMap {";
                let _, elms =
                    Map.fold (
                        fun (is_first, acc) key value ->
                            let pref = if is_first then " " else ", " in
                            false, (pref, C key) :: (" -> ", value) :: acc
                    ) (true, []) map
                in
                go_up ((elms, " }") :: stack)

            | Either disj -> (
                match disj with
                | Lft l ->
                    fprintf fmt "(Left ";
                    go_down (([], ")") :: stack) l
                |Rgt r ->
                    fprintf fmt "(Right ";
                    go_down (([], ")") :: stack) r
            )

            | Option opt -> (
                match opt with
                | Some sub ->
                    fprintf fmt "(Some ";
                    go_down (([], ")") :: stack) sub
                | None ->
                    fprintf fmt "None";
                    go_up stack
            )

            | Lst lst ->
                fprintf fmt "[";
                let _, elms =
                    lst |> List.fold_left (
                        fun (is_first, acc) value ->
                            let pref = if is_first then " " else ", " in
                            false, (pref, value) :: acc
                    ) (true, [])
                in
                go_up ((elms, " ]") :: stack)

            | Pair (lft ,rgt) ->
                fprintf fmt "(";
                go_down ( ([", ", rgt], ")") :: stack ) lft

            | Contract contract ->
                fprintf fmt "{ storage : @[%a@] ; param : @[%a@] ; code : @[%a@] ; }"
                    Dtyp.fmt contract.Mic.storage
                    Dtyp.fmt contract.Mic.param
                    Mic.fmt contract.Mic.entry;
                go_up stack
            
            | Operation op ->
                fprintf fmt "%a" fmt_operation op;
                go_up stack

        and go_up (stack : ((string * value) list * string) list) : unit =
            match stack with
            | [] -> ()
            | ([], close) :: stack ->
                fprintf fmt "%s" close;
                go_up stack
            | ( (sep, to_do) :: to_do_tail, close ) :: stack ->
                fprintf fmt "%s" sep;
                let stack = (to_do_tail, close) :: stack in
                go_down stack to_do
                
        in

        go_down [] v

    let cast (dtyp : Dtyp.t) (v : value) : value =
        let bail_msg () =
            asprintf "cannot cast value `%a` to type `%a`" fmt v Dtyp.fmt dtyp
        in
        match dtyp.typ, v with
        | Dtyp.Leaf Dtyp.Key, C (Cmp.S s) ->
            Key (Str.to_str s |> Key.of_str)
        | Dtyp.Contract param, Contract c ->
            if c.Mic.param = param then v else bail_msg () |> Exc.throw
        | _ -> (
            match v with
            | C cmp -> C (Cmp.cast dtyp cmp)
            | _ -> bail_msg () |> Exc.throw
        )

    module Of = struct
        let int (i : Int.t) : value = C (Cmp.I i)
        let nat (i : Nat.t) : value = C (Cmp.N i)
        let str (i : Str.t) : value = C (Cmp.S i)
        let bytes (by : Bytes.t) : value = C (Cmp.By by)
        let timestamp (ts : TStamp.t) : value = C (Cmp.Ts ts)
        let key (k : Key.t) : value = Key k
        let key_h (kh : KeyH.t) : value = C (Cmp.KeyH kh)

        let address (a : Address.t) : value = Address a

        let primitive_str (dtyp : Dtyp.t) (s : string) : value =
            match dtyp.typ with
            | Dtyp.Leaf Dtyp.Key -> Key (Key.of_str s)
            | Dtyp.Leaf Dtyp.Bytes -> C (Cmp.By (Bytes.of_str s))
            | Dtyp.Leaf Dtyp.Timestamp -> C (Cmp.Ts (TStamp.of_str s))
            | Dtyp.Leaf Dtyp.Str -> C (Cmp.S (Str.of_str s))
            | _ -> asprintf "cannot cast a primitive string to `%a`" Dtyp.fmt dtyp |> Exc.throw

        let unit : value = U
        let cmp (cmp : Cmp.t) : value = C cmp
        let set (set : Set.t) : value = Set set
        let map (map : value Map.t) : value = Map map
        let big_map (map : value BigMap.t) : value = BigMap map
        let either (e : (value, value) Either.t) : value = Either e
        let option (o : value Option.t) : value = Option o
        let list (l : value Lst.t) : value = Lst l
        let pair (lft : value) (rgt : value) : value = Pair (lft, rgt)

        let contract (c : Mic.contract) : value = Contract c

        let const (c : Mic.const) : value =
            let rec go_down (stack : (value -> value) list) (c : Mic.const) : value =
                match c with
                | Unit -> U

                | Bool b -> C (Cmp.B b) |> go_up stack
                | Int i -> C (Cmp.I (Cmp.Int.of_str i)) |> go_up stack
                | Str s -> C (Cmp.S (Cmp.Str.of_str s)) |> go_up stack
                | Bytes by -> C (Cmp.By (Cmp.Bytes.of_str by)) |> go_up stack

                | Contract c -> Contract c

                | No -> Option None |> go_up stack

                | So c -> go_down ((fun v -> Option (Some v)) :: stack) c
                | Lft c -> go_down ((fun v -> Either (Either.Lft v)) :: stack) c
                | Rgt c -> go_down ((fun v -> Either (Either.Rgt v)) :: stack) c

            and go_up (stack : (value -> value) list) (v : value) : value =
                match stack with
                | [] -> v
                | constructor :: stack -> constructor v |> go_up stack
            in
            go_down [] c

        module Operation = struct
            let create (params : contract_params) (contract : Mic.contract) : value =
                Operation (Create (params, contract))
            let create_named (params : contract_params) (contract : Contract.t) : value =
                Operation (CreateNamed (params, contract))
            let init_named (params : contract_params) (input : value) (name : string) : value =
                Operation (InitNamed (params, input, name))
        end
    end

    module Inspect = struct
        let key (v : value) : Key.t =
            match v with
            | Key k -> k
            | _ -> asprintf "expected a key, found `%a`" fmt v |> Exc.throw

        let list (v : value) : value Lst.t =
            match v with
            | Lst l -> l
            | _ -> asprintf "expected a list value, found `%a`" fmt v |> Exc.throw
    end

    let cons (head : value) (tail : value) : value =
        let tail = Inspect.list tail in
        Lst (head :: tail)
end
