(** Functors combining evaluators. *)

open Base
open Base.Common

(** Builds theories.

    The input module describes primitive value's representation and operations. This functor then
    creates all the other values (list, set, ...), the type for values which aggregates everything,
    the comparable values *etc*.

    Note that the functor creates its own representation for mutez which just wraps `Int64.t`.
*)
module Theory (
    P : Sigs.Primitive
) : Sigs.Theory with module Prim = P = struct
    module Prim = P


    module Cmp = struct
        include P

        module Tez = struct
            type t = Int64.t
            type nat = Nat.t

            let to_string (t : t) : string =
                Int64.to_string t

            let fmt (fmt : formatter) (t : t) : unit =
                Int64.to_string t |> fprintf fmt "%sutz"

            let to_nat (t : t) : nat =
                (fun () -> Int64.to_string t |> Nat.of_string)
                |> Exc.erase_err (
                    fun () -> asprintf "cannot convert %a to nat" fmt t
                )
            let of_nat (n : nat) : t =
                (fun () -> Nat.to_string n |> Int64.of_string)
                |> Exc.chain_err (
                    fun () -> asprintf "cannot convert %a to mutez" Nat.fmt n
                )

            let of_string (s : string) : t =
                (fun () -> Int64.of_string s)
                |> Exc.erase_err (
                    fun () -> sprintf "cannot convert string `%s` to tezos" s
                )
            let of_native (n : Int64.t) : t = n
            let to_native (t : t) : Int64.t = t

            let add (t_1 : t) (t_2 : t) : t =
                let overflow_if_gt_zero = Int64.sub Int64.max_int t_1 |> Int64.compare t_2 in
                if overflow_if_gt_zero <= 0 then
                    Int64.add t_1 t_2
                else (
                    asprintf "while adding %a and %a" fmt t_1 fmt t_2
                    |> Exc.Throw.mutez_overflow
                )

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
                    fun () -> asprintf "while evaluating `%a * %a`" fmt t Nat.fmt n
                )
            let div_nat (t : t) (n : nat) : t =
                (fun () -> of_nat n |> Int64.mul t)
                |> Exc.erase_err (
                    fun () -> asprintf "while evaluating `%a / %a`" fmt t Nat.fmt n
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

            | S s, Leaf Timestamp -> Ts (Str.to_string s |> TStamp.of_native)
            | I i, Leaf Timestamp -> Ts (TStampConv.int_to_tstamp i)
            | N n, Leaf Timestamp -> Ts (
                NatConv.nat_to_int n
                |> TStampConv.int_to_tstamp
            )
            | Ts _, Leaf Timestamp -> t

            | KeyH _, Leaf KeyH -> t

            | _ -> bail ()

        module Unwrap = struct
            let bool (c : t) : bool =
                match c with
                | B b -> b
                | _ -> asprintf "unwrapping %a as bool" fmt c |> Exc.throw
            let int (c : t) : Int.t =
                match c with
                | I i -> i
                | _ -> asprintf "unwrapping %a as int" fmt c |> Exc.throw
            let nat (c : t) : Nat.t =
                match c with
                | N n -> n
                | _ -> asprintf "unwrapping %a as nat" fmt c |> Exc.throw
            let str (c : t) : Str.t =
                match c with
                | S s -> s
                | _ -> asprintf "unwrapping %a as string" fmt c |> Exc.throw
            let bytes (c : t) : Bytes.t =
                match c with
                | By by -> by
                | _ -> asprintf "unwrapping %a as bytes" fmt c |> Exc.throw
        end

    end

    module Address = P.Address

    module Tez = Cmp.Tez

    module Int = struct
        include Cmp.Int
        let to_nat : t -> Cmp.Nat.t option = Cmp.NatConv.int_to_nat
        let of_nat : Cmp.Nat.t -> t = Cmp.NatConv.nat_to_int
        let abs : t -> Cmp.Nat.t = Cmp.NatConv.int_abs
        let ediv : t -> t -> (t * Cmp.Nat.t) option = Cmp.NatConv.ediv
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
            Inner.cardinal t |> sprintf "%i" |> Cmp.Nat.of_string
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
        let fold_as
            (f_k : key -> 'k)
            (f_v : 'a -> 'b)
            (f : 'acc -> 'k -> 'b -> 'acc)
            (acc : 'acc)
            (map : 'a t)
            : 'acc
        =
            fold (fun acc k v -> f acc (f_k k) (f_v v)) acc map
        let map (f : key -> 'a -> 'b) (t : 'a t) : 'b t = Inner.mapi f t
        let map_as (f_k : key -> 'k) (f_v : 'a -> 'b) (f : 'k -> 'b -> 'c) (t : 'a t) : 'c t =
            map (
                fun k v -> f (f_k k) (f_v v)
            ) t
        let size (t : 'a t) : Cmp.Nat.t =
            Inner.cardinal t |> sprintf "%i" |> Cmp.Nat.of_string

        let bindings (self : 'a t) : (key * 'a) list = Inner.bindings self

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
            List.length l |> sprintf "%i" |> Cmp.Nat.of_string
        let fold = List.fold_left
        let map = List.map
        let is_nil lst = lst = []
        let cons hd tl = hd :: tl
        let nil = []

        let of_list (l : 'a list) : 'a t = l
        let to_list (l : 'a t) : 'a list = l

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
        
        let snoc (lst : 'a t) : ('a * 'a t) option =
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
    | Contract of Address.t option * Mic.contract
    | Operation of int * operation
    | Address of Address.t
    | Lambda of Dtyp.t * Dtyp.t * Mic.t

    and contract_params = {
        address : Address.t ;
        manager : KeyH.t ;
        mutable delegate : KeyH.t option ;
        spendable : bool ;
        delegatable : bool ;
        tez : Tez.t ;
        value : value ;
    }

    and transfer_info = {
        source : Address.t ;
        sender : Address.t ;
        target : Address.t ;
        contract : Mic.contract ;
        amount : Tez.t ;
        param : value ;
    }

    and operation =
    | MustFail of (value * Dtyp.t) option * operation * int
    | Create of contract_params * Mic.contract
    | CreateNamed of contract_params * Contract.t
    | InitNamed of contract_params * value * string
    | Transfer of transfer_info
    | SetDelegate of Address.t * KeyH.t option

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

    let set_delegate (delegate : KeyH.t option) (self : contract_params) : unit =
        self.delegate <- delegate

    let rec fmt_contract_params (fmtt : formatter) (params : contract_params) : unit =
        fprintf fmtt "(@%a, %a, %a, %b, %b, %a)"
            Address.fmt params.address
            KeyH.fmt params.manager
            (Option.fmt KeyH.fmt) params.delegate
            params.spendable params.delegatable
            Tez.fmt params.tez

    and fmt_operation (uid : int) (fmtt : formatter) (op : operation) : unit =
        match op with
        | MustFail (value, operation, op_uid) ->
            fprintf fmtt "MUST_FAIL[uid:%i] " uid;
            (
                match value with
                | None -> fprintf fmtt "_"
                | Some (v, t) -> fprintf fmtt "%a : %a" fmt v Dtyp.fmt t
            );
            fprintf fmtt " (%a)" (fmt_operation op_uid) operation
        | Create (params, contract) ->
            fprintf fmtt "@[<hv 4>CREATE[uid:%i] %a %a@]"
                uid fmt_contract_params params Mic.fmt_contract contract
        | CreateNamed (params, contract) ->
            fprintf fmtt "@[<hv 4>CREATE[uid:%i] %a \"%s\"@]"
                uid fmt_contract_params params contract.name
        | InitNamed (params, value, name) ->
            fprintf fmtt "@[<hv 4>CREATE[uid:%i] %a %a %s@]"
                uid fmt_contract_params params fmt value name
        | Transfer { target ; amount ; param ; _ } ->
            fprintf fmtt "@[<hv 4>TRANSFER[uid:%i] %a %a %a@]"
                uid Address.fmt target Tez.fmt amount fmt param
        | SetDelegate (address, delegate) ->
            fprintf fmtt "@[<hv 4>SET_DELEGATE[uid:%i] %a %a@]"
                uid Address.fmt address (Opt.fmt KeyH.fmt) delegate
        

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
                fprintf fmt "AASet {";
                let elms =
                    Set.fold (
                        fun (is_first, acc) value ->
                            let pref = if is_first then " " else ", " in
                            false, (pref, C value) :: acc
                    ) (true, []) set
                    |> snd
                    |> List.rev
                in
                go_up ((elms, " }") :: stack)
            | Map map ->
                fprintf fmt "Map {";
                let elms =
                    Map.fold (
                        fun (is_first, acc) key value ->
                            let pref = if is_first then " " else ", " in
                            false, (pref, C key) :: (" -> ", value) :: acc
                    ) (true, []) map
                    |> snd
                    |> List.rev
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
                    List.fold_right (
                        fun value (cnt, acc) ->
                            let is_first = cnt = List.length lst in
                            let pref = if is_first then " " else ", " in
                            cnt + 1, (pref, value) :: acc
                    ) lst (1, [])
                in
                go_up ((elms, " ]") :: stack)

            | Pair (lft ,rgt) ->
                fprintf fmt "(";
                go_down ( ([", ", rgt], ")") :: stack ) lft

            | Contract (address, contract) -> (
                match address with
                | Some a ->
                    Address.fmt fmt a;
                    go_up stack
                | None -> (
                    fprintf fmt "{ storage : @[%a@] ; param : @[%a@] ; code : @[%a@] ; }"
                        Dtyp.fmt contract.Mic.storage
                        Dtyp.fmt contract.Mic.param
                        Mic.fmt contract.Mic.entry;
                    go_up stack
                )
            )
            
            | Operation (uid, op) ->
                fprintf fmt "%a" (fmt_operation uid) op;
                go_up stack

            | Lambda (dom, codom, mic) ->
                fprintf fmt "LAMBDA %a %a %a"
                    Dtyp.fmt dom Dtyp.fmt codom Mic.fmt mic;
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

    module Of = struct
        let int (i : Int.t) : value = C (Cmp.I i)
        let nat (i : Nat.t) : value = C (Cmp.N i)
        let str (i : Str.t) : value = C (Cmp.S i)
        let bytes (by : Bytes.t) : value = C (Cmp.By by)
        let timestamp (ts : TStamp.t) : value = C (Cmp.Ts ts)
        let key (k : Key.t) : value = Key k
        let key_hash (kh : KeyH.t) : value = C (Cmp.KeyH kh)
        let tez (tz : Tez.t) : value = C (Cmp.Tz tz)

        let lambda (domain : Dtyp.t) (codomain : Dtyp.t) (mic : Mic.t) : value =
            Lambda (domain, codomain, mic)

        let address (a : Address.t) : value = Address a

        let primitive_str (dtyp : Dtyp.t) (s : string) : value =
            match dtyp.typ with
            | Dtyp.Leaf Dtyp.Key -> Key (Key.of_native s)
            | Dtyp.Leaf Dtyp.Bytes -> C (Cmp.By (Bytes.of_native s))
            | Dtyp.Leaf Dtyp.Timestamp -> C (Cmp.Ts (TStamp.of_native s))
            | Dtyp.Leaf Dtyp.Str -> C (Cmp.S (Str.of_native s))
            | _ -> asprintf "cannot cast a primitive string to `%a`" Dtyp.fmt dtyp |> Exc.throw

        let unit : value = U
        let bool (b : bool) : value = C (Cmp.B b)
        let cmp (cmp : Cmp.t) : value = C cmp
        let set (set : Set.t) : value = Set set
        let map (map : value Map.t) : value = Map map
        let big_map (map : value BigMap.t) : value = BigMap map
        let either (e : (value, value) Either.t) : value = Either e
        let option (o : value Option.t) : value = Option o
        let list (l : value Lst.t) : value = Lst l
        let pair (lft : value) (rgt : value) : value = Pair (lft, rgt)

        let contract (a : Address.t) (c : Mic.contract) : value = Contract (Some a, c)

        type constructor = value -> value
        type list_constructor = value list -> value
        type frame =
        | Con of constructor
        (* A simple constructor. Takes a value and produces a value. *)
        | LstCon of list_constructor * Mic.const list * value list
        (* Constructs a value from a list.

            First is the constructor, then the constants to transpose, values already processed in
            reverse order.
        *)

        let const (c : Mic.const) : value =
            let rec go_down (stack : frame list) (c : Mic.const) : value =
                match c with
                | Mic.U -> U

                | Mic.Bool b -> C (Cmp.B b) |> go_up stack
                | Mic.Int i -> C (Cmp.I (Cmp.Int.of_string i)) |> go_up stack
                | Mic.Str s -> C (Cmp.S (Cmp.Str.of_native s)) |> go_up stack
                | Mic.Bytes by -> C (Cmp.By (Cmp.Bytes.of_native by)) |> go_up stack

                | Mic.Cont c -> Contract (None, c)

                | Mic.No -> Option None |> go_up stack

                | Mic.So c -> go_down (Con (fun v -> Option (Some v)) :: stack) c
                | Mic.Lft c -> go_down (Con (fun v -> Either (Either.Lft v)) :: stack) c
                | Mic.Rgt c -> go_down (Con (fun v -> Either (Either.Rgt v)) :: stack) c

                | Mic.Lst (hd :: tl) ->
                    let frame =
                        LstCon ((fun lst -> Lst (Lst.of_list lst)), tl, [])
                    in
                    go_down (frame :: stack) hd
                | Mic.Lst [] -> Lst (Lst.of_list [])

                | Mic.Pr (fst, snd) ->
                    let frame =
                        (fun () ->
                            LstCon (
                                (function
                                    | [ fst ; snd ] -> pair fst snd
                                    | l ->
                                        List.length l
                                        |> sprintf "\
                                            expected list of two elements for pair constructor, \
                                            found %i\
                                        "
                                        |> Exc.throw
                                ),
                                [snd],
                                []
                            )
                        )
                        |> Exc.chain_err (
                            fun () ->
                                asprintf
                                    "internal error while constructing value from %a"
                                    Mic.fmt_const c
                        )
                    in
                    go_down (frame :: stack) fst

            and go_up (stack : frame list) (v : value) : value =
                match stack with
                | [] -> v
                | (Con constructor) :: stack -> constructor v |> go_up stack
                | (LstCon (constructor, [], tail)) :: stack ->
                    v :: tail |> List.rev |> constructor |> go_up stack
                | (LstCon (constructor, cst :: cst_tail, values)) ::  stack ->
                    let stack = (LstCon (constructor, cst_tail, v :: values)) :: stack in
                    go_down stack cst
            in
            go_down [] c

        module Operation = struct
            let create (uid : int) (params : contract_params) (contract : Mic.contract) : value =
                Operation (uid, Create (params, contract))
            let create_named
                (uid : int)
                (params : contract_params)
                (contract : Contract.t)
                : value
            =
                Operation (uid, CreateNamed (params, contract))
            let init_named
                (uid : int)
                (params : contract_params)
                (input : value)
                (name : string)
                : value
            =
                Operation (uid, InitNamed (params, input, name))
            let transfer (uid : int) (info : transfer_info) : value
            =
                Operation (uid, Transfer info)
            let must_fail
                (uid : int)
                (value : (value * Dtyp.t) option)
                (op, op_uid : operation * int)
                : value
            =
                Operation (uid, MustFail (value, op, op_uid))
            let set_delegate (uid : int) (address : Address.t) (delegate : KeyH.t option) : value =
                Operation (uid, SetDelegate (address, delegate))
        end
    end

    module Inspect = struct
        let cmp (v : value) : Cmp.t =
            match v with
            | C cmp -> cmp
            | _ -> asprintf "expected a comparable value, found `%a`" fmt v |> Exc.throw

        let key (v : value) : Key.t =
            match v with
            | Key k -> k
            | _ -> asprintf "expected a key, found `%a`" fmt v |> Exc.throw

        let pair (v : value) : value * value =
            match v with
            | Pair (v_1, v_2) -> v_1, v_2
            | _ -> asprintf "expected a pair, found `%a`" fmt v |> Exc.throw

        let list (v : value) : value Lst.t =
            match v with
            | Lst l -> l
            | _ -> asprintf "expected a list value, found `%a`" fmt v |> Exc.throw

        let map (v : value) : value Map.t =
            match v with
            | Map map -> map
            | _ -> asprintf "expected a map value, found `%a`" fmt v |> Exc.throw
    end

    (*  # TODO

        - stackless
    *)
    let rec cast (dtyp : Dtyp.t) (v : value) : value =
        let bail_msg () =
            asprintf "cannot cast value `%a` to type `%a`" fmt v Dtyp.fmt dtyp
        in
        match dtyp.typ, v with
        | Dtyp.Leaf Dtyp.Key, C (Cmp.S s) ->
            Key (Str.to_string s |> Key.of_native)
        | Dtyp.Contract param, Contract (_, c) ->
            if c.Mic.param = param then v else bail_msg () |> Exc.throw
        | Dtyp.Leaf Dtyp.Unit, U -> v
        | Dtyp.List sub, Lst l ->
            (fun () -> l |> List.map (cast sub))
            |> Exc.chain_err bail_msg
            |> Of.list
        | Dtyp.Or (lft, _), Either (Either.Lft v) ->
            (fun () -> Either (Either.Lft (cast lft.inner v)))
            |> Exc.chain_err bail_msg
        | Dtyp.Or (_, rgt), Either (Either.Rgt v) ->
            (fun () -> Either (Either.Rgt (cast rgt.inner v)))
            |> Exc.chain_err bail_msg
        | Dtyp.Pair (lft, rgt), Pair (l, r) ->
            (
                fun () ->
                    let lft = cast lft.inner l in
                    let rgt = cast rgt.inner r in
                    Pair (lft, rgt)
            )
            |> Exc.chain_err bail_msg
        | _ -> (
            match v with
            | C cmp -> C (Cmp.cast dtyp cmp)
            | _ -> bail_msg () |> Exc.throw
        )

    let cons (head : value) (tail : value) : value =
        let tail = Inspect.list tail in
        Lst (head :: tail)

    let car (v : value) : value =
        match v with
        | Pair (lft, _) -> lft
        | _ -> asprintf "expected pair, found `%a`" fmt v |> Exc.throw

    let cdr (v : value) : value =
        match v with
        | Pair (_, rgt) -> rgt
        | _ -> asprintf "expected pair, found `%a`" fmt v |> Exc.throw

    let cmp (v_1 : value) (v_2 : value) : value * Dtyp.t =
        match v_1, v_2 with
        | C c_1, C c_2 -> C (Cmp.I (Cmp.cmp c_1 c_2 |> Int.of_native)), Dtyp.Int |> Dtyp.mk_leaf
        | _ -> asprintf "cannot compare values %a and %a" fmt v_1 fmt v_2 |> Exc.throw

    let eq_raw (v_1 : value) (v_2 : value) : bool =
        let rec go_down (stack : (value * value) list) (v_1 : value) (v_2 : value) : bool =
            match v_1, v_2 with
            | U, U -> true
            | C c_1, C c_2 -> (
                let cmp = Cmp.cmp c_1 c_2 in
                if cmp <> 0 then false else go_up stack
            )
            | Pair (v_1_1, v_1_2), Pair (v_2_1, v_2_2) -> (
                go_down ( (v_1_2, v_2_2) :: stack ) v_1_1 v_2_1
            )
            | Option (Some v_1), Option (Some v_2) -> go_down stack v_1 v_2
            | Option None, Option None -> go_up stack
            | Option _, Option _ -> false

            | Either (Either.Lft v_1), Either (Either.Lft v_2) -> go_down stack v_1 v_2
            | Either (Either.Rgt v_1), Either (Either.Rgt v_2) -> go_down stack v_1 v_2
            | Either _, Either _ -> false

            | Key k_1, Key k_2 -> if k_1 <> k_2 then false else go_up stack

            | _ -> asprintf "cannot compare values %a and %a" fmt v_1 fmt v_2 |> Exc.throw
        and go_up (stack : (value * value) list) : bool =
            match stack with
            | [] -> true
            | (v_1, v_2) :: stack -> go_down stack v_1 v_2
        in
        go_down [] v_1 v_2
    
    let eq (v_1 : value) (v_2 : value) : value = eq_raw v_1 v_2 |> Of.bool
    let neq (v_1 : value) (v_2 : value) : value = eq_raw v_1 v_2 |> not |> Of.bool

    let zero_cmp_raw (v : value) : int =
        match v with
        | C (Cmp.I i) -> Int.compare i Int.zero
        | C (Cmp.N n) -> Nat.compare n Nat.zero
        | C (Cmp.Tz tz) -> Tez.compare tz Tez.zero
        | _ -> asprintf "cannot compare %a to zero" fmt v |> Exc.throw

    let is_zero (v : value) : value =
        zero_cmp_raw v =  0 |> Of.bool
    let is_not_zero (v : value) : value =
        zero_cmp_raw v <> 0 |> Of.bool
    let lt_zero (v : value) : value =
        zero_cmp_raw v <  0 |> Of.bool
    let le_zero (v : value) : value =
        zero_cmp_raw v <= 0 |> Of.bool
    let ge_zero (v : value) : value =
        zero_cmp_raw v >= 0 |> Of.bool
    let gt_zero (v : value) : value =
        zero_cmp_raw v >  0 |> Of.bool

    let abs (v : value) : value * Dtyp.t =
        match v with
        | C (Cmp.I i) -> C (Cmp.N (Int.abs i)), Dtyp.mk_leaf Dtyp.Nat
        | _ -> asprintf "cannot compute absolute value for %a" fmt v |> Exc.throw

    let neg (v : value) : value * Dtyp.t =
        match v with
        | C (Cmp.I i) -> C (Cmp.I (Int.sub Int.zero i)), Dtyp.mk_leaf Dtyp.Int
        | C (Cmp.N n) -> C (Cmp.I (Int.of_nat n |> Int.sub Int.zero)), Dtyp.mk_leaf Dtyp.Int

        | _ -> asprintf "cannot compute negation of %a" fmt v |> Exc.throw

    let sub (v_1 : value) (v_2 : value) : value * Dtyp.t =
        match v_1, v_2 with
        | C (Cmp.I i_1), C (Cmp.I i_2) ->
            C (Cmp.I (Int.sub i_1 i_2)), Dtyp.Int |> Dtyp.mk_leaf
        | C (Cmp.I i_1), C (Cmp.N n_2) ->
            C (Cmp.I (Nat.to_int n_2 |> Int.sub i_1)), Dtyp.Int |> Dtyp.mk_leaf
        | C (Cmp.N n_1), C (Cmp.I i_2) ->
            C (Cmp.I (Int.sub (Nat.to_int n_1) i_2)), Dtyp.Int |> Dtyp.mk_leaf
        | C (Cmp.N n_1), C (Cmp.N n_2) ->
            C (Cmp.I (Nat.sub n_1 n_2)), Dtyp.Int |> Dtyp.mk_leaf
        | C (Cmp.Ts ts_1), C (Cmp.Ts ts_2) ->
            C (Cmp.I (TStamp.sub ts_1 ts_2)), Dtyp.Int |> Dtyp.mk_leaf
        | C (Cmp.Ts ts_1), C (Cmp.I i_2) ->
            C (Cmp.Ts (TStamp.sub_int ts_1 i_2)), Dtyp.Timestamp |> Dtyp.mk_leaf
        | C (Cmp.Tz tz_1), C (Cmp.Tz tz_2) ->
            C (Cmp.Tz (Tez.sub tz_1 tz_2)), Dtyp.Mutez |> Dtyp.mk_leaf
        | _ -> asprintf "cannot subtract %a to %a" fmt v_2 fmt v_1 |> Exc.throw

    let add (v_1 : value) (v_2 : value) : value * Dtyp.t =
        match v_1, v_2 with
        | C (Cmp.I i_1), C (Cmp.I i_2) ->
            C (Cmp.I (Int.add i_1 i_2)), Dtyp.Int |> Dtyp.mk_leaf
        | C (Cmp.I i), C (Cmp.N n)
        | C (Cmp.N n), C (Cmp.I i) ->
            C (Cmp.I (Nat.to_int n |> Int.add i)), Dtyp.Int |> Dtyp.mk_leaf
        | C (Cmp.N n_1), C (Cmp.N n_2) ->
            C (Cmp.N (Nat.add n_1 n_2)), Dtyp.Nat |> Dtyp.mk_leaf

        | C (Cmp.Ts ts), C (Cmp.I i)
        | C (Cmp.I i), C (Cmp.Ts ts) ->
            C (Cmp.Ts (TStamp.add ts i)), Dtyp.Timestamp |> Dtyp.mk_leaf

        | C (Cmp.Tz tz_1), C (Cmp.Tz tz_2) ->
            C (Cmp.Tz (Tez.add tz_1 tz_2)), Dtyp.Mutez |> Dtyp.mk_leaf
        | _ -> asprintf "cannot add %a to %a" fmt v_1 fmt v_2 |> Exc.throw

    let mul (v_1 : value) (v_2 : value) : value * Dtyp.t =
        match v_1, v_2 with
        | C (Cmp.I i_1), C (Cmp.I i_2) ->
            C (Cmp.I (Int.mul i_1 i_2)), Dtyp.Int |> Dtyp.mk_leaf
        | C (Cmp.I i), C (Cmp.N n)
        | C (Cmp.N n), C (Cmp.I i) ->
            C (Cmp.I (Nat.to_int n |> Int.mul i)), Dtyp.Int |> Dtyp.mk_leaf
        | C (Cmp.N n_1), C (Cmp.N n_2) ->
            C (Cmp.N (Nat.mul n_1 n_2)), Dtyp.Nat |> Dtyp.mk_leaf

        | C (Cmp.Tz tz), C (Cmp.N n)
        | C (Cmp.N n), C (Cmp.Tz tz) ->
            C (Cmp.Tz (Tez.mul_nat tz n)), Dtyp.Mutez |> Dtyp.mk_leaf

        | _ -> asprintf "cannot multiply %a and %a" fmt v_1 fmt v_2 |> Exc.throw

    let ediv (v_1 : value) (v_2 : value) : value * Dtyp.t =
        (*  - the numerator
            - the denominator
            - function to apply to the quotient: `Int.t -> value`
            - type of the quotient
            - function to apply to the remainder: `Nat.t -> value`
            - type of the remainder
        *)
        let i_1, i_2, f_div, div_dtyp, f_rem, rem_dtyp =
            let bail () =
                asprintf "internal fatal error in `ediv` application to %a, %a"
                    fmt v_1 fmt v_2
                |> Exc.throw
            in
            match v_1, v_2 with
            | C (Cmp.I i_1), C (Cmp.I i_2) -> (
                i_1, i_2,
                Of.int, Dtyp.mk_leaf Int,
                Of.nat, Dtyp.mk_leaf Nat
            )
            | C (Cmp.N n_1), C (Cmp.I i_2) -> (
                Nat.to_int n_1, i_2,
                Of.int, Dtyp.mk_leaf Int,
                Of.nat, Dtyp.mk_leaf Nat
            )
            | C (Cmp.I i_1), C (Cmp.N n_2) -> (
                i_1, Nat.to_int n_2,
                Of.int, Dtyp.mk_leaf Int,
                Of.nat, Dtyp.mk_leaf Nat
            )
            | C (Cmp.N n_1), C (Cmp.N n_2) -> (
                Nat.to_int n_1, Nat.to_int n_2,
                (
                    fun i ->
                        match Nat.of_int i with
                        | Some n -> Of.nat n
                        | None -> bail ()
                ), Dtyp.mk_leaf Nat,
                Of.nat, Dtyp.mk_leaf Nat
            )

            | C (Cmp.Tz tz_1), C (Cmp.N n_2) -> (
                Tez.to_nat tz_1 |> Nat.to_int, Nat.to_int n_2,
                (
                    fun i -> match Nat.of_int i with
                    | Some n -> Tez.of_nat n |> Of.tez
                    | None -> bail ()
                ), Dtyp.mk_leaf Dtyp.Mutez,
                (
                    fun n -> Tez.of_nat n |> Of.tez
                ), Dtyp.mk_leaf Dtyp.Mutez
            )

            | C (Cmp.Tz tz_1), C (Cmp.Tz tz_2) -> (
                Tez.to_nat tz_1 |> Nat.to_int, Tez.to_nat tz_2 |> Nat.to_int,
                (
                    fun i -> match Nat.of_int i with
                    | Some n -> Of.nat n
                    | None -> bail ()
                ), Dtyp.mk_leaf Dtyp.Nat,
                (
                    fun n -> Tez.of_nat n |> Of.tez
                ), Dtyp.mk_leaf Dtyp.Mutez
            )

            | _ -> asprintf "cannot apply EDIV to %a and %a" fmt v_1 fmt v_2 |> Exc.throw
        in
        let dtyp =
            Dtyp.Pair (
                div_dtyp |> Dtyp.mk_named None,
                rem_dtyp |> Dtyp.mk_named None
            ) |> Dtyp.mk
        in
        let value () =
            Int.ediv i_1 i_2 |> Opt.map (
                fun (div, rem) ->
                    let div, rem = f_div div, f_rem rem in
                    Of.pair div rem
            )
            |> Of.option
        in

        let value = value |> Exc.catch_fail in
        let dtyp = Dtyp.Option (Dtyp.mk_named None dtyp) |> Dtyp.mk in
        value, dtyp

    let lshift_lft (v_1 : value) (v_2 : value) : value * Dtyp.t =
        let dtyp = Dtyp.mk_leaf Dtyp.Nat in
        match v_1, v_2 with
        | C (Cmp.N n_1), C (Cmp.N n_2) ->
            C (Cmp.N (Nat.lshift_lft n_1 n_2)), dtyp
        | _ -> asprintf "cannot apply LSL to %a and %a" fmt v_1 fmt v_2 |> Exc.throw

    let lshift_rgt (v_1 : value) (v_2 : value) : value * Dtyp.t =
        let dtyp = Dtyp.mk_leaf Dtyp.Nat in
        match v_1, v_2 with
        | C (Cmp.N n_1), C (Cmp.N n_2) ->
            C (Cmp.N (Nat.lshift_rgt n_1 n_2)), dtyp
        | _ -> asprintf "cannot apply LSL to %a and %a" fmt v_1 fmt v_2 |> Exc.throw

    let disj (v_1 : value) (v_2 : value) : value * Dtyp.t =
        match v_1, v_2 with
        | C (Cmp.B b_1), C (Cmp.B b_2) -> C (Cmp.B (b_1 || b_2)), Dtyp.Bool |> Dtyp.mk_leaf

        | _ -> asprintf "cannot compute disjunction of %a and %a" fmt v_1 fmt v_2 |> Exc.throw

    let conj (v_1 : value) (v_2 : value) : value * Dtyp.t =
        match v_1, v_2 with
        | C (Cmp.B b_1), C (Cmp.B b_2) -> C (Cmp.B (b_1 && b_2)), Dtyp.Bool |> Dtyp.mk_leaf

        | _ -> asprintf "cannot compute conjunction of %a and %a" fmt v_1 fmt v_2 |> Exc.throw

    let xor (v_1 : value) (v_2 : value) : value * Dtyp.t =
        match v_1, v_2 with
        | C (Cmp.B b_1), C (Cmp.B b_2) ->
            C (Cmp.B ((b_1 && not b_2) || (not b_1 && b_2))), Dtyp.Bool |> Dtyp.mk_leaf
        | C (Cmp.N n_1), C (Cmp.N n_2) ->
            C (Cmp.N (Nat.xor n_1 n_2)), Dtyp.Nat |> Dtyp.mk_leaf

        | _ -> asprintf "cannot compute conjunction of %a and %a" fmt v_1 fmt v_2 |> Exc.throw

    let not (v : value) : value * Dtyp.t =
        match v with
        | C (Cmp.B b) -> C (Cmp.B (not b)), Dtyp.Bool |> Dtyp.mk_leaf

        | _ -> asprintf "cannot compute negation of %a" fmt v |> Exc.throw

    let coll_to_list (v : value) : value list =
        match v with
        | Lst lst -> lst
        | Set set -> (
            Set.fold_as (Of.cmp) (fun acc elm -> elm :: acc) [] set |> List.rev
        )
        | Map map -> (
            Map.fold (
                fun (acc : value list) key value ->
                    let key = key |> Of.cmp in
                    let pair = Of.pair key value in
                    pair :: acc
            ) ([] : value list) map |> List.rev
        )
        | BigMap map -> (
            BigMap.fold (
                fun acc key value ->
                    let key = key |> Of.cmp in
                    let pair = Of.pair key value in
                    pair :: acc
            ) [] map |> List.rev
        )
        | _ -> asprintf "cannot turn this value into a list: %a" fmt v |> Exc.throw
end
