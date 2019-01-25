open Base
open Common

(** Creates a `StackBase` from a theory. *)
module StackBase (T : Theo.Sigs.Theory) : Sigs.StackBase with
    module Theory = T
= struct
    module Theory = T
    module Env = Contracts.Contracts (Theory)

    type frame = {
        mutable value : Theory.value ;
        typ : Dtyp.t ;
        binding : Annot.Var.t option ;
    }

    let clone
        (binding : Annot.Var.t option)
        ({value ; typ ; _} : frame)
        : frame
    = { value ; typ ; binding }

    let mk_frame (value : Theory.value) (typ : Dtyp.t) (binding : Annot.Var.t option) : frame =
        { value ; typ ; binding }
    
    let fmt_frame (fmt : formatter) (frame : frame) : unit =
        fprintf fmt "@[<h>%-11s| %-57s | %-23s@]"
            (
                match frame.binding with
                | None -> ""
                | Some annot -> asprintf "%a " Annot.Var.fmt annot
            ) (asprintf "%a" Theory.fmt frame.value) (asprintf "%a" Dtyp.fmt frame.typ)

    type t = {
        mutable dipped : frame list ;
        mutable stack : frame list ;
        env : Env.t
    }

    let contract_env (self : t) : Env.t = self.env

    let fmt (fmt : formatter) (self : t) : unit =
        fprintf fmt "@[<v>\
            |================================================\
            ==================================================|";
        self.stack |> List.rev |> List.iter (
            fprintf fmt "@,| %a |" fmt_frame
        );

        if self.dipped <> [] then (
           fprintf fmt "@,\
            |================================================\
            dipped============================================|";
            self.dipped |> List.iter (
                fprintf fmt "@,| %a |" fmt_frame
            )
        );

        fprintf fmt "@,\
            |================================================\
            ==================================================|";
        fprintf fmt "@]"

    let empty (env : Env.t) : t = { dipped = [] ; stack = [] ; env }
    let is_empty ({ dipped ; stack ; _ } : t) : bool = dipped = [] && stack = []
    let push ?binding:(binding=None) (dtyp : Dtyp.t) (value : Theory.value) (self : t) : unit =
        let frame = mk_frame value dtyp binding in
        self.stack <- frame :: self.stack

    let map_last (f : Theory.value -> Dtyp.t -> Theory.value) (self : t) : unit =
        match self.stack with
        | frame :: _ ->
            let value = f frame.value frame.typ in
            frame.value <- value
        | [] -> Exc.throw "empty stack"

    let swap (self : t) : unit =
        match self.stack with
        | hd_1 :: hd_2 :: tail -> self.stack <- hd_2 :: hd_1 :: tail
        | _ -> Exc.throw "cannot `swap` a stack with less than two frames"

    let dup ?binding:(binding=None) (self : t) : unit =
        match self.stack with
        | hd :: _ -> self.stack <- (clone binding hd) :: self.stack
        | [] ->
            Exc.throw "cannot `dup` an empty stack"

    let dip (self : t) : unit =
        match self.stack with
        | hd :: stack ->
            self.dipped <- hd :: self.dipped;
            self.stack <- stack
        | [] ->
            Exc.throw "cannot `dip` an empty stack"

    let undip (self : t) : unit =
        match self.dipped with
        | hd :: dipped ->
            self.dipped <- dipped;
            self.stack <- hd :: self.stack
        | [] ->
            Exc.throw "cannot `dip` a dip-free stack"

    let pop (self : t) : Theory.value * Dtyp.t =
        match self.stack with
        | hd :: stack ->
            self.stack <- stack;
            hd.value, hd.typ
        | [] -> Exc.throw "the stack is empty"

    let clear (self : t) : unit =
        self.stack <- []
end

(** Adds convenience functions to a basic stack module.

    This functor is automatically used by the main `Interpreter` functor. *)
module Stack (S : Sigs.StackBase)
    : Sigs.Stack with type t = S.t and module Theory = S.Theory and module Env = S.Env
= struct
    include S

    module Pop = struct

        let bool (self : t) : bool * Dtyp.t =
            let run () =
                match pop self with
                | Theory.C (Theory.Cmp.B b), dtyp -> b, dtyp
                | v, dtyp ->
                    asprintf "found a value of type %a : %a" Dtyp.fmt dtyp Theory.fmt v
                    |> Exc.throw
            in
            run |> Exc.chain_err (
                fun () -> "while popping a bool from the stack"
            )

        let int (self : t) : Theory.Int.t * Dtyp.t =
            let run () =
                match pop self with
                | Theory.C (Theory.Cmp.I i), dtyp -> i, dtyp
                | v, dtyp ->
                    asprintf "found a value of type %a : %a" Dtyp.fmt dtyp Theory.fmt v
                    |> Exc.throw
            in
            run |> Exc.chain_err (
                fun () -> "while popping an int from the stack"
            )

        let nat (self : t) : Theory.Nat.t * Dtyp.t =
            let run () =
                match pop self with
                | Theory.C (Theory.Cmp.N n), dtyp -> n, dtyp
                | v, dtyp ->
                    asprintf "found a value of type %a : %a" Dtyp.fmt dtyp Theory.fmt v
                    |> Exc.throw
            in
            run |> Exc.chain_err (
                fun () -> "while popping a nat from the stack"
            )

        let str (self : t) : Theory.Str.t * Dtyp.t =
            let run () =
                match pop self with
                | Theory.C (Theory.Cmp.S s), dtyp -> s, dtyp
                | v, dtyp ->
                    asprintf "found a value of type %a : %a" Dtyp.fmt dtyp Theory.fmt v
                    |> Exc.throw
            in
            run |> Exc.chain_err (
                fun () -> "while popping a string from the stack"
            )

        let bytes (self : t) : Theory.Bytes.t * Dtyp.t =
            let run () =
                match pop self with
                | Theory.C (Theory.Cmp.By by), dtyp -> by, dtyp
                | v, dtyp ->
                    asprintf "found a value of type %a : %a" Dtyp.fmt dtyp Theory.fmt v
                    |> Exc.throw
            in
            run |> Exc.chain_err (
                fun () -> "while popping bytes from the stack"
            )

        let key_hash (self : t) : Theory.KeyH.t * Dtyp.t =
            let run () =
                match pop self with
                | Theory.C (Theory.Cmp.KeyH kh), dtyp -> kh, dtyp
                | v, dtyp ->
                    asprintf "found a value of type %a : %a" Dtyp.fmt dtyp Theory.fmt v
                    |> Exc.throw
            in
            run |> Exc.chain_err (
                fun () -> "while popping a key hash from the stack"
            )

        let key_hash_option (self : t) : (Theory.KeyH.t option) * Dtyp.t =
            let run () =
                match pop self with
                | Theory.Option (
                    Some (Theory.C (Theory.Cmp.KeyH kh))
                ), dtyp -> Some kh, Dtyp.Inspect.option dtyp
                | Theory.Option None, dtyp -> (
                    let dtyp = Dtyp.Inspect.option dtyp in
                    Dtyp.check (Dtyp.mk_leaf Dtyp.KeyH) dtyp;
                    None, dtyp
                )
                | v, dtyp ->
                    asprintf "found a value of type %a : %a" Dtyp.fmt dtyp Theory.fmt v
                    |> Exc.throw
            in
            run |> Exc.chain_err (
                fun () -> "while popping an optional key hash from the stack"
            )

        let tez (self : t) : Theory.Tez.t * Dtyp.t =
            let run () =
                match pop self with
                | Theory.C (Theory.Cmp.Tz tz), dtyp -> tz, dtyp
                | v, dtyp ->
                    asprintf "found a value of type %a : %a" Dtyp.fmt dtyp Theory.fmt v
                    |> Exc.throw
            in
            run |> Exc.chain_err (
                fun () -> "while popping a mutez from the stack"
            )

        let cmp (self : t) : Theory.Cmp.t * Dtyp.t =
            let run () =
                match pop self with
                | Theory.C cmp, dtyp -> cmp, dtyp
                | v, dtyp ->
                    asprintf "found a value of type %a : %a" Dtyp.fmt dtyp Theory.fmt v
                    |> Exc.throw
            in
            run |> Exc.chain_err (
                fun () -> "while popping a value of a comparable type from the stack"
            )

        let address (self : t) : Theory.Address.t * Dtyp.t =
            let run () =
                match pop self with
                | Theory.Address a, dtyp -> a, dtyp
                | v, dtyp ->
                    asprintf "found a value of type %a : %a" Dtyp.fmt dtyp Theory.fmt v
                    |> Exc.throw
            in
            run |> Exc.chain_err (
                fun () -> "while popping an address from the stack"
            )

        let contract (self : t) : Theory.Address.t option * Mic.contract =
            let run () =
                match pop self with
                | Theory.Contract (a, c), _ -> a, c
                | v, dtyp ->
                    asprintf "found a value of type %a : %a" Dtyp.fmt dtyp Theory.fmt v
                    |> Exc.throw
            in
            run |> Exc.chain_err (
                fun () -> "while popping a contract from the stack"
            )

        let either (self : t) : (Theory.value, Theory.value) Theory.Either.t * Dtyp.t =
            let run () =
                match pop self with
                | Theory.Either either, dtyp -> either, dtyp
                | v, dtyp ->
                    asprintf "found a value of type %a : %a" Dtyp.fmt dtyp Theory.fmt v
                    |> Exc.throw
            in
            run |> Exc.chain_err (
                fun () -> "while popping a union (`or`) from the stack"
            )

        let option (self : t) : Theory.value Theory.Option.t * Dtyp.t =
            let run () =
                match pop self with
                | Theory.Option opt, dtyp -> opt, dtyp
                | v, dtyp ->
                    asprintf "found a value of type %a : %a" Dtyp.fmt dtyp Theory.fmt v
                    |> Exc.throw
            in
            run |> Exc.chain_err (
                fun () -> "while popping an option from the stack"
            )

        let list (self : t) : Theory.value Theory.Lst.t * Dtyp.t =
            let run () =
                match pop self with
                | Theory.Lst lst, dtyp -> lst, dtyp
                | v, dtyp ->
                    asprintf "found a value of type %a : %a" Dtyp.fmt dtyp Theory.fmt v
                    |> Exc.throw
            in
            run |> Exc.chain_err (
                fun () -> "while popping a list from the stack"
            )

        let set (self : t) : Theory.Set.t * Dtyp.t =
            let run () =
                match pop self with
                | Theory.Set set, dtyp -> set, dtyp
                | v, dtyp ->
                    asprintf "found a value of type %a : %a" Dtyp.fmt dtyp Theory.fmt v
                    |> Exc.throw
            in
            run |> Exc.chain_err (
                fun () -> "while popping a set from the stack"
            )

        let map (self : t) : Theory.value Theory.Map.t * Dtyp.t =
            let run () =
                match pop self with
                | Theory.Map map, dtyp -> map, dtyp
                | v, dtyp ->
                    asprintf "found a value of type %a : %a" Dtyp.fmt dtyp Theory.fmt v
                    |> Exc.throw
            in
            run |> Exc.chain_err (
                fun () -> "while popping a map from the stack"
            )

        let pair (self : t) : (Theory.value * Dtyp.t) * (Theory.value * Dtyp.t) =
            match pop self with
            | Theory.Pair (lft, rgt), dtyp ->
                let lft_dtyp, rgt_dtyp = Dtyp.Inspect.pair dtyp in
                (lft, lft_dtyp), (rgt, rgt_dtyp)
            | v, dtyp ->
                asprintf "found a value of type %a : %a" Dtyp.fmt dtyp Theory.fmt v |> Exc.throw

        let lambda (self : t) : Dtyp.t * Dtyp.t * Mic.t =
            match pop self with
            | Theory.Lambda (dom, codom, mic), _ -> (dom, codom, mic)
            | v, dtyp ->
                asprintf "found a value of type %a : %a" Dtyp.fmt dtyp Theory.fmt v |> Exc.throw

        let to_operation (value : Theory.value) : Env.operation =
            match value with
            | Theory.Operation (uid, op) ->
                Env.Op.mk uid op
            | _ ->
                asprintf "expected an operation but found value %a" Theory.fmt value
                |> Exc.throw

        let to_operation_list
            (values : Theory.value Theory.Lst.t)
            : Env.operation list
        =
            Theory.Lst.fold (
                fun lst value ->
                    let op = to_operation value in
                    op :: lst
            ) [] values
            |> List.rev

        let operation (self : t) : Env.operation * Dtyp.t =
            let run () : Env.operation * Dtyp.t =
                let op, dtyp = pop self in
                (to_operation op), dtyp
            in
            run |> Exc.chain_err (
                fun () -> "while popping an operation from the stack"
            )

        let operation_list (self : t) : Env.operation list * Dtyp.t =
            let run () : Env.operation list * Dtyp.t =
                let lst, dtyp = list self in
                (to_operation_list lst), dtyp
            in
            run |> Exc.chain_err (
                fun () -> "while popping a list of operations from the stack"
            )
        
        let contract_res (self : t) : Env.operation list * Theory.value * Dtyp.t =
            let run () : Env.operation list * Theory.value * Dtyp.t =
                let (ops, ops_dtyp), (storage, storage_dtyp) = pair self in
                let ops =
                    match ops with
                    | Theory.Lst lst -> to_operation_list lst
                    | _ ->
                        asprintf "expected an operation list, found a value of type %a : %a"
                            Dtyp.fmt ops_dtyp Theory.fmt ops
                        |> Exc.throw
                in
                ops, storage, storage_dtyp
            in
            run |> Exc.chain_err (
                fun () -> "while popping a contract result: operation list * storage value"
            )

        (*  Pops contract creation parameters.

            - manager
            - delegate
            - spendable
            - delegatable
            - mutez
        *)
        let contract_params_head
            ~(is_account : bool)
            (self :t)
            : Theory.KeyH.t * Theory.KeyH.t option * bool * bool * Theory.Tez.t
        =
            let run () =
                let manager =
                    (fun () -> key_hash self |> fst)
                    |> Exc.chain_err (
                        fun () ->
                            "while retrieving the `manager` argument"
                    )
                in
                let delegate =
                    (fun () -> key_hash_option self |> fst)
                    |> Exc.chain_err (
                        fun () ->
                            "while retrieving the `delegate` argument"
                    )
                in
                let spendable =
                    if is_account then true else (
                        (fun () -> bool self |> fst)
                        |> Exc.chain_err (
                            fun () ->
                                "while retrieving the `spendable` argument"
                        )
                    )
                in
                let delegatable =
                    (fun () -> bool self |> fst)
                    |> Exc.chain_err (
                        fun () ->
                            "while retrieving the `delegatable` argument"
                    )
                in
                let tez =
                    (fun () -> tez self |> fst)
                    |> Exc.chain_err (
                        fun () ->
                            "while retrieving the `mutez` argument"
                    )
                in
                manager, delegate, spendable, delegatable, tez
            in
            run |> Exc.chain_err (
                fun () -> "while popping parameters for a contract creation operation"
            )


        let contract_params
            (address : Theory.Address.t)
            (self : t)
            : Theory.contract_params * Dtyp.t
        =
            let run () =
                let manager, delegate, spendable, delegatable, tez =
                    contract_params_head ~is_account:false self
                in
                let storage, storage_dtyp =
                    (fun () -> pop self)
                    |> Exc.chain_err (
                        fun () -> "while retrieving the storage value"
                    )
                in

                Theory.mk_contract_params
                    ~spendable ~delegatable manager delegate tez address storage, storage_dtyp
            in
            run |> Exc.chain_err (
                fun () -> "while popping parameters for a contract creation operation"
            )

        let account_params
            (address : Theory.Address.t)
            (self :t)
            : Theory.contract_params
        =
            let run () =
                let manager, delegate, spendable, delegatable, tez =
                    contract_params_head ~is_account:true self
                in
                let storage = Theory.Of.unit in

                Theory.mk_contract_params
                    ~spendable ~delegatable manager delegate tez address storage
            in
            run |> Exc.chain_err (
                fun () -> "while popping parameters for an account creation operation"
            )

        let contract_params_and_lambda
            (address : Theory.Address.t)
            (self : t)
            : Theory.contract_params * Mic.contract
        =
            let manager, delegate, spendable, delegatable, tez =
                contract_params_head ~is_account:false self
            in
            let run () =
                let contract =
                    let dom, codom, lambda = lambda self in
                    let lambda_dom_param, lambda_dom_storage =
                        Dtyp.Inspect.pair dom
                    in
                    let lambda_codom_ops, lambda_codom_storage =
                        Dtyp.Inspect.pair codom
                    in

                    (* Type-checking. *)
                    let lambda_storage_dtyp, lambda_param_dtyp =
                        (fun () ->
                            Dtyp.check lambda_dom_storage lambda_codom_storage;
                            let lambda_codom_op = Dtyp.Inspect.list lambda_codom_ops in
                            Dtyp.check (Dtyp.mk_leaf Dtyp.Operation) lambda_codom_op;
                            lambda_dom_storage, lambda_dom_param
                        )
                        |> Exc.chain_errs (
                            fun () -> [
                                "expected lambda \
                                    (pair 'p 'g) -> (pair (list operation) 'g)" ;
                                asprintf "found lambda %a %a" Dtyp.fmt dom Dtyp.fmt codom
                            ]
                        )
                    in

                    let contract =
                        Mic.mk_contract
                            ~storage:lambda_storage_dtyp ~param:lambda_param_dtyp lambda
                    in

                    contract
                in

                let storage =
                    (fun () ->
                        let storage, dtyp = pop self in
                        Dtyp.check contract.storage dtyp;
                        storage
                    )
                    |> Exc.chain_err (
                        fun () -> "while retrieving the storage value"
                    )
                in

                let params =
                    Theory.mk_contract_params
                        ~spendable ~delegatable manager delegate tez address storage
                in
                params, contract
            in
            run |> Exc.chain_err (
                fun () -> "while popping parameters for a contract creation operation"
            )

    end

    module Push = struct
        let some ?alias:(alias=None) ?field:(field=None) (self : t) : unit =
            let value, dtyp = pop self in
            let dtyp = Dtyp.Option (Dtyp.mk_named field dtyp) |> Dtyp.mk ~alias in
            push dtyp (Theory.Of.option (Some value)) self
        let none ?alias:(alias=None) ?field:(field=None) (dtyp : Dtyp.t) (self : t) : unit =
            let dtyp = Dtyp.Option (Dtyp.mk_named field dtyp) |> Dtyp.mk ~alias in
            push dtyp (Theory.Of.option None) self

        let left ?alias:(alias=None) (rgt_dtyp : Dtyp.t) (self : t) : unit =
            let value, dtyp = pop self in
            let lft_dtyp = { Dtyp.inner = dtyp ; name = None} in
            let rgt_dtyp = { Dtyp.inner = rgt_dtyp ; name = None } in
            let dtyp = Dtyp.Or (lft_dtyp, rgt_dtyp) |> Dtyp.mk ~alias in
            push dtyp (Theory.Of.either (Theory.Either.Lft value)) self

        let right ?alias:(alias = None) (lft_dtyp : Dtyp.t) (self : t) : unit =
            let value, dtyp = pop self in
            let lft_dtyp = { Dtyp.inner = lft_dtyp ; name = None} in
            let rgt_dtyp = { Dtyp.inner = dtyp ; name = None } in
            let dtyp = Dtyp.Or (lft_dtyp, rgt_dtyp) |> Dtyp.mk ~alias in
            push dtyp (Theory.Of.either (Theory.Either.Rgt value)) self

        let nil ?binding:(binding=None) ?alias:(alias=None) (dtyp : Dtyp.t) (self : t) : unit =
            let dtyp = Dtyp.List dtyp |> Dtyp.mk ~alias in
            push ~binding dtyp (Theory.Of.list Theory.Lst.nil) self

        let empty_set
            ?binding:(binding=None)
            ?alias:(alias=None)
            (dtyp : Dtyp.t)
            (self : t)
            : unit
        =
            let dtyp = Dtyp.Set dtyp |> Dtyp.mk ~alias in
            push ~binding dtyp (Theory.Of.set Theory.Set.empty) self

        let empty_map
            ?binding:(binding=None)
            ?alias:(alias=None)
            (key_dtyp : Dtyp.t)
            (val_dtyp : Dtyp.t)
            (self : t)
            : unit
        =
            let dtyp = Dtyp.Map (key_dtyp, val_dtyp) |> Dtyp.mk ~alias in
            push ~binding dtyp (Theory.Of.map Theory.Map.empty) self

        let address (address : Theory.Address.t) (self : t) : unit =
            let dtyp = Dtyp.Address |> Dtyp.mk_leaf in
            push dtyp (Theory.Of.address address) self
    end


    let cons (self : t) : unit =
        let run () =
            let head_value, head_dtyp = pop self in
            self |> map_last (
                fun tail_value tail_dtyp ->
                    let inner =
                        (fun () -> Dtyp.Inspect.list tail_dtyp)
                        |> Exc.chain_err (
                            fun () -> "while type-checking `CONS`"
                        )
                    in
                    (fun () -> Dtyp.check head_dtyp inner)
                    |> Exc.chain_err (
                        fun () ->
                            asprintf "head has type `%a`, but tail has type `%a`"
                                Dtyp.fmt head_dtyp Dtyp.fmt tail_dtyp
                    );
                    Theory.cons head_value tail_value
            )
        in
        run |> Exc.chain_err (
            fun () -> "while running `CONS`"
        )

    let rename (binding : Annot.Var.t option) (self : t) : unit =
        let value, dtyp = pop self in
        push ~binding dtyp value self

end
