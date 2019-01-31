open Base
open Common

module Contracts (T : Theo.Sigs.Theory) : Sigs.ContractEnv with module Theory = T = struct
    module Theory = T

    type live = {
        address : Theory.Address.t ;
        contract : Contract.t ;
        mutable balance : Theory.Tez.t ;
        mutable storage : Theory.value ;
        params : Theory.contract_params ;
    }

    type t = {
        mutable defs : (string, Contract.t) Hashtbl.t ;
        mutable live : (int, live) Hashtbl.t ;
        mutable next_op_uid : int ;
        mutable expired_uids : IntSet.t ;
        mutable typ_cxt : DtypCheck.t
    }

    let empty () : t = {
        defs = Hashtbl.create 47 ;
        live = Hashtbl.create 47 ;
        next_op_uid = 0 ;
        expired_uids = IntSet.empty () ;
        typ_cxt = DtypCheck.empty () ;
    }

    let clone (self : t) : t = {
        self with
        defs = Hashtbl.copy self.defs ;
        live = Hashtbl.copy self.live ;
        expired_uids = IntSet.clone self.expired_uids ;
    }

    let add (contract : Contract.t) (self : t) : unit =
        if Hashtbl.mem self.defs contract.name then (
            asprintf "trying to register two contracts named `%s`" contract.Contract.name
            |> Exc.throw
        ) else (
            Hashtbl.add self.defs contract.name contract
        )

    let get (name : string) (self : t) : Contract.t =
        (fun () -> Hashtbl.find self.defs name)
        |> Exc.erase_err (
            fun () -> sprintf "could not find contract `%s`" name
        )

    let unify (self : t) (t_1 : Dtyp.t) (t_2 : Dtyp.t) : unit =
        DtypCheck.unify self.typ_cxt t_1 t_2

    module Live = struct
        let update
            (balance : Theory.Tez.t)
            ((storage, dtyp) : Theory.value * Dtyp.t)
            (address : Theory.Address.t)
            (self : t)
            : unit
        =
            log_0 "ping@.";
            let uid = Theory.Address.uid address in
            let live =
                try Hashtbl.find self.live uid with
                | Not_found ->
                    asprintf "cannot update contract at unknown address %a"
                        Theory.Address.fmt address
                    |> Exc.throw
            in
            (fun () -> unify self dtyp live.contract.storage |> ignore)
            |> Exc.chain_err (
                fun () -> asprintf "while updating storage at %a" Theory.Address.fmt address
            );
            live.balance <- balance;
            live.storage <- storage;
            ()

        let update_storage
            (self : t)
            (storage : Theory.value)
            (storage_dtyp : Dtyp.t)
            (live : live)
            : unit
        =
            unify self live.contract.storage storage_dtyp
            |> ignore;
            live.storage <- storage


        let fmt (fmt: formatter) (self : t) : unit =
            fprintf fmt "@[<v>";
            Hashtbl.fold (
                fun _ live is_first ->
                    if not is_first then (
                        fprintf fmt "@ "
                    );
                    fprintf fmt "%s %a"
                        live.contract.name
                        Theory.Address.fmt live.address;
                    false
            ) self.live true
            |> ignore;
            fprintf fmt "@]"

        let count (self : t) : int = Hashtbl.length self.live

        let create
            (params : Theory.contract_params)
            (contract : Contract.t)
            (self : t)
        : unit =
            let address = params.address in
            let uid = Theory.Address.uid address in
            if Hashtbl.mem self.live uid then (
                [
                    asprintf "trying to create two contracts with address `%a`"
                        Theory.Address.fmt address ;
                    asprintf "one called `%s`" contract.name ;
                    asprintf "another one called `%s`" (Hashtbl.find self.live uid).contract.name ;
                ] |> Exc.throws
            ) else (
                let deployed =
                    { address ; contract ; balance = params.tez ; storage = params.value ; params }
                in
                Hashtbl.add self.live uid deployed
            )

        let get (address : Theory.Address.t) (self : t) : live option =
            try Some (Hashtbl.find self.live (Theory.Address.uid address)) with
            | Not_found -> None

        let transfer (tez : Theory.Tez.t) (live : live) : unit =
            live.balance <- Theory.Tez.add live.balance tez

        let collect ~(tgt : string) (tez : Theory.Tez.t) (live : live) : unit =
            if Theory.Tez.compare live.balance tez >= 0 then
                live.balance <- Theory.Tez.sub live.balance tez
            else
                Exc.Throw.too_poor ~src:live.contract.name ~tgt ~amount:(Theory.Tez.to_native tez)

        let set_delegate (delegate : Theory.KeyH.t option) (self : live) : unit =
            Theory.set_delegate delegate self.params
    end

    type operation = {
        operation : Theory.operation ;
        uid : int ;
    }

    let get_uid (self : t) : int =
        let res = self.next_op_uid in
        self.next_op_uid <- self.next_op_uid + 1;
        res

    module Op = struct
        let fmt (fmt : formatter) (self : operation) : unit =
            Theory.fmt_operation self.uid fmt self.operation

        let uid (self : operation) : int = self.uid

        let op (env : t) (self : operation) : Theory.operation =
            let is_new = IntSet.add self.uid env.expired_uids in
            if is_new then (
                self.operation
            ) else (
                asprintf "cannot run the exact same operation twice: %a"
                    (Theory.fmt_operation self.uid) self.operation
                |> Exc.Throw.tezos
            )

        let must_fail
            (env : t)
            (expected : Theory.value option)
            (self : operation)
            : Theory.value
        =
            let must_fail_uid = get_uid env in
            Theory.Of.Operation.must_fail must_fail_uid expected (self.operation, self.uid)

        let mk (uid : int) (operation : Theory.operation) : operation =
            { operation ; uid }
    end
end
