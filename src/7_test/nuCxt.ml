open Base
open Common


module Cxt (I : Interpreter.Sigs.Interpreter) : Sigs.TestCxt = struct
    module Run = I
    module RunTest = Interpreter.Make.TestInterpreter (Run)
    module Theory = Run.Theory

    type t = {
        env : Run.Contracts.t ;
        test_run : RunTest.t ;
        mutable interp : Run.t option ;
        mutable ops : Theory.operation list ;
    }

    let mk (contracts : Contract.t list) (tc : Testcase.t) =
        let src = Run.Src.of_test tc in
        let env = Run.Contracts.empty () in
        contracts |> List.iter (
            fun c -> Run.Contracts.add c env
        );
        let test_run = RunTest.mk src tc env in
        {
            env ;
            test_run ;
            interp = None ;
            ops = [] ;
        }

    let is_done (self : t) : bool =
        (RunTest.is_done self.test_run) && self.interp = None && self.ops = []

    let test_step (self : t) : bool =
        if self.interp <> None then (
            Exc.throw "trying to stage operations from test case but a run is in progress"
        );
        if self.ops <> [] then (
            Exc.throw
                "trying to stage operations from test case but there are operations to process"
        );
        match RunTest.step self.test_run with
        | Some ops ->
            self.ops <- ops;
            false
        | None ->
            true

    let env (self : t) = self.env

    let is_in_progress (self : t) : bool =
        self.interp <> None

    (* let init (self : t) (tc : Testcase.t) : unit =
        (
            match self.interp with
            | None -> ()
            | Some interp ->
                Run.src interp
                |> asprintf "trying to start a test run while one is already active for %a" Run.Src.fmt
                |> Exc.throw
        );
        let src = Run.Src.of_test tc in
        let interp = Run.init src self.env [] [ tc.code ] in
        self.interp <- Some interp *)

    let init_contract_run (self : t) : bool =
        if self.interp <> None then (
            Exc.throw "trying to initialize the next operation but a run is in progress"
        );
        let rec loop () : bool =
            match self.ops with
            | next :: ops -> (
                self.ops <- ops;
                let is_done =
                    match next with
                    | Theory.CreateNamed (params, contract) -> (
                        (fun () -> Run.Contracts.Live.create params contract self.env)
                        |> Exc.chain_err (
                            fun () ->
                                asprintf
                                    "while spawning contract %s at address %a"
                                    contract.name Theory.Address.fmt params.address
                        );
                        false
                    )
                    | Theory.Create (params, contract) -> (
                        let contract = Contract.of_mic contract in
                        (fun () -> Run.Contracts.Live.create params contract self.env)
                        |> Exc.chain_err (
                            fun () ->
                                asprintf
                                    "while spawning contract %s at address %a"
                                    contract.name Theory.Address.fmt params.address
                        );
                        false
                    )
                    | Theory.InitNamed _ -> (
                        Exc.throw "contract spawning is not implemented" |> ignore;
                        false
                    )

                    | Theory.Transfer (address, contract, tez, param) -> (
                        let _ =
                            match Run.Contracts.Live.get address self.env with
                            | None ->
                                asprintf "address %a has no contract attached" Theory.Address.fmt address
                                |> Exc.throw
                            | Some live ->
                                Run.Contracts.Live.transfer tez live;
                                let src = Run.Src.of_address address in
                                let param_dtyp =
                                    contract.param |> Dtyp.mk_named (Some (Annot.Field.of_string "param"))
                                in
                                let storage_dtyp =
                                    contract.storage |> Dtyp.mk_named (Some (Annot.Field.of_string "storage"))
                                in
                                let dtyp = Dtyp.Pair (param_dtyp, storage_dtyp) |> Dtyp.mk in
                                let value = Theory.Of.pair param live.storage in
                                let interp =
                                    Run.init src ~balance:live.balance ~amount:tez self.env [
                                        (value, dtyp, Some (Annot.Var.of_string "input"))
                                    ] [ live.contract.entry ]
                                in
                                self.interp <- Some interp
                        in
                        true
                    )
                in
                if is_done then false else loop ()
            )
            | [] -> true
        in
        loop ()

    let apply_operations (self : t) : Run.t option =
        let rec loop () : Run.t option =
            match self.ops with
            | next :: ops -> (
                self.ops <- ops;
                let res =
                    match next with
                    | Theory.CreateNamed (params, contract) -> (
                        (fun () -> Run.Contracts.Live.create params contract self.env)
                        |> Exc.chain_err (
                            fun () ->
                                asprintf
                                    "while spawning contract %s at address %a"
                                    contract.name Theory.Address.fmt params.address
                        );
                        None
                    )
                    | Theory.Create (params, contract) -> (
                        let contract = Contract.of_mic contract in
                        (fun () -> Run.Contracts.Live.create params contract self.env)
                        |> Exc.chain_err (
                            fun () ->
                                asprintf
                                    "while spawning contract %s at address %a"
                                    contract.name Theory.Address.fmt params.address
                        );
                        None
                    )
                    | Theory.InitNamed _ -> (
                        Exc.throw "contract spawning is not implemented" |> ignore;
                        None
                    )

                    | Theory.Transfer (address, contract, tez, param) -> (
                        match Run.Contracts.Live.get address self.env with
                        | None ->
                            asprintf "address %a has no contract attached" Theory.Address.fmt address
                            |> Exc.throw
                        | Some live ->
                            Run.Contracts.Live.transfer tez live;
                            let src = Run.Src.of_address address in
                            let param_dtyp =
                                contract.param |> Dtyp.mk_named (Some (Annot.Field.of_string "param"))
                            in
                            let storage_dtyp =
                                contract.storage |> Dtyp.mk_named (Some (Annot.Field.of_string "storage"))
                            in
                            let dtyp = Dtyp.Pair (param_dtyp, storage_dtyp) |> Dtyp.mk in
                            let value = Theory.Of.pair param live.storage in
                            let interp =
                                Run.init src ~balance:live.balance ~amount:tez self.env [
                                    (value, dtyp, Some (Annot.Var.of_string "input"))
                                ] [ live.contract.entry ]
                            in
                            Some interp
                    )
                in
                if res = None then loop () else res
            )
            | [] -> None
        in
        loop ()

    let contract_step (self : t) : bool =
        match self.interp with
        | None ->
            Exc.throw "trying to run a test on uninitialized context"
        | Some interp -> Run.step interp

    let interp (self : t) : Run.t =
        match self.interp with
        | Some interp -> interp
        | None -> Exc.throw "trying to recover test runtime on uninitialized context"

    let test (self : t) : Run.t =
        RunTest.interp self.test_run

    let terminate_contract_run (self : t) =
        let interp =
            match self.interp with
            | Some interp -> interp
            | None -> Exc.throw "trying to terminate run on uninitialized context"
        in
        self.interp <- None;
        if not (Run.is_done interp) then (
            Exc.throw "trying to terminate a run that's still in progress"
        );
        match Run.src interp with
        | Run.Src.Test _ ->
            let operations =
                (fun () -> Run.stack interp |> Run.Stack.pop_operation_list |> fst)
                |> Exc.chain_err (
                    fun () -> Run.src interp |> asprintf "on test run %a" Run.Src.fmt
                )
            in
            self.ops <- operations @ self.ops
        | Run.Src.Contract address ->
            let ops, storage, storage_dtyp = Run.stack interp |> Run.Stack.pop_contract_res in
            let balance = Run.balance interp in
            Run.Contracts.Live.update balance (storage, storage_dtyp) address self.env;
            self.ops <- ops @ self.ops

    module Contracts = struct

        let add contract self = Run.Contracts.add contract self.env
        let get name self = Run.Contracts.get name self.env

        module Live = struct
            let create params contract self =
                Run.Contracts.Live.create params contract self.env
            let get address self = Run.Contracts.Live.get address self.env
        end
    end

    let fmt (fmt : formatter) (self : t) : unit =
        fprintf fmt "@[<v>";
        fprintf fmt "live contracts: @[";
        if Run.Contracts.Live.count self.env = 0 then (
            fprintf fmt "none"
        ) else (
            fprintf fmt "%a" Run.Contracts.Live.fmt self.env
        );
        fprintf fmt "@]@     operations: @[";
        if self.ops = [] then (
            fprintf fmt "none"
        ) else (
            self.ops |> Lst.fold (
                fun is_first op ->
                    if not is_first then (
                        fprintf fmt "@ "
                    );
                    fprintf fmt "%a" Theory.fmt_operation op;
                    false
            ) true |> ignore
        );
        fprintf fmt "@]@]"
end