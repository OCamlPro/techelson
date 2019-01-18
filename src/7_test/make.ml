(** This module contains the functor that builds a test run state machine. *)

open Base
open Common

module TestCxt (
    R : Interpreter.Sigs.Interpreter
) : Sigs.TestCxt with module Run = R = struct
    module Run = R
    module RunTest = Interpreter.Make.TestInterpreter (Run)
    module Env = Run.Env
    module Theory = Run.Theory

    type run_test = {
        test : RunTest.t ;
        (** Test interpreter. *)
        mutable obsolete : bool ;
        (** This flag is set to true when changing states. *)
    }

    type apply_ops = {
        test : RunTest.t ;
        (** Saved state of the testcase execution. *)
        mutable ops : Env.operation list ;
        (** Operations awaiting treatment. *)
        mutable obsolete : bool ;
        (** This flag is set to true when changing states. *)
    }

    type transfer = {
        test : RunTest.t ;
        (** Saved state of the testcase execution. *)
        ops : Env.operation list ;
        (** Saved list of operations awaiting treatment. *)
        transfer : Run.t ;
        (** Interpreter for the transfer. *)
        mutable obsolete : bool ;
        (** This flag is set to true when changing states. *)
    }

    let fmt_contracts (fmt : formatter) (env : Env.t) : unit =
        fprintf fmt "live contracts: @[";
        if Run.Env.Live.count env = 0 then (
            fprintf fmt "none"
        ) else (
            fprintf fmt "%a" Run.Env.Live.fmt env
        );
        fprintf fmt "@]"
    
    let fmt_operations (fmt : formatter) (ops : Env.operation list) : unit =
        fprintf fmt "operations    : @[";
        if ops = [] then (
            fprintf fmt "none"
        ) else (
            ops |> Lst.fold (
                fun is_first op ->
                    if not is_first then (
                        fprintf fmt "@ "
                    );
                    fprintf fmt "%a" Env.Op.fmt op;
                    false
            ) true |> ignore
        );
        fprintf fmt "@]"

    let init (contracts : Contract.t list) (tc : Testcase.t) : run_test =
        let src = Run.Src.of_test tc in
        let env = Run.Env.empty () in
        contracts |> List.iter (
            fun c -> Run.Env.add c env
        );
        let test = RunTest.mk src tc env in
        { test ; obsolete = false }

    module Test = struct
        let run (self : run_test) : apply_ops option =
            if self.obsolete then (
                Exc.throw "trying to call `run` on an obsolete `run_test` value"
            );
            match RunTest.step self.test with
            | Some ops ->
                self.obsolete <- true ;
                Some { test = self.test ; ops ; obsolete = false }
            | None -> None

        let interpreter (self : run_test) : RunTest.t = self.test
        let contract_env (self : run_test) : Env.t = RunTest.contract_env self.test

        let fmt (fmt : formatter) (self : run_test) : unit =
            contract_env self |> fmt_contracts fmt
    end

    module Ops = struct
        let apply (self : apply_ops) : (run_test, transfer) Either.t =
            if self.obsolete then (
                Exc.throw "trying to call `ops_apply` on an obsolete `apply_ops` value"
            );
            let contract_env = RunTest.contract_env self.test in
            let rec loop () : Run.t option =
                match self.ops with
                | next :: ops -> (
                    self.ops <- ops;
                    let next = Env.Op.op contract_env next in
                    let build (_env : Env.t) (op : Theory.operation) : Run.t option =
                        match op with
                        | Theory.MustFail (_value, _operation) -> failwith "aaa"

                        | Theory.CreateNamed (params, contract) -> (
                            (fun () -> Run.Env.Live.create params contract contract_env)
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
                            (fun () -> Run.Env.Live.create params contract contract_env)
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
                            match Run.Env.Live.get address contract_env with
                            | None ->
                                asprintf "address %a has no contract attached" Theory.Address.fmt address
                                |> Exc.throw
                            | Some live ->
                                Run.Env.Live.transfer tez live;
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
                                    Run.init src ~balance:live.balance ~amount:tez contract_env [
                                        (value, dtyp, Some (Annot.Var.of_string "input"))
                                    ] [ live.contract.entry ]
                                in
                                Some interp
                        )
                    in
                    let res = build contract_env next in
                    if res = None then loop () else res
                )
                | [] -> None
            in

            self.obsolete <- true;

            match loop () with
            | None ->
                assert (self.ops = []);
                Either.Lft { test = self.test ; obsolete = false }
            | Some transfer ->
                Either.Rgt { test = self.test ; ops = self.ops ; transfer ; obsolete = false }

        let operations (self : apply_ops) : Env.operation list = self.ops

        let contract_env (self : apply_ops) : Env.t = RunTest.contract_env self.test

        let fmt (fmt : formatter) (self : apply_ops) : unit =
            fprintf fmt "@[<v>%a@,%a@]" fmt_contracts (contract_env self) fmt_operations (operations self)
    end

    module Transfer = struct
        let transfer_step (self : transfer) : apply_ops option =
            let is_done = Run.step self.transfer in
            if not is_done then (
                None
            ) else (
                self.obsolete <- true;
                Some { test = self.test ; ops = self.ops ; obsolete = false }
            )

        let transfer_run (self : transfer) : apply_ops =
            let rec loop () : apply_ops =
                match transfer_step self with
                | None -> loop ()
                | Some ops ->
                    self.obsolete <- true;
                    ops
            in
            loop ()

        let interpreter (self : transfer) : Run.t = self.transfer

        let operations (self : transfer) : Env.operation list = self.ops

        let contract_env (self : transfer) : Env.t = RunTest.contract_env self.test

        let fmt (fmt : formatter) (self : transfer) : unit =
            fprintf fmt "@[<v>%a@,%a@]" fmt_contracts (contract_env self) fmt_operations (operations self)
    end
end