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

    type event = Run.event
    type test_event = RunTest.test_event

    (** Expected outcome of operations. *)
    type outcome =
    | Success of Env.operation
    (** Operation is expected to succeed. *)
    | Fail of Theory.value option * Env.operation * Env.t
    (** Operation must fail.

        The optional value is the expected failure value. If `None`, the operation can fail with
        any value. The operation is the source operation created by the testcase.
    *)

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
        (** Expected outcome of the list of operations above, and original test operation. *)
        mutable outcome : outcome ;
        (** Operations awaiting treatment.
        
            These operations are created by contract transfers. They are separate from the test
            operation below because if an operation must fail, and if it is a transfer, the *must
            fail* property is propagated to the operations it creats.
        *)
        mutable test_ops : Env.operation list ;
        (** Test operations awaiting treatment. *)
        mutable obsolete : bool ;
        (** This flag is set to true when changing states. *)
    }

    type transfer = {
        outcome : outcome ;
        (** Expected outcome of the transfer. *)
        test : RunTest.t ;
        (** Saved state of the testcase execution. *)
        ops : Env.operation list ;
        (** Operations awaiting treatment.
        
            These operations are created by contract transfers. They are separate from the test
            operation below because if an operation must fail, and if it is a transfer, the *must
            fail* property is propagated to the operations it creats.
        *)
        mutable test_ops : Env.operation list ;
        (** Test operations awaiting treatment. *)
        transfer : Run.t ;
        (** Interpreter for the transfer. *)
        mutable obsolete : bool ;
        (** This flag is set to true when changing states. *)
    }

    let fmt_outcome (fmt : formatter) (outcome : outcome) : unit =
        match outcome with
        | Success _ -> fprintf fmt "success"
        | Fail (value, _, _) -> (
            fprintf fmt "fail ";
            match value with
            | None -> fprintf fmt "<none>"
            | Some v -> fprintf fmt "%a" Theory.fmt v
        )

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
        let run (self : run_test) : Run.event list * apply_ops option =
            if self.obsolete then (
                Exc.throw "trying to call `run` on an obsolete `run_test` value"
            );
            let rec loop (acc : Run.event list) : Run.event list * apply_ops option =
                let event = RunTest.run self.test in
                match event with
                | RunTest.ApplyOps [] ->
                    (Run.Warn (fun fmt -> fprintf fmt "skipping application of zero operations"))
                    :: acc
                    |> List.rev,
                    None
                | RunTest.ApplyOps (op :: test_ops) ->
                    self.obsolete <- true;
                    let next_state =
                        Some {
                            test = self.test ;
                            ops = [op] ;
                            outcome = Success op ;
                            test_ops ;
                            obsolete = false ;
                        }
                    in
                    List.rev acc, next_state
                | RunTest.Normal event -> (
                    match event with
                    | Run.Done | Run.Step _ | Failure _ -> event :: acc |> List.rev, None
                    | Run.PrintStack | Run.Print _ | Run.Warn _ -> event :: acc |> loop
                )
            in
            loop []

        let interpreter (self : run_test) : RunTest.t = self.test
        let stack (self : run_test) : Run.Stack.t = RunTest.stack self.test
        let contract_env (self : run_test) : Env.t = RunTest.contract_env self.test

        let fmt (fmt : formatter) (self : run_test) : unit =
            fprintf fmt "@[<v>state : test@,";
            contract_env self |> fmt_contracts fmt;
            fprintf fmt "@]"
    end

    module Ops = struct
        let contract_env (self : apply_ops) : Env.t =
            match self.outcome with
            | Success _ -> RunTest.contract_env self.test
            | Fail (_, _, env) -> env

        let operations (self : apply_ops) : Env.operation list = self.ops

        let fmt (fmt : formatter) (self : apply_ops) : unit =
            fprintf fmt "@[<v>state   : apply_ops@,outcome : %a@,"
                fmt_outcome self.outcome;
            fprintf fmt "@[<v>%a@,%a@,test %a@]"
                fmt_contracts (contract_env self)
                fmt_operations self.ops
                fmt_operations self.test_ops;
            fprintf fmt "@]"

        let next_op (self : apply_ops) : Env.operation option =
            let op = Lst.hd self.ops in
            if op <> None then op else Lst.hd self.test_ops

        let pop_next_op (self : apply_ops) : Env.operation option =
            match self.ops with
            | op :: ops ->
                self.ops <- ops;
                Some op
            | [] -> None

        let pop_next_test_op (self : apply_ops) : Env.operation option =
            match self.test_ops with
            | op :: test_ops ->
                self.test_ops <- test_ops;
                Some op
            | [] -> None

        let rec apply (self : apply_ops) : (run_test, transfer) Either.t option =
            if self.obsolete then (
                Exc.throw "trying to call `ops_apply` on an obsolete `apply_ops` value"
            );

            match pop_next_op self with
            | None -> (
                assert (self.ops = []);
                (
                    match self.outcome with
                    | Success _ -> ()
                    | Fail (v, op, _) ->
                        [
                            asprintf "while applying operation %a" Env.Op.fmt op ;
                            asprintf "operation was successful, expected a failure %s" (
                                match v with
                                | None -> "of any kind"
                                | Some v -> asprintf "on %a" Theory.fmt v
                            )
                        ] |> Exc.throws
                );

                match pop_next_test_op self with
                | None ->
                    self.obsolete <- true;
                    Some (Either.Lft { test = self.test ; obsolete = false })
                | Some op ->
                    self.outcome <- Success op;
                    self.ops <- [ op ];
                    apply self

            )

            | Some next -> (
                let rec loop
                    (op : Env.operation)
                    : (transfer, Env.operation * (Exc.Protocol.t option)) Either.t
                =
                    let contract_env = contract_env self in
                    let next_op =
                        (fun () -> Env.Op.op contract_env op)
                        |> catch_protocol_exn
                        |> Either.map_rgt (fun p -> op, Some p)
                    in

                    match next_op with
                    | Either.Rgt _ as res -> res

                    | Either.Lft (
                        Theory.MustFail (value, sub_op, sub_uid)
                    ) -> (
                        match self.outcome with
                        | Fail (_, src, _) -> Exc.throws [
                            asprintf "while running test operation %a" Env.Op.fmt src ;
                            "illegal nested `MUST_FAIL` operation"
                        ]
                        | Success _ -> (
                            let env = Env.clone contract_env in
                            self.outcome <- Fail (value, op, env);
                            Env.Op.mk sub_uid sub_op |> loop
                        )
                    )

                    | Either.Lft (
                        Theory.CreateNamed (params, contract)
                    ) -> (
                        (
                            fun () ->
                                (fun () -> Run.Env.Live.create params contract contract_env)
                                |> Exc.chain_err (
                                    fun () ->
                                        asprintf
                                            "while spawning contract %s at address %a"
                                            contract.name Theory.Address.fmt params.address
                                )
                        )
                        |> catch_protocol_exn
                        |> (
                            function
                            | Either.Lft () -> Either.Rgt (op, None)
                            | Either.Rgt e -> Either.Rgt (op, Some e)
                        )
                    )

                    | Either.Lft (
                        Theory.Create (params, contract)
                    )-> (
                        let contract = Contract.of_mic contract in
                        (
                            fun () ->
                                (fun () -> Run.Env.Live.create params contract contract_env)
                                |> Exc.chain_err (
                                    fun () ->
                                        asprintf
                                            "while spawning contract %s at address %a"
                                            contract.name Theory.Address.fmt params.address
                                )
                        )
                        |> catch_protocol_exn
                        |> (
                            function
                            | Either.Lft () -> Either.Rgt (op, None)
                            | Either.Rgt e -> Either.Rgt (op, Some e)
                        )
                    )

                    | Either.Lft (
                        Theory.InitNamed _
                    ) -> Exc.unimplemented ()

                    | Either.Lft (
                        Theory.SetDelegate (address, delegate)
                    ) -> (
                        match Run.Env.Live.get address contract_env with
                        | None ->
                            asprintf "address %a has no contract attached"
                                Theory.Address.fmt address
                            |> Exc.throw
                        | Some live ->
                            if not live.params.delegatable then (
                                Exc.Throw.tezos
                                    "cannot `SET_DELEGATE` on a non-delegatable contract"
                            ) else (
                                (fun () -> Env.Live.set_delegate delegate live)
                                |> catch_protocol_exn
                                |> (
                                    function
                                    | Either.Lft () -> Either.Rgt (op, None)
                                    | Either.Rgt e -> Either.Rgt (op, Some e)
                                )
                            )
                    )

                    | Either.Lft (
                        Theory.Transfer (address, contract, tez, param)
                    ) -> (
                        match Run.Env.Live.get address contract_env with
                        | None ->
                            asprintf "address %a has no contract attached"
                                Theory.Address.fmt address
                            |> Exc.throw
                        | Some live ->
                            Run.Env.Live.transfer tez live;
                            let src = Run.Src.of_address address in
                            let param_dtyp =
                                contract.param
                                |> Dtyp.mk_named (Some (Annot.Field.of_string "param"))
                            in
                            let storage_dtyp =
                                contract.storage
                                |> Dtyp.mk_named (Some (Annot.Field.of_string "storage"))
                            in
                            let dtyp = Dtyp.Pair (param_dtyp, storage_dtyp) |> Dtyp.mk in
                            let value = Theory.Of.pair param live.storage in
                            let transfer =
                                Run.init src ~balance:live.balance ~amount:tez contract_env [
                                    (value, dtyp, Some (Annot.Var.of_string "input"))
                                ] [ live.contract.entry ]
                            in
                            Either.Lft {
                                transfer ;
                                outcome = self.outcome ;
                                ops = self.ops ;
                                test_ops = self.test_ops ;
                                test = self.test ;
                                obsolete = false ;
                            }
                    )
                in

                let res = loop next in

                log_0 "resolving outcome@.%a@.@."
                    fmt self;

                match res, self.outcome with

                (* No error, need to apply a transfer. *)
                | Either.Lft transfer, _ ->
                    self.obsolete <- true;
                    Some (Either.rgt transfer)

                (*  No error, error expected.
                    We do not fail: pending operations in `self.ops` can still fail.
                *)
                | Either.Rgt (_, None), Fail _
                (* No error, success expected. *)
                | Either.Rgt (_, None), Success _ ->
                    None

                (* Error, success expected. *)
                | Either.Rgt (op, Some e), Success src ->
                    let uid = Env.Op.uid op in
                    let src_uid = Env.Op.uid src in
                    log_1 "failure in test operation %a" Env.Op.fmt src;
                    if src_uid <> uid then (
                        log_1 "on operation %a" Env.Op.fmt op
                    );
                    log_1 "failure on operation %a@." Env.Op.fmt op;
                    Exc.Exc (Exc.Protocol e) |> raise

                (* Protocol error, any failure expected. *)
                | Either.Rgt (op, Some err), Fail (None, src, _) ->
                    let uid = Env.Op.uid op in
                    let src_uid = Env.Op.uid src in
                    log_1 "confirmed failure in test operation %a@." Env.Op.fmt src;
                    if src_uid <> uid then (
                        log_1 "on operation %a@." Env.Op.fmt op
                    );
                    log_1 "%a@." Exc.fmt (Exc.Exc (Exc.Protocol err));
                    (*  Cancel all operations in `ops`, update `self.outcome`.
                    *)
                    self.ops <- [];
                    self.outcome <- Success src;
                    None
                
                (* Non-failure exception. *)
                | Either.Rgt (op, Some e), Fail (_, src, _) ->
                    let uid = Env.Op.uid op in
                    let src_uid = Env.Op.uid src in
                    let chain =
                        [ asprintf "while running test operation %a" Env.Op.fmt src ]
                    in
                    let chain =
                        if src_uid <> uid then (
                            (asprintf "while running operation %a" Env.Op.fmt op) :: chain
                        ) else (
                            chain
                        )
                    in
                    Exc.Exc (Exc.Protocol e) |> raise
                    |> Exc.chain_errs (fun () -> chain)
            )
    end

    module Transfer = struct
        let step (self : transfer) : (Run.event, apply_ops) Either.t option =
            Run.step self.transfer |> Opt.map (
                function
                | Run.Done -> (
                    self.obsolete <- true;
                    Either.Rgt {
                        test = self.test ;
                        ops = self.ops ;
                        test_ops = self.test_ops ;
                        outcome = self.outcome ;
                        obsolete = false
                    }
                )
                | event -> Either.Lft event
            )

        let run (self : transfer) : (Run.event, apply_ops) Either.t =
            let rec loop () =
                match step self with
                | None -> loop ()
                | Some res -> res |> Either.map_rgt (
                    fun next_state ->
                        self.obsolete <- true;
                        next_state
                )
            in
            loop ()

        let interpreter (self : transfer) : Run.t = self.transfer
        let stack (self : transfer) : Run.Stack.t = Run.stack self.transfer

        let operations (self : transfer) : Env.operation list = self.ops

        let contract_env (self : transfer) : Env.t = RunTest.contract_env self.test

        let fmt (fmt : formatter) (self : transfer) : unit =
            fprintf fmt "@[<v>state   : transfer@,outcome : %a@,"
                fmt_outcome self.outcome;
            fprintf fmt "@[<v>%a@,%a@,test %a@]"
                fmt_contracts (contract_env self)
                fmt_operations self.ops
                fmt_operations self.test_ops;
            fprintf fmt "@]"
    end
end
