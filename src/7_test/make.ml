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
    | Fail of (Theory.value * Dtyp.t) option * Env.operation * Env.t
    (** Operation must fail.

        The optional value is the expected failure value. If `None`, the operation can fail with
        any value. The operation is the source operation created by the testcase.
    *)

    (** Result of an operation application. *)
    type apply_op_res =
    | Okay
    (** The transfer went fine, no error. *)
    | Failure of Theory.value * Dtyp.t
    (** Explicit failure on a value of some type. *)
    | Protocol of Exc.Protocol.t
    (** Tezos protocol error. *)

    type run_test = {
        test : RunTest.t ;
        (** Test interpreter. *)
        mutable obsolete : bool ;
        (** This flag is set to true when changing states. *)
    }

    type apply_ops = {
        test : RunTest.t ;
        (** Saved state of the testcase execution. *)
        mutable prev : (Env.operation * apply_op_res) option ;
        (** Result of the previous transfer, if any. *)
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
        op : Env.operation ;
        (** The operation the transfer corresponds to. *)
        contract : Env.live ;
        (** Live contract running the transfer. *)
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
            | Some (v, t) -> fprintf fmt "%a : %a" Theory.fmt v Dtyp.fmt t
        )

    let fmt_contracts (fmt : formatter) (env : Env.t) : unit =
        fprintf fmt "live contracts: @[";
        if Run.Env.Live.count env = 0 then (
            fprintf fmt "none"
        ) else (
            fprintf fmt "%a" Run.Env.fmt env
        );
        fprintf fmt "@]"
    
    let fmt_operations (fmt : formatter) (ops : Env.operation list) : unit =
        fprintf fmt "operations: @[";
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
                            prev = None ;
                            ops = [op] ;
                            outcome = Success op ;
                            test_ops ;
                            obsolete = false ;
                        }
                    in
                    List.rev acc |> List.rev, next_state
                | RunTest.Normal event -> (
                    match event with
                    | Run.Done | Run.Step _ | Failure _ | Run.PrintStack ->
                        event :: acc |> List.rev, None
                    | Run.Print _ | Run.Warn _ -> event :: acc |> loop
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
            RunTest.contract_env self.test

        let operations (self : apply_ops) : Env.operation list = self.ops

        let fmt (fmt : formatter) (self : apply_ops) : unit =
            fprintf fmt "@[<v>state: apply_ops@,outcome: %a@,"
                fmt_outcome self.outcome;
            fprintf fmt "@[<v>%a@,%a@,test %a@]"
                fmt_contracts (contract_env self)
                fmt_operations self.ops
                fmt_operations self.test_ops;
            fprintf fmt "@]"

        let fmt_contracts (fmt : formatter) (self : apply_ops) : unit =
            fmt_contracts fmt (contract_env self)

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

        (* Returns the next operation.

            The only side effect this function can have is if
            - `self.ops` is empty, and
            - `self.test_ops` is `test_op :: tail`.

            Then `self.ops <- [test_op]` and `self.test_ops <- tail`. This is semantics preserving:
            the next test operation is staged, but no operation is lost (popped). This is why these
            details are not discussed in the `mli`'s doc for this function.
        *)
        let next_op (self : apply_ops) : Env.operation option =
            let op = Lst.hd self.ops in
            if op <> None then op else (
                match pop_next_test_op self with
                | None -> None
                | Some op ->
                    self.ops <- [op];
                    Some op
            )

        let stage_next_test_op (self : apply_ops) : (run_test, transfer) Either.t option =
            self.ops <- [];
            match pop_next_test_op self with
            | Some op -> (
                self.outcome <- Success op;
                self.ops <- [op];
                None
            )
            | None -> (
                self.obsolete <- true;
                Some (Either.Lft { test = self.test ; obsolete = false })
            )

        type handle_confirmed_failure =
            Env.operation ->
            Env.operation ->
            ((Theory.value * Dtyp.t), Exc.Protocol.t) Either.t ->
            unit

        (** Checks the results of the previous operation, if any.

            This function is responsible for catching failures, expected or not.
        *)
        let check_prev
            (handle_confirmed_failure : handle_confirmed_failure)
            (self : apply_ops)
            : (run_test, transfer) Either.t option
        =

            match self.prev, self.outcome with
            | Some (sub_op, Failure (value, dtyp)), Success op -> Exc.throws [
                asprintf "operation %a was expected to succeed" Env.Op.fmt op ;
                asprintf "but failed on operation %a" Env.Op.fmt sub_op ;
                asprintf "operation failed on %a : %a" Theory.fmt value Dtyp.fmt dtyp ;
            ]
            | Some (sub_op, Protocol e), Success op ->
                (fun () -> raise (Exc.Exc (Exc.Protocol e)))
                |> Exc.chain_errs (
                    fun () -> [
                        asprintf "while applying operation %a" Env.Op.fmt sub_op ;
                        asprintf "while applying test operation %a" Env.Op.fmt op ;
                    ]
                )

            | Some (sub_op, Protocol e), Fail (Some (value, dtyp), op, _) -> (
                (fun () -> raise (Exc.Exc (Exc.Protocol e)))
                |> Exc.chain_errs (
                    fun () -> [
                        asprintf "while applying operation %a" Env.Op.fmt sub_op ;
                        asprintf "operation %a was expected to fail with %a : %a"
                            Env.Op.fmt op Theory.fmt value Dtyp.fmt dtyp ;
                    ]
                )
            )
            | Some (sub_op, Failure (value, dtyp)), Fail (expected, op, env) -> (
                (
                    match expected with
                    | Some (e_v, e_t) -> (
                        (
                            try
                                DtypCheck.unify (DtypCheck.empty ()) e_t dtyp |> ignore;
                                if value <> e_v then Exc.throw "bailing"
                            with
                            | _ -> Exc.throws [
                                asprintf "operation %a was expected to fail" Env.Op.fmt op ;
                                asprintf "with value @[<h>%a : %a@]"
                                    Theory.fmt e_v
                                    Dtyp.fmt e_t ;
                                asprintf "but failed with value @[<h>%a : %a@]"
                                    Theory.fmt value
                                    Dtyp.fmt dtyp ;
                                asprintf "while running operation %a"
                                    Env.Op.fmt sub_op ;
                            ]
                        )
                    )
                    | None -> ()
                );

                Either.Lft (value, dtyp)
                |> handle_confirmed_failure op sub_op;

                RunTest.set_contract_env env self.test;

                stage_next_test_op self
            )
            | Some (sub_op, Protocol e), Fail (None, op, env) -> (
                Either.Rgt e
                |> handle_confirmed_failure op sub_op;

                RunTest.set_contract_env env self.test;
                stage_next_test_op self
            )

            | Some (_, Okay), _
            | None, _ -> None

        let rec apply
            (handle_confirmed_failure : handle_confirmed_failure)
            (self : apply_ops)
            : (run_test, transfer) Either.t option
        =
            if self.obsolete then (
                Exc.throw "trying to call `ops_apply` on an obsolete `apply_ops` value"
            );

            (* Check previous result if any. *)
            match check_prev handle_confirmed_failure self with
            | Some res -> Some res
            | None -> (

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
                                    | Some (v, t) -> asprintf "on %a : %a" Theory.fmt v Dtyp.fmt t
                                )
                            ] |> Exc.throws
                    );

                    stage_next_test_op self
                )

                | Some next -> (
                    (* Applies all the operations it can until either

                        - there are no more operations
                        - an operation fails
                        - a transfer operation shows up
                    *)
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
                            Theory.Transfer {
                                source ; sender ; target ; contract ; amount ; param
                            }
                        ) -> (
                            let tgt =
                                match Run.Env.Live.get target contract_env with
                                | None ->
                                    asprintf "address %a has no contract attached"
                                        Theory.Address.fmt target
                                    |> Exc.throw
                                | Some live -> live
                            in
                            let src =
                                match Run.Env.Live.get sender contract_env with
                                | Some src ->
                                    Run.Env.Live.collect ~tgt:tgt amount src;
                                    Some src
                                | None -> None
                            in

                            Run.Env.Live.transfer ~src amount tgt;

                            let src = Run.Src.of_address ~source ~sender ~address:target in
                            let param_dtyp =
                                contract.param
                                |> Dtyp.mk_named (Some (Annot.Field.of_string "param"))
                            in
                            let storage_dtyp =
                                contract.storage
                                |> Dtyp.mk_named (Some (Annot.Field.of_string "storage"))
                            in
                            let dtyp = Dtyp.Pair (param_dtyp, storage_dtyp) |> Dtyp.mk in
                            let value = Theory.Of.pair param tgt.storage in
                            let transfer =
                                Run.init src ~balance:tgt.balance ~amount contract_env [
                                    (value, dtyp, Some (Annot.Var.of_string "input"))
                                ] [ tgt.contract.entry ]
                            in
                            Either.Lft {
                                op ;
                                contract = tgt ;
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

                    match res with

                    (* No error, need to apply a transfer. *)
                    | Either.Lft transfer ->
                        self.obsolete <- true;
                        Some (Either.rgt transfer)

                    (* No error. *)
                    | Either.Rgt (op, opt) ->
                        self.prev <- Some (
                            match opt with
                            | Some e -> op, Protocol e
                            | None -> op, Okay
                        );
                        apply handle_confirmed_failure self
                )
            )
    end

    module Transfer = struct
        let step (self : transfer) : (Run.event, apply_ops) Either.t option =
            let res =
                (fun () -> Run.step self.transfer)
                |> catch_protocol_exn
                |> Either.map_lft (
                    Opt.map (
                        function
                        | Run.Done -> (
                            let ops, nu_storage, storage_dtyp = Run.terminate self.transfer in
                            let env = RunTest.contract_env self.test in
                            Env.Live.update_storage env nu_storage storage_dtyp self.contract;
                            self.obsolete <- true;
                            Either.Rgt {
                                test = self.test ;
                                prev = Some (self.op, Okay) ;
                                ops = ops @ self.ops ;
                                test_ops = self.test_ops ;
                                outcome = self.outcome ;
                                obsolete = false
                            }
                        )
                        | Run.Failure (v, t) -> (
                            self.obsolete <- true;
                            Either.Rgt {
                                test = self.test ;
                                prev = Some (self.op, Failure (v, t)) ;
                                ops = self.ops ;
                                test_ops = self.test_ops ;
                                outcome = self.outcome ;
                                obsolete = false ;
                            }
                        )
                        | event -> Either.Lft event
                    )
                )
            in
            match res with
            | Either.Lft res -> res
            | Either.Rgt e -> (
                self.obsolete <- true;
                Some (
                    Either.Rgt {
                        test = self.test ;
                        prev = Some (self.op, Protocol e) ;
                        ops = self.ops ;
                        test_ops = self.test_ops ;
                        outcome = self.outcome ;
                        obsolete = false ;
                    }
                )
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

        let fmt_op (fmt : formatter) (self : transfer) : unit =
            fprintf fmt "%a" Env.Op.fmt self.op                        

        let fmt_contracts (fmt : formatter) (self : transfer) : unit =
            fmt_contracts fmt (contract_env self)
    end
end
