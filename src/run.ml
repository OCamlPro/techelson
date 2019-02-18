(** Module in charge of running things depending on clap. *)

open Base
open Base.Common

let load_contracts (conf : Conf.t) : Contract.t list =
    let inner () =
        conf.contracts |> List.map (
            fun { Conf.file ; Conf.init } ->
                let _ = init in
                log_3 "Opening contract file `%s`@." file;
                Exc.chain_err (
                    fun () -> sprintf "while loading contract file `%s`" file
                ) (
                    fun () ->
                        let chan = open_file file in
                        let src = Source.File file in
                        Test.Load.contract (Contract.name_of_file file) src chan
                )
        )
    in
    Exc.chain_err (
        fun () -> "while loading contracts from `--contract` arguments"
    ) inner

module Cxt = Test.Default
module Tests = Test.Testcases

let step (conf : Conf.t) : unit =
    log_0 "press `return` to continue@.";
    if not conf.skip then (
        input_line stdin |> ignore
    ) else (
        log_0 "@."
    )

let cond_step (conf : Conf.t) : unit =
    if conf.step then step conf

(* Returns true if the event is `Done`. *)
let handle_event (conf : Conf.t) (stack : Cxt.Run.Stack.t) (event : Cxt.event) : bool =
    match event with
    | Cxt.Run.Done -> true
    | Cxt.Run.Step opt -> (
        unwrap_or "no information" opt
        |> log_0 "stopping [%s] ";
        step conf;
        false
    )
    | Cxt.Run.PrintStack -> (
        log_0 "stack:@.%a@." Cxt.Run.Stack.fmt stack;
        false
    )
    | Cxt.Run.Print fmt -> (
        log_1 "@.> %a@." (fun f () -> fmt f) ();
        false
    )
    | Cxt.Run.Warn fmt -> (
        log_0 "@.WARNING: %a@." (fun f () -> fmt f) ();
        false
    )
    | Cxt.Run.Failure (value, dtyp) -> (
        asprintf "%a : %a" Cxt.Theory.fmt value Dtyp.fmt dtyp
        |> Exc.Throw.failure
    )

(* Returns true if the last event is `Done`. *)
let rec handle_events (conf : Conf.t) (stack : Cxt.Run.Stack.t) (events : Cxt.event list) : bool =
    match events with
    | [] -> false
    | event :: events ->
        let is_done = handle_event conf stack event in
        if is_done then (
            if events <> [] then Exc.throws [
                "inconsistent internal state" ;
                "witnessed event `Done` but there are still event to process"
            ] else true
        ) else handle_events conf stack events

let run_tests (conf : Conf.t) (cxt : Test.Testcases.t) : unit =
    Tests.get_tests cxt |> Lst.fold (
        fun (err_count : int) (test : Testcase.t) ->
            log_1 "Running test `%s`@." test.name;
            let cxt = Cxt.init (Tests.get_contracts cxt) test in

            let rec test_loop (cxt : Cxt.run_test) : unit =
                log_1 "Running test script...@.";
                log_3 "%a@." Cxt.Test.fmt cxt;
                cond_step conf;
                let events, next_state = Cxt.Test.run cxt in
                let is_done = handle_events conf (Cxt.Test.stack cxt) events in
                match next_state with
                | Some _ when is_done -> Exc.throws [
                    "inconsistent internal state" ;
                    "test code is done but there are still operations to process" ;
                ]
                | Some ops -> ops_loop ops
                | None when is_done -> log_1 "Done running test `%s`@." test.name
                | None -> test_loop cxt

            and ops_loop (cxt : Cxt.apply_ops) : unit =
                log_1 "@.Applying Operations...@.";
                log_3 "%a@." Cxt.Ops.fmt cxt;
                (
                    match Cxt.Ops.next_op cxt with
                    | Some op -> log_2 "> %a@." Cxt.Env.Op.fmt op
                    | None -> log_2 "> <none>@."
                );
                cond_step conf;
                let rec loop () =
                    match Cxt.Ops.apply cxt with
                    | Some (Either.Lft test) -> test_loop test
                    | Some (Either.Rgt transfer) -> transfer_loop transfer
                    | None -> loop ()
                in
                loop ()

            and transfer_loop (cxt : Cxt.transfer) : unit =
                if conf.step then (
                    let rec loop () : unit =
                        log_0 "@.Contract Transfer Step...@.";
                        log_3 "%a@." Cxt.Transfer.fmt cxt;
                        Cxt.Transfer.interpreter cxt
                        |> Cxt.Run.stack
                        |> log_3 "@.@[<v>%a@]@.@." Cxt.Run.Stack.fmt;
                        Cxt.Transfer.interpreter cxt
                        |> Cxt.Run.next_ins
                        |> (
                            fun (pre_ops, ins) -> (
                                pre_ops |> List.iter (
                                    log_0 "@[<v 4>next > %s@]@."
                                );
                                match ins with
                                | None when pre_ops = [] -> log_0 "@[<v 4>next > done"
                                | None -> ()
                                | Some mic -> log_0 "@[<v 4>next > %a@]@." Mic.fmt mic
                        )
                        );
                        
                        match Cxt.Transfer.step cxt with
                        | None ->
                            step conf;
                            loop ()
                        | Some (Either.Lft event) ->
                            let is_done = handle_event conf (Cxt.Transfer.stack cxt) event in
                            if is_done then (
                                Exc.throws [
                                    "internal inconsistent state" ;
                                    "transfer is done but retrieved no operation list \
                                    (not even empty)" ;
                                ]
                            )
                        | Some (Either.Rgt ops) -> ops_loop ops
                    in
                    loop ()
                ) else (
                    log_1 "@.Contract Transfer...@.";
                    log_3 "%a@." Cxt.Transfer.fmt cxt;
                    let rec loop () =
                        match Cxt.Transfer.run cxt with
                        | Either.Rgt ops -> ops_loop ops
                        | Either.Lft event ->
                            let is_done = handle_event conf (Cxt.Transfer.stack cxt) event in
                            if is_done then (
                                Exc.throws [
                                    "internal inconsistent state" ;
                                    "transfer is done but retrieved no operation list \
                                    (not even empty)" ;
                                ]
                            );
                            loop ()
                    in
                    loop ()
                )
            in

            try (
                test_loop cxt;
                err_count
            ) with
            | e ->
                log_0 "Test `%s` failed:@.    @[%a@]@." test.name Exc.fmt e;
                err_count + 1
    ) 0
    |> (
        function
        | 0 -> ()
        | n ->
            let test_count = Tests.get_tests cxt |> List.length in
            sprintf "%i of the %i testcase%s failed" n test_count (Fmt.plurify test_count)
            |> Exc.throw
    )

let run_testgen (conf : Conf.t) (gen_conf : Conf.testgen_mode) (cxt : Test.Testcases.t) : unit =
    let testcases : (Contract.t * (int * Testcase.t) list) list =
        Tests.get_contracts cxt |> List.fold_left (
            fun acc contract ->
                let count : int ref = ref 1 in

                let get_index () : int option =
                    if !count <= gen_conf.count then (
                        let res = Some !count in
                        count := !count + 1;
                        res
                    ) else None
                in
                
                let rec loop (acc : (int * Testcase.t) list) : (int * Testcase.t) list =
                    match get_index () with
                    | Some index -> (
                        let name = sprintf "%sTest%i" contract.Contract.name index in
                        let testcase = Testgen.Test.generate contract name in
                        (index, testcase) :: acc |> loop
                    )
                    | None ->
                        log_1 "done generating test%s for contract %s@."
                            (Fmt.plurify gen_conf.count) contract.name;
                        List.rev acc
                in

                let testcases =
                    (fun () -> loop [])
                    |> Exc.chain_err (
                        fun () -> sprintf "while generating tests for contract %s" contract.name
                    )
                in

                (contract, testcases) :: acc
        ) []
        |> List.rev
    in

    match conf.args with
    | [] ->
        let testcases =
            testcases |> List.fold_left (
                fun acc (_, lst) ->
                    lst |> List.fold_left (
                        fun acc (_, testcase) -> testcase :: acc
                    ) acc
            ) []
            |> List.rev
        in
        Tests.add_tests testcases cxt
        |> run_tests conf
    | target :: tail -> (
        if tail <> [] then (
            log_0 "WARNING: ignoring tail arguments `%a` in test generation@."
                (Fmt.fmt_list (fun fmt () -> fprintf fmt "@ ") Fmt.fmt_str) tail
        );
        if Sys.is_directory target |> not then (
            sprintf "argument for test generation's `dump` option `%s` is not a directory" target
            |> Exc.throw
        );
        log_1 "dumping testcases to `%s`@." target;
        let target_of (contract : Contract.t) (test_index : int) : string * formatter =
            let target = sprintf "%s/%sTest%i.techel" target contract.name test_index in
            target,
            target |> open_file_write |> formatter_of_out_channel
        in

        testcases |> List.iter (
            fun (contract, list) -> list |> List.iter (
                fun (index, testcase) ->
                    let target, fmt = target_of contract index in
                    log_2 "dumping testcase `%s` for contract `%s` to `%s`@."
                        testcase.Testcase.name contract.Contract.name target;
                    fprintf fmt "%a@." Mic.fmt testcase.code
            )
        )
    )

let run () : unit =
    let conf = conf () in

    match conf.Conf.mode with
    | Conf.Inactive ->
        log_2 "loading context...@.";
        let context, errs =
            Test.Load.context
                ~contract_files:conf.contracts
                ~test_files:conf.args
        in
        log_2 "done loading context@.%a@.@." (Tests.fmt ~full:false) context;

        if errs > 0 then (
            sprintf "encountered %i error%s while loading context" errs (Fmt.plurify errs)
            |> Exc.throw
        );
        run_tests conf context
    | Conf.Testgen testgen_conf ->
    log_2 "loading context...@.";
        let context, errs =
            Test.Load.context
                ~contract_files:conf.contracts
                ~test_files:[]
        in
        log_2 "done loading context@.%a@.@." (Tests.fmt ~full:false) context;

        if errs > 0 then (
            sprintf "encountered %i error%s while loading context" errs (Fmt.plurify errs)
            |> Exc.throw
        );
        (fun () -> run_testgen conf testgen_conf context)
        |> Exc.chain_err (
            fun () -> "while running test generation"
        )
