(** Module in charge of running things depending on clap. *)

open Base
open Base.Common

let load_contracts (conf : Conf.t) : Contract.t list =
    let inner () =
        conf.contracts |> List.map (
            fun { Conf.file ; Conf.init } ->
                let _ = init in
                log_1 "Opening contract file `%s`@." file;
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

let run_tests (conf : Conf.t) (cxt : Test.Testcases.t) : unit =
    Tests.get_tests cxt |> Lst.fold (
        fun (err_count : int) (test : Testcase.t) ->
            if conf.step then (
                log_0 "@.@.|=====| Running test `%s` |=====|@.@." test.name;
                input_line stdin |> ignore;
            );
            let cxt = Cxt.init (Tests.get_contracts cxt) test in

            let rec test_loop (cxt : Cxt.run_test) : unit =
                log_1 "@.@.|===| Test Step...@.@.%a@." Cxt.Test.fmt cxt;
                if conf.step then (
                    input_line stdin |> ignore;
                );
                match Cxt.Test.run cxt with
                | Some ops -> ops_loop ops
                | None -> log_1 "@.@.done running test `%s`@." test.name

            and ops_loop (cxt : Cxt.apply_ops) : unit =
                log_1 "@.@.|===| Applying Operation...@.@.%a@." Cxt.Ops.fmt cxt;
                (
                    match Cxt.Ops.next_op cxt with
                    | Some op -> log_1 "> %a@." Cxt.Env.Op.fmt op
                    | None -> log_1 "> <none>@."
                );
                if conf.step then (
                    input_line stdin |> ignore;
                );
                match Cxt.Ops.apply cxt with
                | Some (Either.Lft test) ->
                    log_1 "No more operations to apply";
                    test_loop test
                | Some (Either.Rgt transfer) -> transfer_loop transfer
                | None -> ops_loop cxt

            and transfer_loop (cxt : Cxt.transfer) : unit =
                if conf.step then (
                    let rec loop () : unit =
                        log_1 "@.@.|===| Contract Transfer Step...@.@.%a@.@."
                            Cxt.Transfer.fmt cxt;
                        Cxt.Transfer.interpreter cxt
                        |> Cxt.Run.stack
                        |> printf "@.@[<v>%a@]@.@." Cxt.Run.Stack.fmt;
                        Cxt.Transfer.interpreter cxt
                        |> Cxt.Run.next_ins
                        |> (
                            fun (pre_ops, ins) -> (
                                pre_ops |> List.iter (
                                    log_1 "@[<v 4>next > %s@]@."
                                );
                                match ins with
                                | None when pre_ops = [] -> log_1 "@[<v 4>next > done"
                                | None -> ()
                                | Some mic -> log_1 "@[<v 4>next > %a@]@." Mic.fmt mic
                        )
                        );
                        
                        match Cxt.Transfer.transfer_step cxt with
                        | None ->
                            input_line stdin |> ignore;
                            loop ()
                        | Some ops -> ops_loop ops
                    in
                    loop ()
                ) else (
                    log_1 "@.@.|===| Contract Transfer...@.@.%a@." Cxt.Transfer.fmt cxt;
                    Cxt.Transfer.transfer_run cxt
                    |> ops_loop
                )
            in

            try (
                test_loop cxt;
                err_count
            ) with
            | e ->
                log_0 "@.@.Test `%s` failed:@.    @[%a@]@." test.name Exc.fmt e;
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
                        log_1 "done generating tests for contract %s@." contract.name;
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

    match gen_conf.dump with
    | None ->
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
    | Some target -> (
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
    log_1 "loading context...@.";
    let context, errs =
        Test.Load.context
            ~contract_files:conf.contracts
            ~test_files:conf.args
    in
    log_1 "done loading context@.%a@.@." (Tests.fmt ~full:false) context;

    if errs > 0 then (
        sprintf "encountered %i error%s while loading context" errs (Fmt.plurify errs)
        |> Exc.throw
    );

    match conf.Conf.mode with
    | Conf.Inactive -> run_tests conf context
    | Conf.Testgen testgen_conf ->
        (fun () -> run_testgen conf testgen_conf context)
        |> Exc.chain_err (
            fun () -> "while running test generation"
        )
