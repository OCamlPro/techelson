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

module Cxt = Test.Naive
module Tests = Test.Testcases

let run () : unit =
    let conf = conf () in
    log_1 "loading context...@.";
    let context, errs =
        Test.Load.context
            ~contract_files:conf.contracts
            ~test_files:conf.args
            ~else_chan:(Some (stdin, Source.Stdin))
    in
    log_1 "done loading context@.%a@.@." (Tests.fmt ~full:false) context;

    if errs > 0 then (
        sprintf "encountered %i error%s while loading context" errs (Fmt.plurify errs)
        |> Exc.throw
    );

    Tests.get_tests context |> Lst.fold (
        fun (err_count : int) (test : Testcase.t) ->
            let cxt = Cxt.init (Tests.get_contracts context) test in

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
            let test_count = Tests.get_tests context |> List.length in
            sprintf "%i of the %i testcase%s failed" n test_count (Fmt.plurify test_count)
            |> Exc.throw
    );

(* 
    let val_gen stuff = Testgen.Values.from stuff |> Mic.mk_seq in

    log_0 "Arith.zero: %i@." Proba.Arith.zero ;
    log_0 "From int: %a@." Mic.fmt (Dtyp.Int |> Dtyp.mk_leaf |> val_gen) ;
    log_0 "From int: %a@." Mic.fmt (Dtyp.Int |> Dtyp.mk_leaf |> val_gen) ;
    log_0 "From int: %a@." Mic.fmt (Dtyp.Int |> Dtyp.mk_leaf |> val_gen) ;
    log_0 "From int: %a@." Mic.fmt (Dtyp.Int |> Dtyp.mk_leaf |> val_gen) ;
    log_0 "From int: %a@." Mic.fmt (Dtyp.Int |> Dtyp.mk_leaf |> val_gen) ;
    log_0 "From int: %a@." Mic.fmt (Dtyp.Int |> Dtyp.mk_leaf |> val_gen) ;
    log_0 "From int: %a@." Mic.fmt (Dtyp.Int |> Dtyp.mk_leaf |> val_gen) ;
    log_0 "From int: %a@." Mic.fmt (Dtyp.Int |> Dtyp.mk_leaf |> val_gen) ;
    log_0 "From int: %a@.@." Mic.fmt (Dtyp.Int |> Dtyp.mk_leaf |> val_gen) ;

    let int = Dtyp.Int |> Dtyp.mk_leaf in
    let nat = Dtyp.Nat |> Dtyp.mk_leaf in
    let pair = Dtyp.Pair (int |> Dtyp.mk_named None, nat |> Dtyp.mk_named None) |> Dtyp.mk in
    let either = Dtyp.Or (pair |> Dtyp.mk_named None, nat |> Dtyp.mk_named None) |> Dtyp.mk in

    log_0 "From %a: %a@." Dtyp.fmt either Mic.fmt (val_gen either);
    log_0 "From %a: %a@." Dtyp.fmt either Mic.fmt (val_gen either);
    log_0 "From %a: %a@." Dtyp.fmt either Mic.fmt (val_gen either);
    log_0 "From %a: %a@.@." Dtyp.fmt either Mic.fmt (val_gen either);

    let option = Dtyp.Option either |> Dtyp.mk in

    log_0 "From %a: %a@." Dtyp.fmt option Mic.fmt (val_gen option);
    log_0 "From %a: %a@." Dtyp.fmt option Mic.fmt (val_gen option);
    log_0 "From %a: %a@." Dtyp.fmt option Mic.fmt (val_gen option);
    log_0 "From %a: %a@.@." Dtyp.fmt option Mic.fmt (val_gen option);

    let list = Dtyp.List option |> Dtyp.mk in

    log_0 "From %a: %a@." Dtyp.fmt list Mic.fmt (val_gen list);
    log_0 "From %a: %a@." Dtyp.fmt list Mic.fmt (val_gen list);
    log_0 "From %a: %a@." Dtyp.fmt list Mic.fmt (val_gen list);
    log_0 "From %a: %a@.@." Dtyp.fmt list Mic.fmt (val_gen list);

    let set = Dtyp.Set option |> Dtyp.mk in

    log_0 "From %a: %a@." Dtyp.fmt set Mic.fmt (val_gen set);
    log_0 "From %a: %a@." Dtyp.fmt set Mic.fmt (val_gen set);
    log_0 "From %a: %a@." Dtyp.fmt set Mic.fmt (val_gen set);
    log_0 "From %a: %a@.@." Dtyp.fmt set Mic.fmt (val_gen set);

    let map = Dtyp.Map (int, option) |> Dtyp.mk in

    log_0 "From %a: %a@." Dtyp.fmt map Mic.fmt (val_gen map);
    log_0 "From %a: %a@." Dtyp.fmt map Mic.fmt (val_gen map);
    log_0 "From %a: %a@." Dtyp.fmt map Mic.fmt (val_gen map);
    log_0 "From %a: %a@.@." Dtyp.fmt map Mic.fmt (val_gen map);

    log_0 "Generating testcases...@.";

    Tests.get_contracts context |> List.iter (
        fun contract ->
            let testcase = Testgen.Test.generate contract "test_testgen" in
            log_0 "@.testcase: @[%a@]@." (Testcase.fmt ~full:true) testcase
    ); *)

    ()
