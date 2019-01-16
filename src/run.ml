(* Module in charge of running things depending on clap. *)

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

module NewCxt = Test.Top.NewCxt

let run () : unit =
    let conf = conf () in
    log_1 "loading context...@.";
    let context, errs =
        Test.Load.context
            ~contract_files:conf.contracts
            ~test_files:conf.args
            ~else_chan:(Some (stdin, Source.Stdin))
    in
    log_1 "done loading context@.%a@.@." (Test.Cxt.fmt ~full:false) context;

    if errs > 0 then (
        sprintf "encountered %i error%s while loading context" errs (Fmt.plurify errs)
        |> Exc.throw
    );

    Test.Cxt.get_tests context |> List.iter (
        fun (test : Testcase.t) ->
            let cxt = NewCxt.init (Test.Cxt.get_contracts context) test in

            let rec test_loop (cxt : NewCxt.run_test) : unit =
                log_1 "@.@.|===| Test Step...@.@.%a@." NewCxt.Test.fmt cxt;
                if conf.step then (
                    input_line stdin |> ignore;
                );
                match NewCxt.Test.run cxt with
                | Some ops -> ops_loop ops
                | None -> log_1 "@.@.done running test `%s`@." test.name

            and ops_loop (cxt : NewCxt.apply_ops) : unit =
                log_1 "@.@.|===| Applying Operations...@.@.%a@." NewCxt.Ops.fmt cxt;
                if conf.step then (
                    input_line stdin |> ignore;
                );
                match NewCxt.Ops.apply cxt with
                | Either.Lft test ->
                    log_1 "No more operations to apply";
                    test_loop test
                | Either.Rgt transfer -> transfer_loop transfer

            and transfer_loop (cxt : NewCxt.transfer) : unit =
                if conf.step then (
                    let rec loop () : unit =
                        log_1 "@.@.|===| Contract Transfer Step...@.@.%a@.@."
                            NewCxt.Transfer.fmt cxt;
                        NewCxt.Transfer.interpreter cxt
                        |> NewCxt.Run.stack
                        |> printf "@.@[<v>%a@]@.@." NewCxt.Run.Stack.fmt;
                        NewCxt.Transfer.interpreter cxt
                        |> NewCxt.Run.next_ins
                        |> if_let_some (
                            log_1 "@[<v 4>> %a@]@." Mic.fmt
                        );
                        
                        match NewCxt.Transfer.transfer_step cxt with
                        | None ->
                            input_line stdin |> ignore;
                            loop ()
                        | Some ops -> ops_loop ops
                    in
                    loop ()
                ) else (
                    log_1 "@.@.|===| Contract Transfer...@.@.%a@." NewCxt.Transfer.fmt cxt;
                    NewCxt.Transfer.transfer_run cxt
                    |> ops_loop
                )
            in

            test_loop cxt
    )
