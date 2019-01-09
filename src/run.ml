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

open Test.Top

module Interp = Cxt.Run

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

    let cxt = Cxt.of_cxt context in

    Test.Cxt.get_tests context |> List.iter (
        fun (test : Testcase.t) ->
            Cxt.init cxt test;
            let rec loop () =
                if Cxt.is_done cxt then () else (
                    Cxt.interp cxt |> Interp.stack |> printf "@.@[<v>%a@]@.@." Interp.Stack.fmt;
                    Cxt.interp cxt |> Interp.next_ins |> if_let_some (
                        log_1 "@[<v 4>> %a@]@." Mic.fmt
                    );
                    if conf.Conf.step then (
                        input_line stdin |> ignore
                    );
                    let is_done = Cxt.step cxt in
                    if not is_done then loop ()
                    else (
                        log_1 "@.@.terminating run@.";
                        Cxt.terminate_run cxt;
                        log_1 "staging next operation@.";
                        let is_done = Cxt.init_next cxt in
                        log_1 "context: @[<v>%a@]@." Cxt.fmt cxt;
                        if not is_done then loop () else ()
                    )
                )
            in
            loop |> Exc.chain_err (
                fun () ->
                    asprintf
                        "while running test case `%s` from %a" test.name Source.fmt test.source
            );
            log_1 "done with test `%s`@." test.name
    )
