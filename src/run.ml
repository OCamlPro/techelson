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

open Eval.Top

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
            let cxt = NaiveCxt.init [] [ test.code ] in
            let rec loop () =
                NaiveCxt.stack cxt |> printf "@.@[<v>%a@]@.@." NaiveCxt.Stack.fmt;
                NaiveCxt.next_ins cxt |> if_let_some (
                    log_1 "@[<v 4>> %a@]@." Mic.fmt
                );
                if conf.Conf.step then (
                    input_line stdin |> ignore
                );
                let is_done = NaiveCxt.step cxt in
                if not is_done then loop () else ()
            in
            loop |> Exc.chain_err (
                fun () ->
                    asprintf
                        "while running test case `%s` from %a" test.name Source.fmt test.source
            );
            log_1 "done with test `%s`@." test.name
    )
