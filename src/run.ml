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
    )
    (* let contracts = load_contracts conf in
    log_1 "Contracts from CLAs: @[<v>%a@]@." (Fmt.fmt_list Fmt.sep_spc (Contract.fmt ~full:true)) contracts;
    let sources =
        match conf.args with
        | [] ->
            [ Source.Stdin ]
        | lst -> lst |> List.map (
            fun file -> Source.File file
        )
    in
    let chans, errs = Test.Load.of_source sources in
    if errs > 0 then (
        log_1 "Failed to open %i test file%s." errs (Fmt.plurify errs)
    );
    let _ = chans in
    () *)
