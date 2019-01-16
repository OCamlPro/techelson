(* Types and helpers to load test scenarii. *)

open Base
open Common

type error_count = int

let of_source (src : Source.t list) : in_channel list * error_count =
    Lst.fold (
        fun (res, err_count) src ->
            let () = log_3 "extracting input channel from %a.@." Source.fmt src in
            match Exc.catch_print 3 (fun () -> Source.to_channel src) with
            | None -> log_0 "@." ; res, err_count + 1
            | Some chan -> res @ [chan], err_count
    ) ([], 0) src

let contract (name : string) (source : Source.t) (chan : in_channel) : Contract.t =
    (Lexing.from_channel chan |> Parse.Micparse.just_contract Parse.Miclex.token) name source

let test (name : string) (source : Source.t) (chan : in_channel) : Testcase.t =
    let code = (Lexing.from_channel chan |> Parse.Micparse.just_mic Parse.Miclex.token) in
    Testcase.mk name source code

let load_map
    (desc :string)
    (files : 'a list)
    (file : 'a -> string)
    (f : 'a -> in_channel -> 'b)
    : 'b list * error_count
=
    files |> List.fold_left (
        fun (acc, err_count) data ->
            let file = file data in
            let () = log_3 "loading file %s...@." file in
            let load () = open_file file |> f data in
            match
                (
                    fun () -> load |> Exc.chain_err (
                        fun () -> sprintf "while loading %s file `%s`" desc file
                    )
                )
                |> Exc.catch_print 3
            with
            | None -> log_3 "failed@." ; acc, err_count + 1
            | Some stuff -> log_3 "success@." ; stuff :: acc, err_count
    ) ([], 0)
    |> fun (result, count) ->
        assert (count + List.length result = List.length files);
        (List.rev result), count

let contracts (files : Conf.contract list) : Contract.t list * error_count =
    load_map "contract" files (fun contract -> contract.Conf.file) (
        fun c chan ->
            let file = c.Conf.file in
            let name = Contract.name_of_file file in
            let src = Source.File file in
            contract name src chan
    )

let tests (files : string list) : Testcase.t list * error_count =
    load_map "test" files id (
        fun file chan ->
            let name = Contract.name_of_file file in
            let src = Source.File file in
            test name src chan
    )

let scenario
    (src : Source.t)
    (chan : in_channel)
    : Contract.t list * Testcase.t list
=
    let everything = (Lexing.from_channel chan |> Parse.Micparse.scenario Parse.Miclex.token) in
    everything src

let context
    ~(contract_files : Conf.contract list)
    ~(test_files : string list)
    ~(else_chan : (in_channel * Source.t) option)
    : Testcases.t * error_count
=
    let contract_count = List.length contract_files in
    if contract_count > 0 then (
        log_2 "loading %i contract file%s@." contract_count (Fmt.plurify contract_count)
    );
    let contracts, c_errors = contracts contract_files in

    let test_count = List.length test_files in
    if test_count > 0 then (
        log_2 "loading %i test file%s@." test_count (Fmt.plurify test_count)
    );
    let tests, t_errors = tests test_files in
    if c_errors + t_errors > 0 then (
        if c_errors > 0 then (
            log_0 "%i error%s occured during contract loading@." c_errors (Fmt.plurify c_errors)
        );
        if t_errors > 0 then (
            log_0 "%i error%s occured during testcase loading@." t_errors (Fmt.plurify t_errors)
        );
        log_0 "@."
    );

    let contracts, tests =
        if test_files <> [] then contracts, tests else (
            match else_chan with
            | Some (chan, src) ->
                let nu_contracts, nu_tests = scenario src chan in
                contracts @ nu_contracts, tests @ nu_tests
            | None -> contracts, tests
        )
    in

    let cxt = Testcases.of_raw contracts tests in

    cxt, c_errors + t_errors
