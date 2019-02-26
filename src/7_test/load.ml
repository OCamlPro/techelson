(* Types and helpers to load test scenarii. *)

open Base
open Common

type error_count = int

let of_source (src : Source.t list) : in_channel list * error_count =
    Lst.fold (
        fun (res, err_count) src ->
            let () = log_3 "extracting input channel from %a.@." Source.fmt src in
            match Exc.catch_print (fun () -> Source.to_channel src) with
            | None -> log_0 "@." ; res, err_count + 1
            | Some chan -> res @ [chan], err_count
    ) ([], 0) src

let contract (name : string) (source : Source.t) (chan : in_channel) : Contract.t =
    let lexbuf = Lexing.from_channel chan in
    try Parse.Micparse.just_contract Parse.Miclex.token lexbuf name source
    with e -> (
        let line = lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum in
        let token = Lexing.lexeme lexbuf in
        (fun () -> raise e)
        |> Exc.chain_err (fun () -> sprintf "on token `%s`, line %i" token line)
    )

let test (name : string) (source : Source.t) (chan : in_channel) : Testcase.t =
    let lexbuf = Lexing.from_channel chan in
    let parse_res =
        try Parse.Micparse.mic_or_contract Parse.Miclex.token lexbuf
        with e -> (
            let line = lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum in
            let token = Lexing.lexeme lexbuf in
            (fun () -> raise e)
            |> Exc.chain_err (fun () -> sprintf "on token `%s`, line %i" token line)
        )
    in
    let code =
        match parse_res with
        | Either.Lft code -> code
        | Either.Rgt contract -> (
            let contract = contract name source in
            let unit = Dtyp.unit in
            let unit_named = Dtyp.mk_named None unit in
            let env = DtypCheck.empty () in
            let code = contract.entry in
            if DtypCheck.is_compatible env unit contract.entry_param |> not then (
                Exc.throws [
                    asprintf "testcase contract parameter must have type unit, found %a"
                        Dtyp.fmt contract.entry_param ;
                    "while parsing a contract as a testcase" ;
                ]
            );
            if DtypCheck.is_compatible env unit contract.storage |> not then (
                Exc.throws [
                    asprintf "contract storage must have type unit, found %a"
                        Dtyp.fmt contract.storage ;
                    "while parsing a contract as a testcase" ;
                ]
            );
            let code =
                Mic.Seq [
                    Mic.Push (
                        Dtyp.Pair (unit_named, unit_named) |> Dtyp.mk,
                        Mic.Pr (Mic.U, Mic.U)
                    ) |> Mic.mk ;
                    code
                ] |> Mic.mk
            in
            code
        )
    in
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
                |> Exc.catch_print
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
(* 
let scenario
    (src : Source.t)
    (chan : in_channel)
    : Contract.t list * Testcase.t list
=
    let everything = (Lexing.from_channel chan |> Parse.Micparse.scenario Parse.Miclex.token) in
    everything src *)

let context
    ~(contract_files : Conf.contract list)
    ~(test_files : string list)
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

    let cxt = Testcases.of_raw contracts tests in

    cxt, c_errors + t_errors
