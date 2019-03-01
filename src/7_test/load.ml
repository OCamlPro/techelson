(* Types and helpers to load test scenarii. *)

open Base
open Common

type error_list = Exc.exc list
let fmt_error_list (fmt : formatter) (errs : error_list) : unit =
    errs |> List.iter (
        fun e -> fprintf fmt "@[<v>%a@]" Exc.fmt (Exc.Exc e)
    )

type errors = {
    contracts : error_list ;
    testcases : error_list ;
}
let has_errors (self : errors) : bool =
    self.contracts <> [] || self.testcases <> []
let fmt_errors (fmt : formatter) (self : errors) : unit =
    let sep =
        if self.contracts <> [] then (
            fprintf fmt "on contracts: %a" fmt_error_list self.contracts;
            fun () -> fprintf fmt "@ "
        ) else (
            ignore
        )
    in
    if self.testcases <> [] then (
        sep ();
        fprintf fmt "on testcases: %a" fmt_error_list self.testcases
    )
let error_count (self : errors) : int = List.length self.contracts + List.length self.testcases

let of_source (src : Source.t list) : in_channel list * error_list =
    Lst.fold (
        fun (res, err_list) src ->
            try (
                let chan = Source.to_channel src in
                res @ [chan], err_list
            ) with
            | (Exc.Exc e) -> res, err_list @ [e]
    ) ([], []) src

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
    : 'b list * error_list
=
    files |> List.fold_left (
        fun (acc, err_list) data ->
            try (
                let stuff =
                    (fun () -> file data |> open_file |> f data) |> Exc.chain_err (
                        fun () -> sprintf "while loading %s file `%s`" desc (file data)
                    )
                in
                acc @ [stuff], err_list
            ) with
            | Exc.Exc e -> acc, err_list @ [e]
    ) ([], [])
    |> fun (result, errors) ->
        assert (List.length errors + List.length result = List.length files);
        result, errors

let contracts (files : Conf.contract list) : Contract.t list * error_list =
    load_map "contract" files (fun contract -> contract.Conf.file) (
        fun c chan ->
            let file = c.Conf.file in
            let name = Contract.name_of_file file in
            let src = Source.File file in
            contract name src chan
    )

let tests (files : string list) : Testcase.t list * error_list =
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
    : Testcases.t * errors
=
    let contracts, c_errors = contracts contract_files in
    let tests, t_errors = tests test_files in
    let errors = { contracts = c_errors ; testcases = t_errors } in
    let cxt = Testcases.of_raw contracts tests in
    cxt, errors
