(* Configuration structure, built from CLAs. *)

open Format

type contract = {
    file : string ;
    init : string option ;
}

let fmt_contract (fmt : formatter) ({ file ; init } : contract) : unit =
    fprintf fmt "`%s`, init: %s" file (
        match init with
        | None -> "<none>"
        | Some init -> sprintf "`%s`" init
    )

let mk_contract (file : string) (init : string option) : contract = {
    file ; init
}

type testgen_mode = {
    mutable count : int ;
}

let fmt_testgen_mode (fmt : formatter) ({ count } : testgen_mode) : unit =
    fprintf fmt "count: %i@" count

let default_testgen_mode : testgen_mode = {
    count = 10 ;
}

type mode =
| Testgen of testgen_mode
| Inactive

let fmt_mode (fmt : formatter) (mode : mode) : unit =
    match mode with
    | Inactive -> fprintf fmt "<none>"
    | Testgen opts -> fprintf fmt "testgen @[<v>%a@]" fmt_testgen_mode opts

type t = {
    mutable verb : int ;

    mutable step : bool ;
    mutable contracts : contract list ;
    mutable args : string list ;

    mutable mode : mode ;
}

let map_testgen_mode (f : testgen_mode -> unit) (self : t) : unit =
    match self.mode with
    | Testgen opts -> f opts
    | mode -> asprintf "expected testgen mode, found %a" fmt_mode mode |> Exc.throw


let default : t = {
    verb = 1 ;
    step = false ;
    contracts = [] ;
    args = [] ;
    mode = Inactive ;
}

let fmt (fmt : formatter) ({
    verb ; step ; contracts ; args ; mode
} : t) =
    fprintf fmt "@[<v>{@[<v 4>";

    fprintf fmt "@,verb: %i," verb;
    fprintf fmt "@,step: %b," step;

    if contracts = [] then (
        fprintf fmt "@,contracts: none"
    ) else (
        fprintf fmt "@,@[<v 4>contracts:";
        contracts |> List.iter (
            fun c -> fprintf fmt "@,%a" fmt_contract c
        );
        fprintf fmt "@]"
    );

    fprintf fmt "@,args:";
    args |> List.iter (
        fun arg -> fprintf fmt " %s," arg
    );

    fprintf fmt "@,@[<v>@[<v 4>mode: {@,%a@]@,}@]" fmt_mode mode;
    fprintf fmt "@]@,}@]"

let set_mode (mode : mode) (self : t) : unit =
    (
        match self.mode with
        | Inactive -> ()
        | mode ->
            asprintf "trying to set mode, but mode is already set to %a" fmt_mode mode
            |> Exc.throw
    );
    self.mode <- mode