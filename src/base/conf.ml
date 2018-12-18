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

type t = {
    mutable verb : int;

    mutable contracts : contract list;
    mutable args : string list;
}


let default : t = {
    verb = 1;
    contracts = [];
    args = [];
}

let fmt (fmt : formatter) ({
    verb ; contracts ; args
} : t) =
    fprintf fmt "@[<v>{@[<v 4>";

    fprintf fmt "@,verb: %i," verb;

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
    fprintf fmt "@]@,}@]"