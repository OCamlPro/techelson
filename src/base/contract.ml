(* Contract type and helpers. *)

open Common

type signature = {
    name : string option ;
    entry_param : Dtyp.t ;
}

let fmt_sig (fmt : formatter) {
    name ; entry_param
} : unit =
    let name = unwrap_or "<anonymous>" name in
    fprintf
        fmt
        "@[<v>@[<v 4>contract sig %s {@,%a@]@,}@]"
        name Dtyp.fmt entry_param;

type t = {
    name : string ;
    storage : Dtyp.t ;
    entry_param : Dtyp.t ;
    entry : Ins.t ;
    init : Ins.t option ;
}

let mk
    (name : string)
    ~(storage : Dtyp.t)
    ~(entry_param : Dtyp.t)
    (entry : Ins.t)
    (init : Ins.t option)
    : t
= { name ; storage ; entry_param ; entry ; init }

let fmt (full : bool) (fmt : formatter) {
    name ; storage ; entry_param ; entry ; init
} : unit =
    fprintf
        fmt
        "@[<v>@[<v 4>contract %s {@,storage : %a@,entry_param : %a@,"
        name Dtyp.fmt storage Dtyp.fmt entry_param;
    (
        if full then (
            fprintf fmt "entry: @[%a@]@,init: " Ins.fmt entry;
            match init with
            | None -> fprintf fmt "none"
            | Some ins -> fprintf fmt "@[%a@]" Ins.fmt ins
        ) else (
            fprintf fmt "entry: ...@,init: ";
            match init with
            | None -> fprintf fmt "none"
            | Some _ -> fprintf fmt "..."
        )
    );
    fprintf fmt "@]@,}@]"
