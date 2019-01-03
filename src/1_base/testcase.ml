open Common

type t = {
    name : string ;
    source : Source.t ;
    code : Mic.t ;
}

let mk (name : string) (source : Source.t) (code : Mic.t) : t = {
    name ; source ; code
}

let rename (name : string) (self : t) : t = { self with name }

let fmt ~(full : bool) (fmt : formatter) (self : t) : unit =
    fprintf fmt "@[test %s { # loaded from %a@,    " self.name Source.fmt self.source;
    (
        if not full then
            fprintf fmt "...@,"
        else
            fprintf fmt "@[%a@]@," Mic.fmt self.code
    );
    fprintf fmt "}@]"