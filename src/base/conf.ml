(* Configuration structure, built from CLAs. *)

open Format

type t = {
    mutable verb : int;

    mutable args : string list;
}


let default : t = {
    verb = 0;
    args = [];
}

let fmt (fmt : formatter) ({
    verb ; args
} : t) =
    fprintf fmt "@[<v>{@[<v 4>";

    fprintf fmt "@,verb: %i," verb;

    fprintf fmt "@,args:";
    args |> List.iter (
        fun arg -> fprintf fmt " %s," arg
    );
    fprintf fmt "@]@,}@]"