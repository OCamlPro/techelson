(* Pretty-printing functions. *)

open Common

let conf (fmt : formatter) (
    { verb ; args } : Conf.t
) =
    fprintf fmt "@[<v>{@[<v 4>";
    fprintf fmt "@,verb: %i," verb;
    fprintf fmt "@,args:";
    args |> List.iter (
        fun arg -> fprintf fmt " %s," arg
    );
    fprintf fmt "@]@,}@]"