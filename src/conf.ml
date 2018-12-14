(* Configuration structure, built from CLAs. *)

type t = {
    mutable verb : int ;

    mutable args : string list;
}


let default : t = {
    verb = 0;
    args = [];
}
