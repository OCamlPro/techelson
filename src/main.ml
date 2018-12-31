open Base
open Base.Common


(* Set configuration from CLAP and print it. *)
let _ =
    Clap.set_conf ();
    conf () |> printf "@[<v>Configuration:@,%a@]@.@." Conf.fmt

(* Actually do stuff. *)
let _ =
    Exc.catch_fail Run.run