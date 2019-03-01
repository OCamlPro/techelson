open Base

(* Set configuration from CLAP and print it. *)
let _ =
    Clap.set_conf ()

(* Actually do stuff. *)
let _ =
    Exc.catch_fail Run.run;