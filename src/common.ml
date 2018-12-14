(* Common types and functions used by the whole program. *)

include Format

(* Configuration, built by CLAP. *)
let conf : Conf.t =
    let chained : unit -> Conf.t =
        fun () -> Exc.chain_err (
            fun () -> "while parsing command-line arguments"
        ) (fun () -> Clap.run ())
    in
    Exc.catch_fail chained