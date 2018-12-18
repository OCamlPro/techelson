(* Types and helpers to load test scenarii. *)

open Base
open Base.Common

let of_channel (in_chan : in_channel) : unit =
    let _ =
        Lexing.from_channel in_chan |> Parse.Micparse.mic Parse.Miclex.token
    in
    ()

type error_count = int

let of_source (src : Source.t list) : in_channel list * error_count =
    Lst.fold (
        fun (res, err_count) src ->
            let () = log_1 "Extracting input channel from %a.@." Source.fmt src in
            match Exc.catch_print (fun () -> Source.to_channel src) with
            | None -> log_0 "@." ; res, err_count + 1
            | Some chan -> res @ [chan], err_count
    ) ([], 0) src

let contract (name : string) (chan : in_channel) : Contract.t =
    (Lexing.from_channel chan |> Parse.Micparse.just_contract Parse.Miclex.token) name
