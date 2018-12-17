open Base
open Base.Common


let _ =
    conf () |> printf "@[<v>Configuration:@,%a@]@.@." Conf.fmt;
    let lst = Dtyp.List Dtyp.str in
    printf "lst : %a@." Dtyp.fmt lst;
    let per = Dtyp.Pair (lst, Dtyp.bool) in
    printf "per : %a@." Dtyp.fmt per;
    let opt = Dtyp.Option per in
    printf "opt : %a@." Dtyp.fmt opt;
    let orr = Dtyp.Or (per, opt) in
    printf "orr : %a@." Dtyp.fmt orr;
    let set = Dtyp.Set per in
    printf "set : %a@." Dtyp.fmt set;
    let map = Dtyp.Map (Dtyp.nat, orr) in
    printf "map : %a@." Dtyp.fmt map;
    let bmp = Dtyp.BigMap (Dtyp.str, set) in
    printf "bmp : %a@." Dtyp.fmt bmp;
    ()

let test_parser () =
    let lexbuf = Lexing.from_channel stdin in
    let () = Parse.Micparse.mic Parse.Miclex.token lexbuf in
    ()

let _ = Exc.catch_fail test_parser