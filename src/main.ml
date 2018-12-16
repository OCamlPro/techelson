open Common

let _ =
    printf "@[<v>Configuration:@,%a@]@.@." Conf.fmt conf;
    let lst = Mic.DTyp.List Mic.DTyp.str in
    printf "lst : %a@." Mic.DTyp.fmt lst;
    let per = Mic.DTyp.Pair (lst, Mic.DTyp.bool) in
    printf "per : %a@." Mic.DTyp.fmt per;
    let opt = Mic.DTyp.Option per in
    printf "opt : %a@." Mic.DTyp.fmt opt;
    let orr = Mic.DTyp.Or (per, opt) in
    printf "orr : %a@." Mic.DTyp.fmt orr;
    let set = Mic.DTyp.Set per in
    printf "set : %a@." Mic.DTyp.fmt set;
    let map = Mic.DTyp.Map (Mic.DTyp.nat, orr) in
    printf "map : %a@." Mic.DTyp.fmt map;
    let bmp = Mic.DTyp.BigMap (Mic.DTyp.str, set) in
    printf "bmp : %a@." Mic.DTyp.fmt bmp;
    ()
