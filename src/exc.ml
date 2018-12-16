open Format

exception Exc of string list

let throw (s : string) : 'a = Exc [s] |> raise
let throws (ss : string list) : 'a = Exc ss |> raise

let erase_err (blah : unit -> string) (stuff : unit -> 'a) : 'a =
    try stuff () with 
    | _ -> blah () |> throw

let chain_err (blah : unit -> string) (stuff : unit -> 'a) : 'a =
    try stuff () with
    | Exc trace -> Exc (blah () :: trace) |> raise
    | e -> Exc [blah () ; (Printexc.to_string e)] |> raise

let catch_fail (stuff : unit -> 'a) : 'a =
    try stuff () with
    | Exc trace -> (
        printf "@[<v 4>Error";
        trace |> List.iter (
            fun line -> printf "@,%s" line
        );
        printf "@]@.";
        exit 2
    )
    | e -> (
        Printexc.to_string e |> printf "@[<v 4>Error@,%s@]@.";
        exit 2
    )