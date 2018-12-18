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

let catch_print (stuff : unit -> 'a) : 'a option =
    try Some (stuff ()) with
    | Exc trace -> (
        printf "@[<v 4>Error";
        trace |> List.iter (
            fun line -> printf "@,%s" line
        );
        printf "@]@.";
        None
    )
    | e -> (
        Printexc.to_string e |> printf "@[<v 4>Error@,%s@]@.";
        None
    )

let catch_fail (stuff : unit -> 'a) : 'a =
    match catch_print stuff with
    | None -> exit 2
    | Some a -> a
