open Format

exception Exc of string list * exn option

let throw (s : string) : 'a = Exc ([s], None) |> raise
let throws (ss : string list) : 'a = Exc (ss, None) |> raise

let erase_err (blah : unit -> string) (stuff : unit -> 'a) : 'a =
    try stuff () with 
    | _ -> blah () |> throw

let chain_errs (blah : unit -> string list) (stuff : unit -> 'a) : 'a =
    try stuff () with
    | Exc (trace, opt) -> Exc (blah () @ trace, opt) |> raise
    | e -> Exc (blah (), Some e) |> raise

let chain_err (blah : unit -> string) (stuff : unit -> 'a) : 'a =
    try stuff () with
    | Exc (trace, opt) -> Exc (blah () :: trace, opt) |> raise
    | e -> Exc ([blah ()], Some e) |> raise

let catch_print (indent : int) (stuff : unit -> 'a) : 'a option =
    let pref () =
        let rec loop n =
            if n > 0 then (
                printf "    ";
                n - 1 |> loop
            ) else ()
        in
        indent - 1 |> loop
    in
    try Some (stuff ()) with
    | Exc (trace, opt) -> (
        pref ();
        printf "@[<v 4>Error";
        trace |> List.iter (
            fun line ->
                pref ();
                printf "@,@[%s@]" line
        );
        (
            match opt with
            | None -> ()
            | Some e ->
                pref ();
                printf "@,@[%s@]" (Printexc.to_string e)
        );
        printf "@]@.";
        None
    )
    | e -> (
        Printexc.to_string e |> printf "@[<v 4>Error@,%s@]@.";
        None
    )

let catch_fail (stuff : unit -> 'a) : 'a =
    match catch_print 0 stuff with
    | None -> exit 2
    | Some a -> a

let unreachable () : 'a = throw "entered unreachable code, please notify the developers"