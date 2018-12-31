(* Parsing-related helpers. *)

let tail_of_pref ~(pref : string) (token : string) : string option =
    let pref_len = String.length pref in
    let token_len = String.length token in
    if pref_len <= token_len then (
        let token_pref = String.sub token 0 pref_len in
        if token_pref = pref then (
            Some (
                String.sub token pref_len (token_len - pref_len)
            )
        ) else (
            None
        )
    ) else (
        None
    )

let sequence (f : char -> 'a option) (token : string) : ('a list * string) =
    let token_len = String.length token in
    (* Input `n` is the index of the character in the string we're currently dealing with. *)
    let rec loop lst n =
        let op =
            if n < token_len then (
                String.get token n |> f
            ) else (
                None
            )
        in
        match op with
        | Some op -> loop (op :: lst) (n + 1)
        | None -> List.rev lst, String.sub token n (token_len - n)
    in
    loop [] 0