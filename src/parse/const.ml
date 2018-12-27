(* Constant parsing. *)

open Base
open Base.Common

let parse (token : string) (args : Mic.const list) : Mic.const =
    let arity_check (expected : int) : unit =
        if (List.length args) <> expected then (
            sprintf "constructor `%s` expects %i argument%s, found %i"
                token expected (Fmt.plurify expected) (List.length args)
            |> Exc.throw
        )
    in
    match token with
    | "Left" ->
        arity_check 1;
        Mic.Lft (List.hd args)
    | "Right" ->
        arity_check 1;
        Mic.Rgt (List.hd args)
    | "Some" ->
        arity_check 1;
        Mic.So (List.hd args)
    | "None" ->
        arity_check 0;
        Mic.No
    | _ -> sprintf "unknown constructor `%s`" token |> Exc.throw