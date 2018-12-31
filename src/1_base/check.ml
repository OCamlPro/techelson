(* Types and helpers for checking stuff. *)

open Common

let arity
    (desc : string)
    (token : unit -> string)
    (expected : int)
    (args : 'a list)
    : unit
=
    if expected <> List.length args then (
        sprintf "%s expects %i %s%s" (token ()) expected desc (Fmt.plurify expected)
        |> Exc.throw
    )

let arity_ge
    (desc : string)
    (token : unit -> string)
    (expected : int)
    (args : 'a list)
    : unit
=
    if expected > List.length args then (
        sprintf "%s expects %i or more %ss" (token ()) expected desc
        |> Exc.throw
    )
let arity_le
    (desc : string)
    (token : unit -> string)
    (expected : int)
    (args : 'a list)
    : unit
=
    if expected < List.length args then (
        sprintf "%s expects %i or fewer %ss" (token ()) expected desc
        |> Exc.throw
    )

let distinct (desc : string) (fmt : formatter -> 'a -> unit) (elms : 'a list) : unit =
    let rec loop (elms : 'a list) : unit =
        match elms with
        | [] -> ()
        | hd :: tl ->
            if List.mem hd tl then (
                asprintf "%s `%a` appears twice" desc fmt hd
                |> Exc.throw
            ) else (
                loop tl
            )
    in
    loop elms

module Annots = struct
    let typ_arity_le (token : unit -> string) (expected : int) (annots : Annot.typs) : unit =
        arity_le "type annotation" token expected annots;
        distinct "type annotation" Annot.Typ.fmt annots

    let var_arity_le (token : unit -> string) (expected : int) (annots : Annot.vars) : unit =
        arity_le "variable annotation" token expected annots;
        distinct "variable annotation" Annot.Var.fmt annots

    let field_arity_le (token : unit -> string) (expected : int) (annots : Annot.fields) : unit =
        arity_le "field annotation" token expected annots;
        distinct "field annotation" Annot.Field.fmt annots

    let arity_le
        (token : unit -> string)
        ((typ_expected, typ_annots) : int * Annot.typs)
        ((var_expected, var_annots) : int * Annot.vars)
        ((field_expected, field_annots) : int * Annot.fields)
        : unit
    =
        typ_arity_le token typ_expected typ_annots;
        var_arity_le token var_expected var_annots;
        field_arity_le token field_expected field_annots;
        ()
end

let typ_arity (token : unit -> string) (expected : int) (dtyps : Dtyp.t list) : unit =
    arity "type argument" token expected dtyps

let args_arity (token : unit -> string) (expected : int) (args : 'a list) : unit =
    arity "instruction argument" token expected args

let param_arity
    (token : unit -> string)
    ((typs_expected, typs) : int * Dtyp.t list)
    ((args_expected, args) : int * 'a list)
    : unit
=
    typ_arity token typs_expected typs;
    args_arity token args_expected args

let full_arity
    (token : (unit -> string))
    (typ_annots : (int * Annot.typs))
    (var_annots : (int * Annot.vars))
    (field_annots : (int * Annot.fields))
    (typs : (int * Dtyp.t list))
    (args : (int * 'a list))
    : unit
=
    Annots.arity_le token typ_annots var_annots field_annots;
    param_arity token typs args