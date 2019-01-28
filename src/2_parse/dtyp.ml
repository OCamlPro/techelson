(* Datatype parsing. *)

open Base
open Base.Common

let parse
    (token : string)
    (annot : Annot.t)
    (dtyps : (Dtyp.t * Annot.Field.t option) list)
    : (Dtyp.t * Annot.Field.t option)
=
    if annot.vars <> [] then (
        "type constructors do not accept variable annotations" |> Base.Exc.throw
    ) else if List.length annot.typs > 1 then (
        "type constructors do not accept more than one type annotation" |> Base.Exc.throw
    ) else if List.length annot.fields > 1 then (
        "type constructors do not accept more than one field annotation" |> Base.Exc.throw
    ) ;

    let typ_annot, field_annot = Lst.hd annot.typs, Lst.hd annot.fields in

    let typ_arity (expected : int) : unit =
        Check.typ_arity
            (fun () -> sprintf "type constructor `%s`" token)
            expected
            (List.map fst dtyps)
    in
    (* Fails if `dtyps` has a annotated elements. *)
    let no_annot (dtyps : (Dtyp.t * Annot.Field.t option) list) : Dtyp.t list =
        dtyps |> List.map (
            fun (dtyp, annot) ->
                annot |> if_let_some (
                    fun annot -> [
                        sprintf
                            "type constructor `%s` does not accept field annotations"
                            token;
                        asprintf
                            "found field annotation `%a` on type %a"
                            Annot.Field.fmt annot Dtyp.fmt dtyp
                    ] |> Exc.throws
                );
                dtyp
        )
    in

    let inner () =
        match token with
        | "list" ->
            typ_arity 1;
            Dtyp.List (no_annot dtyps |> List.hd)

        | "option" ->
            typ_arity 1;
            let sub : Dtyp.named =
                let dtyp = List.hd dtyps in
                { inner = fst dtyp ; name = snd dtyp }
            in
            Dtyp.Option sub

        | "set" ->
            typ_arity 1;
            Dtyp.Set (no_annot dtyps |> List.hd)

        | "contract" ->
            typ_arity 1;
            Dtyp.Contract (no_annot dtyps |> List.hd)

        | "pair" ->
            typ_arity 2;
            let lft : Dtyp.named =
                let dtyp = List.hd dtyps in
                { inner = fst dtyp ; name = snd dtyp }
            in
            let rgt : Dtyp.named =
                let dtyp = List.tl dtyps |> List.hd in
                { inner = fst dtyp ; name = snd dtyp }
            in
            Dtyp.Pair (lft, rgt)

        | "or" ->
            typ_arity 2;
            let lft : Dtyp.named =
                let dtyp = List.hd dtyps in
                { inner = fst dtyp ; name = snd dtyp }
            in
            let rgt : Dtyp.named =
                let dtyp = List.tl dtyps |> List.hd in
                { inner = fst dtyp ; name = snd dtyp }
            in
            Dtyp.Pair (lft, rgt)

        | "map" ->
            typ_arity 2;
            let dtyps = no_annot dtyps in
            let keys, vals = List.hd dtyps, List.tl dtyps |> List.hd in
            Dtyp.Map (keys, vals)

        | "big_map" ->
            typ_arity 2;
            let dtyps = no_annot dtyps in
            let keys, vals = List.hd dtyps, List.tl dtyps |> List.hd in
            Dtyp.BigMap (keys, vals)

        | "lambda" ->
            typ_arity 2;
            let dtyps = no_annot dtyps in
            let dom, codom = List.hd dtyps, List.tl dtyps |> List.hd in
            Dtyp.Lambda (dom, codom)

        | _ -> (
            match Dtyp.leaf_of_string token with
            | None -> sprintf "unknown type constructor `%s`" token |> Exc.throw
            | Some leaf ->
                typ_arity 0;
                Dtyp.Leaf leaf
        )
    in

    Exc.chain_err (
        fun () ->
            let dtyps =
                if dtyps = [] then "" else
                    asprintf " %a"
                        (Fmt.fmt_list
                            Fmt.sep_spc
                            (
                                fun fmtt (dtyp, annot) ->
                                match annot with
                                | None ->
                                    Dtyp.fmt fmtt dtyp
                                | Some annot ->
                                    fprintf fmtt "(%a %a)" Dtyp.fmt dtyp Annot.Field.fmt annot
                            )
                        )
                        dtyps
            in
            asprintf "while parsing \"%s%a%s\"" token Annot.fmt annot dtyps
    ) inner
    |> fun dtyp -> (Dtyp.mk ~alias:typ_annot dtyp, field_annot)
