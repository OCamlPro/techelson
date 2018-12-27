(* Datatype parsing. *)

open Base
open Base.Common

let parse
    (token : string)
    (name : Annot.Typ.t option)
    (dtyps : (Dtyp.t * Annot.Field.t option) list)
    : Dtyp.t
=
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
            Dtyp.Option (no_annot dtyps |> List.hd)

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
                                | None -> Dtyp.fmt fmtt dtyp
                                | Some annot -> fprintf fmtt "(%a %a)" Dtyp.fmt dtyp Annot.Field.fmt annot
                            )
                        )
                        dtyps
            in
            sprintf "while parsing \"%s%s\"" token dtyps
    ) inner
    |> fun dtyp -> Dtyp.mk ~alias:(name) dtyp