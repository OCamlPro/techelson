open Common

(** Type checking context. Stores type constraints. *)
type t = {
    constraints : (Dtyp.tvar, Dtyp.t option) Hashtbl.t
    (** Contraints.

        Maps type variables to their constraints

        - bool flag: true if the type must be comparable
        - datatypes: type variable must be compatible with these datatypes
    *)
}
let fmt (fmt : formatter) (self : t) : unit =
    fprintf fmt "@[<v>";
    self.constraints |> Hashtbl.iter (
        fun tvar constr ->
            match constr with
            | Some dtyp ->
                fprintf fmt "t_%i'= %a@,"
                    tvar Dtyp.fmt dtyp
            | None -> ()
    );
    fprintf fmt "@]"

let empty () : t =
    { constraints = Hashtbl.create 17 }

let annot_check (a_1 : 'a option) (a_2 : 'a option) (bail : unit -> 'b) : 'b =
    match a_1, a_2 with
    | Some a_1, Some a_2 when a_1 <> a_2 -> bail ()
    | _ -> ()

(* Checks that two type-variable-free-types are compatible.

    TODO: stackless.
*)
let rec check (t_1 : Dtyp.t) (t_2 : Dtyp.t) =
    let bail () =
        asprintf "types `%a` and `%a` are not compatible" Dtyp.fmt t_1 Dtyp.fmt t_2
        |> Exc.throw
    in

    (* log_0 "type-checking@.  %a@.  %a@." Dtyp.fmt t_1 Dtyp.fmt t_2; *)

    annot_check t_1.alias t_2.alias bail;

    match t_1.typ, t_2.typ with
    | Leaf leaf_1, Leaf leaf_2 ->
        if leaf_1 <> leaf_2 then bail ()

    | List sub_1, List sub_2
    | Set sub_1, Set sub_2
    | Contract sub_1, Contract sub_2 -> check sub_1 sub_2

    | Map (k_1, v_1), Map (k_2, v_2)
    | BigMap (k_1, v_1), BigMap (k_2, v_2) ->
        check k_1 k_2;
        check v_1 v_2

    | Option sub_1, Option sub_2 -> (
        annot_check sub_1.name sub_2.name bail;

        check sub_1.inner sub_2.inner
    )

    | Pair (lft_1, rgt_1), Pair (lft_2, rgt_2)
    | Or (lft_1, rgt_1), Or (lft_2, rgt_2) -> (
        annot_check lft_1.name lft_2.name bail;
        annot_check rgt_1.name rgt_2.name bail;

        check lft_1.inner lft_2.inner;
        check rgt_1.inner rgt_2.inner
    )
    | Lambda (dom_1, codom_1), Lambda (dom_2, codom_2) -> (
        check dom_1 dom_2;
        check codom_1 codom_2
    )

    | Leaf _, _
    | Option _, _
    | List _, _
    | Set _, _
    | Contract _, _
    | Map _, _
    | BigMap _, _
    | Pair _, _
    | Or _, _
    | Lambda _, _ -> bail ()

let set_var_def (cxt : t) (index : Dtyp.tvar) (dtyp : Dtyp.t) : unit =
    let prev =
        try Some (Hashtbl.find cxt.constraints index) with | Not_found -> None
    in
    match prev with
    | None -> Hashtbl.add cxt.constraints index (Some dtyp)
    | Some definition ->
        match definition with
        | None -> (
            Hashtbl.add cxt.constraints index (Some dtyp)
        )
        | Some definition -> (
            (fun () -> check definition dtyp)
            |> Exc.chain_err (
                fun () -> asprintf "while resolving constraints on %a" Dtyp.fmt_tvar index
            );
            ()
        )

let unify (cxt : t) (t_1 : Dtyp.t) (t_2 : Dtyp.t) : unit =
    let bail () =
        asprintf "types `%a` and `%a` are not compatible" Dtyp.fmt t_1 Dtyp.fmt t_2
        |> Exc.throw
    in

    (* log_0 "type-checking@.  %a@.  %a@." Dtyp.fmt t_1 Dtyp.fmt t_2; *)

    let rec loop (to_do : (Dtyp.t * Dtyp.t) list) =
        match to_do with
        | [] -> ()
        | (t_1, t_2) :: to_do -> (
            annot_check t_1.alias t_2.alias bail;
            let to_do =
                match t_1.typ, t_2.typ with
                | Dtyp.Leaf (Var _), _ ->
                    Exc.throw "first type given to `unify` cannot contain type variables"
                | _, Dtyp.Leaf (Var i) ->
                    set_var_def cxt i t_1;
                    to_do
                | Dtyp.Leaf _, Dtyp.Leaf _ ->
                    check t_1 t_2;
                    to_do

                | Dtyp.Pair (lft_1, rgt_1), Dtyp.Pair (lft_2, rgt_2)
                | Dtyp.Or (lft_1, rgt_1), Dtyp.Or (lft_2, rgt_2) -> (
                    (lft_1.inner, lft_2.inner) :: (rgt_1.inner, rgt_2.inner) :: to_do
                )

                | Dtyp.Option t_1, Dtyp.Option t_2 -> (
                    (t_1.inner, t_2.inner) :: to_do
                )
                | Dtyp.List t_1, Dtyp.List t_2
                | Dtyp.Set t_1, Dtyp.Set t_2
                | Dtyp.Contract t_1, Dtyp.Contract t_2 -> (
                    (t_1, t_2) :: to_do
                )

                | Dtyp.Map (lft_1, rgt_1), Dtyp.Map (lft_2, rgt_2)
                | Dtyp.BigMap (lft_1, rgt_1), Dtyp.BigMap (lft_2, rgt_2)
                | Dtyp.Lambda (lft_1, rgt_1), Dtyp.Lambda (lft_2, rgt_2) -> (
                    (lft_1, lft_2) :: (rgt_1, rgt_2) :: to_do
                )

                | Dtyp.Leaf _, _
                | Dtyp.Pair _, _
                | Dtyp.Or _, _
                | Dtyp.Option _, _
                | Dtyp.List _, _
                | Dtyp.Set _, _
                | Dtyp.Contract _, _
                | Dtyp.Map _, _
                | Dtyp.BigMap _, _
                | Dtyp.Lambda _, _ ->
                    asprintf "cannot unify %a and %a" Dtyp.fmt t_1 Dtyp.fmt t_2
                    |> Exc.throw
            in

            loop to_do
        )
    in

    (fun () ->
        try loop [ t_1, t_2 ] with
        | e -> (
            try loop [ t_2, t_1 ] with _ -> raise e
        )
    )
    |> Exc.chain_err (
        fun () -> asprintf "while unifying %a and %a" Dtyp.fmt t_1 Dtyp.fmt t_2
    )

let is_compatible (cxt : t) (t_1 : Dtyp.t) (t_2 : Dtyp.t) : bool =
    try unify cxt t_1 t_2 ; true with
    | _ -> false

