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

(* TODO: stackless. *)
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

type stack = Dtyp.t list
type builders = (stack -> stack) list
type to_do = (Dtyp.t * Dtyp.t) list

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

(* Builds a type that takes a single parameter. *)
let build_one (blah : string) (build : Dtyp.t -> Dtyp.t) (stack : stack) : stack =
    match stack with
    | dtyp :: stack ->
        (build dtyp) :: stack
    | _ -> sprintf "unreachable: illegal unification stack state (%s)" blah |> Exc.throw
(* Builds a type that takes two parameters. *)
let build_two (blah : string) (build : Dtyp.t -> Dtyp.t -> Dtyp.t) (stack : stack) : stack =
    match stack with
    | dtyp_1 :: dtyp_2 :: stack ->
        (build dtyp_1 dtyp_2) :: stack
    | _ -> sprintf "unreachable: illegal unification stack state (%s)" blah |> Exc.throw

let unify (cxt : t) (t_1 : Dtyp.t) (t_2 : Dtyp.t) : Dtyp.t =
    let bail () =
        asprintf "types `%a` and `%a` are not compatible" Dtyp.fmt t_1 Dtyp.fmt t_2
        |> Exc.throw
    in

    (* log_0 "type-checking@.  %a@.  %a@." Dtyp.fmt t_1 Dtyp.fmt t_2; *)

    let rec go_down (builders : builders) (stack : stack) (to_do : to_do) =
        match to_do with
        | [] -> go_up builders stack
        | (t_1, t_2) :: to_do -> (
            annot_check t_1.alias t_2.alias bail;
            let builders, stack, to_do =
                match t_1.typ, t_2.typ with
                | Dtyp.Leaf (Var _), _ ->
                    Exc.throw "first type given to `unify` cannot contain type variables"
                | _, Dtyp.Leaf (Var i) ->
                    set_var_def cxt i t_1;
                    builders, t_1 :: stack, to_do
                | Dtyp.Leaf _, Dtyp.Leaf _ ->
                    check t_1 t_2;
                    let dtyp = if t_1.alias = None then t_2 else t_1 in
                    builders, dtyp :: stack, to_do

                | Dtyp.Pair (lft_1, rgt_1), Dtyp.Pair (lft_2, rgt_2) -> (
                    let lft_name = if lft_1.name = None then lft_2.name else lft_1.name in
                    let rgt_name = if rgt_1.name = None then rgt_2.name else rgt_1.name in
                    let to_do =
                        (lft_1.inner, lft_2.inner) :: (rgt_1.inner, rgt_2.inner) :: to_do
                    in
                    let builder =
                        build_two "pair" (
                            fun lft rgt ->
                                let lft = lft |> Dtyp.mk_named lft_name in
                                let rgt = rgt |> Dtyp.mk_named rgt_name in
                                Dtyp.Pair (lft, rgt) |> Dtyp.mk
                        )
                    in
                    builder :: builders, stack, to_do
                )
                | Dtyp.Or (lft_1, rgt_1), Dtyp.Or (lft_2, rgt_2) -> (
                    let lft_name = if lft_1.name = None then lft_2.name else lft_1.name in
                    let rgt_name = if rgt_1.name = None then rgt_2.name else rgt_1.name in
                    let to_do =
                        (lft_1.inner, lft_2.inner) :: (rgt_1.inner, rgt_2.inner) :: to_do
                    in
                    let builder =
                        build_two "or" (
                            fun lft rgt ->
                                let lft = lft |> Dtyp.mk_named lft_name in
                                let rgt = rgt |> Dtyp.mk_named rgt_name in
                                Dtyp.Or (lft, rgt) |> Dtyp.mk
                        )
                    in
                    builder :: builders, stack, to_do
                )

                | Dtyp.Option t_1, Dtyp.Option t_2 -> (
                    let name = if t_1.name = None then t_2.name else t_1.name in
                    let to_do = (t_1.inner, t_2.inner) :: to_do in
                    let builder =
                        build_one "option" (
                            fun sub ->
                                let sub = sub |> Dtyp.mk_named name in
                                Dtyp.Option sub |> Dtyp.mk
                        )
                    in
                    builder :: builders, stack, to_do
                )
                | Dtyp.List t_1, Dtyp.List t_2 -> (
                    let to_do = (t_1, t_2) :: to_do in
                    let builder = build_one "list" (fun sub -> Dtyp.List sub |> Dtyp.mk) in
                    builder :: builders, stack, to_do
                )
                | Dtyp.Set t_1, Dtyp.Set t_2 -> (
                    let to_do = (t_1, t_2) :: to_do in
                    let builder = build_one "list" (fun sub -> Dtyp.Set sub |> Dtyp.mk) in
                    builder :: builders, stack, to_do
                )
                | Dtyp.Contract t_1, Dtyp.Contract t_2 -> (
                    let to_do = (t_1, t_2) :: to_do in
                    let builder = build_one "list" (fun sub -> Dtyp.Contract sub |> Dtyp.mk) in
                    builder :: builders, stack, to_do
                )

                | Dtyp.Map (lft_1, rgt_1), Dtyp.Map (lft_2, rgt_2) -> (
                    let to_do =
                        (lft_1, lft_2) :: (rgt_1, rgt_2) :: to_do
                    in
                    let builder =
                        build_two "map" (
                            fun lft rgt -> Dtyp.Map (lft, rgt) |> Dtyp.mk
                        )
                    in
                    builder :: builders, stack, to_do
                )
                | Dtyp.BigMap (lft_1, rgt_1), Dtyp.BigMap (lft_2, rgt_2) -> (
                    let to_do =
                        (lft_1, lft_2) :: (rgt_1, rgt_2) :: to_do
                    in
                    let builder =
                        build_two "map" (
                            fun lft rgt -> Dtyp.BigMap (lft, rgt) |> Dtyp.mk
                        )
                    in
                    builder :: builders, stack, to_do
                )
                | Dtyp.Lambda (lft_1, rgt_1), Dtyp.Lambda (lft_2, rgt_2) -> (
                    let to_do =
                        (lft_1, lft_2) :: (rgt_1, rgt_2) :: to_do
                    in
                    let builder =
                        build_two "map" (
                            fun lft rgt -> Dtyp.Lambda (lft, rgt) |> Dtyp.mk
                        )
                    in
                    builder :: builders, stack, to_do
                )

                | _ -> asprintf "cannot unify %a and %a" Dtyp.fmt t_1 Dtyp.fmt t_2 |> Exc.throw
            in
            go_down builders stack to_do
        )

    and go_up (builders : builders) (stack : stack) =
        match builders with
        | [] -> (
            match stack with
            | [result] -> result
            | [] ->
                Exc.throw "unreachable: unification stack is empty"
            | _ ->
                Exc.throw
                    "unreachable: done unifying, but unification stack has more than one element"
        )
        | builder :: builders ->
            let stack = builder stack in
            go_up builders stack

    in

    (fun () -> go_down [] [] [ t_1, t_2 ])
    |> Exc.chain_err (
        fun () -> asprintf "while unifying %a and %a" Dtyp.fmt t_1 Dtyp.fmt t_2
    )