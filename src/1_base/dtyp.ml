(* Datatype types and helpers. *)

open Common

type leaf =
| Str
| Nat
| Int
| Bytes
| Bool
| Unit
| Mutez
| Address
| Operation
| Key
| KeyH
| Signature
| Timestamp

let fmt_leaf (fmt : formatter) (typ : leaf) = match typ with
| Str -> fprintf fmt "string"
| Nat -> fprintf fmt "nat"
| Int -> fprintf fmt "int"
| Bytes -> fprintf fmt "bytes"
| Bool -> fprintf fmt "bool"
| Unit -> fprintf fmt "unit"
| Mutez -> fprintf fmt "mutez"
| Address -> fprintf fmt "address"
| Operation -> fprintf fmt "operation"
| Key -> fprintf fmt "key"
| KeyH -> fprintf fmt "key_hash"
| Signature -> fprintf fmt "signature"
| Timestamp -> fprintf fmt "timestamp"

let leaf_of_string (s : string) : leaf option = match s with
| "string" -> Some Str
| "nat" -> Some Nat
| "int" -> Some Int
| "bytes" -> Some Bytes
| "bool" -> Some Bool
| "unit" -> Some Unit
| "mutez" -> Some Mutez
| "address" -> Some Address
| "operation" -> Some Operation
| "key" -> Some Key
| "key_hash" -> Some KeyH
| "signature" -> Some Signature
| "timestamp" -> Some Timestamp
| _ -> None

type alias = Annot.Typ.t option

type named = {
    inner : t ;
    name : Annot.Field.t option ;
}

and dtyp =
| Leaf of leaf

| Pair of named * named
| Or of named * named
| Option of named

| List of t
| Set of t
| Contract of t

| Map of t * t
| BigMap of t * t
| Lambda of t * t

and t = {
    typ : dtyp ;
    alias : alias ;
}

let mk ?alias:(alias = None) (typ : dtyp) : t = { typ ; alias }

let mk_named (name : Annot.Field.t option) (inner : t) : named =
    { inner ; name }

let mk_leaf ?alias:(alias = None) (leaf : leaf) : t = { typ = Leaf leaf ; alias }

let rename (alias : alias) ({ typ ; _ } : t) : t = { typ ; alias }
let rename_if_some (nu_alias : alias) ({ typ ; alias } : t) : t =
    match nu_alias with
    | None -> { typ ; alias }
    | Some _ -> { typ ; alias = nu_alias }

let fmt (fmtt : formatter) (typ : t) =
    (* Forms the stack frame for types with annotated subtypes. *)
    let frame_of_named (named : named) : (Fmt.sep * t * Annot.Field.t option * Fmt.sep) =
        match named.name with
        | Some name ->
            (fun () -> fprintf fmtt "@ ("),
            named.inner,
            Some name,
            (fun () -> fprintf fmtt ")")
        | None ->
            (fun () -> fprintf fmtt "@ "),
            named.inner,
            None,
            ignore
    in

    (* Loop with a manual stack.
    
        A frame on the stack is of the form `[(pre, t, post)], close`. First element is a list of
        things to print `t` with something to print before `pre` and after `post`. Once everything
        in the list is printed, `close` prints whatever needs to be printed to close the sequence.
    *)
    let rec loop (
        stack : ( (Fmt.sep * t * Annot.Field.t option * Fmt.sep) list * Fmt.seq_end) list
    ) : unit =
        match stack with

        (* Stack is empty, we're done. *)
        | [] -> ()

        (* Done with the current sequence of things to print. Close and keep moving. *)
        | ([], seq_end) :: stack ->
            seq_end ();
            loop stack

        (* There's stuff to print in the current sequence, let's do this. *)
        | (((pre, to_do, field, post) :: to_do_tail), seq_end) :: stack ->
            (* Remember there might be more stuff to print in this sequence. *)
            let stack = (to_do_tail, seq_end) :: stack in
            let { typ = to_do ; alias } = to_do in
            let fmt_field (fmt : formatter) (field : Annot.Field.t option) : unit =
                match field with
                | Some field -> fprintf fmt " %a" Annot.Field.fmt field
                | None -> ()
            in

            (* Print the part before printing `to_do` so that we don't forget. *)
            pre ();

            let fmt_alias (fmtt : formatter) () : unit =
                alias |> if_let_some (
                    fun alias -> fprintf fmtt " %a" Annot.Typ.fmt alias
                )
            in

            let stack = match to_do with
                | Leaf leaf ->
                    let alias_pre, alias_post =
                        match alias with
                        | None -> "", ""
                        | Some _ -> "(", ")"
                    in
                    fprintf fmtt "%s" alias_pre;
                    fmt_leaf fmtt leaf;
                    fmt_field fmtt field;
                    fmt_alias fmtt ();
                    fprintf fmtt "%s" alias_post;
                    post ();
                    stack

                | List sub ->
                    fprintf fmtt "(list%a%a " fmt_alias () fmt_field field;
                    (
                        [(ignore, sub, None, ignore)],
                        Fmt.unit_combine (Fmt.cls_paren fmtt) post
                    ) :: stack

                | Option sub ->
                    fprintf fmtt "(option%a%a " fmt_alias () fmt_field field;
                    (
                        [ frame_of_named sub ],
                        Fmt.unit_combine (Fmt.cls_paren fmtt) post
                    ) :: stack

                | Set sub ->
                    fprintf fmtt "(set%a%a " fmt_alias () fmt_field field;
                    (
                        [(ignore, sub, None, ignore)],
                        Fmt.unit_combine (Fmt.cls_paren fmtt) post
                    ) :: stack

                | Contract sub ->
                    fprintf fmtt "(contract%a%a " fmt_alias () fmt_field field;
                    (
                        [(ignore, sub, None, ignore)],
                        Fmt.unit_combine (Fmt.cls_paren fmtt) post
                    ) :: stack

                | Or (lft, rgt) ->
                    fprintf fmtt "@[@[<hv 4>(or%a%a" fmt_alias () fmt_field field;
                    (
                        [
                            frame_of_named lft ;
                            frame_of_named rgt
                        ],
                        (fun () -> fprintf fmtt "@]@,)@]" ; post ())
                    ) :: stack

                | Pair (lft, rgt) ->
                    fprintf fmtt "@[@[<hv 4>(pair%a%a" fmt_alias () fmt_field field;
                    (
                        [
                            frame_of_named lft ;
                            frame_of_named rgt
                        ],
                        (fun () -> fprintf fmtt "@])@]" ; post ())
                    ) :: stack

                | Map (k, v) ->
                    fprintf fmtt "@[@[<hv 4>(map%a%a@ " fmt_alias () fmt_field field;

                    (
                        [ (ignore, k, None, ignore) ; (Fmt.sep_spc fmtt, v, None, ignore) ],
                        Fmt.unit_combine (Fmt.cls_of fmtt "@]@,)@]") post
                    ) :: stack

                | BigMap (k, v) ->
                    fprintf fmtt "@[@[<hv 4>(big_map%a%a@ " fmt_alias () fmt_field field;

                    (
                        [ (ignore, k, None, ignore) ; (Fmt.sep_spc fmtt, v, None, ignore) ],
                        Fmt.unit_combine (fun () -> fprintf fmtt "@]@,)@]") post
                    ) :: stack

                | Lambda (dom, codom) ->
                    fprintf fmtt "@[@[<hv 4>(lambda%a%a@ " fmt_alias () fmt_field field;

                    (
                        [ (ignore, dom, None, ignore) ; (Fmt.sep_spc fmtt, codom, None, ignore) ],
                        Fmt.unit_combine (fun () -> fprintf fmtt "@]@,)@]") post
                    ) :: stack
        in
        loop stack
    in
    fprintf fmtt "@[";
    loop [ [ignore, typ, None, ignore], ignore ];
    fprintf fmtt "@]"

module Inspect = struct

    let either (dtyp : t) : t * t =
        match dtyp.typ with
        | Or (lft, rgt) -> lft.inner, rgt.inner
        | _ -> asprintf "expected union type, found %a" fmt dtyp |> Exc.throw

    let option (dtyp : t) : t =
        match dtyp.typ with
        | Option sub -> sub.inner
        | _ -> asprintf "expected option type, found %a" fmt dtyp |> Exc.throw

    let list (dtyp : t) : t =
        match dtyp.typ with
        | List sub -> sub
        | _ -> asprintf "expected list type, found %a" fmt dtyp |> Exc.throw

    let pair (dtyp : t) : t * t =
        match dtyp.typ with
        | Pair (lft, rgt) -> lft.inner, rgt.inner
        | _ -> asprintf "expected pair type, found %a" fmt dtyp |> Exc.throw

    let set (dtyp : t) : t =
        match dtyp.typ with
        | Set dtyp -> dtyp
        | _ -> asprintf "expected set type, found %a" fmt dtyp |> Exc.throw

    let map (dtyp : t) : t * t =
        match dtyp.typ with
        | Map (k, v)
        | BigMap (k, v) -> k, v
        | _ -> asprintf "expected map type, found %a" fmt dtyp |> Exc.throw

    let iter_elm (dtyp : t) : t =
        match dtyp.typ with
        | List sub
        | Set sub -> sub
        | Map (key, value) ->
            let key = mk_named None key in
            let value = mk_named None value in
            Pair (key, value) |> mk
        | _ -> asprintf "expected collection type, found %a" fmt dtyp |> Exc.throw

end

let unit : t = mk_leaf Unit
let int : t = mk_leaf Int
let nat : t = mk_leaf Nat
let timestamp : t = mk_leaf Timestamp

let annot_check (a_1 : 'a option) (a_2 : 'a option) (bail : unit -> 'b) : 'b =
    match a_1, a_2 with
    | Some a_1, Some a_2 when a_1 <> a_2 -> bail ()
    | _ -> ()

(* # TODO

    - stackless
*)
let rec check (t_1 : t) (t_2 : t) : unit =
    let bail () =
        asprintf "types `%a` and `%a` are not compatible" fmt t_1 fmt t_2
        |> Exc.throw
    in
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
