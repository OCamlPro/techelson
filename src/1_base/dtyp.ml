(* Datatype types and helpers. *)

open Common

type tvar = int

let fmt_tvar (fmt : formatter) (self : tvar) : unit =
    fprintf fmt "'t_%i" self

let tvar_counter : int ref = ref 0
let fresh_tvar () : tvar =
    let res = !tvar_counter in
    tvar_counter := 1 + !tvar_counter;
    res

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
| Var of tvar

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
| Var v -> fmt_tvar fmt v

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

let mk_var ?alias:(alias = None) () : t =
    Var (fresh_tvar ()) |> mk_leaf ~alias

let is_comparable (dtyp : t) : bool =
    match dtyp.typ with
    | Leaf Int | Leaf Nat | Leaf Str | Leaf Bytes | Leaf Mutez | Leaf Bool
    | Leaf KeyH | Leaf Timestamp -> true
    | _ -> false

let rename (alias : alias) ({ typ ; _ } : t) : t = { typ ; alias }
let rename_if_some (nu_alias : alias) ({ typ ; alias } : t) : t =
    match nu_alias with
    | None -> { typ ; alias }
    | Some _ -> { typ ; alias = nu_alias }

let fmt (fmtt : formatter) (typ : t) =
    (* Forms the stack frame for types with annotated subtypes. *)
    let frame_of_named (named : named) : (Fmt.sep * t * Annot.Field.t option * Fmt.sep) =
        (fun () -> fprintf fmtt "@ "),
        named.inner,
        named.name,
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
                        if alias = None && field = None
                        then ("", "") else "(", ")"
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
                    fprintf fmtt "(option%a%a" fmt_alias () fmt_field field;
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
                        Fmt.unit_combine (fun () -> fprintf fmtt "@]@,)@]") post
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
    let lambda (dtyp : t) : t * t =
        match dtyp.typ with
        | Lambda (dom, codom) -> dom, codom
        | _ -> asprintf "expected lambda, found %a" fmt dtyp |> Exc.throw

end

let unit : t = mk_leaf Unit
let int : t = mk_leaf Int
let nat : t = mk_leaf Nat
let timestamp : t = mk_leaf Timestamp