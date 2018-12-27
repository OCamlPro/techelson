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

type named = {
    inner : t ;
    name : Annot.Field.t option ;
}

and dtyp =
| Leaf of leaf

| List of t
| Option of t
| Set of t
| Contract of t

| Pair of named * named
| Or of named * named
| Map of t * t
| BigMap of t * t

and t = {
    typ : dtyp ;
    alias : Annot.Typ.t option ;
}

let mk ?alias:(alias = None) (typ : dtyp) : t = { typ ; alias }

let fmt (fmtt : formatter) (typ : t) =
    (* Forms the stack frame for types with annotated subtypes. *)
    let frame_of_named (named : named) : (Fmt.sep * t * Fmt.sep) =
        match named.name with
        | Some name ->
            (fun () -> fprintf fmtt "@ ("),
            named.inner,
            (fun () -> fprintf fmtt " %a)" Annot.Field.fmt name)
        | None ->
            (fun () -> fprintf fmtt "@ "),
            named.inner,
            ignore
    in

    (* Loop with a manual stack.
    
        A frame on the stack is of the form `[(pre, t, post)], close`. First element is a list of
        things to print `t` with something to print before `pre` and after `post`. Once everything
        in the list is printed, `close` prints whatever needs to be printed to close the sequence.
    *)
    let rec loop (stack : ( (Fmt.sep * t * Fmt.sep) list * Fmt.seq_end) list) : unit =
        match stack with

        (* Stack is empty, we're done. *)
        | [] -> ()

        (* Done with the current sequence of things to print. Close and keep moving. *)
        | ([], seq_end) :: stack ->
            seq_end ();
            loop stack

        (* There's stuff to print in the current sequence, let's do this. *)
        | (((pre, to_do, post) :: to_do_tail), seq_end) :: stack ->
            (* Remember there might be more stuff to print in this sequence. *)
            let stack = (to_do_tail, seq_end) :: stack in
            let { typ = to_do ; alias } = to_do in

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
                    fmt_alias fmtt ();
                    fprintf fmtt "%s" alias_post;
                    post ();
                    stack

                | List sub ->
                    fprintf fmtt "(list%a " fmt_alias ();
                    (
                        [(ignore, sub, ignore)],
                        Fmt.unit_combine (Fmt.cls_paren fmtt) post
                    ) :: stack

                | Option sub ->
                    fprintf fmtt "(option%a " fmt_alias ();
                    (
                        [(ignore, sub, ignore)],
                        Fmt.unit_combine (Fmt.cls_paren fmtt) post
                    ) :: stack

                | Set sub ->
                    fprintf fmtt "(set%a " fmt_alias ();
                    (
                        [(ignore, sub, ignore)],
                        Fmt.unit_combine (Fmt.cls_paren fmtt) post
                    ) :: stack

                | Contract sub ->
                    fprintf fmtt "(contract%a " fmt_alias ();
                    (
                        [(ignore, sub, ignore)],
                        Fmt.unit_combine (Fmt.cls_paren fmtt) post
                    ) :: stack

                | Or (lft, rgt) ->
                    fprintf fmtt "@[@[<hv 4>(or%a" fmt_alias ();
                    (
                        [
                            frame_of_named lft ;
                            frame_of_named rgt
                        ],
                        (fun () -> fprintf fmtt "@]@,@]" ; post ())
                    ) :: stack

                | Pair (lft, rgt) ->
                    fprintf fmtt "@[@[<hv 4>(pair%a" fmt_alias ();
                    (
                        [
                            frame_of_named lft ;
                            frame_of_named rgt
                        ],
                        (fun () -> fprintf fmtt "@])@]" ; post ())
                    ) :: stack

                | Map (k, v) ->
                    fprintf fmtt "@[@[<hv 4>(map%a@ " fmt_alias ();

                    (
                        [ (ignore, k, ignore) ; (Fmt.sep_spc fmtt, v, ignore) ],
                        Fmt.unit_combine (Fmt.cls_of fmtt "@]@,)@]") post
                    ) :: stack

                | BigMap (k, v) ->
                    fprintf fmtt "@[@[<hv 4>(big_map%a@ " fmt_alias ();

                    (
                        [ (ignore, k, ignore) ; (Fmt.sep_spc fmtt, v, ignore) ],
                        Fmt.unit_combine (fun () -> fprintf fmtt "@]@,)@]") post
                    ) :: stack
        in
        loop stack
    in
    fprintf fmtt "@[";
    loop [ [ignore, typ, ignore], ignore ];
    fprintf fmtt "@]"
