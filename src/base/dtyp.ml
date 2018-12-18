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

type 'sub named = {
    typ : 'sub ;
    name : string option ;
}

type 'sub dtyp =
| Leaf of leaf

| List of 'sub
| Option of 'sub
| Set of 'sub
| Contract of 'sub

| Pair of (t named) list
| Or of (t named) list
| Map of 'sub * 'sub
| BigMap of 'sub * 'sub

and t = {
    typ : t dtyp ;
    alias : string option ;
}

let mk (alias : string option) (typ : t dtyp) : t = { typ ; alias }

let fmt (fmt : formatter) (typ : t) =
    let rec loop (stack : ( (Fmt.sep * t * Fmt.sep) list * Fmt.seq_end) list) : unit = match stack with
    | [] -> ()
    | ([], seq_end) :: tail ->
        seq_end ();
        loop tail
    | (((sep_1, to_do, sep_2) :: to_do_tail), seq_end) :: tail ->
        sep_1 ();
        let tail = (to_do_tail, seq_end) :: tail in
        let { typ = to_do ; alias } = to_do in
        let fmt_alias (fmt : formatter) () : unit =
            alias |> if_let_some (
                fun alias -> fprintf fmt " :%s" alias
            )
        in
        let tail = match to_do with
            | Leaf leaf ->
                fmt_leaf fmt leaf;
                fmt_alias fmt ();
                sep_2 ();
                tail

            | List sub ->
                fprintf fmt "(list%a " fmt_alias ();

                ([(ignore, sub, ignore)], Fmt.unit_combine (Fmt.cls_paren fmt) sep_2) :: tail
            | Option sub ->
                fprintf fmt "(option%a " fmt_alias ();

                ([(ignore, sub, ignore)], Fmt.unit_combine (Fmt.cls_paren fmt) sep_2) :: tail
            | Set sub ->
                fprintf fmt "(set%a " fmt_alias ();

                ([(ignore, sub, ignore)], Fmt.unit_combine (Fmt.cls_paren fmt) sep_2) :: tail
            | Contract sub ->
                fprintf fmt "(contract%a " fmt_alias ();

                ([(ignore, sub, ignore)], Fmt.unit_combine (Fmt.cls_paren fmt) sep_2) :: tail

            | Or lst -> (
                assert (2 <= Lst.len lst);
                fprintf fmt "@[<v>@[<v 4>(or%a@ " fmt_alias ();

                let hd : t named = List.hd lst in
                let tl : (t named) list = List.tl lst in

                (
                    (
                        (fun () -> if_let_some (fun _ -> fprintf fmt "(") hd.name),
                        hd.typ,
                        fun () -> if_let_some (fun name -> fprintf fmt " %%%s)" name) hd.name
                    ) :: (
                        tl |> List.map (
                            fun (stuff : t named) ->
                                (fun () ->
                                    fprintf fmt "@ ";
                                    if_let_some (fun _ -> fprintf fmt "(") stuff.name
                                ),
                                stuff.typ,
                                fun () -> if_let_some (fun name -> fprintf fmt " %%%s)" name) stuff.name
                        )
                    ),
                    Fmt.unit_combine (fun () -> fprintf fmt "@]@,)@]") sep_2
                ) :: tail
            )

            | Pair lst ->
                assert (2 <= Lst.len lst);
                fprintf fmt "@[<v>@[<v 4>(pair%a@ " fmt_alias ();

                let hd : t named = List.hd lst in
                let tl : (t named) list = List.tl lst in

                (
                    (
                        (fun () -> if_let_some (fun _ -> fprintf fmt "(") hd.name),
                        hd.typ,
                        fun () -> if_let_some (fun name -> fprintf fmt " %%%s)" name) hd.name
                    ) :: (
                        tl |> List.map (
                            fun (stuff : t named) ->
                                (fun () ->
                                    fprintf fmt "@ ";
                                    if_let_some (fun _ -> fprintf fmt "(") stuff.name
                                ),
                                stuff.typ,
                                fun () -> if_let_some (fun name -> fprintf fmt " %%%s)" name) stuff.name
                        )
                    ),
                    Fmt.unit_combine (fun () -> fprintf fmt "@]@,)@]") sep_2
                ) :: tail

            | Map (k, v) ->
                fprintf fmt "@[@[<hv 4>(map%a@ " fmt_alias ();

                (
                    [ (ignore, k, ignore) ; (Fmt.sep_spc fmt, v, ignore) ],
                    Fmt.unit_combine (Fmt.cls_of fmt "@]@,)@]") sep_2
                ) :: tail

            | BigMap (k, v) ->
                fprintf fmt "@[@[<hv 4>(big_map%a@ " fmt_alias ();

                (
                    [ (ignore, k, ignore) ; (Fmt.sep_spc fmt, v, ignore) ],
                    Fmt.unit_combine (fun () -> fprintf fmt "@]@,)@]") sep_2
                ) :: tail
        in
        loop tail
    in
    fprintf fmt "@[";
    loop [ [ignore, typ, ignore], ignore ];
    fprintf fmt "@]"
    
let parse (token : string) (name : string option) (dtyps : (t * string option) list) : t =
    let arity_check (expected : int) : unit =
        Check.arity "type argument" expected (
            fun () -> sprintf "type constructor `%s`" token
        ) dtyps
    in
    let arity_check_ge (expected : int) : unit =
        Check.arity_ge "type argument" expected (
            fun () -> sprintf "type constructor `%s`" token
        ) dtyps
    in
    let no_annot (dtyps : (t * string option) list) : t list =
        dtyps |> List.map (
            fun (dtyp, annot) ->
                if_let_some (
                    fun annot ->
                        [
                            sprintf
                                "parameters of type constructor `%s` \
                                do not accept annoted parameters"
                                token;
                            asprintf "found annotation `:%s` on type %a" annot fmt dtyp
                        ] |> Exc.throws
                ) annot;
                dtyp
        )
    in
    let inner () =
        match token with
        | "list" ->
            arity_check 1;
            List (no_annot dtyps |> List.hd)
        | "option" ->
            arity_check 1;
            Option (no_annot dtyps |> List.hd)
        | "set" ->
            arity_check 1;
            Set (no_annot dtyps |> List.hd)
        | "contract" ->
            arity_check 1;
            Contract (no_annot dtyps |> List.hd)
        | "pair" ->
            arity_check_ge 2;
            Pair (List.map (fun (typ, name) -> { name ; typ }) dtyps)
        | "or" ->
            arity_check_ge 2;
            Or (List.map (fun (typ, name) -> { name ; typ }) dtyps)
        | "map" ->
            arity_check 2;
            let dtyps = no_annot dtyps in
            Map (List.hd dtyps, List.tl dtyps |> List.hd)
        | "big_map" ->
            arity_check 2;
            let dtyps = no_annot dtyps in
            BigMap (List.hd dtyps, List.tl dtyps |> List.hd)
        | _ -> (
            match leaf_of_string token with
            | None ->
                sprintf "unknown type constructor `%s`" token
                |> Exc.throw
            | Some leaf ->
                arity_check 0;
                Leaf leaf
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
                                fun fmtt (dtyp, name) ->
                                match name with
                                | None -> fmt fmtt dtyp
                                | Some name -> fprintf fmtt "(%a :%s)" fmt dtyp name
                            )
                        )
                        dtyps
            in
            sprintf "while parsing \"%s%s\"" token dtyps
    ) inner
    |> fun dtyp -> mk name dtyp