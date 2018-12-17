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

type t =
| Leaf of leaf

| List of t
| Option of t
| Set of t
| Contract of t

| Pair of t * t
| Or of t * t
| Map of t * t
| BigMap of t * t

let str = Leaf Str
let int = Leaf Int
let nat = Leaf Nat
let bytes = Leaf Bytes
let bool = Leaf Bool
let unit = Leaf Unit
let mutez = Leaf Mutez
let address = Leaf Address
let operation = Leaf Operation
let key = Leaf Key
let key_hash = Leaf KeyH
let signature = Leaf Signature
let timestamp = Leaf Timestamp

let fmt (fmt : formatter) (typ : t) =
    let rec loop (stack : ( (Fmt.sep * t) list * Fmt.seq_end) list) : unit = match stack with
    | [] -> ()
    | ([], seq_end) :: tail ->
        seq_end ();
        loop tail
    | (((sep, to_do) :: to_do_tail), seq_end) :: tail ->
        sep ();
        let tail = (to_do_tail, seq_end) :: tail in
        let tail = match to_do with
            | Leaf leaf ->
                fmt_leaf fmt leaf;
                tail

            | List sub ->
                fprintf fmt "list (";

                ([(ignore, sub)], Fmt.cls_paren fmt) :: tail
            | Option sub ->
                fprintf fmt "option (";

                ([(ignore, sub)], Fmt.cls_paren fmt) :: tail
            | Set sub ->
                fprintf fmt "set (";

                ([(ignore, sub)], Fmt.cls_paren fmt) :: tail
            | Contract sub ->
                fprintf fmt "contract (";

                ([(ignore, sub)], Fmt.cls_paren fmt) :: tail

            | Or (lft, rgt) ->
                fprintf fmt "or @[(";

                (
                    [ (ignore, lft) ; ((fun () -> fprintf fmt ")@ ("), rgt) ],
                    Fmt.cls_of fmt ")"
                ) :: tail

            | Pair (lft, rgt) ->
                fprintf fmt "pair @[(";

                (
                    [ (ignore, lft) ; ((fun () -> fprintf fmt ")@ ("), rgt) ],
                    Fmt.cls_of fmt ")"
                ) :: tail

            | Map (k, v) ->
                fprintf fmt "map @[(";

                (
                    [ (ignore, k) ; ((fun () -> fprintf fmt ")@ ("), v) ],
                    Fmt.cls_of fmt ")"
                ) :: tail

            | BigMap (k, v) ->
                fprintf fmt "big_map @[(";

                (
                    [ (ignore, k) ; ((fun () -> fprintf fmt ")@ ("), v) ],
                    Fmt.cls_of fmt ")"
                ) :: tail
        in
        loop tail
    in
    fprintf fmt "@[";
    loop [ [ignore, typ], ignore ];
    fprintf fmt "@]"
    
let parse (token : string) (dtyps : t list) : t =
    let arity_check (expected : int) : unit =
        Check.arity "type argument" expected (
            fun () -> sprintf "type constructor `%s`" token
        ) dtyps
    in
    let inner () =
        match token with
        | "list" ->
            arity_check 1;
            List (List.hd dtyps)
        | "option" ->
            arity_check 1;
            Option (List.hd dtyps)
        | "set" ->
            arity_check 1;
            Set (List.hd dtyps)
        | "contract" ->
            arity_check 1;
            Contract (List.hd dtyps)
        | "pair" ->
            arity_check 2;
            Pair (List.hd dtyps, List.tl dtyps |> List.hd)
        | "or" ->
            arity_check 2;
            Or (List.hd dtyps, List.tl dtyps |> List.hd)
        | "map" ->
            arity_check 2;
            Map (List.hd dtyps, List.tl dtyps |> List.hd)
        | "big_map" ->
            arity_check 2;
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
                if dtyps = [] then "" else asprintf " %a" (Fmt.fmt_list Fmt.sep_spc fmt) dtyps
            in
            sprintf "while parsing \"%s%s\"" token dtyps
    ) inner