open Base
open Common

module Const = struct
    let unit : Mic.const = Mic.U
    let bool () : Mic.const =
        Mic.Bool (Rng.bool ())
    let int () : Mic.const =
        Mic.Int (Rng.big_nat ())
    let string () : Mic.const =
        Mic.Str (Rng.string ())
    let bytes () : Mic.const =
        Mic.Bytes (Rng.string ())

    let tstamp () : Mic.const =
        Mic.Int (Rng.tiny_nat ())

    let int64_10000 = Int64.of_int 10000
    let mutez () : Mic.const =
        let int64 = Rng.int64 () in
        let int64 =
            if Int64.compare int64 int64_10000 > 0 then
                Int64.div int64 int64_10000
            else
                int64
        in
        Mic.Int (Int64.to_string int64)

    let key () : Mic.const =
        Mic.Str (Rng.key ())

    let lft (inner : Mic.const) : Mic.const = Mic.Lft inner
    let rgt (inner : Mic.const) : Mic.const = Mic.Rgt inner

    let none : Mic.const = Mic.No
    let some (inner : Mic.const) : Mic.const = Mic.So inner
end

let unit (dtyp : Dtyp.t) : Mic.t =
    Mic.Push (dtyp, Const.unit) |> Mic.mk

let bool (dtyp : Dtyp.t) : Mic.t =
    Mic.Push (dtyp, Const.bool ()) |> Mic.mk

let int (dtyp : Dtyp.t) : Mic.t =
    let int = Const.int () in
    let head = Mic.Push (dtyp, int) |> Mic.mk in
    let tail =
        if int <> Mic.Int "0" && Rng.bool () then [ Mic.Neg |> Mic.mk_leaf ] else []
    in
    head :: tail |> Mic.mk_seq

let nat (dtyp : Dtyp.t) : Mic.t =
    Mic.Push (dtyp, Mic.Int (Rng.big_nat ())) |> Mic.mk

let str (dtyp : Dtyp.t) : Mic.t =
    Mic.Push (dtyp, Const.string ()) |> Mic.mk

let bytes (dtyp : Dtyp.t) : Mic.t =
    Mic.Push (dtyp, Const.bytes ()) |> Mic.mk

let mutez (dtyp : Dtyp.t) : Mic.t =
    Mic.Push (dtyp, Const.mutez ()) |> Mic.mk

let key (dtyp : Dtyp.t) : Mic.t =
    Mic.Push (dtyp, Const.key ()) |> Mic.mk

let timestamp (dtyp : Dtyp.t) : Mic.t =
    Mic.Push (dtyp, Const.tstamp ()) |> Mic.mk

let hash () : Mic.t =
    Mic.Hash (
        match Rng.pos_int ~bound:(Some 3) () with
        | 0 -> Mic.B58Check
        | 1 -> Mic.Blake2B
        | 2 -> Mic.Sha256
        | 3 -> Mic.Sha512
        (* | n -> log_0 "n: %i@." n ; Exc.unreachable () *)
        | _ -> Exc.unreachable ()
    )
    |> Mic.mk_leaf

(** Issues a rename instruction if the datatype is named. *)
let rename (dtyp : Dtyp.t) : Mic.t list =
    match dtyp.alias with
    | None -> []
    | Some alias -> [
        Mic.Rename |> Mic.mk_leaf ~typs:([alias])
    ]

(** Issues a cast instruction. *)
let cast (dtyp : Dtyp.t) : Mic.t list =
    [Mic.Cast dtyp |> Mic.mk]

let key_hash (dtyp : Dtyp.t) : Mic.t list =
    let push_key = key (Dtyp.Key |> Dtyp.mk_leaf) in
    let hash = hash () in
    let tail = rename dtyp in
    push_key :: hash :: tail

type frame = {
    (* Instruction that must preceed the datatype we're handling, **in reverse order**. *)
    mic_pref : Mic.t list ;
    (* Next datatypes to generate values for. *)
    to_do : ((Dtyp.t, Mic.t list) Either.t) list ;
    (* Instruction that must succeed the datatypes we're handling, in normal order. *)
    mic_suff : Mic.t list ;
}
type stack = frame list

(** Builds some instructions that encode a collection.

    - `empty`: instructions creating an empty collection
    - `add_one`: instructions adding an element to the collection
*)
let build_coll
    (empty : Mic.t list)
    (suff : Mic.t list)
    (add_one : unit -> frame)
    (stack : stack)
    : stack
=
    let rec loop (stack : stack) : stack =
        let stack = (add_one ()) :: stack in
        if Rng.Coll.add_one () then loop stack else stack
    in

    let top = { mic_pref = empty ; to_do = [] ; mic_suff = suff } in
    let stack = top :: stack in

    if Rng.Coll.empty () then stack else loop stack

type contract_generator = Dtyp.t -> Mic.t list
type address_generator = Dtyp.t -> Mic.t list

let from
    (generate_contract : contract_generator)
    (generate_address : address_generator)
    (dtyp : Dtyp.t)
    : Mic.t list
=
    let rec go_down (stack : stack) (current : (Dtyp.t, Mic.t list) Either.t) : Mic.t list =
        (* log_4 "go down %a@." Dtyp.fmt dtyp; *)
        match current with
        | Either.Rgt mic -> go_up stack mic
        | Either.Lft dtyp -> (
            match dtyp.typ with
            | Dtyp.Leaf Unit -> go_up stack [unit dtyp]
            | Dtyp.Leaf Bool -> go_up stack [bool dtyp]
            | Dtyp.Leaf Int -> go_up stack [int dtyp]
            | Dtyp.Leaf Nat -> go_up stack [nat dtyp]
            | Dtyp.Leaf Str -> go_up stack [str dtyp]
            | Dtyp.Leaf Bytes -> go_up stack [bytes dtyp]
            | Dtyp.Leaf Key -> go_up stack [key dtyp]
            | Dtyp.Leaf Mutez -> go_up stack [mutez dtyp]
            | Dtyp.Leaf Timestamp -> go_up stack [timestamp dtyp]
            | Dtyp.Leaf Var _ -> Exc.throw "cannot generate values for type parameter"

            | Dtyp.Leaf KeyH -> go_up stack (key_hash dtyp)

            | Dtyp.Contract param -> generate_contract param |> go_up stack

            | Dtyp.Leaf Address ->
                Dtyp.Unit |> Dtyp.mk_leaf |> generate_address |> go_up stack

            | Dtyp.Leaf Signature ->
                go_up stack [Mic.Push (dtyp, Const.string ()) |> Mic.mk]

            | Dtyp.Pair (lft, rgt) ->
                let mic_pref = [] in
                let mic_suff = [ Mic.Pair |> Mic.mk_leaf ] in
                let to_do = [ Either.Lft lft.inner] in
                Either.Lft rgt.inner |> go_down ({ to_do ; mic_pref ; mic_suff } :: stack)

            | Dtyp.Or (lft, rgt) ->
                let mic_suff = rename dtyp in
                let mic_pref, to_do, (mic_suff, next) =
                    [], [], (
                        if Rng.bool () then (
                            (* log_4 "> left@."; *)
                            (Mic.Left rgt.inner |> Mic.mk) :: mic_suff, lft.inner
                        ) else (
                            (* log_4 "> right@."; *)
                            (Mic.Right lft.inner |> Mic.mk) :: mic_suff, rgt.inner
                        )
                    )
                in
                Either.Lft next |> go_down ({ to_do ; mic_pref ; mic_suff } :: stack)

            | Dtyp.Option sub -> (
                let fields = Opt.to_list sub.name in
                let mic_suff = rename dtyp in
                if Rng.bool () then (
                    let mic_pref, to_do, mic_suff =
                        [], [], (Mic.Som |> Mic.mk_leaf ~fields) :: mic_suff
                    in
                    Either.Lft sub.inner |> go_down ({ to_do ; mic_pref ; mic_suff } :: stack)
                ) else (
                    (Mic.Non sub.inner |> Mic.mk ~fields) :: mic_suff
                    |> go_up stack
                )
            )

            | Dtyp.List inner -> (
                let empty = [ Mic.Nil inner |> Mic.mk ] in
                let suff = rename dtyp in
                let add_one () : frame =
                    {
                        mic_pref = [] ;
                        to_do = [ Either.Lft inner ] ;
                        mic_suff = [ Mic.Cons |> Mic.mk_leaf ]
                    }
                in
                let stack =
                    build_coll empty suff add_one stack
                in

                go_up stack []
            )

            | Dtyp.Set inner -> (
                let empty = [ Mic.EmptySet inner |> Mic.mk ] in
                let suff = rename dtyp in
                let add_one () : frame =
                    {
                        mic_pref = [] ;
                        to_do = [
                            Either.Rgt [
                                Mic.Push (Dtyp.Bool |> Dtyp.mk_leaf, Mic.Bool true) |> Mic.mk
                            ] ;
                            Either.Lft inner
                        ] ;
                        mic_suff = [ Mic.Update |> Mic.mk_leaf ] ;
                    }
                in
                let stack =
                    build_coll empty suff add_one stack
                in

                go_up stack []
            )

            | Dtyp.Map (keys, vals) -> (
                let val_opt = Dtyp.Option (Dtyp.mk_named None vals) |> Dtyp.mk in
                let empty = [ Mic.EmptyMap (keys, vals) |> Mic.mk ] in
                let suff = rename dtyp in
                let add_one () : frame =
                    {
                        mic_pref = [] ;
                        to_do = [
                            Either.Lft val_opt;
                            Either.Lft keys;
                        ] ;
                        mic_suff = [ Mic.Update |> Mic.mk_leaf ] ;
                    }
                in
                let stack =
                    build_coll empty suff add_one stack
                in

                go_up stack []
            )

            | Dtyp.BigMap (keys, vals) -> (
                let val_opt = Dtyp.Option (Dtyp.mk_named None vals) |> Dtyp.mk in
                let empty = [ Mic.EmptyMap (keys, vals) |> Mic.mk ] in
                let suff = cast dtyp in
                let add_one () : frame =
                    {
                        mic_pref = [] ;
                        to_do = [
                            Either.Lft val_opt;
                            Either.Lft keys;
                        ] ;
                        mic_suff = [ Mic.Update |> Mic.mk_leaf ] ;
                    }
                in
                let stack =
                    build_coll empty suff add_one stack
                in

                go_up stack []
            )

            | Dtyp.Lambda _
            | Dtyp.Leaf Operation ->
                asprintf "cannot generate random values of type %a" Dtyp.fmt dtyp
                |> Exc.throw
        )

    and go_up (stack : stack) (current : Mic.t list) : Mic.t list =
        (* log_4 "current : %a@." Mic.fmt (Mic.mk_seq current); *)

        match stack with
        | [] -> current

        | { to_do = next :: to_do ; mic_pref ; mic_suff } :: stack_tail ->
            (* log_4 "  pref 1: %a@." Mic.fmt (Mic.mk_seq mic_pref); *)
            (* log_4 "  suff 1: %a@." Mic.fmt (Mic.mk_seq mic_suff); *)
            let mic_pref = List.rev_append current mic_pref in
            let stack = { to_do ; mic_pref ; mic_suff } :: stack_tail in
            go_down stack next

        | { to_do = [] ; mic_pref ; mic_suff } :: stack ->
            (* log_4 "  pref 2: %a@." Mic.fmt (Mic.mk_seq mic_pref); *)
            (* log_4 "  suff 2: %a@." Mic.fmt (Mic.mk_seq mic_suff); *)
            current @ mic_suff |> List.rev_append mic_pref |> go_up stack

    in

    (fun () -> Either.Lft dtyp |> go_down [])
    |> Exc.chain_err (
        fun () ->
            asprintf "while generating instruction creating a random value for %a"
                Dtyp.fmt dtyp
    )
