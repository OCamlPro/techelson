open Base
open Common

(* Pushes a print stack. *)
let _print_stack () : Mic.t =
    Mic.Extension Mic.PrintStack |> Mic.mk

(* Pushes some money. *)
let push_mutez () : Mic.t = Dtyp.Mutez |> Dtyp.mk_leaf |> Values.mutez
(* Pushes a boolean. *)
let push_bool () : Mic.t = Dtyp.Bool |> Dtyp.mk_leaf |> Values.bool

(* Adds the operation on the top of the stack to a list and applies it. *)
let push_apply () : Mic.t =
    [
        Mic.Nil (Dtyp.Operation |> Dtyp.mk_leaf) |> Mic.mk ;
        Mic.Swap |> Mic.mk_leaf ;
        Mic.Cons |> Mic.mk_leaf ;
        Mic.Extension Mic.ApplyOps |> Mic.mk ;
    ]
    |> Mic.mk_seq
    |> Mic.comments [ "create a list of operations and apply" ]

(* Retrieves a contract from an address. *)
let get_contract (param_dtyp : Dtyp.t) (name : string) : Mic.t =
    [
        Mic.Contract (param_dtyp) |> Mic.mk ;
        Mic.IfNone (
            [
                Mic.Push (
                    Dtyp.Str |> Dtyp.mk_leaf,
                    Mic.Str (sprintf "unable to spawn contract `%s`" name)
                ) |> Mic.mk ;
                Mic.Failwith |> Mic.mk_leaf ;
            ] |> Mic.mk_seq,
            [] |> Mic.mk_seq
        ) |> Mic.mk ;
    ]
    |> Mic.mk_seq
    |> Mic.comments [ "retrieve contract from address, fail if none" ]

(* Pushes parameters for contract deployment. *)
let rec push_deploy_params (make_storage : unit -> Mic.t) : Mic.t =
    [
        make_storage () ;
        push_mutez () |> Mic.comments ["money transferred"] ;
        push_bool () |> Mic.comments ["delegatable"] ;
        push_bool () |> Mic.comments ["spendable"];

        Dtyp.Option (Dtyp.KeyH |> Dtyp.mk_leaf |> Dtyp.mk_named None) |> Dtyp.mk
        |> Values.from generate_contract generate_address |> Mic.mk_seq
        |> Mic.comments ["delegate"] ;

        Dtyp.KeyH |> Dtyp.mk_leaf
        |> Values.from generate_contract generate_address |> Mic.mk_seq
        |> Mic.comments ["manager"] ;
    ]
    |> Mic.mk_seq
    |> Mic.comments [ "creating contract creation parameters" ]

and generate_address (param : Dtyp.t) : Mic.t list =
    let storage = Dtyp.unit in
    let unit = Mic.U in
    let mic =
        [
            Mic.Drop |> Mic.mk_leaf |> Mic.comments ["discarding inputs"] ;
            Mic.Push (storage, unit) |> Mic.mk ;
            Mic.Nil (Dtyp.Operation |> Dtyp.mk_leaf) |> Mic.mk ;
            Mic.Pair |> Mic.mk_leaf ;
        ]
        |> Mic.mk_seq
    in
    let contract = Mic.mk_contract ~storage ~param mic in
    let deploy_params =
        push_deploy_params (fun () -> Mic.Push (storage, unit) |> Mic.mk)
    in
    deploy_params ::
    (Mic.CreateContract (Either.Lft (Some contract)) |> Mic.mk) ::
    (push_apply ()) ::
    []

and generate_contract (param : Dtyp.t) : Mic.t list =
    (generate_address param) @ [
        (get_contract param "test-generated anonymous contract")
    ]

let generate (contract : Contract.t) (name : string) : Testcase.t =
    (* Creates a storage value. *)
    let make_storage () : Mic.t =
        Values.from generate_contract generate_address contract.storage
        |> Mic.mk_seq
        |> Mic.comments [ sprintf "creating storage for contract `%s`" contract.name ]
    in

    (* Pushes a contract deployment. *)
    let push_deploy () : Mic.t =
        [
            push_deploy_params make_storage ;
            Mic.CreateContract (Either.Rgt contract.name) |> Mic.mk ;
        ]
        |> Mic.mk_seq
        |> Mic.comments [ sprintf "deploying contract `%s`" contract.name ]
    in

    (* Retrieves a contract from an address. *)
    let get_contract () : Mic.t =
        [
            Mic.Contract (contract.entry_param) |> Mic.mk ;
            Mic.IfNone (
                [
                    Mic.Push (
                        Dtyp.Str |> Dtyp.mk_leaf,
                        Mic.Str (sprintf "unable to spawn contract `%s`" contract.name)
                    ) |> Mic.mk ;
                    Mic.Failwith |> Mic.mk_leaf ;
                ] |> Mic.mk_seq,
                [] |> Mic.mk_seq
            ) |> Mic.mk ;
        ]
        |> Mic.mk_seq
        |> Mic.comments [ "retrieve contract from address, fail if none" ]
    in

    (* Creates a param value. *)
    let make_param () : Mic.t = Values.from generate_contract generate_address contract.entry_param |> Mic.mk_seq in

    (* Pushes a transfer. The address must be on top of the stack (it will be dupped). *)
    let push_transfer () : Mic.t =
        [
            Mic.Dup |> Mic.mk_leaf ;
            get_contract () ;
            push_mutez () ;
            make_param () ;
            Mic.TransferTokens |> Mic.mk_leaf ;
        ]
        |> Mic.mk_seq
        |> Mic.comments [ "create transfer operation" ]
    in

    let rec generate_transfers (acc : Mic.t list) : Mic.t list =
        let acc =
            push_transfer ()
            :: push_apply ()
            :: acc
        in
        if Rng.Test.transfer () then generate_transfers acc else acc
    in

    (
        push_deploy ()
        :: push_apply ()

        :: generate_transfers []
    )
    |> Mic.mk_seq
    |> Testcase.mk name Source.Gen
