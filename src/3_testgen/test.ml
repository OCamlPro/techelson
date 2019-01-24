open Base
open Common

let generate (contract : Contract.t) (name : string) : Testcase.t =
    log_0 "working on contract %s@." contract.name;
    (* Creates a storage value. *)
    let make_storage () : Mic.t =
        log_0 "making storage for %a@." Dtyp.fmt contract.storage;
        Values.from contract.storage
        |> Mic.mk_seq
        |> Mic.comments [ sprintf "creating storage for contract `%s`" contract.name ]
    in

    (* Pushes some money. *)
    let push_mutez () : Mic.t = Dtyp.Mutez |> Dtyp.mk_leaf |> Values.mutez in
    (* Pushes a boolean. *)
    let push_bool () : Mic.t = Dtyp.Bool |> Dtyp.mk_leaf |> Values.bool in

    (* Pushes parameters for contract deployment. *)
    let push_deploy_params () : Mic.t =
        [
            make_storage () ;
            push_mutez () |> Mic.comments ["money transferred"] ;
            push_bool () |> Mic.comments ["delegatable"] ;
            push_bool () |> Mic.comments ["spendable"];
            Dtyp.Option (Dtyp.KeyH |> Dtyp.mk_leaf) |> Dtyp.mk |> Values.from |> Mic.mk_seq
            |> Mic.comments ["delegate"] ;
            Dtyp.KeyH |> Dtyp.mk_leaf |> Values.from |> Mic.mk_seq
            |> Mic.comments ["manager"] ;
        ]
        |> Mic.mk_seq
        |> Mic.comments [ "creating contract creation parameters" ]
    in

    (* Pushes a contract deployment. *)
    let push_deploy () : Mic.t =
        [
            push_deploy_params () ;
            Mic.CreateContract (Either.Rgt contract.name) |> Mic.mk ;
        ]
        |> Mic.mk_seq
        |> Mic.comments [ sprintf "deploying contract `%s`" contract.name ]
    in

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
    let make_param () : Mic.t = Values.from contract.entry_param |> Mic.mk_seq in

    (* Pushes a transfer. The address must be on top of the stack (it will be dupped). *)
    let push_transfer () : Mic.t =
        [
            Mic.Dup |> Mic.mk_leaf ;
            get_contract () ;
            push_mutez () ;
            make_param () ;
        ]
        |> Mic.mk_seq
        |> Mic.comments [ "create transfer operation" ]
    in

    (
        push_deploy ()
        :: push_apply ()

        :: push_transfer ()
        :: push_apply ()

        :: push_transfer ()
        :: push_apply ()
        :: []
    )
    |> Mic.mk_seq
    |> Testcase.mk name Source.Gen
