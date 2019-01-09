open Base
open Common

module type SigCxt = sig
    module Run : Eval.Sigs.SigInterpreter
    module Theory = Run.Theory

    (** Type of contexts. *)
    type t

    (** Empty context constructor. *)
    val empty : t

    val fmt : formatter -> t -> unit

    val is_done : t -> bool

    val of_cxt : Cxt.t -> t

    val env : t -> Run.Contracts.t

    val init : t -> Testcase.t -> unit
    val init_next : t -> bool

    val step : t -> bool

    val interp : t -> Run.t

    val terminate_run : t -> unit

    module Contracts : sig
        val add : Contract.t -> t -> unit
        val get : string -> t -> Contract.t

        module Live : sig
            val create : Theory.contract_params -> Contract.t -> t -> unit
            val get : Theory.Address.t -> t -> Run.Contracts.live
        end
    end
end


module Cxt (I : Eval.Sigs.SigInterpreter) : SigCxt = struct
    module Run = I
    module Theory = Run.Theory

    type t = {
        env : Run.Contracts.t ;
        mutable interp : Run.t option ;
        mutable ops : Theory.operation list ;
    }

    let empty = {
        env = Run.Contracts.empty ;
        interp = None ;
        ops = [] ;
    }

    let is_done (self : t) : bool =
        self.interp = None && self.ops = []

    let env (self : t) = self.env

    let init (self : t) (tc : Testcase.t) : unit =
        (
            match self.interp with
            | None -> ()
            | Some interp ->
                Run.src interp
                |> asprintf "trying to start a test run while one is already active for %a" Run.Src.fmt
                |> Exc.throw
        );
        let src = Run.Src.of_test tc in
        let interp = Run.init src self.env [] [ tc.code ] in
        self.interp <- Some interp

    (* let init_contract (self : t) (c : Contract.t) : unit =
        Exc.throw "contract runs are not implemented" *)

    let init_next (self : t) : bool =
        if self.interp <> None then (
            Exc.throw "trying to initialize the next operation but a run is in progress"
        );
        let rec loop () : bool =
            match self.ops with
            | next :: ops -> (
                self.ops <- ops;
                (
                    match next with
                    | Theory.CreateNamed (params, contract) -> (
                        (fun () -> Run.Contracts.Live.create params contract self.env)
                        |> Exc.chain_err (
                            fun () ->
                                asprintf
                                    "while spawning contract %s at address %a"
                                    contract.name Theory.Address.fmt params.address
                        )
                    )
                    | Theory.Create (params, contract) -> (
                        let contract = Contract.of_mic contract in
                        (fun () -> Run.Contracts.Live.create params contract self.env)
                        |> Exc.chain_err (
                            fun () ->
                                asprintf
                                    "while spawning contract %s at address %a"
                                    contract.name Theory.Address.fmt params.address
                        )
                    )
                    | Theory.InitNamed _ -> Exc.throw "contract spawning is not implemented" |> ignore
                );
                loop ()
            )
            | [] -> true
        in
        loop ()

    let step (self : t) : bool =
        match self.interp with
        | None ->
            Exc.throw "trying to run a test on uninitialized context"
        | Some interp -> Run.step interp

    let interp (self : t) : Run.t =
        match self.interp with
        | Some interp -> interp
        | None -> Exc.throw "trying to recover test runtime on uninitialized context"

    let terminate_run (self : t) =
        let interp =
            match self.interp with
            | Some interp -> interp
            | None -> Exc.throw "trying to terminate run on uninitialized context"
        in
        self.interp <- None;
        if not (Run.is_done interp) then (
            Exc.throw "trying to terminate a run that's still in progress"
        );
        match Run.src interp with
        | Run.Src.Test _ ->
            let operations =
                (fun () -> Run.stack interp |> Run.Stack.pop_operation_list)
                |> Exc.chain_err (
                    fun () -> Run.src interp |> asprintf "on test run %a" Run.Src.fmt
                )
            in
            self.ops <- operations @ self.ops
        | Run.Src.Contract _ ->
            Exc.throw "contract run termination is not supported yet"

    module Contracts = struct

        let add contract self = Run.Contracts.add contract self.env
        let get name self = Run.Contracts.get name self.env

        module Live = struct
            let create params contract self =
                Run.Contracts.Live.create params contract self.env
            let get address self = Run.Contracts.Live.get address self.env
        end
    end

    let of_cxt (cxt : Cxt.t) : t =
        let self = empty in
        Cxt.get_contracts cxt |> List.iter (
            fun c -> Contracts.add c self
        );
        self

    let fmt (fmt : formatter) (self : t) : unit =
        fprintf fmt "@[<v>";
        fprintf fmt "live contracts: @[";
        if Run.Contracts.Live.len self.env = 0 then (
            fprintf fmt "none"
        ) else (
            fprintf fmt "%a" Run.Contracts.Live.fmt self.env
        );
        fprintf fmt "@]@     operations: @[";
        if self.ops = [] then (
            fprintf fmt "none"
        ) else (
            self.ops |> Lst.fold (
                fun is_first op ->
                    if not is_first then (
                        fprintf fmt "@ "
                    );
                    fprintf fmt "%a" Theory.fmt_operation op;
                    false
            ) true |> ignore
        );
        fprintf fmt "@]@]"
end