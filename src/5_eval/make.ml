open Base
open Base.Common

module Stack (S : Sigs.SigStackRaw)
    : Sigs.SigStack with type t = S.t and module Theory = S.Theory
= struct
    include S
    let pop_bool (self : t) : bool * Dtyp.t =
        let run () =
            match pop self with
            | Theory.C (Theory.Cmp.B b), dtyp -> b, dtyp
            | v, dtyp ->
                asprintf "found a value of type %a : %a" Dtyp.fmt dtyp Theory.fmt v
                |> Exc.throw
        in
        run |> Exc.chain_err (
            fun () -> "while popping a bool from the stack"
        )

    let pop_int (self : t) : Theory.Cmp.Int.t * Dtyp.t =
        let run () =
            match pop self with
            | Theory.C (Theory.Cmp.I i), dtyp -> i, dtyp
            | v, dtyp ->
                asprintf "found a value of type %a : %a" Dtyp.fmt dtyp Theory.fmt v
                |> Exc.throw
        in
        run |> Exc.chain_err (
            fun () -> "while popping an int from the stack"
        )

    let pop_nat (self : t) : Theory.Cmp.Nat.t * Dtyp.t =
        let run () =
            match pop self with
            | Theory.C (Theory.Cmp.N n), dtyp -> n, dtyp
            | v, dtyp ->
                asprintf "found a value of type %a : %a" Dtyp.fmt dtyp Theory.fmt v
                |> Exc.throw
        in
        run |> Exc.chain_err (
            fun () -> "while popping a nat from the stack"
        )

    let pop_str (self : t) : Theory.Cmp.Str.t * Dtyp.t =
        let run () =
            match pop self with
            | Theory.C (Theory.Cmp.S s), dtyp -> s, dtyp
            | v, dtyp ->
                asprintf "found a value of type %a : %a" Dtyp.fmt dtyp Theory.fmt v
                |> Exc.throw
        in
        run |> Exc.chain_err (
            fun () -> "while popping a string from the stack"
        )

    let pop_key_hash (self : t) : Theory.KeyH.t * Dtyp.t =
        let run () =
            match pop self with
            | Theory.C (Theory.Cmp.KeyH kh), dtyp -> kh, dtyp
            | v, dtyp ->
                asprintf "found a value of type %a : %a" Dtyp.fmt dtyp Theory.fmt v
                |> Exc.throw
        in
        run |> Exc.chain_err (
            fun () -> "while popping a key hash from the stack"
        )

    let pop_tez (self : t) : Theory.Tez.t * Dtyp.t =
        let run () =
            match pop self with
            | Theory.C (Theory.Cmp.Tz tz), dtyp -> tz, dtyp
            | v, dtyp ->
                asprintf "found a value of type %a : %a" Dtyp.fmt dtyp Theory.fmt v
                |> Exc.throw
        in
        run |> Exc.chain_err (
            fun () -> "while popping a mutez from the stack"
        )

    let pop_either (self : t) : (Theory.value, Theory.value) Theory.Either.t * Dtyp.t =
        let run () =
            match pop self with
            | Theory.Either either, dtyp -> either, dtyp
            | v, dtyp ->
                asprintf "found a value of type %a : %a" Dtyp.fmt dtyp Theory.fmt v
                |> Exc.throw
        in
        run |> Exc.chain_err (
            fun () -> "while popping a union (`or`) from the stack"
        )

    let pop_option (self : t) : Theory.value Theory.Option.t * Dtyp.t =
        let run () =
            match pop self with
            | Theory.Option opt, dtyp -> opt, dtyp
            | v, dtyp ->
                asprintf "found a value of type %a : %a" Dtyp.fmt dtyp Theory.fmt v
                |> Exc.throw
        in
        run |> Exc.chain_err (
            fun () -> "while popping an option from the stack"
        )

    let pop_list (self : t) : Theory.value Theory.Lst.t * Dtyp.t =
        let run () =
            match pop self with
            | Theory.Lst opt, dtyp -> opt, dtyp
            | v, dtyp ->
                asprintf "found a value of type %a : %a" Dtyp.fmt dtyp Theory.fmt v
                |> Exc.throw
        in
        run |> Exc.chain_err (
            fun () -> "while popping a list from the stack"
        )

    let pop_contract_params (address : Theory.Address.t) (self : t) : Theory.contract_params =
        let run () =
            let manager =
                (fun () -> pop_key_hash self |> fst)
                |> Exc.chain_err (
                    fun () ->
                        "while retrieving the `manager` argument"
                )
            in
            let delegate =
                (fun () ->
                    let flag, opt_dtyp = pop_option self in
                    let dtyp = Dtyp.Inspect.option opt_dtyp in
                    (
                        if dtyp.typ <> Dtyp.Leaf Dtyp.KeyH then
                            asprintf
                                "expected an optional key hash, found a value of type %a"
                                Dtyp.fmt opt_dtyp
                            |> Exc.throw
                    );
                    match flag with
                    | Some (Theory.C (Theory.Cmp.KeyH kh)) -> Some kh
                    | None -> None
                    | Some value ->
                        asprintf "unexpected value %a found while retrieving key hash" Theory.fmt value
                        |> Exc.throw
                ) |> Exc.chain_err (
                    fun () ->
                        "while retrieving the `delegate` argument"
                )
            in
            let spendable =
                (fun () -> pop_bool self |> fst)
                |> Exc.chain_err (
                    fun () ->
                        "while retrieving the `spendable` argument"
                )
            in
            let delegatable =
                (fun () -> pop_bool self |> fst)
                |> Exc.chain_err (
                    fun () ->
                        "while retrieving the `delegatable` argument"
                )
            in
            let tez =
                (fun () -> pop_tez self |> fst)
                |> Exc.chain_err (
                    fun () ->
                        "while retrieving the `mutez` argument"
                )
            in
            Theory.mk_contract_params ~spendable ~delegatable manager delegate tez address
        in
        run |> Exc.chain_err (
            fun () -> "while popping parameters for a contract creation operation"
        )

    let some ?alias:(alias=None) (self : t) : unit =
        let value, dtyp = pop self in
        let dtyp = Dtyp.Option dtyp |> Dtyp.mk ~alias in
        push dtyp (Theory.Of.option (Some value)) self
    let none ?alias:(alias=None) (dtyp : Dtyp.t) (self : t) : unit =
        let dtyp = Dtyp.Option dtyp |> Dtyp.mk ~alias in
        push dtyp (Theory.Of.option None) self

    let left ?alias:(alias=None) (rgt_dtyp : Dtyp.t) (self : t) : unit =
        let value, dtyp = pop self in
        let lft_dtyp = { Dtyp.inner = dtyp ; name = None} in
        let rgt_dtyp = { Dtyp.inner = rgt_dtyp ; name = None } in
        let dtyp = Dtyp.Or (lft_dtyp, rgt_dtyp) |> Dtyp.mk ~alias in
        push dtyp (Theory.Of.either (Theory.Either.Lft value)) self

    let right ?alias:(alias = None) (lft_dtyp : Dtyp.t) (self : t) : unit =
        let value, dtyp = pop self in
        let lft_dtyp = { Dtyp.inner = lft_dtyp ; name = None} in
        let rgt_dtyp = { Dtyp.inner = dtyp ; name = None } in
        let dtyp = Dtyp.Or (lft_dtyp, rgt_dtyp) |> Dtyp.mk ~alias in
        push dtyp (Theory.Of.either (Theory.Either.Rgt value)) self

    let cons (self : t) : unit =
        let run () =
            let head_value, head_dtyp = pop self in
            self |> map_last (
                fun tail_value tail_dtyp ->
                    let inner =
                        (fun () -> Dtyp.Inspect.list tail_dtyp)
                        |> Exc.chain_err (
                            fun () -> "while type-checking `CONS`"
                        )
                    in
                    (fun () -> Dtyp.check head_dtyp inner)
                    |> Exc.chain_err (
                        fun () -> asprintf "head has type `%a`, but tail has type `%a`" Dtyp.fmt head_dtyp Dtyp.fmt tail_dtyp
                    );
                    Theory.cons head_value tail_value
            )
        in
        run |> Exc.chain_err (
            fun () -> "while running `CONS`"
        )
    let nil ?alias:(alias=None) (dtyp : Dtyp.t) (self : t) : unit =
        let dtyp = Dtyp.List dtyp |> Dtyp.mk ~alias in
        push dtyp (Theory.Of.list Theory.Lst.nil) self
end

module Cxt (S : Sigs.SigStackRaw) : Sigs.SigCxt = struct
    module Theory = S.Theory
    module Stack = Stack(S)
    module Address = Theory.Address

    (* Represents the end of a runtime block of instruction. *)
    type block_end =
    (* Need to un-dip when exiting this block, and run what's left. *)
    | Dip of Mic.t list
    (* Reached the end of the body of a loop.
    
        First argument is the loop instruction. Second is whatever is left to run after the loop.Base
    *)
    | Loop of Mic.t * Mic.t list
    (* Reached the end of a block that triggers nothing on exit.
    
        First argument is the instruction that caused the block to start. `SEQ` for instance.
    *)
    | Nop of Mic.t * Mic.t list

    type t = {
        stack : Stack.t ;
        mutable blocks : block_end list ;
        mutable next : Mic.t list ;
        mutable last : Mic.t option ;
    }

    let push_block (block : block_end) (self : t) : unit =
        self.blocks <- block :: self.blocks
    let pop_block (self : t) : block_end option =
        match self.blocks with
        | [] -> None
        | head :: blocks ->
            self.blocks <- blocks;
            Some head

    module Ops = Ops.Make(Stack)

    let is_done ({ stack = _ ; blocks ; next ; last = _ } : t) : bool =
        blocks = [] && next = []

    let last_ins (self : t) : Mic.t option = self.last

    let next_ins (self : t) : Mic.t option =  Lst.hd self.next

    let stack (self : t) : Stack.t = self.stack

    (* Fetches the next instruction to run.

        The instruction returned does not appear in `self.next` (anymore). This function might go
        up the stack of blocks and un-dip the stack.
    *)
    let rec fetch_next (self : t) : Mic.t option =
        match self.next with
        | next :: tail ->
            self.next <- tail;
            Some next
        | [] -> (
            match pop_block self with
            | Some (Dip next) ->
                Stack.undip self.stack;
                self.next <- next;
                fetch_next self
            | Some (Loop (ins, next)) ->
                    self.next <- ins :: next;
                    fetch_next self
            | Some (Nop (_, next)) ->
                self.next <- next;
                fetch_next self
            | None -> None
        )

    let step (self : t) : bool =
        let run () : bool = match fetch_next self with
        (* Nothing left to do, done. *)
        | None -> true

        (* Let's do this. *)
        | Some mic -> (
            log_0 "running @[%a@]@.@." Mic.fmt mic;
            self.last <- Some mic;
            (
                match mic.ins with

                (* # Basic stack manipulation. *)

                | Mic.Push (dtyp, const) ->
                    let binding = Lst.hd mic.vars in
                    let value = Theory.Of.const const |> Theory.cast dtyp in
                    self.stack |> Stack.push ~binding dtyp value

                | Mic.Leaf Drop ->
                    let _ = Stack.pop self.stack in
                    ()

                | Mic.Leaf Som ->
                    let _ = Stack.some self.stack in
                    ()

                | Mic.Non dtyp ->
                    let alias = Lst.hd mic.typs in
                    let _ = Stack.none ~alias dtyp self.stack in
                    ()

                | Mic.Left dtyp ->
                    let alias = Lst.hd mic.typs in
                    let _ = Stack.left ~alias dtyp self.stack in
                    ()

                | Mic.Right dtyp ->
                    let alias = Lst.hd mic.typs in
                    let _ = Stack.right ~alias dtyp self.stack in
                    ()

                | Mic.Leaf Dup ->
                    Stack.dup self.stack

                | Mic.Leaf Swap ->
                    Stack.swap self.stack

                | Mic.Leaf Unit ->
                    Stack.push Dtyp.unit Theory.Of.unit self.stack

                (* # Control structures. *)

                | Mic.Seq seq ->
                    push_block (Nop (mic, self.next)) self;
                    self.next <- seq

                | Mic.Dip mic ->
                    (* Remember we need to undip later. *)
                    push_block (Dip self.next) self;
                    (* Actually dip the stack. *)
                    Stack.dip self.stack;
                    (* Need to go down this thing. *)
                    self.next <- [ mic ]

                | Mic.Loop body ->
                    let cond, _ = Stack.pop_bool self.stack in
                    (* Are we looping? *)
                    if cond then (
                        (* Remember to loop later, when going up. *)
                        push_block (Loop (mic, self.next)) self;
                        (* Run body. *)
                        self.next <- [ body ]
                    ) else (
                        (* Skipping this loop, nothing to do. *)
                        ()
                    )

                | Mic.LoopLeft body -> (
                    let cond, dtyp = Stack.pop_either self.stack in
                    let lft_dtyp, rgt_dtyp =
                        (fun () -> Dtyp.Inspect.either dtyp) |> Exc.chain_err (
                            fun () -> "while retrieving the condition for a LOOP_LEFT instruction"
                        )
                    in
                    (* Are we looping? *)
                    match cond with
                    | Theory.Either.Lft lft_value ->
                        (* Push accumulator. *)
                        Stack.push lft_dtyp lft_value self.stack;
                        (* Remember to loop later, when going up. *)
                        push_block (Loop (mic, self.next)) self;
                        (* Run body. *)
                        self.next <- [ body ]
                    | Theory.Either.Rgt rgt_value ->
                        (* Push exit value. *)
                        Stack.push rgt_dtyp rgt_value self.stack;
                        (* Nothing to do. *)
                        ()
                )

                | Mic.If (mic_then, mic_else) -> (
                    let cond, _ = Stack.pop_bool self.stack in
                    (* Remember whatever next instructions there is. *)
                    push_block (Nop (mic, self.next)) self;
                    (* Which branch are we in? *)
                    if cond then (
                        (* Run then branch. *)
                        self.next <- [ mic_then ]
                    ) else (
                        (* Run else branch. *)
                        self.next <- [ mic_else ]
                    )
                )

                | Mic.IfNone (mic_then, mic_else) -> (
                    let cond, dtyp = Stack.pop_option self.stack in
                    (* Remember whatever next instructions there is. *)
                    push_block (Nop (mic, self.next)) self;
                    (* Which branch are we in? *)
                    match cond with
                    | None ->
                        (* Run then branch. *)
                        self.next <- [ mic_then ]
                    | Some sub_value ->
                        let sub_dtyp = Dtyp.Inspect.option dtyp in
                        (* Push value. *)
                        Stack.push sub_dtyp sub_value self.stack;
                        (* Run else branch. *)
                        self.next <- [ mic_else ]
                )

                | Mic.IfLeft (mic_then, mic_else) -> (
                    let cond, dtyp = Stack.pop_either self.stack in
                    let lft_dtyp, rgt_dtyp = Dtyp.Inspect.either dtyp in
                    (* Remember whatever next instructions there is. *)
                    push_block (Nop (mic, self.next)) self;
                    (* Which branch are we in? *)
                    match cond with
                    | Theory.Either.Lft lft_value ->
                        (* Push value. *)
                        Stack.push lft_dtyp lft_value self.stack;
                        (* Run then branch. *)
                        self.next <- [ mic_then ]
                    | Theory.Either.Rgt rgt_value ->
                        (* Push value. *)
                        Stack.push rgt_dtyp rgt_value self.stack;
                        (* Run else branch. *)
                        self.next <- [ mic_else ]
                )

                | Mic.IfRight (mic_then, mic_else) -> (
                    let cond, dtyp = Stack.pop_either self.stack in
                    let lft_dtyp, rgt_dtyp = Dtyp.Inspect.either dtyp in
                    (* Remember whatever next instructions there is. *)
                    push_block (Nop (mic, self.next)) self;
                    (* Which branch are we in? *)
                    match cond with
                    | Theory.Either.Lft lft_value ->
                        (* Push value. *)
                        Stack.push lft_dtyp lft_value self.stack;
                        (* Run else branch. *)
                        self.next <- [ mic_else ]
                    | Theory.Either.Rgt rgt_value ->
                        (* Push value. *)
                        Stack.push rgt_dtyp rgt_value self.stack;
                        (* Run then branch. *)
                        self.next <- [ mic_then ]
                )

                | Mic.IfCons (mic_then, mic_else) -> (
                    let lst, dtyp = Stack.pop_list self.stack in
                    let sub_dtyp = Dtyp.Inspect.list dtyp in
                    (* Remember whatever next instructions there is. *)
                    push_block (Nop (mic, self.next)) self;
                    (* Which branch are we in? *)
                    match Theory.Lst.head lst with
                    | Some (sub_value, lst) ->
                        (* Push tail. *)
                        Stack.push dtyp (Theory.Of.list lst) self.stack;
                        (* Push value. *)
                        Stack.push sub_dtyp sub_value self.stack;
                        (* Run then branch. *)
                        self.next <- [ mic_then ]
                    | None ->
                        (* Run else branch. *)
                        self.next <- [ mic_else ]
                )

                (* # Basic value creation. *)

                | Mic.Leaf Pair ->
                    let binding = Lst.hd mic.vars in
                    let alias = Lst.hd mic.typs in
                    let (snd, snd_dtyp), (fst, fst_dtyp) = Stack.pop self.stack, Stack.pop self.stack in
                    let value = Theory.Of.pair fst snd in

                    let fst_field = Lst.hd mic.fields in
                    let snd_field =
                        match Lst.tl mic.fields with
                        | None -> None
                        | Some fields -> Lst.hd fields
                    in
                    let fst_typ, snd_typ =
                        Dtyp.mk_named fst_field fst_dtyp, Dtyp.mk_named snd_field snd_dtyp
                    in
                    let dtyp = Dtyp.Pair (fst_typ, snd_typ) |> Dtyp.mk ~alias in

                    Stack.push ~binding dtyp value self.stack

                (* # List operations. *)

                | Mic.Leaf Cons ->
                    Stack.cons self.stack

                | Mic.Nil dtyp ->
                    let alias = Lst.hd mic.typs in
                    Stack.nil ~alias dtyp self.stack

                | Mic.Leaf Size ->
                    let lst, _ = Stack.pop_list self.stack in
                    let len = Theory.Lst.size lst |> Theory.Of.nat in
                    Stack.push (Dtyp.nat) len self.stack

                (* # Domain-specific. *)

                (* ## Timestamps. *)

                | Mic.Leaf Now ->
                    let binding = Lst.hd mic.vars in
                    let now = Theory.TStamp.now () |> Theory.Of.timestamp in
                    Stack.push ~binding Dtyp.timestamp now self.stack

                (* ## Crypto. *)

                | Mic.Leaf HashKey ->
                    let binding = Lst.hd mic.vars in
                    let key, _ = Stack.pop self.stack in
                    let value =
                        (fun () ->
                            let key = Theory.Inspect.key key in
                            Theory.Key.b58check key |> Theory.Of.key_h
                        ) |> Exc.chain_err (
                            fun () -> "while retrieving a key to hash (b58check)"
                        )
                    in
                    let dtyp = Dtyp.KeyH |> Dtyp.mk_leaf in
                    Stack.push ~binding dtyp value self.stack

                | Mic.CreateContract param -> (
                    let binding = Lst.hd mic.vars in
                    let address = Address.fresh binding in
                    let params = Stack.pop_contract_params address self.stack in
                    (* Push address. *)
                    let dtyp = Dtyp.Address |> Dtyp.mk_leaf in
                    Stack.push dtyp (Theory.Of.address address) self.stack;
                    (* Push operation. *)
                    let dtyp = Dtyp.Operation |> Dtyp.mk_leaf in
                    match param with
                    | Either.Lft (Some c) ->
                        let operation = Theory.Of.Operation.create params c in
                        Stack.push ~binding dtyp operation self.stack
                    | Either.Lft None -> Exc.throw "aaa"
                    | Either.Rgt name ->
                        let operation = Theory.Of.Operation.create_named params name in
                        Stack.push ~binding dtyp operation self.stack
                )

                (* # Macros. *)

                | Mic.Macro (subs, _) ->
                    push_block (Nop (mic, self.next)) self;
                    self.next <- subs

                (* # Unimplemented stuff. *)

                | _ -> asprintf "unsupported instruction @[%a@]" Mic.fmt mic |> Exc.throw
            );
            false
        )
        in
        run |> Exc.chain_err (
            fun () -> "while making a step in the evaluator"
        )

    let init (values : (Theory.value * Dtyp.t * Annot.Var.t option) list) (inss : Mic.t list) : t = 
        let cxt =
            {
                stack = Stack.empty ;
                blocks = [] ;
                next = inss ;
                last = None ;
            }
        in
        values |> List.iter (
            fun (value, dtyp, binding) ->
                Stack.push ~binding dtyp value cxt.stack
        );
        cxt
end
