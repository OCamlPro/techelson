open Base
open Base.Common

(** Creates an interpreter from a base stack module and a contract environment module.

    This functor contains the code actually interpreting the instructions. *)
module Interpreter (
    S : Stack.Sigs.StackBase
) : Sigs.Interpreter with module Theory = S.Theory
= struct
    module Stack = Stack.Make.Stack (S)
    module Theory = Stack.Theory
    module Env = Stack.Env

    module Src = struct
        module Theory = Theory
        type t =
        | Test of Testcase.t
        | Contract of Theory.Address.t


        let fmt (fmt : formatter) (src : t) : unit =
            match src with
            | Test tc -> fprintf fmt "(test %s)" tc.name
            | Contract address -> fprintf fmt "(contract @%a)" Theory.Address.fmt address

        let of_test (test : Testcase.t) : t =
            Test test
        let of_address (address : Theory.Address.t) : t =
            Contract address
    end

    (* Represents the end of a runtime block of instruction. *)
    type block_end =
    | Dip of Mic.t list
    (* Need to un-dip when exiting this block, and run what's left. *)
    | Loop of Mic.t * Mic.t list
    (* Reached the end of the body of a loop.
    
        First argument is the loop instruction. Second is whatever is left to run after the
        loop.Base
    *)
    | Nop of Mic.t * Mic.t list
    (* Reached the end of a block that triggers nothing on exit.
    
        First argument is the instruction that caused the block to start. `SEQ` for instance.
    *)
    | Iter of Dtyp.t * Theory.value list * Mic.t * Mic.t list

    type t = {
        mutable stack : Stack.t ;
        mutable blocks : block_end list ;
        mutable next : Mic.t list ;
        mutable last : Mic.t option ;
        mutable src : Src.t ;
        mutable balance : Theory.Tez.t ;
        amount : Theory.Tez.t ;
        env : Env.t ;
    }

    let balance (self : t) : Theory.Tez.t = self.balance

    let contract_env (self : t) : Env.t = self.env

    let push_block (block : block_end) (self : t) : unit =
        self.blocks <- block :: self.blocks
    let pop_block (self : t) : block_end option =
        match self.blocks with
        | [] -> None
        | head :: blocks ->
            self.blocks <- blocks;
            Some head

    let is_done ({ blocks ; next ; _ } : t) : bool =
        blocks = [] && next = []

    let src ({src ; _} : t) : Src.t = src

    let last_ins (self : t) : Mic.t option = self.last

    let next_ins (self : t) : string list * Mic.t option =
        match self.next with
        | hd :: _ -> [], Some hd
        | [] -> (
            let rec loop
                (acc : string list)
                (blocks : block_end list)
                : string list * Mic.t option
            =
                match blocks with
                | [] -> acc, None
                | Dip next :: blocks -> (
                    let acc = "undip" :: acc in
                    match next with
                    | [] -> loop acc blocks
                    | hd :: _ -> acc, Some hd
                )
                | Loop (ins, _) :: _ -> "re-loop" :: acc, Some ins
                | Nop (ins, next) :: blocks -> (
                    let acc = (asprintf "exit block %a" Mic.fmt ins) :: acc in
                    match next with
                    | [] -> loop acc blocks
                    | hd :: _ -> acc, Some hd
                )
                | Iter (_, elm :: _, mic, _) :: _ -> (
                    let acc =
                        (
                            asprintf "continueing iteration, next: %a, code: %a"
                                Theory.fmt elm Mic.fmt mic
                        ) :: acc
                    in
                    acc, Some mic
                )
                | Iter (_, [], mic, next) :: blocks -> (
                    let acc = (asprintf "exiting iteration %a" Mic.fmt mic) :: acc in
                    match next with
                    | [] -> loop acc blocks
                    | hd :: _ -> acc, Some hd
                )
            in
            let pre_ops, ins = loop [] self.blocks in
            List.rev pre_ops, ins
        )

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
            | Some (Iter (dtyp, value :: coll, mic, next)) -> (
                push_block (Iter (dtyp, coll, mic, next)) self;
                let binding = Some (Annot.Var.of_string "lambda_param") in
                Stack.push ~binding dtyp value self.stack;
                Some mic
            )
            | Some (Iter (_, [], _, next)) -> (
                self.next <- next;
                fetch_next self
            )
            | None -> None
        )

    let step (self : t) : bool =
        let run () : bool = match fetch_next self with
        (* Nothing left to do, done. *)
        | None -> true

        (* Let's do this. *)
        | Some mic -> (
            (* log_0 "running @[%a@]@.@." Mic.fmt mic; *)
            self.last <- Some mic;
            (
                match mic.ins with

                | Mic.Leaf Mic.Failwith ->
                    Stack.pop self.stack |> fst
                    |> asprintf "%a" Theory.fmt
                    |> Exc.Throw.failure

                (* # Basic stack manipulation. *)

                | Mic.Push (dtyp, const) ->
                    let binding = Lst.hd mic.vars in
                    let alias = Lst.hd mic.typs in
                    let dtyp =
                        if alias = None then dtyp
                        else Dtyp.rename alias dtyp
                    in
                    let value = Theory.Of.const const |> Theory.cast dtyp in
                    self.stack |> Stack.push ~binding dtyp value

                | Mic.Leaf Drop ->
                    let _ = Stack.pop self.stack in
                    ()

                | Mic.Leaf Som ->
                    let alias = Lst.hd mic.typs in
                    let field = Lst.hd mic.fields in
                    let _ = Stack.Push.some ~alias ~field self.stack in
                    ()

                | Mic.Non dtyp ->
                    let alias = Lst.hd mic.typs in
                    let field = Lst.hd mic.fields in
                    let _ = Stack.Push.none ~alias ~field dtyp self.stack in
                    ()

                | Mic.Left dtyp ->
                    let alias = Lst.hd mic.typs in
                    let _ = Stack.Push.left ~alias dtyp self.stack in
                    ()

                | Mic.Right dtyp ->
                    let alias = Lst.hd mic.typs in
                    let _ = Stack.Push.right ~alias dtyp self.stack in
                    ()

                | Mic.Leaf Dup ->
                    let binding = Lst.hd mic.vars in
                    Stack.dup ~binding self.stack

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
                    let cond, _ = Stack.Pop.bool self.stack in
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
                    let cond, dtyp = Stack.Pop.either self.stack in
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
                    let cond, _ = Stack.Pop.bool self.stack in
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
                    let cond, dtyp = Stack.Pop.option self.stack in
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
                    let cond, dtyp = Stack.Pop.either self.stack in
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
                    let cond, dtyp = Stack.Pop.either self.stack in
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
                    let lst, dtyp = Stack.Pop.list self.stack in
                    let sub_dtyp = Dtyp.Inspect.list dtyp in
                    (* Remember whatever next instructions there is. *)
                    push_block (Nop (mic, self.next)) self;
                    (* Which branch are we in? *)
                    match Theory.Lst.snoc lst with
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
                    let (snd, snd_dtyp), (fst, fst_dtyp) =
                        Stack.pop self.stack, Stack.pop self.stack
                    in
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

                (* # Casts, renaming... *)

                | Mic.Cast target ->
                    let value, _ = Stack.pop self.stack in
                    let value = Theory.cast target value in

                    Stack.push target value self.stack

                | Mic.Leaf Rename ->
                    let binding = Lst.hd mic.vars in
                    Stack.rename binding self.stack

                (* # Booleans. *)

                | Mic.Leaf And ->
                    let binding = Lst.hd mic.vars in
                    let v_1, _ = Stack.pop self.stack in
                    let v_2, _ = Stack.pop self.stack in
                    let value, dtyp = Theory.conj v_1 v_2 in

                    Stack.push ~binding dtyp value self.stack

                | Mic.Leaf Or ->
                    let binding = Lst.hd mic.vars in
                    let v_1, _ = Stack.pop self.stack in
                    let v_2, _ = Stack.pop self.stack in
                    let value, dtyp = Theory.disj v_1 v_2 in

                    Stack.push ~binding dtyp value self.stack

                | Mic.Leaf Not ->
                    let binding = Lst.hd mic.vars in
                    let value, _ = Stack.pop self.stack in
                    let value, dtyp = Theory.not value in

                    Stack.push ~binding dtyp value self.stack

                | Mic.Leaf Xor ->
                    let binding = Lst.hd mic.vars in
                    let v_1, _ = Stack.pop self.stack in
                    let v_2, _ = Stack.pop self.stack in
                    let value, dtyp = Theory.xor v_1 v_2 in

                    Stack.push ~binding dtyp value self.stack

                (* # Arithmetic. *)

                | Mic.Leaf Neg ->
                    let binding = Lst.hd mic.vars in
                    let value, _ = Stack.pop self.stack in
                    let value, dtyp = Theory.neg value in

                    Stack.push ~binding dtyp value self.stack

                | Mic.Leaf Add ->
                    let binding = Lst.hd mic.vars in
                    let v_1, _ = Stack.pop self.stack in
                    let v_2, _ = Stack.pop self.stack in
                    let value, dtyp = Theory.add v_1 v_2 in

                    Stack.push ~binding dtyp value self.stack

                | Mic.Leaf Sub ->
                    let binding = Lst.hd mic.vars in
                    let v_1, _ = Stack.pop self.stack in
                    let v_2, _ = Stack.pop self.stack in
                    let value, dtyp = Theory.sub v_1 v_2 in

                    Stack.push ~binding dtyp value self.stack

                | Mic.Leaf Mul ->
                    let binding = Lst.hd mic.vars in
                    let v_1, _ = Stack.pop self.stack in
                    let v_2, _ = Stack.pop self.stack in
                    let value, dtyp = Theory.mul v_1 v_2 in

                    Stack.push ~binding dtyp value self.stack
                
                | Mic.Leaf Abs ->
                    let binding = Lst.hd mic.vars in
                    let value, _ = Stack.pop self.stack in
                    let value, dtyp = Theory.abs value in

                    Stack.push ~binding dtyp value self.stack

                | Mic.Leaf Lsl ->
                    let binding = Lst.hd mic.vars in
                    let v_1, _ = Stack.pop self.stack in
                    let v_2, _ = Stack.pop self.stack in
                    let value, dtyp = Theory.lshift_lft v_1 v_2 in

                    Stack.push ~binding dtyp value self.stack

                | Mic.Leaf Lsr ->
                    let binding = Lst.hd mic.vars in
                    let v_1, _ = Stack.pop self.stack in
                    let v_2, _ = Stack.pop self.stack in
                    let value, dtyp = Theory.lshift_rgt v_1 v_2 in

                    Stack.push ~binding dtyp value self.stack

                | Mic.Leaf EDiv ->
                    let binding = Lst.hd mic.vars in
                    let v_1, _ = Stack.pop self.stack in
                    let v_2, _ = Stack.pop self.stack in
                    let value, dtyp = Theory.ediv v_1 v_2 in

                    Stack.push ~binding dtyp value self.stack

                (* # String/bytes. *)

                | Mic.Leaf Slice -> (
                    let binding = Lst.hd mic.vars in
                    let start, _ = Stack.Pop.nat self.stack in
                    let length, _ = Stack.Pop.nat self.stack in
                    let value, dtyp =
                        match Stack.pop self.stack with
                        | Theory.C (Theory.Cmp.S s), dtyp ->
                            Theory.Str.slice start length s |> Theory.Of.str, dtyp
                        | Theory.C (Theory.Cmp.By by), dtyp ->
                            Theory.Bytes.slice start length by |> Theory.Of.bytes, dtyp
                        | v, d ->
                            asprintf "expected string or bytes, found %a of type %a"
                                Theory.fmt v Dtyp.fmt d
                            |> Exc.throw
                    in

                    Stack.push ~binding dtyp value self.stack
                )

                | Mic.Leaf Concat -> (
                    let binding = Lst.hd mic.vars in
                    let value, dtyp =
                        match Stack.pop self.stack with
                        | Theory.C (Theory.Cmp.S s_1), dtyp_1 ->
                            let s_2, dtyp_2 = Stack.Pop.str self.stack in
                            Dtyp.check dtyp_1 dtyp_2;
                            Theory.Str.concat s_1 s_2 |> Theory.Of.str, dtyp_1
                        | Theory.C (Theory.Cmp.By by_1), dtyp_1 ->
                            let by_2, dtyp_2 = Stack.Pop.bytes self.stack in
                            Dtyp.check dtyp_1 dtyp_2;
                            Theory.Bytes.concat by_1 by_2 |> Theory.Of.bytes, dtyp_2
                        | v, d ->
                            asprintf "expected string or bytes, found %a of type %a"
                                Theory.fmt v Dtyp.fmt d
                            |> Exc.throw
                    in

                    Stack.push ~binding dtyp value self.stack
                )

                (* # Comparison. *)

                | Mic.Leaf Compare ->
                    let binding = Lst.hd mic.vars in
                    let v_1, _ = Stack.pop self.stack in
                    let v_2, _ = Stack.pop self.stack in
                    let value, dtyp = Theory.cmp v_1 v_2 in
                    Stack.push ~binding dtyp value self.stack

                | Mic.Leaf Eq ->
                    let binding = Lst.hd mic.vars in
                    let value, _ = Stack.pop self.stack in
                    let value = Theory.is_zero value in
                    let dtyp = Dtyp.Bool |> Dtyp.mk_leaf in

                    Stack.push ~binding dtyp value self.stack

                | Mic.Leaf Neq ->
                    let binding = Lst.hd mic.vars in
                    let value, _ = Stack.pop self.stack in
                    let value = Theory.is_not_zero value in
                    let dtyp = Dtyp.Bool |> Dtyp.mk_leaf in

                    Stack.push ~binding dtyp value self.stack

                | Mic.Leaf Le ->
                    let binding = Lst.hd mic.vars in
                    let value, _ = Stack.pop self.stack in
                    let value = Theory.le_zero value in
                    let dtyp = Dtyp.Bool |> Dtyp.mk_leaf in

                    Stack.push ~binding dtyp value self.stack

                | Mic.Leaf Lt ->
                    let binding = Lst.hd mic.vars in
                    let value, _ = Stack.pop self.stack in
                    let value = Theory.lt_zero value in
                    let dtyp = Dtyp.Bool |> Dtyp.mk_leaf in

                    Stack.push ~binding dtyp value self.stack

                | Mic.Leaf Gt ->
                    let binding = Lst.hd mic.vars in
                    let value, _ = Stack.pop self.stack in
                    let value = Theory.gt_zero value in
                    let dtyp = Dtyp.Bool |> Dtyp.mk_leaf in

                    Stack.push ~binding dtyp value self.stack

                | Mic.Leaf Ge ->
                    let binding = Lst.hd mic.vars in
                    let value, _ = Stack.pop self.stack in
                    let value = Theory.ge_zero value in
                    let dtyp = Dtyp.Bool |> Dtyp.mk_leaf in

                    Stack.push ~binding dtyp value self.stack

                (* # Pair operations. *)
                | Mic.Leaf Car ->
                    let binding = Lst.hd mic.vars in
                    let value, dtyp = Stack.pop self.stack in
                    let value = Theory.car value in
                    let dtyp, _ = Dtyp.Inspect.pair dtyp in

                    Stack.push ~binding dtyp value self.stack
                | Mic.Leaf Cdr ->
                    let binding = Lst.hd mic.vars in
                    let value, dtyp = Stack.pop self.stack in
                    let value = Theory.cdr value in
                    let _, dtyp = Dtyp.Inspect.pair dtyp in

                    Stack.push ~binding dtyp value self.stack

                (* # List operations. *)

                | Mic.Leaf Cons ->
                    Stack.cons self.stack

                | Mic.Nil dtyp ->
                    let binding = Lst.hd mic.vars in
                    let alias = Lst.hd mic.typs in
                    Stack.Push.nil ~binding ~alias dtyp self.stack

                | Mic.Leaf Size ->
                    let lst, _ = Stack.Pop.list self.stack in
                    let len = Theory.Lst.size lst |> Theory.Of.nat in
                    Stack.push (Dtyp.nat) len self.stack

                (* # Set operations. *)

                | Mic.EmptySet dtyp ->
                    let binding = Lst.hd mic.vars in
                    let alias = Lst.hd mic.typs in

                    Stack.Push.empty_set ~binding ~alias dtyp self.stack

                (* # Map operations. *)

                | Mic.EmptyMap (key_dtyp, val_dtyp) ->
                    let binding = Lst.hd mic.vars in
                    let alias = Lst.hd mic.typs in

                    Stack.Push.empty_map ~binding ~alias key_dtyp val_dtyp self.stack

                (* # Set/Map operations. *)

                | Mic.Leaf Update ->
                    let key, key_dtyp = Stack.Pop.cmp self.stack in
                    let apply : Theory.value -> Dtyp.t -> Theory.value =
                        match Stack.pop self.stack with

                        (* Boolean flag means set update. *)
                        | Theory.C (Theory.Cmp.B add), _ -> (
                            function
                            | Theory.Set set -> (
                                fun set_dtyp ->
                                    (* Type check. *)
                                    let _ =
                                        let edtyp = Dtyp.Inspect.set set_dtyp in
                                        Dtyp.check edtyp key_dtyp
                                    in
                                    Theory.Set.update key add set
                                    |> Theory.Of.set
                            )
                            | value -> (
                                fun dtyp ->
                                    asprintf "expected set, found value of type %a : %a"
                                        Dtyp.fmt dtyp Theory.fmt value
                                    |> Exc.throw
                            )
                        )
                        (* Option means (big)map update. *)
                        | Theory.Option v, val_dtyp -> (
                            function
                            | Theory.Map map ->
                                fun map_dtyp ->
                                    (* Type check. *)
                                    let _ =
                                        let kdtyp, vdtyp = Dtyp.Inspect.map map_dtyp in
                                        Dtyp.check kdtyp key_dtyp ;
                                        Dtyp.check vdtyp val_dtyp
                                    in
                                    Theory.Map.update key v map
                                    |> Theory.Of.map
                            | Theory.BigMap big_map ->
                                fun big_map_dtyp ->
                                    (* Type check. *)
                                    let _ =
                                        let kdtyp, vdtyp = Dtyp.Inspect.map big_map_dtyp in
                                        Dtyp.check kdtyp key_dtyp ;
                                        Dtyp.check vdtyp val_dtyp
                                    in
                                    Theory.BigMap.update key v big_map
                                    |> Theory.Of.big_map
                            | value -> (
                                fun dtyp ->
                                    asprintf "expected map, found value of type %a : %a"
                                        Dtyp.fmt dtyp Theory.fmt value
                                    |> Exc.throw
                            )
                        )
                        | _, dtyp ->
                            asprintf "second parameter of UPDATE cannot have type %a" Dtyp.fmt dtyp
                            |> Exc.throw
                    in

                    self.stack |> Stack.map_last apply

                | Mic.Leaf Get ->
                    let binding = Lst.hd mic.vars in
                    let key, key_dtyp = Stack.Pop.cmp self.stack in
                    let value, map_dtyp =
                        match Stack.pop self.stack with
                        | Theory.Map map, dtyp ->
                            Theory.Map.get key map |> Theory.Of.option, dtyp
                        | Theory.BigMap map, dtyp ->
                            Theory.BigMap.get key map |> Theory.Of.option, dtyp
                        | v, d ->
                            asprintf "expected (big) map, found vaule of type %a : %a"
                                Dtyp.fmt d Theory.fmt v
                            |> Exc.throw
                    in
                    let kdtyp, value_dtyp = Dtyp.Inspect.map map_dtyp in
                    Dtyp.check kdtyp key_dtyp;

                    Stack.push ~binding value_dtyp value self.stack

                | Mic.Leaf Mem ->
                    let binding = Lst.hd mic.vars in
                    let key, key_dtyp = Stack.Pop.cmp self.stack in
                    let mem, kdtyp =
                        match Stack.pop self.stack with
                        | Theory.Set set, dtyp ->
                            Theory.Set.mem key set, Dtyp.Inspect.set dtyp
                        | Theory.Map map, dtyp ->
                            Theory.Map.mem key map, Dtyp.Inspect.map dtyp |> fst
                        | Theory.BigMap map, dtyp ->
                            Theory.BigMap.mem key map, Dtyp.Inspect.map dtyp |> fst
                        | v, d ->
                            asprintf "expected set or (big) map, found vaule of type %a : %a"
                                Dtyp.fmt d Theory.fmt v
                            |> Exc.throw
                    in
                    let dtyp = Dtyp.mk_leaf Dtyp.Bool in
                    Dtyp.check kdtyp key_dtyp;
                    let mem = Theory.Of.bool mem in

                    Stack.push ~binding dtyp mem self.stack

                (* # Iterations. *)

                | Mic.Iter mic -> (
                    let value, dtyp = Stack.pop self.stack in
                    let elm_dtyp = Dtyp.Inspect.iter_elm dtyp in
                    let elms = Theory.coll_to_list value in
                    push_block (Iter (elm_dtyp, elms, mic, self.next)) self;
                    self.next <- []
                )

                (* # Lambdas. *)

                | Mic.Leaf Exec ->
                    let _, _, mic = Stack.Pop.lambda self.stack in
                    push_block (Nop (mic, self.next)) self;
                    self.next <- [mic]

                | Mic.Lambda (dom, codom, mic) ->
                    let binding = Lst.hd mic.vars in
                    let alias = Lst.hd mic.typs in
                    let value = Theory.Of.lambda dom codom mic in
                    let dtyp = Dtyp.Lambda (dom, codom) |> Dtyp.mk ~alias in

                    Stack.push ~binding dtyp value self.stack

                (* # Domain-specific. *)

                (* ## Timestamps. *)

                | Mic.Leaf Now ->
                    let binding = Lst.hd mic.vars in
                    let now = Theory.TStamp.now () |> Theory.Of.timestamp in
                    Stack.push ~binding Dtyp.timestamp now self.stack

                (* ## Crypto. *)

                | Mic.Leaf (Hash h) ->
                    let binding = Lst.hd mic.vars in
                    let key, _ = Stack.pop self.stack in
                    let value =
                        (fun () ->
                            let key = Theory.Inspect.key key in
                            (
                                match h with
                                | Mic.B58Check -> Theory.Key.b58check
                                | Mic.Blake2B -> Theory.Key.blake2b
                                | Mic.Sha256 -> Theory.Key.sha256
                                | Mic.Sha512 -> Theory.Key.sha512
                            ) key |> Theory.Of.key_hash
                        ) |> Exc.chain_err (
                            fun () -> "while retrieving a key to hash (b58check)"
                        )
                    in
                    let dtyp = Dtyp.KeyH |> Dtyp.mk_leaf in

                    Stack.push ~binding dtyp value self.stack

                (* ## Env. *)

                | Mic.Leaf Mic.Balance ->
                    let binding = Lst.hd mic.vars in
                    let value = Theory.Of.tez self.balance in
                    let dtyp = Dtyp.Mutez |> Dtyp.mk_leaf in

                    Stack.push ~binding dtyp value self.stack


                | Mic.Leaf Mic.Amount ->
                    let binding = Lst.hd mic.vars in
                    let value = self.amount |> Theory.Of.tez in
                    let dtyp = Dtyp.Mutez |> Dtyp.mk_leaf in

                    Stack.push ~binding dtyp value self.stack

                | Mic.Contract dtyp -> (
                    let binding = Lst.hd mic.vars in
                    let address = Stack.Pop.address self.stack |> fst in
                    let value =
                        match Env.Live.get address self.env with
                        | None -> Theory.Of.option None
                        | Some contract -> (
                            let contract = Contract.to_mic contract.contract in
                            try (
                                Dtyp.check dtyp contract.param;
                                Some (Theory.Of.contract address contract)
                                |> Theory.Of.option
                            ) with
                            | _ -> Theory.Of.option None
                        )
                    in
                    let dtyp = Dtyp.Contract dtyp |> Dtyp.mk in
                    let dtyp = Dtyp.Option (Dtyp.mk_named None dtyp) |> Dtyp.mk in
                    Stack.push ~binding dtyp value self.stack
                )

                | Mic.Leaf Self -> (
                    let binding = Lst.hd mic.vars in
                    let bail () =
                        Exc.throw "internal error while retrieving self contract"
                    in
                    match self.src with
                    | Src.Test _ -> "instruction `SELF` is illegal in testcases" |> Exc.throw
                    | Src.Contract address -> (
                        match Env.Live.get address self.env with
                        | None -> bail ()
                        | Some contract -> (
                            let contract = Contract.to_mic contract.contract in
                            let value = Theory.Of.contract address contract in
                            let dtyp = Dtyp.Contract contract.param |> Dtyp.mk in
                            Stack.push ~binding dtyp value self.stack
                        )
                    )
                )

                | Mic.CreateContract (Either.Lft None) -> (
                    let binding = Lst.hd mic.vars in
                    let address = Theory.Address.fresh binding in
                    let params, contract =
                        Stack.Pop.contract_params_and_lambda address self.stack
                    in

                    (* Push address. *)
                    Stack.Push.address address self.stack;

                    (* Push operation. *)
                    let dtyp = Dtyp.Operation |> Dtyp.mk_leaf in
                    let uid = Env.get_uid self.env in
                    let operation = Theory.Of.Operation.create uid params contract in
                    Stack.push ~binding dtyp operation self.stack
                )

                | Mic.CreateContract (Either.Lft Some (contract)) -> (
                    let binding = Lst.hd mic.vars in
                    let address = Theory.Address.fresh binding in
                    let params, storage_val_dtyp =
                        Stack.Pop.contract_params address self.stack
                    in

                    (* Type check. *)
                    (
                        (fun () -> Dtyp.check contract.storage storage_val_dtyp)
                        |> Exc.chain_err (
                            fun () -> "storage value does not typecheck"
                        )
                    );

                    (* Push address. *)
                    Stack.Push.address address self.stack;

                    (* Push operation. *)
                    let dtyp = Dtyp.Operation |> Dtyp.mk_leaf in
                    let uid = Env.get_uid self.env in
                    let operation = Theory.Of.Operation.create uid params contract in
                    Stack.push ~binding dtyp operation self.stack
                )

                | Mic.Leaf CreateAccount -> (
                    let binding = Lst.hd mic.vars in
                    let address = Theory.Address.fresh binding in
                    let params =
                        Stack.Pop.account_params address self.stack
                    in

                    (* Push address. *)
                    Stack.Push.address address self.stack;

                    (* Push operation. *)
                    let dtyp = Dtyp.Operation |> Dtyp.mk_leaf in
                    let uid = Env.get_uid self.env in
                    let operation = Theory.Of.Operation.create uid params Mic.unit_contract in
                    Stack.push ~binding dtyp operation self.stack
                )

                | Mic.CreateContract (Either.Rgt name) -> (
                    let contract = Env.get name self.env in
                    let binding = Lst.hd mic.vars in
                    let address = Theory.Address.fresh binding in
                    let params, storage_val_dtyp =
                        Stack.Pop.contract_params address self.stack
                    in

                    (* Type check. *)
                    (
                        (fun () -> Dtyp.check contract.storage storage_val_dtyp)
                        |> Exc.chain_err (
                            fun () -> "storage value does not typecheck"
                        )
                    );

                    (* Push address. *)
                    Stack.Push.address address self.stack;

                    (* Push operation. *)
                    let dtyp = Dtyp.Operation |> Dtyp.mk_leaf in
                    let uid = Env.get_uid self.env in
                    let operation =
                        Contract.to_mic contract
                        |> Theory.Of.Operation.create uid params
                    in
                    Stack.push ~binding dtyp operation self.stack
                )

                | Mic.Leaf SetDelegate -> (
                    let binding = Lst.hd mic.vars in
                    let delegate = Stack.Pop.key_hash_option self.stack |> fst in

                    let address =
                        match self.src with
                        | Src.Contract address -> address
                        | Src.Test _ ->
                            Exc.throw "cannot use `SET_DELEGATE` in a testcase"
                    in

                    (* Push operation. *)
                    let dtyp = Dtyp.Operation |> Dtyp.mk_leaf in
                    let uid = Env.get_uid self.env in
                    let operation =
                        Theory.Of.Operation.set_delegate uid address delegate
                    in

                    Stack.push ~binding dtyp operation self.stack
                )

                | Mic.Leaf TransferTokens ->
                    let binding = Lst.hd mic.vars in
                    let param, param_dtyp = Stack.pop self.stack in
                    let tez = Stack.Pop.tez self.stack |> fst in
                    let address, contract = Stack.Pop.contract self.stack in
                    Dtyp.check contract.param param_dtyp;
                    let address =
                        match address with
                        | Some a -> a
                        | None -> Exc.throw "cannot transfer to undeployed contract"
                    in
                    let uid = Env.get_uid self.env in
                    let operation = Theory.Of.Operation.transfer uid address contract tez param in
                    let dtyp = Dtyp.Operation |> Dtyp.mk_leaf in
                    Stack.push ~binding dtyp operation self.stack

                (* # Macros. *)

                | Mic.Macro (subs, _) ->
                    push_block (Nop (mic, self.next)) self;
                    self.next <- subs

                (* # Extensions. *)

                | Mic.Extension (Mic.StorageOf storage_dtyp) ->
                    let binding = Lst.hd mic.vars in
                    let alias = Lst.hd mic.typs in
                    let address, _ = Stack.Pop.contract self.stack in
                    let address =
                        match address with
                        | Some address -> address
                        | None -> Exc.throw "cannot retrieve storage of an undeployed contract"
                    in
                    let value, dtyp =
                        match Env.Live.get address self.env with
                        | None ->
                            asprintf "there is no contract at address %a"
                                Theory.Address.fmt address
                            |> Exc.throw
                        | Some contract -> (
                            let dtyp =
                                let sub = Dtyp.mk_named None contract.contract.storage in
                                Dtyp.Option sub |> Dtyp.mk ~alias
                            in
                            let value =
                                try (
                                    Dtyp.check storage_dtyp contract.contract.storage;
                                    Some contract.storage |> Theory.Of.option
                                ) with
                                | _ -> Theory.Of.option None
                            in
                            value, dtyp
                        )
                    in

                    Stack.push ~binding dtyp value self.stack

                | Mic.Extension Mic.ApplyOps -> Exc.Throw.apply_ops ()

                | Mic.Extension Mic.PrintStack ->
                    log_0 "@[%a@]@." Stack.fmt self.stack

                | Mic.Extension Mic.BalanceOf ->
                    let binding = Lst.hd mic.vars in
                    let address, _ = Stack.Pop.contract self.stack in
                    let address =
                        match address with
                        | Some address -> address
                        | None -> Exc.throw "cannot retrieve balance of an undeployed contract"
                    in
                    let value =
                        match Env.Live.get address self.env with
                        | None ->
                            asprintf "there is no contract at address %a"
                                Theory.Address.fmt address
                            |> Exc.throw
                        | Some contract -> contract.balance |> Theory.Of.tez
                    in
                    let dtyp = Dtyp.Mutez |> Dtyp.mk_leaf in

                    Stack.push ~binding dtyp value self.stack
                
                | Mic.Extension Mic.MustFail ->
                    let expected = Stack.Pop.option self.stack |> fst in
                    let value =
                        Stack.Pop.operation self.stack
                        |> fst
                        |> Env.Op.must_fail self.env expected
                    in
                    let dtyp = Dtyp.Operation |> Dtyp.mk_leaf in

                    Stack.push dtyp value self.stack

                (* # Unimplemented stuff. *)

                | _ -> asprintf "unsupported instruction @[%a@]" Mic.fmt mic |> Exc.throw
            );
            false
        )
        in
        run |> Exc.chain_errs (
            fun () ->
                let tail =
                    match self.last with
                    | Some ins -> [asprintf "on instruction @[%a@]" Mic.fmt ins]
                    | None -> []
                in
                (asprintf "while running code for %a" Src.fmt self.src) :: tail
        )
    
    let rec run (self : t) : unit =
        let is_done = step self in
        if is_done then () else run self

    let init
        (src : Src.t)
        ~(balance : Theory.Tez.t)
        ~(amount : Theory.Tez.t)
        (env : Env.t)
        (values : (Theory.value * Dtyp.t * Annot.Var.t option) list)
        (inss : Mic.t list)
        : t
    =
        let self =
            {
                stack = Stack.empty env ;
                blocks = [] ;
                next = inss ;
                last = None ;
                balance ;
                amount ;
                src ;
                env ;
            }
        in
        values |> List.iter (
            fun (value, dtyp, binding) ->
                Stack.push ~binding dtyp value self.stack
        );
        self
end

(** Creates a test interpreter from a normal interpreter. *)
module TestInterpreter (
    I : Sigs.Interpreter
) : Sigs.TestInterpreter with module Run = I
= struct
    module Run = I
    module Env = Run.Env
    module Theory = Run.Theory
    module Src = Run.Src

    type t = {
        interp : Run.t ;
        tc : Testcase.t ;
        src : Src.t ;
    }

    let contract_env (self : t) : Env.t = self.interp |> Run.contract_env

    let is_done (self : t) : bool = Run.is_done self.interp
    let interp (self : t) : Run.t = self.interp
    let mk (src : Src.t) (tc : Testcase.t) (contracts : Env.t) : t =
        let many_tez = Theory.Tez.of_native Int64.max_int in
        let interp = I.init src ~balance:many_tez ~amount:many_tez contracts [] [ tc.code ] in
        { interp ; tc ; src }

    let step (self : t) : Env.operation list option =
        if Run.is_done self.interp then None else (
            try (
                Run.run self.interp;
                None
            ) with
            | Exc.Exc (Exc.Internal Exc.Internal.ApplyOps)-> Some (
                Run.stack self.interp |> Run.Stack.Pop.operation_list |> fst
            )
        )
    
    let balance (self : t) : Theory.Tez.t = Run.balance self.interp
end
