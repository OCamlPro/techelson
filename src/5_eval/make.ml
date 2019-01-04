open Base
open Base.Common

module Cxt (T : Sigs.SigStack) : Sigs.SigCxt = struct
    module Theory = T.Theory
    module Stack = T

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

    let interpret (mic : Mic.t) (self : t) : bool =
        let run () : bool = match fetch_next self with
        (* Nothing left to do, done. *)
        | None -> true

        (* Let's do this. *)
        | Some mic -> (
            self.last <- Some mic;
            (
                match mic.ins with

                (* # Basic stack manipulation. *)

                | Mic.Push (dtyp, const) ->
                    let binding = Lst.hd mic.vars in
                    self.stack |> Stack.push ~binding dtyp (Theory.Of.const const)

                | Mic.Leaf Drop ->
                    let _ = Stack.pop self.stack in
                    ()

                | Mic.Leaf Som ->
                    let _ = Stack.some self.stack in
                    ()
                | Mic.Non dtyp ->
                    let _ = Stack.none dtyp self.stack in
                    ()

                | Mic.Left dtyp ->
                    let _ = Stack.left dtyp self.stack in
                    ()
                | Mic.Right dtyp ->
                    let _ = Stack.right dtyp self.stack in
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

                (* # List operations. *)

                | Mic.Leaf Cons ->
                    Stack.cons self.stack;
                    ()

                | Mic.Nil dtyp ->
                    Stack.nil dtyp self.stack;
                    ()

                | Mic.Leaf Size ->
                    let lst, _ = Stack.pop_list self.stack in
                    let len = Theory.Lst.size lst |> Theory.Of.nat in
                    Stack.push (Dtyp.nat) len self.stack

                (* # Domain-specific. *)

                | Mic.Leaf Now ->
                    let binding = Lst.hd mic.vars in
                    let now = Theory.TStamp.now () |> Theory.Of.timestamp in
                    Stack.push ~binding Dtyp.timestamp now self.stack

                (* # Unimplemented stuff. *)

                | _ -> asprintf "unsupported instruction @[%a@]" Mic.fmt mic |> Exc.throw
            );
            false
        )
        in
        run |> Exc.chain_err (
            fun () -> asprintf "while making a step in the evaluator : @[%a@]" Mic.fmt  mic
        )

    let step (self : t) : bool =
        match next_ins self with
        | None -> true
        | Some ins -> interpret ins self

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
