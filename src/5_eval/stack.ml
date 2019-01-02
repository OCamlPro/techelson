open Base
open Base.Common

module Naive : Sigs.SigStack = struct
    module Theory = Theo.Naive.Theory

    type frame = {
        mutable value : Theory.value ;
        typ : Dtyp.t ;
        binding : Annot.Var.t option ;
    }

    let mk_frame (value : Theory.value) (typ : Dtyp.t) (binding : Annot.Var.t option) : frame =
        { value ; typ ; binding }

    type t = {
        mutable dipped : frame list ;
        mutable stack : frame list ;
    }

    let empty : t = { dipped = [] ; stack = [] }
    let is_empty (t : t) : bool = t = empty
    let push ?binding:(binding=None) (dtyp : Dtyp.t) (value : Theory.value) (self : t) : unit =
        let frame = mk_frame value dtyp binding in
        self.stack <- frame :: self.stack

    let swap (self : t) : unit =
        match self.stack with
        | hd_1 :: hd_2 :: tail -> self.stack <- hd_2 :: hd_1 :: tail
        | _ -> Exc.throw "cannot `swap` a stack with less than two frames"

    let dup (self : t) : unit =
        match self.stack with
        | hd :: _ -> self.stack <- hd :: self.stack
        | [] ->
            Exc.throw "cannot `dup` an empty stack"

    let dip (self : t) : unit =
        match self.stack with
        | hd :: stack ->
            self.dipped <- hd :: self.dipped;
            self.stack <- stack
        | [] ->
            Exc.throw "cannot `dip` an empty stack"

    let undip (self : t) : unit =
        match self.dipped with
        | hd :: dipped ->
            self.dipped <- dipped;
            self.stack <- hd :: self.stack
        | [] ->
            Exc.throw "cannot `dip` a dip-free stack"

    let pop (self : t) : Theory.value * Dtyp.t =
        match self.stack with
        | hd :: stack ->
            self.stack <- stack;
            hd.value, hd.typ
        | [] -> Exc.throw "cannot pop value on an empty value stack"

    let pop_bool (self : t) : bool * Dtyp.t =
        let run () =
            match pop self with
            | Theory.C (Theory.Cmp.B b), dtyp -> b, dtyp
            | _, dtyp -> asprintf "found a value of type %a" Dtyp.fmt dtyp |> Exc.throw
        in
        run |> Exc.chain_err (
            fun () -> "while popping a boolean value from the stack"
        )

    let pop_int (self : t) : Theory.Cmp.Int.t * Dtyp.t =
        let run () =
            match pop self with
            | Theory.C (Theory.Cmp.I i), dtyp -> i, dtyp
            | _, dtyp -> asprintf "found a value of type %a" Dtyp.fmt dtyp |> Exc.throw
        in
        run |> Exc.chain_err (
            fun () -> "while popping an integer value from the stack"
        )

    let pop_nat (self : t) : Theory.Cmp.Nat.t * Dtyp.t =
        let run () =
            match pop self with
            | Theory.C (Theory.Cmp.N n), dtyp -> n, dtyp
            | _, dtyp -> asprintf "found a value of type %a" Dtyp.fmt dtyp |> Exc.throw
        in
        run |> Exc.chain_err (
            fun () -> "while popping a natural value from the stack"
        )

    let pop_str (self : t) : Theory.Cmp.Str.t * Dtyp.t =
        let run () =
            match pop self with
            | Theory.C (Theory.Cmp.S s), dtyp -> s, dtyp
            | _, dtyp -> asprintf "found a value of type %a" Dtyp.fmt dtyp |> Exc.throw
        in
        run |> Exc.chain_err (
            fun () -> "while popping a natural value from the stack"
        )

    let pop_either (self : t) : (Theory.value, Theory.value) Theory.Either.t * Dtyp.t =
        let run () =
            match pop self with
            | Theory.Either either, dtyp -> either, dtyp
            | _, dtyp -> asprintf "found a value of type %a" Dtyp.fmt dtyp |> Exc.throw
        in
        run |> Exc.chain_err (
            fun () -> "while popping a union value from the stack"
        )

    let pop_option (self : t) : Theory.value Theory.Option.t * Dtyp.t =
        let run () =
            match pop self with
            | Theory.Option opt, dtyp -> opt, dtyp
            | _, dtyp -> asprintf "found a value of type %a" Dtyp.fmt dtyp |> Exc.throw
        in
        run |> Exc.chain_err (
            fun () -> "while popping an option value from the stack"
        )

    let pop_list (self : t) : Theory.value Theory.Lst.t * Dtyp.t =
        let run () =
            match pop self with
            | Theory.Lst opt, dtyp -> opt, dtyp
            | _, dtyp -> asprintf "found a value of type %a" Dtyp.fmt dtyp |> Exc.throw
        in
        run |> Exc.chain_err (
            fun () -> "while popping a list value from the stack"
        )

    let some (self : t) : unit =
        match self.stack with
        | frame :: stack ->
            self.stack <- stack;
            let dtyp = Dtyp.Option frame.typ |> Dtyp.mk in
            push dtyp (Theory.Of.option (Some frame.value)) self
        | [] -> Exc.throw "cannot build a `SOME` value on an empty stack"
    let none (dtyp : Dtyp.t) (self : t) : unit =
        push dtyp (Theory.Of.option None) self

    let left (rgt_dtyp : Dtyp.t) (self : t) : unit =
        match self.stack with
        | frame :: stack ->
            self.stack <- stack;
            let lft_dtyp = { Dtyp.inner = frame.typ ; name = None} in
            let rgt_dtyp = { Dtyp.inner = rgt_dtyp ; name = None } in
            let dtyp = Dtyp.Or (lft_dtyp, rgt_dtyp) |> Dtyp.mk in
            push dtyp (Theory.Of.either (Theory.Either.Lft frame.value)) self
        | [] -> Exc.throw "cannot build a `LEFT` value on an empty stack"
    let right (lft_dtyp : Dtyp.t) (self : t) : unit =
        match self.stack with
        | frame :: stack ->
            self.stack <- stack;
            let lft_dtyp = { Dtyp.inner = lft_dtyp ; name = None} in
            let rgt_dtyp = { Dtyp.inner = frame.typ ; name = None } in
            let dtyp = Dtyp.Or (lft_dtyp, rgt_dtyp) |> Dtyp.mk in
            push dtyp (Theory.Of.either (Theory.Either.Rgt frame.value)) self
        | [] -> Exc.throw "cannot build a `Right` value on an empty stack"

    let cons (self : t) : unit =
        let run () =
            match self.stack with
            | head_frame :: tail_frame :: stack ->
                let value = Theory.cons head_frame.value tail_frame.value in
                tail_frame.value <- value;
                self.stack <- tail_frame :: stack;
            | _ -> Exc.throw "cannot build a `CONS` value on a empty stack with less than two elements"
        in
        run |> Exc.chain_err (
            fun () -> "while running `CONS`"
        )
    let nil (dtyp : Dtyp.t) (self : t) : unit =
        push dtyp (Theory.Of.list Theory.Lst.nil) self
end