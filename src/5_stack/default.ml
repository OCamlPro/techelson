open Base
open Common

(** A simple stack implementation using lists.

    While this implementation is called *naive*, this is currently the default stack structure.
*)
module Stack : Sigs.StackBase = struct
    module Theory = Theo.Naive
    module Env = Contracts.Contracts (Theory)

    type frame = {
        mutable value : Theory.value ;
        typ : Dtyp.t ;
        binding : Annot.Var.t option ;
    }

    let clone
        (binding : Annot.Var.t option)
        ({value ; typ ; _} : frame)
        : frame
    = { value ; typ ; binding }

    let mk_frame (value : Theory.value) (typ : Dtyp.t) (binding : Annot.Var.t option) : frame =
        { value ; typ ; binding }
    
    let fmt_frame (fmt : formatter) (frame : frame) : unit =
        fprintf fmt "@[<h>%-11s| %-57s | %-23s@]"
            (
                match frame.binding with
                | None -> ""
                | Some annot -> asprintf "%a " Annot.Var.fmt annot
            ) (asprintf "%a" Theory.fmt frame.value) (asprintf "%a" Dtyp.fmt frame.typ)

    type t = {
        mutable dipped : frame list ;
        mutable stack : frame list ;
        env : Env.t
    }

    let contract_env (self : t) : Env.t = self.env

    let fmt (fmt : formatter) (self : t) : unit =
        fprintf fmt "@[<v>\
            |================================================\
            ==================================================|";
        self.stack |> List.rev |> List.iter (
            fprintf fmt "@,| %a |" fmt_frame
        );

        if self.dipped <> [] then (
           fprintf fmt "@,\
            |================================================\
            dipped============================================|";
            self.dipped |> List.iter (
                fprintf fmt "@,| %a |" fmt_frame
            )
        );

        fprintf fmt "@,\
            |================================================\
            ==================================================|";
        fprintf fmt "@]"

    let empty (env : Env.t) : t = { dipped = [] ; stack = [] ; env }
    let is_empty ({ dipped ; stack ; _ } : t) : bool = dipped = [] && stack = []
    let push ?binding:(binding=None) (dtyp : Dtyp.t) (value : Theory.value) (self : t) : unit =
        let frame = mk_frame value dtyp binding in
        self.stack <- frame :: self.stack

    let map_last (f : Theory.value -> Dtyp.t -> Theory.value) (self : t) : unit =
        match self.stack with
        | frame :: _ ->
            let value = f frame.value frame.typ in
            frame.value <- value
        | [] -> Exc.throw "empty stack"

    let swap (self : t) : unit =
        match self.stack with
        | hd_1 :: hd_2 :: tail -> self.stack <- hd_2 :: hd_1 :: tail
        | _ -> Exc.throw "cannot `swap` a stack with less than two frames"

    let dup ?binding:(binding=None) (self : t) : unit =
        match self.stack with
        | hd :: _ -> self.stack <- (clone binding hd) :: self.stack
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
        | [] -> Exc.throw "the stack is empty"

    let clear (self : t) : unit =
        self.stack <- []
end
