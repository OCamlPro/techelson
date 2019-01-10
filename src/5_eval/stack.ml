open Base
open Base.Common

module Naive : Sigs.SigStackRaw = struct
    module Theory = Theo.Naive.Theory

    type frame = {
        mutable value : Theory.value ;
        typ : Dtyp.t ;
        binding : Annot.Var.t option ;
    }

    let clone ({value ; typ ; binding} : frame) : frame = { value ; typ ; binding }

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
    }

    let fmt (fmt : formatter) (self : t) : unit =
        fprintf fmt "@[<v>|==================================================================================================|";
        self.stack |> List.rev |> List.iter (
            fprintf fmt "@,| %a |" fmt_frame
        );

        if self.dipped <> [] then (
           fprintf fmt "@,|================================================dipped============================================|";
            self.dipped |> List.iter (
                fprintf fmt "@,| %a |" fmt_frame
            )
        );

        fprintf fmt "@,|==================================================================================================|";
        fprintf fmt "@]"

    let empty () : t = { dipped = [] ; stack = [] }
    let is_empty ({ dipped ; stack } : t) : bool = dipped = [] && stack = []
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

    let dup (self : t) : unit =
        match self.stack with
        | hd :: _ -> self.stack <- (clone hd) :: self.stack
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
end