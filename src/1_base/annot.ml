(* Types and helpers for annotations. *)

open Common

module Var = struct
    type t = string option

    let of_string (s : string) : t = if s = "" then None else Some s
    let wild = of_string ""
    let is_wild s = s = wild

    let fmt (fmt : formatter) (t : t) : unit =
        t |> unwrap_or "" |> fprintf fmt "@@%s"
end

module Field = struct
    type t = string option

    let of_string (s : string) : t = if s = "" then None else Some s
    let wild = of_string ""
    let is_wild s = s = wild

    let fmt (fmt : formatter) (t : t) : unit =
        t |> unwrap_or "" |> fprintf fmt "%%%s"
end

module Typ = struct
    type t = string option

    let of_string (s : string) : t = if s = "" then None else Some s
    let wild = of_string ""
    let is_wild s = s = wild

    let fmt (fmt : formatter) (t : t) : unit =
        t |> unwrap_or "" |> fprintf fmt ":%s"
end

type typs = Typ.t list

let fmt_typs (fmt : formatter) (typs : typs) : unit =
    if typs <> [] then (
        fprintf fmt " %a" (Fmt.fmt_list Fmt.sep_spc Typ.fmt) typs
    )

type vars = Var.t list

let fmt_vars (fmt : formatter) (vars : vars) : unit =
    if vars <> [] then (
        fprintf fmt " %a" (Fmt.fmt_list Fmt.sep_spc Var.fmt) vars
    )

type fields = Field.t list

let fmt_fields (fmt : formatter) (fields : fields) : unit =
    if fields <> [] then (
        fprintf fmt " %a" (Fmt.fmt_list Fmt.sep_spc Field.fmt) fields
    )

type t = {
    typs : typs ;
    vars : vars ;
    fields : fields ;
}

let mk (typs : typs) (vars : vars) (fields : fields) : t = { typs ; vars ; fields }
let empty : t = mk [] [] []

let fmt (fmt : formatter) (t : t) : unit =
    fmt_typs fmt t.typs;
    fmt_vars fmt t.vars;
    fmt_fields fmt t.fields

let to_dtyp_annots (self : t) : Typ.t option * Field.t option =
    let t_annot =
        match self.typs with
        | [] -> None
        | [annot] -> Some annot
        | _ -> (
            asprintf "expected zero or one type annotation, found %i" (List.length self.typs)
            |> Exc.throw
        )
    in
    let f_annot =
        match self.fields with
        | [] -> None
        | [annot] -> Some annot
        | _ -> (
            asprintf "expected zero or one field annotation, found %i" (List.length self.fields)
            |> Exc.throw
        )
    in
    if self.vars <> [] then (
        asprintf "expected no variable annotation, found %i" (List.length self.vars)
        |> Exc.throw
    );

    t_annot, f_annot

let rev (self : t) : t =
    { typs = List.rev self.typs ; vars = List.rev self.vars ; fields = List.rev self.fields }

let cons_typ (annot : Typ.t) (self : t) : t =
    { self with typs = annot :: self.typs }
let cons_var (annot : Var.t) (self : t) : t =
    { self with vars = annot :: self.vars }
let cons_field (annot : Field.t) (self : t) : t =
    { self with fields = annot :: self.fields }