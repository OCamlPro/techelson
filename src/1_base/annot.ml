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