(* Types and helpers for annotations. *)

open Common

module Var = struct
    type t = string option

    let of_string (s : string) : t = if s = "" then None else Some s

    let fmt (fmt : formatter) (t : t) : unit =
        t |> unwrap_or "" |> fprintf fmt "@@%s"
end

module Field = struct
    type t = string option

    let of_string (s : string) : t = if s = "" then None else Some s

    let fmt (fmt : formatter) (t : t) : unit =
        t |> unwrap_or "" |> fprintf fmt "@@%s"
end

module Typ = struct
    type t = string option

    let of_string (s : string) : t = if s = "" then None else Some s

    let fmt (fmt : formatter) (t : t) : unit =
        t |> unwrap_or "" |> fprintf fmt "@@%s"
end

type typs = Typ.t list

let fmt_typs (fmt: formatter) (typs : typs) : unit =
    if typs <> [] then (
        fprintf fmt " %a" (Fmt.fmt_list Fmt.sep_spc Var.fmt) typs
    )

type vars = Var.t list

let fmt_vars (fmt: formatter) (vars : vars) : unit =
    if vars <> [] then (
        fprintf fmt " %a" (Fmt.fmt_list Fmt.sep_spc Var.fmt) vars
    )

type fields = Field.t list

let fmt_fields (fmt: formatter) (fields : fields) : unit =
    if fields <> [] then (
        fprintf fmt " %a" (Fmt.fmt_list Fmt.sep_spc Var.fmt) fields
    )
