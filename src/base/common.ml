(* Common types and functions used by the whole program. *)

include Format

let if_let_some (f : 'a -> unit) (opt : 'a option) : unit = match opt with
| None -> ()
| Some a -> f a

let unwrap_or (a : 'a) (opt : 'a option) : 'a = match opt with
| None -> a
| Some a -> a

let unwrap_or_else (f : unit -> 'a) (opt : 'a option) : 'a = match opt with
| None -> f ()
| Some a -> a

module Lst = struct
    let len = List.length
    let hd (l : 'a list) : 'a option =
        match l with
        | hd :: _ -> Some hd
        | [] -> None
    let tl (l : 'a list) : 'a list option =
        match l with
        | _ :: tl -> Some tl
        | [] -> None
    let fold = List.fold_left
end

module Fmt = struct
    type sep = unit -> unit
    type seq_end = unit -> unit

    let fmt_list (sep : formatter -> unit -> unit) :
        (formatter -> 'a -> unit) ->
        formatter ->
        'a list ->
        unit
    = pp_print_list ~pp_sep:sep

    let fmt_paren
        (fmt_stuff : (formatter -> 'a -> unit))
        (fmt : formatter)
        (stuff : 'a)
        : unit
    =
        fprintf fmt "(%a)" fmt_stuff stuff

    let cls (fmt : formatter) : seq_end =
        fun () -> fprintf fmt "@]"
    let cls2 (fmt : formatter) : seq_end =
        fun () -> fprintf fmt "@]@]"
    let cls_of (fmt : formatter) (s : string) : seq_end =
        fun () -> fprintf fmt "%s@]" s
    let cls2_of (fmt : formatter) (s : string) : seq_end =
        fun () -> fprintf fmt "%s@]@]" s
    let cls_paren (fmt : formatter) : seq_end =
        fun () -> fprintf fmt ")"

    let sep_brk (fmt : formatter) : sep =
        fun () -> fprintf fmt "@,"
    let sep_spc (fmt : formatter) : sep =
        fun () -> fprintf fmt " "
    let sep_non (_ : formatter) : sep =
        fun () -> ()

    let plurify (n : int) : string = if n = 1 then "" else "s"
end

module Check = struct
    let arity
        (desc : string)
        (expected : int)
        (blah : unit -> string)
        (args : 'a list)
        : unit
    =
        if expected <> List.length args then (
            sprintf "%s expects %i %s%s" (blah ()) expected desc (List.length args |> Fmt.plurify)
            |> Exc.throw
        )
end

let conf_ref : Conf.t ref = ref Conf.default

let set_conf (conf : Conf.t) : unit = conf_ref := conf

let conf () : Conf.t = ! conf_ref