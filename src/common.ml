(* Common types and functions used by the whole program. *)

include Format

let if_let_some (f: 'a -> unit) (opt : 'a option) : unit = match opt with
| None -> ()
| Some a -> f a

module Fmt = struct
    type sep = unit -> unit
    type seq_end = unit -> unit

    let fmt_list (sep : formatter -> unit -> unit) :
        (formatter -> 'a -> unit) ->
        formatter ->
        'a list ->
        unit
    = pp_print_list ~pp_sep:sep

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

(* Configuration, built by CLAP. *)
let conf : Conf.t =
    let chained : unit -> Conf.t =
        fun () -> Exc.chain_err (
            fun () -> "while parsing command-line arguments"
        ) (fun () -> Clap.run ())
    in
    Exc.catch_fail chained