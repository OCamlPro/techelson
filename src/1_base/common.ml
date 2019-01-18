(* Common types and functions used by the whole program. *)

include Format

let id (a : 'a) : 'a = a

let if_let_some (f : 'a -> unit) (opt : 'a option) : unit = match opt with
| None -> ()
| Some a -> f a

let is_some (opt : 'a option) : bool = match opt with
| Some _ -> true
| None -> false

let unwrap (opt : 'a option) : 'a = match opt with
| Some a -> a
| None -> Exc.unreachable ()

let unwrap_or (a : 'a) (opt : 'a option) : 'a = match opt with
| None -> a
| Some a -> a

let unwrap_or_else (f : unit -> 'a) (opt : 'a option) : 'a = match opt with
| None -> f ()
| Some a -> a

let open_file (file : string) : in_channel =
    try open_in file with
    | e -> Exc.throws [
        sprintf "while opening file `%s`" file ;
        Printexc.to_string e
    ]

module Either = struct
    type ('l, 'r) t = | Lft of 'l | Rgt of 'r

    let lft (lft : 'l) : ('l, 'r) t = Lft lft
    let rgt (rgt : 'r) : ('l, 'r) t = Rgt rgt

    let map_lft (f : 'l -> 'lft) (self : ('l, 'r) t) : ('lft, 'r) t =
        match self with
        | Lft lft -> Lft (f lft)
        | Rgt rgt -> Rgt rgt

    let map_rgt (f : 'r -> 'rgt) (self : ('l, 'r) t) : ('l, 'rgt) t =
        match self with
        | Lft lft -> Lft lft
        | Rgt rgt -> Rgt (f rgt)

    let fmt
        (fmt_l : formatter -> 'l -> unit)
        (fmt_r : formatter -> 'r -> unit)
        (fmt : formatter)
        (either : ('l, 'r) t)
        : unit
    =
        match either with
        | Lft l -> fprintf fmt "Lft(%a)" fmt_l l
        | Rgt r -> fprintf fmt "Rgt(%a)" fmt_r r

    let fmt_through
        (fmt_l : formatter -> 'l -> unit)
        (fmt_r : formatter -> 'r -> unit)
        (fmt : formatter)
        (either : ('l, 'r) t)
        : unit
    =
        match either with
        | Lft l -> fprintf fmt "%a" fmt_l l
        | Rgt r -> fprintf fmt "%a" fmt_r r
end

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

    let of_opt (opt : 'a option) : 'a list =
        match opt with
        | None -> []
        | Some a -> [a]
end

module Fmt = struct
    let fmt_str (fmt : formatter) (s : string) : unit =
        pp_print_string fmt s

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
    let sep_spc_brk (fmt : formatter) : sep =
        fun () -> fprintf fmt "@ "
    let sep_spc (fmt : formatter) : sep =
        fun () -> fprintf fmt " "
    let sep_non (_ : formatter) : sep =
        fun () -> ()

    let unit_combine (f_1 : unit -> unit) (f_2 : unit -> unit) : unit -> unit =
        fun () -> f_1 () ; f_2 ()

    let plurify (n : int) : string = if n = 1 then "" else "s"
end

module IntSet = struct
    type t = (int, unit) Hashtbl.t
    let empty () : t = Hashtbl.create ~random:false 101
    let clone : t -> t = Hashtbl.copy
    let add (elm : int) (set : t) : bool =
        if Hashtbl.mem set elm then false else (
            Hashtbl.add set elm ();
            true
        )
end

module StrMap = struct
    include Map.Make(String)
    let get (key : key) (map : 'a t) : 'a option =
        try Some (find key map) with | Not_found -> None
    let insert (key : key) (value : 'a) (map : 'a t) : 'a t * 'a option =
        let prev = get key map in
        add key value map, prev
    let values (map : 'a t) : 'a list =
        fold (fun _ value acc -> value :: acc) map []
end

module Source = struct
    type t =
    | Stdin
    | File of string

    let fmt (fmt : formatter) (t : t) : unit =
        match t with
        | Stdin -> fprintf fmt "stdin"
        | File file -> fprintf fmt "file `%s`" file

    let to_channel (t : t) : in_channel =
        match t with
        | Stdin -> stdin
        | File file -> open_file file
end

let conf_ref : Conf.t ref = ref Conf.default

let set_conf (conf : Conf.t) : unit = conf_ref := conf

let conf () : Conf.t = ! conf_ref

let out = formatter_of_out_channel stdout

let rec indent (fmt : formatter) (count : int) : unit =
    if count > 0 then (
        fprintf fmt " ";
        count - 1 |> indent fmt
    ) else ()

let log (lvl : int) (args : ('a, formatter, unit) format) : 'a =
    if lvl <= (!conf_ref).verb then (
        (lvl - 1) * 4 |> indent out;
        fprintf out args;
    ) else (
        ifprintf out args
    )

let log_0 (args : ('a, Format.formatter, unit) format) : 'a = log 0 args
let log_1 (args : ('a, Format.formatter, unit) format) : 'a = log 1 args
let log_2 (args : ('a, Format.formatter, unit) format) : 'a = log 2 args
let log_3 (args : ('a, Format.formatter, unit) format) : 'a = log 3 args
let log_4 (args : ('a, Format.formatter, unit) format) : 'a = log 4 args

let catch_exn (f : unit -> 'a) : ('a, exn) Either.t =
    try f () |> Either.lft with
    | e -> Either.Rgt e

let catch_protocol_exn (f : unit -> 'a) : ('a, Exc.Protocol.t) Either.t =
    try f () |> Either.lft with
    | Exc.Exc (Exc.Protocol p) -> Either.rgt p
    | e ->
        log_0 "@.@.%s@.@." (Printexc.to_string e);
        raise e