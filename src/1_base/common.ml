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
    (fun () -> open_in file)
    |> Exc.chain_err (
        fun () -> sprintf "while opening file `%s` in read mode" file
    )

let open_file_write (file : string) : out_channel =
    (fun () -> open_out_gen [ Open_wronly ; Open_creat ; Open_trunc ] 0o644 file)
    |> Exc.chain_err (
        fun () -> sprintf "while opening file `%s` in write mode" file
    )

module Opt = struct
    let map (f : 'a -> 'b) (opt : 'a option) : 'b option =
        match opt with
        | Some a -> Some (f a)
        | None -> None

    let fmt (sub_fmt : formatter -> 'a -> unit) (fmt : formatter) (opt : 'a option) : unit =
        match opt with
        | None -> fprintf fmt "None"
        | Some a ->
            fprintf fmt "Some (%a)" sub_fmt a

    let to_list (opt : 'a option) : 'a list =
        match opt with
        | None -> []
        | Some a -> [a]

    let and_then (f : 'a -> 'b option) (opt : 'a option) : 'b option =
        match opt with
        | None -> None
        | Some a -> f a
end

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
    | Gen

    let fmt (fmt : formatter) (t : t) : unit =
        match t with
        | Stdin -> fprintf fmt "stdin"
        | File file -> fprintf fmt "file `%s`" file
        | Gen -> fprintf fmt "test generation"

    let to_channel (t : t) : in_channel =
        match t with
        | Stdin -> stdin
        | File file -> open_file file
        | Gen -> Exc.throw "no channel associated with `Generated` source"
end

let conf_ref : Conf.t ref = ref Conf.default

let set_conf (conf : Conf.t) : unit =
    conf.verb > 1 |> Exc.set_print_backtrace;
    conf_ref := conf

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
    | e -> (
        match Exc.get_protocol e with
        | Some e -> Either.rgt e
        | None -> raise e
    )

let catch_internal_exn (f : unit -> 'a) : ('a, Exc.Internal.t) Either.t =
    try f () |> Either.lft with
    | e -> (
        match Exc.get_internal e with
        | Some e -> Either.rgt e
        | None -> raise e
    )

module Rng = struct
    let state : Random.State.t ref = (
        let old_state = Random.get_state () in
        Random.init 42;
        let state = Random.get_state () in
        Random.set_state old_state;
        ref state
    )

    let wrap (f : 'a -> 'b) (a : 'a) : 'b =
        let old_state = Random.get_state () in
        Random.set_state !state;
        let set_state () =
            state := Random.get_state ();
            Random.set_state old_state;
        in
        try (
            let res = f a in
            set_state () ;
            res
        ) with e -> set_state () ; raise e

    let unsafe_bool () : bool =
        Random.bool ()
    let bool () : bool = wrap unsafe_bool ()

    let unsafe_pos_int bound : int =
        bound
        |> unwrap_or max_int
        |> Random.int
    let pos_int ?bound:(bound=None) () : int =
        wrap unsafe_pos_int bound

    let unsafe_int bound : int =
        bound
        |> unwrap_or max_int
        |> (
            fun bound ->
                let res = Random.int bound in
                let neg = Random.bool () in
                if neg then - res else res
        )
    let int ?bound:(bound=None) () : int =
        wrap unsafe_int bound

    let unsafe_int64 bound : int64 =
        bound
        |> unwrap_or Int64.max_int
        |> Random.int64
    let int64 ?bound:(bound=None) () : int64 =
        wrap unsafe_int64 bound

    let rec unsafe_char () : char =
        let index = unsafe_pos_int (Some 95) in
        let char = index + 32 |> Char.chr in
        if char = '\\' || char = '"' then unsafe_char () else char
    let char () : char =
        wrap unsafe_char ()

    let unsafe_key_char () : char =
        (* Number or letter? *)
        if unsafe_bool () then (
            (* Number. *)
            let char = unsafe_pos_int (Some 10) in
            char + 48 |> Char.chr
        ) else (
            (* Letter. *)
            let char = unsafe_pos_int (Some 6) in
            char + 97 |> Char.chr
        )
    let key_char () : char =
        wrap unsafe_key_char ()

    module Arith = struct
        let unsafe_zero () : bool =
            let rand = Random.int 101 in
            Proba.Arith.zero >= rand
        let zero () : bool = wrap unsafe_zero ()
    end

    module Opt = struct
        let unsafe_none () : bool =
            Proba.Opt.none >= Random.int 101
        let none () : bool = wrap unsafe_none ()

    end

    module Coll = struct
        let unsafe_empty () : bool =
            Proba.Coll.empty >= Random.int 101
        let empty () : bool = wrap unsafe_empty ()

        let unsafe_add_one () : bool =
            Proba.Coll.add_one >= Random.int 101
        let add_one () : bool = wrap unsafe_add_one ()
    end

    let unsafe_build_coll (empty : 'a) (add_one : 'a -> 'a) : 'a =
        if Coll.unsafe_empty () then empty else (
            let rec loop (coll : 'a) : 'a =
                if Coll.unsafe_add_one () then
                    add_one coll |> loop
                else
                    coll
            in
            loop empty
        )

    let unsafe_string () : string =
        unsafe_build_coll "" (
            fun s -> unsafe_char () |> sprintf "%s%c" s
        )
    let string () : string =
        wrap unsafe_string ()

    let unsafe_key () : string =
        unsafe_build_coll "" (
            fun s -> unsafe_key_char () |> sprintf "%s%c" s
        )
    let key () : string =
        wrap unsafe_key ()

    let unsafe_big_nat () : string =
        if Arith.unsafe_zero () then "0" else (
            let digit = 1 + (Some 8 |> unsafe_pos_int) |> string_of_int in
            let rec loop (big_nat : string) : string =
                if Coll.unsafe_add_one () then
                    sprintf "%s%i" big_nat (Some 9 |> unsafe_pos_int) |> loop
                else big_nat
            in
            loop digit
        )
    let big_nat () : string =
        wrap unsafe_big_nat ()

    let unsafe_tiny_nat () : string =
        if Arith.unsafe_zero () then "0" else (
            let digit = 1 + (Some 8 |> unsafe_pos_int) |> string_of_int in
            let rec loop (tiny_nat : string) : string =
                if String.length tiny_nat > 9 then
                    tiny_nat
                else if Coll.unsafe_add_one () then
                    sprintf "%s%i" tiny_nat (Some 9 |> unsafe_pos_int) |> loop
                else tiny_nat
            in
            loop digit
        )
    let tiny_nat () : string =
        wrap unsafe_tiny_nat ()

    let unsafe_big_int () : string =
        if Arith.unsafe_zero () then "0" else (
            let digit = 1 + (Some 8 |> unsafe_pos_int) |> string_of_int in
            let head =
                if unsafe_bool () then "-" ^ digit else digit
            in
            let rec loop (big_nat : string) : string =
                if Coll.unsafe_add_one () then
                    sprintf "%s%i" big_nat (Some 9 |> unsafe_pos_int) |> loop
                else big_nat
            in
            loop head
        )
    let big_int () : string =
        wrap unsafe_big_int ()

    module Test = struct
        let transfer () : bool =
            let rand = Random.int 101 in
            Proba.Test.add_transfer >= rand
    end
end