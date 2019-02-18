(* CLAP stuff. *)

open Base
open Base.Common

(** Type of arguments and modes descriptions. *)
type description = formatter -> unit -> unit

(** Exception thrown when `help` is asked. *)
exception PrintHelp of Conf.mode

(** Types and helper functions for the type of values accepted by arguments. *)
module Value = struct

    (** Aggregates descriptors for values. *)
    module Desc = struct
        (** Description of the value expected for a string. *)
        let string_val_desc : string = "<string>"

        (** Description of the values expected for a boolean value. *)
        let bool_val_desc : string = "(on|true|True|no|false|False)"

        (** Description of the values expected for an integer value. *)
        let int_val_desc : string = "<int>"
    end

    (** Types and functions for the types of values. *)
    module Typ = struct
        (** Types of values the arguments can accept. *)
        type val_typ =
        | String
        (** Type of string values. *)
        | Bool
        (** Type of bool values. *)
        | Int
        (** Type of int values. *)

        (** Types of values the arguments can accept, with an `optional` flag. *)
        type t = {
            typ : val_typ ;
            (** Type of the values. *)
            opt : bool ;
            (** True if the argument is optional. *)
        }

        (** Value type constructor.

            Boolean argument specifies whether the argument is optional or not.
        *)
        let mk (opt : bool) (typ : val_typ) : t = { typ ; opt }

        (** Integer value type.

            Boolean argument specifies whether the argument is optional or not.
        *)
        let int (opt : bool) : t = mk opt Int

        (** String value type.

            Boolean argument specifies whether the argument is optional or not.
        *)
        let string (opt : bool) : t = mk opt String

        (** Bool value type.

            Boolean argument specifies whether the argument is optional or not.
        *)
        let bool (opt : bool) : t = mk opt Bool

        (** Formatter for `val_typ`. *)
        let fmt_val_typ (fmt : formatter) (val_typ : val_typ) : unit =
            match val_typ with
            | String -> fprintf fmt "%s" Desc.string_val_desc
            | Bool -> fprintf fmt "%s" Desc.bool_val_desc
            | Int -> fprintf fmt "%s" Desc.int_val_desc

        (** Formatter for `t`. *)
        let fmt (fmt : formatter) (self : t) : unit =
            fmt_val_typ fmt self.typ;
            if self.opt then fprintf fmt "?"
    end

    (** Conversions from string to a value. *)
    module To = struct
        (** Retrieves a boolean from a string. *)
        let bool (s : string) : bool = match s with
        | "on" | "true"  | "True"  -> true
        | "no" | "false" | "False" -> false
        | s -> Exc.throws [
            sprintf "expected a boolean value %s" Desc.bool_val_desc ;
            sprintf "found `%s`" s
        ]

        (** Retrieves an integer from a string. *)
        let int (s : string) : int = 
            (fun () -> int_of_string s)
            |> Exc.chain_err (
                fun () -> sprintf "expected integer, found `%s`" s
            )
    end
end

module Arg = struct
    (** Arguments: short (flag) or long. *)
    type t =
    | Short of char
    (** Flag: `-v`. *)
    | Long of string
    (** Option: `--verb`. *)
    | Val of string
    (** Value: some string. *)
    | Comma
    (** A comma separator. *)
    | Sep of string list
    (** Separator: everything after this point are values.
    
        Values appearing in this thing cannot be option arguments. *)
    | Mode of string
    (** Mode. *)

    let _fmt (fmt : formatter) (self : t) : unit =
        match self with
        | Short c -> fprintf fmt "(short '%c')" c
        | Long s -> fprintf fmt "(long \"%s\")" s
        | Val s -> fprintf fmt "(val \"%s\")" s
        | Comma -> fprintf fmt "comma"
        | Sep _ -> fprintf fmt "(sep ...)"
        | Mode s -> fprintf fmt "(mode \"%s\")" s

    (** Returns the head if it is a value, none otherwise. *)
    let next_value (l : t list) : string option * t list = match l with
    | (Val v) :: tail -> Some v, tail
    | _ -> None, l

    (** Returns the next value as a boolean.

        This function fails if the next value cannot be cast to a boolean.
    *)
    let next_bool_value (l : t list)  : bool option * t list =
        let next, tail = next_value l in
        next |> Opt.map Value.To.bool,
        tail

    (** Returns the next value as an integer.
        
        This function fails if the next value cannot be cast to an integer.
    *)
    let next_int_value (l : t list) : int option * t list =
        let next, tail = next_value l in
        next |> Opt.map Value.To.int,
        tail

    (** Returns the next value if the list is a comma and a value. *)
    let next_sep_value (l : t list) : string option * t list = match l with
    | Comma :: (Val v) :: tail -> Some v, tail
    | _ -> None, l
end


module Cla = struct
    (** Type of command line arguments. *)
    type t = {
        short : char list;
        (** Short name of the argument. *)
        long : string list;
        (** Long name of the argument.*)
        values : Value.Typ.t list;
        (** Description of the values accepted by this argument. *)
        action : Arg.t list -> Conf.t -> Arg.t list;
        (** Effect of the argument on the configuration. *)
        help : description
        (** Help message.

            Does not include the description of the values the argument takes.
        *)
    }

    (** Argument constructor. *)
    let mk
        (short : char list)
        (long : string list)
        (values : Value.Typ.t list)
        (help : description)
        (action : Arg.t list -> Conf.t -> Arg.t list)
        : t
    = { short ; long ; values ; action ; help }

    (** Maps long/short arguments to the action they cause. *)
    type options = {
        long_map : (string, Arg.t list -> Conf.t -> Arg.t list) Hashtbl.t ;
        short_map : (char, Arg.t list -> Conf.t -> Arg.t list) Hashtbl.t ;
    }

    let empty_options () = {
        short_map = Hashtbl.create ~random:false 101 ;
        long_map = Hashtbl.create ~random:false 101 ;
    }

    let help : t = mk ['h'] ["help"] [] (
        fun fmt () -> fprintf fmt "prints this help message"
    ) (
        fun _ conf -> PrintHelp conf.mode |> raise
    )

    let verb : t = mk ['v'] ["verb"] [Value.Typ.int true] (
        fun fmt () ->
            fprintf fmt "increases or sets verbosity [default: %i]"
                Conf.default.verb
    ) (
        fun args conf -> match Arg.next_int_value args with
        | None, tail ->
            conf.verb <- conf.verb + 1;
            tail
        | Some i, tail ->
            conf.verb <- i;
            tail
    )

    let quiet : t = mk ['q'] [] [] (
        fun fmt () -> fprintf fmt "decreases verbosity"
    ) (
        fun args conf ->
            conf.verb <- conf.verb - 1;
            args
    )

    let step : t = mk ['s'] ["step"] [Value.Typ.bool true] (
        fun fmt () ->
            fprintf fmt "(de)activates step-by-step evaluation [default: %b]"
                Conf.default.step
    ) (
        fun args conf ->
            let next, tail = Arg.next_bool_value args in
            match next with
            | None
            | Some true ->
                conf.step <- true;
                tail
            | Some false ->
                conf.step <- false;
                tail
    )

    let skip : t = mk [] ["skip"] [Value.Typ.bool true] (
        fun fmt () ->
            fprintf fmt "\
                if true, all steps will automatically advance (and `--step` will be set to@ \
                false) [default: %b]\
            "
                Conf.default.skip
    ) (
        fun args conf ->
            let next, tail = Arg.next_bool_value args in
            match next with
            | None
            | Some true ->
                conf.skip <- true;
                conf.step <- false;
                tail
            | Some false ->
                conf.skip <- false;
                tail
    )

    let contract : t = mk [] [ "contract" ] [Value.Typ.string false ; Value.Typ.string true] (
        fun fmt () ->
            fprintf fmt "\
                adds a contract to the test environment. The second optional argument is the@ \
                contract's initializer.\
            "
    ) (
        fun args conf -> match Arg.next_value args with
        | None, _ -> Exc.throw (
            "argument `--contract` expects at least one value"
        )
        | Some file, tail ->
            let (init, tail) = Arg.next_sep_value tail in
            conf.contracts <- conf.contracts @ [ Conf.mk_contract file init ];
            tail
    )

    let options : t list = [ help ; verb ; quiet ; step ; skip ; contract ]

    let add_all (opts : options) (options : t list) = options |> List.iter (
        fun { short ; long ; action ; _ } ->
            short |> List.iter (
                fun (c : char) -> Hashtbl.add opts.short_map c action
            );
            long |> List.iter (
                fun (s : string) -> Hashtbl.add opts.long_map s action
            );
    )

    (* CLAP modes. *)
    module Modes = struct

        (* Testgen options. *)
        module Testgen = struct

            let test_count : t =
                mk ['n'] ["count"] [Value.Typ.int false] (
                    fun fmt () ->
                        fprintf fmt "sets the number of testcases to generate [default: %i]"
                            Conf.default_testgen_mode.count
                ) (
                    fun args conf -> match Arg.next_int_value args with
                    | None , _ -> Exc.throw (
                        "argument `--count` expects at least one value"
                    )
                    | Some count, tail ->
                        conf |> Conf.map_testgen_mode (fun conf -> conf.count <- count);
                        tail

                )

            let options : t list = [ help ; test_count ]

            let name : string = "testgen"

            let description (fmt : formatter) () : unit =
                fprintf fmt "activates and controls test generation"
        end

        let testgen : string * description * options * Conf.mode =
            let opts = empty_options () in
            add_all opts Testgen.options;
            Testgen.name, Testgen.description, opts, Conf.Testgen Conf.default_testgen_mode

        let all : (string * description * options * Conf.mode) list = [
            testgen
        ]

        let add_all (modes : (string, (Conf.mode * options)) Hashtbl.t) : unit =
            all |> List.iter (
                fun (name, _, options, mode) -> Hashtbl.add modes name (mode, options)
            )
    end

end



(* Command line arguments. *)
let args : string list =
    (Array.length Sys.argv) - 1 |> Array.sub Sys.argv 1 |> Array.to_list

(* Options. *)
let options : Cla.options =
    let opts = Cla.empty_options () in
    Cla.add_all opts Cla.options;
    opts

(* Option modes. *)
let modes : (string, (Conf.mode * Cla.options)) Hashtbl.t =
    let opts = Hashtbl.create ~random:false 101 in
    Cla.Modes.add_all opts;
    opts

(* Preprocesses arguments to split arguments.

    Splits aggregated flags `-vvvafg` in `(short v) :: (short v) :: ...`. *)
let split_args (args : string list) : Arg.t list =
    (* Splits flags. The string must not have the `-` prefix for flags. *)
    let split_flags (acc : Arg.t list) (s : string) : Arg.t list =
        let rec loop (acc : Arg.t list) (cursor : int) (s : string) =
            if cursor >= String.length s then (
                acc
            ) else (
                let acc = (Arg.Short (String.get s cursor)) :: acc in
                loop acc (cursor + 1) s
            )
        in
        loop acc 0 s
    in

    (* Actual short/long splitting. *)
    let rec loop (acc : Arg.t list) : string list -> Arg.t list = function
        | [] -> List.rev acc

        | "--" :: tail ->
            [ Arg.Sep tail ] |> List.rev_append acc

        | "," :: tail -> loop (Arg.Comma :: acc) tail

        | head :: tail -> (
            let head, comma_trail =
                let head_len = String.length head in
                if head_len > 0 && String.sub head (head_len - 1) 1 = "," then (
                    String.sub head 0 (head_len - 1), true
                ) else (
                    head, false
                )
            in
            let acc =
                if String.length head > 1
                && String.get head 0 = '-'
                && String.get head 1 = '-' then (
                    let offset = 2 in
                    let (start, finsh) = (offset, (String.length head - offset)) in
                    (Arg.Long (String.sub head start finsh)) :: acc
                ) else if String.length head > 0 && String.get head 0 = '-' then (
                    let offset = 1 in
                    let (start, finsh) = (offset, (String.length head - offset)) in
                    String.sub head start finsh |> split_flags acc
                ) else if Hashtbl.mem modes head then (
                    Arg.Mode head :: acc
                ) else (
                    (Arg.Val head) :: acc
                )
            in
            let acc =
                if comma_trail then Arg.Comma :: acc else acc
            in
            loop acc tail
        )
    in
    loop [] args

(* Handles a list of arguments. *)
let rec handle_args (options : Cla.options) (conf : Conf.t) (args : Arg.t list) : Conf.t =
    let rec loop (args : Arg.t list) : Conf.t =
        match args with
        | [] -> (
            conf.args <- List.rev conf.args;
            conf
        )
        (* Comma should be eaten by options. *)
        | Arg.Comma :: _ -> (
            Exc.throw "unexpected comma separator"
        )
        (* Value that was not eaten by an option. Add to configuration's args. *)
        | (Arg.Val s) :: tail -> (
            conf.args <- s :: conf.args;
            loop tail
        )
        (* Flag, does not take arguments. *)
        | (Arg.Short c) :: tail -> (match Hashtbl.find_opt options.short_map c with
            | Some action ->
                (* Feed tail arguments, get new tail back. *)
                let tail =
                    (fun () -> action tail conf)
                    |> Exc.chain_err (
                        fun () -> asprintf "while processing short argument `-%c`" c
                    )
                in
                loop tail
            | None -> sprintf "unknown flag `-%c`" c |> Exc.throw
        )
        (* Option, expected to take arguments. *)
        | (Arg.Long s) :: tail -> (match Hashtbl.find_opt options.long_map s with
            | Some action ->
                (* Feed tail arguments, get new tail back. *)
                let tail =
                    (fun () -> action tail conf)
                    |> Exc.chain_err (
                        fun () -> asprintf "while processing long argument `--%s`" s
                    )
                in
                loop tail
            | None -> sprintf "unknown option `--%s`" s |> Exc.throw
        )
        | (Sep vals) :: tail -> (
            assert (tail = []);
            conf.args <- List.rev_append conf.args vals;
            conf
        )
        | (Mode mode_name) :: tail -> (
            let mode, options =
                try Hashtbl.find modes mode_name with
                | Not_found -> sprintf "unknown mode `%s`" mode_name |> Exc.throw
            in
            Conf.set_mode mode conf;
            (fun () -> handle_args options conf tail)
            |> Exc.chain_err (
                fun () -> asprintf "while processing options for mode %s" mode_name
            )
        )
    in
    loop args

(** Formats some options. *)
let rec fmt_options (fmt : formatter) (opts : Cla.t list) : unit =
    match opts with
    | { short ; long ; values ; help ; _ } :: opts -> (
        fprintf fmt "@ @[<v 4>";

        (* Returns `true` if `shorts` is empty. Used as `is_first` for `fmt_long`. *)
        let fmt_shorts (shorts : char list) : bool =
            let rec loop (shorts : char list) (is_first : bool) : bool =
                match shorts with
                | short :: shorts -> (
                    if not is_first then fprintf fmt ", ";
                    fprintf fmt "-%c" short;
                    loop shorts false
                )
                | [] -> is_first
            in
            loop shorts true
        in

        (* Formats a list of values. *)
        let fmt_values (fmt : formatter) (values : Value.Typ.t list) =
            let rec loop (values : Value.Typ.t list) (is_first : bool) : unit =
                match values with
                | value :: values -> (
                    if not is_first then fprintf fmt " ','";
                    fprintf fmt " %a" Value.Typ.fmt value;
                    loop values false
                )
                | [] -> ()
            in
            loop values true
        in
        let rec fmt_long (longs : string list) (is_first : bool) =
            match longs with
            | name :: longs -> (
                if not is_first then fprintf fmt ", ";
                fprintf fmt "--%s" name;
                fmt_long longs false
            )
            | [] -> ()
        in

        fmt_shorts short |> fmt_long long;
        fmt_values fmt values;

        fprintf fmt "@     @[<v>%a@]" help ();

        fprintf fmt "@]";
        fmt_options fmt opts
    )
    | [] -> ()

(** Prints the test generation usage message. *)
let print_testgen_usage (fmt : formatter) () : unit =
    fprintf fmt "%s [OPTIONS] testgen [TESTGEN_OPTIONS] [-- DIR]?" Sys.argv.(0)

(** Prints the test generation help message. *)
let print_testgen_help (fmt : formatter) : unit =
    fprintf fmt "@[<v 4>USAGE:@ %a@]@." print_testgen_usage ();
    fprintf fmt "@.@[<v 4>TESTGEN_OPTIONS:";
    fmt_options fmt Cla.Modes.Testgen.options;
    fprintf fmt "@]@."

(** Prints the top (no mode) help message. *)
let print_top_help (fmt : formatter) : unit =
    fprintf fmt "@[<v 4>USAGE:@ \
        %s [OPTIONS] -- [FILES]*@ \
        %a\
    @]@." Sys.argv.(0) print_testgen_usage ();
    fprintf fmt "@.@[<v 4>OPTIONS:";
    fmt_options fmt Cla.options;
    fprintf fmt "@]@.";
    fprintf fmt "@.@[<v 4>MODES:";
    let rec loop (modes : (string * description * Cla.options * Conf.mode) list) : unit =
        match modes with
        | (name, desc, _, _) :: modes -> (
            fprintf fmt "@ @[<v 4>%s@ %a@]" name desc ();
            loop modes
        )
        | [] -> ()
    in
    loop Cla.Modes.all;
    fprintf fmt "@ \
        run `%s <MODE> --help` to obtain help on a specific mode. For example: `%s testgen --help`\
    " Sys.argv.(0) Sys.argv.(0);
    fprintf fmt "@]@."

(** Prints the help message for a mode. *)
let print_help (mode : Conf.mode) : unit =
    match mode with
    | Conf.Inactive -> formatter_of_out_channel stdout |> print_top_help
    | Conf.Testgen _ -> formatter_of_out_channel stdout |> print_testgen_help

(* Runs CLAP on custom arguments. *)
let run_on (args : string list) : Conf.t =
    try
        args |> split_args |> handle_args options Conf.default
    with
    | Exc.Exc (Exc.Error (_, Some (PrintHelp mode), _))
    | PrintHelp mode -> (
        print_help mode;
        exit 0
    )
    | e -> (
        (fun () -> raise e) |> Exc.catch_print |> ignore;
        printf "@.";
        print_help Conf.Inactive;
        exit 2
    )

(* Runs CLAP on CLAs. *)
let run () : Conf.t = run_on args

(* Sets the configuration in `Common`. *)
let set_conf () : unit =
    let chained : unit -> Conf.t =
        fun () -> Exc.chain_err (
            fun () -> "while parsing command-line arguments"
        ) (fun () -> run ())
    in
    Exc.catch_fail chained |> Base.Common.set_conf
