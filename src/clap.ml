 (* CLAP stuff. *)

open Base
open Base.Common

module Arg = struct
    (* Arguments: short (flag) or long. *)
    type t =
    (* Flag: `-v`. *)
    | Short of char
    (* Option: `--verb`. *)
    | Long of string
    (* Value: some string. *)
    | Val of string
    (* A comma separator. *)
    | Comma
    (* Separator: everything after this point are values.
    
        Values appearing in this thing cannot be option arguments. *)
    | Sep of string list
    (* Mode. *)
    | Mode of string

    let _fmt (fmt : formatter) (self : t) : unit =
        match self with
        | Short c -> fprintf fmt "(short '%c')" c
        | Long s -> fprintf fmt "(long \"%s\")" s
        | Val s -> fprintf fmt "(val \"%s\")" s
        | Comma -> fprintf fmt "comma"
        | Sep _ -> fprintf fmt "(sep ...)"
        | Mode s -> fprintf fmt "(mode \"%s\")" s

    (* Returns the head if it is a value, none otherwise. *)
    let next_value (l : t list) : string option * t list = match l with
    | (Val v) :: tail -> Some v, tail
    | _ -> None, l

    (* Returns the next value if the list is a comma and a value. *)
    let next_sep_value (l : t list) : string option * t list = match l with
    | Comma :: (Val v) :: tail -> Some v, tail
    | _ -> None, l

    (* Same as `next_value` but converts the value to an `int`. *)
    let next_int (l : t list) : int option * t list = match next_value l with
    | Some v, tail ->
        Exc.erase_err
            (fun () -> sprintf "expected integer value, found `%s`" v)
            (fun () -> Some (int_of_string v), tail)
    | None, tail -> None, tail
end


module Cla = struct
    type t = {
        short : char list;
        long : string list;
        action : Arg.t list -> Conf.t -> Arg.t list;
    }
    let mk
        (short : char list)
        (long : string list)
        (action : Arg.t list -> Conf.t -> Arg.t list)
        : t
    = { short ; long ; action }

    type options = {
        long_map : (string, Arg.t list -> Conf.t -> Arg.t list) Hashtbl.t ;
        short_map : (char, Arg.t list -> Conf.t -> Arg.t list) Hashtbl.t ;
    }

    let empty_options = {
        short_map = Hashtbl.create ~random:false 101 ;
        long_map = Hashtbl.create ~random:false 101 ;
    }

    let verb : t =
        mk ['v'] ["verb"] (
            fun args conf -> match Arg.next_int args with
            | None, tail ->
                conf.verb <- conf.verb + 1;
                tail
            | Some i, tail ->
                conf.verb <- i;
                tail
        )

    let quiet : t =
        mk ['q'] [] (
            fun args conf ->
                conf.verb <- conf.verb - 1;
                args
        )
    
    let step : t =
        mk ['s'] ["step"] (
            fun args conf -> match Arg.next_value args with
            | None, tail
            | Some "on", tail
            | Some "true", tail
            | Some "True", tail ->
                conf.step <- true;
                tail
            | Some "off", tail
            | Some "no", tail
            | Some "false", tail
            | Some "False", tail ->
                conf.step <- false;
                tail
            | Some blah, _ ->
                Exc.throw (
                    sprintf
                        "expected a truth value `on|true|True|off|no|false|False`, found `%s`"
                        blah
                )
        )

    let contract : t =
        mk [] ["contract"] (
            fun args conf -> match Arg.next_value args with
            | None, _ ->
                Exc.throw (
                    "argument `--contract` expects at least one value"
                )
            | Some file, tail ->
                let (init, tail) = Arg.next_sep_value tail in
                conf.contracts <- conf.contracts @ [ Conf.mk_contract file init ];
                tail
        )

    let options : t list = [ verb ; quiet ; step ; contract ]

    let add_all (opts : options) = options |> List.iter (
        fun { short ; long ; action } ->
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
                mk ['n'] ["count"] (
                    fun args conf -> match Arg.next_value args with
                    | None , _ -> Exc.throw (
                        "argument `--count` expects at least one value"
                    )
                    | Some n, tail ->
                        let count =
                            (fun () -> int_of_string n)
                            |> Exc.chain_err (
                                fun () -> "while parsing the argument for `--count`"
                            )
                        in
                        conf |> Conf.map_testgen_mode (fun conf -> conf.count <- count);
                        tail

                )

            let dump : t =
                mk ['d'] ["dump"] (
                    fun args conf -> match Arg.next_value args with
                    | None , _ -> Exc.throw (
                        "argument `--dump` expects at least one value"
                    )
                    | Some target, tail ->
                        conf |> Conf.map_testgen_mode (fun conf -> conf.dump <- Some target);
                        tail

                )

            let options : t list = [ test_count ; dump ]

            let add_all (opts : options) = options |> List.iter (
                fun { short ; long ; action } ->
                    short |> List.iter (
                        fun (c : char) -> Hashtbl.add opts.short_map c action
                    );
                    long |> List.iter (
                        fun (s : string) -> Hashtbl.add opts.long_map s action
                    );
            )

            let name : string = "testgen"
        end

        let testgen : string * options * Conf.mode =
            let opts = empty_options in
            Testgen.add_all opts;
            Testgen.name, opts, Conf.Testgen Conf.default_testgen_mode

        let all : (string * options * Conf.mode) list = [
            testgen
        ]

        let add_all (modes : (string, (Conf.mode * options)) Hashtbl.t) : unit =
            all |> List.iter (
                fun (name, options, mode) -> Hashtbl.add modes name (mode, options)
            )
    end
end



(* Command line arguments. *)
let args : string list =
    (Array.length Sys.argv) - 1 |> Array.sub Sys.argv 1 |> Array.to_list

(* Options. *)
let options : Cla.options =
    let opts = Cla.empty_options in
    Cla.add_all opts;
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
                (* Notice the empty list of arguments (first argument). *)
                action [] conf |> ignore;
                loop tail
            | None -> sprintf "unknown flag `-%c`" c |> Exc.throw
        )
        (* Option, expected to take arguments. *)
        | (Arg.Long s) :: tail -> (match Hashtbl.find_opt options.long_map s with
            | Some action ->
                (* Feed tail arguments, get new tail back. *)
                let tail = action tail conf in
                loop tail
            | None -> sprintf "unknown option `--%s`" s |> Exc.throw
        )
        | (Sep vals) :: tail -> (
            assert (tail = []);
            conf.args <- List.rev_append conf.args vals;
            conf
        )
        | (Mode mode) :: tail -> (
            try (
                let mode, options = Hashtbl.find modes mode in
                Conf.set_mode mode conf;
                handle_args options conf tail
            ) with
            | Not_found -> sprintf "unknown mode `%s`" mode |> Exc.throw
        )
    in
    loop args



(* Runs CLAP on custom arguments. *)
let run_on (args : string list) : Conf.t =
    args |> split_args |> handle_args options Conf.default

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