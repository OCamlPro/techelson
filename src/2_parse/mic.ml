(* Instruction parsing. *)

open Base
open Base.Common

type arg = (Mic.t, Mic.const) Either.t
type args = arg list

let arg_to_mic (arg : arg) : Mic.t =
    match arg with
    | Either.Lft arg -> arg
    | Either.Rgt c ->
        asprintf "expected an instruction, found a constant `%a`" Mic.fmt_const c
        |> Exc.throw

let arg_to_const (arg : arg) : Mic.const =
    match arg with
    | Either.Rgt c -> c
    | Either.Lft _ ->
        Exc.throw "expected a constant, found an instruction"

let args_to_mic (args : args) : Mic.t list =
    args |> List.map arg_to_mic

let next_mic_arg (args : args) : Mic.t * args =
    match args with
    | arg :: tail -> (arg_to_mic arg), tail
    | [] -> Exc.throw "expected an instruction argument, found nothing"
let next_const_arg (args : args) : Mic.const * args =
    match args with
    | arg :: tail -> (arg_to_const arg), tail
    | [] -> Exc.throw "expected an instruction argument, found nothing"

let fmt_arg = Either.fmt Mic.fmt Mic.fmt_const

let rec parse
    (token : string)
    (annots : Annot.t)
    (dtyps : Dtyp.t list)
    (args : args)
    : Mic.t
=
    let param_arity_check typs_x args_x =
        Check.param_arity (fun () -> token) (typs_x, dtyps) (args_x, args);
    in
    let annot_arity_check typs_x vars_x fields_x =
        Check.Annots.typ_arity_le (fun () -> token) typs_x annots.typs;
        Check.Annots.var_arity_le (fun () -> token) vars_x annots.vars;
        Check.Annots.field_arity_le (fun () -> token) fields_x annots.fields
    in
    let inner () =
        match token with
        | "CAST" ->
            param_arity_check 1 0;
            annot_arity_check 0 1 0;
            Mic.Cast (List.hd dtyps)
        | "EMPTY_SET" ->
            param_arity_check 1 0;
            annot_arity_check 0 1 0;
            Mic.EmptySet (List.hd dtyps)
        | "EMPTY_MAP" ->
            param_arity_check 2 0;
            annot_arity_check 0 1 0;
            Mic.EmptyMap (List.hd dtyps, List.tl dtyps |> List.hd)

        | "NONE" ->
            param_arity_check 1 0;
            annot_arity_check 0 1 1;
            Mic.Non (List.hd dtyps)
        | "LEFT" ->
            param_arity_check 1 0;
            annot_arity_check 0 1 2;
            Mic.Left (List.hd dtyps)
        | "RIGHT" ->
            param_arity_check 1 0;
            annot_arity_check 0 1 2;
            Mic.Right (List.hd dtyps)
        | "NIL" ->
            param_arity_check 1 0;
            annot_arity_check 0 1 1;
            Mic.Nil (List.hd dtyps)
        | "CONTRACT" ->
            param_arity_check 1 0;
            annot_arity_check 0 1 0;
            Mic.Contract (List.hd dtyps)

        | "IF" ->
            param_arity_check 0 2;
            annot_arity_check 0 0 0;
            let bt, args = next_mic_arg args in
            let bf, _ = next_mic_arg args in
            Mic.If (bt, bf)
        | "IF_NONE" ->
            param_arity_check 0 2;
            annot_arity_check 0 0 0;
            let bt, args = next_mic_arg args in
            let bf, _ = next_mic_arg args in
            Mic.IfNone (bt, bf)
        | "IF_LEFT" ->
            param_arity_check 0 2;
            annot_arity_check 0 0 0;
            let bt, args = next_mic_arg args in
            let bf, _ = next_mic_arg args in
            Mic.IfLeft (bt, bf)
        | "IF_RIGHT" ->
            param_arity_check 0 2;
            annot_arity_check 0 0 0;
            let bt, args = next_mic_arg args in
            let bf, _ = next_mic_arg args in
            IfRight (bt, bf)
        | "IF_CONS" ->
            param_arity_check 0 2;
            annot_arity_check 0 0 0;
            let bt, args = next_mic_arg args in
            let bf, _ = next_mic_arg args in
            IfCons (bt, bf)
        | "LOOP" ->
            param_arity_check 0 1;
            annot_arity_check 0 0 0;
            let code, _ = next_mic_arg args in
            Loop code
        | "LOOP_LEFT" ->
            param_arity_check 0 1;
            annot_arity_check 0 0 0;
            let code, _ = next_mic_arg args in
            LoopLeft code
        | "DIP" ->
            param_arity_check 0 1;
            annot_arity_check 0 0 0;
            let code, _ = next_mic_arg args in
            Dip code
        | "ITER" ->
            param_arity_check 0 1;
            annot_arity_check 0 0 0;
            let code, _ = next_mic_arg args in
            Iter code

        | "PUSH" ->
            param_arity_check 1 1;
            annot_arity_check 1 1 0;
            let const, _ = next_const_arg args in
            Push (List.hd dtyps, const)
        | "LAMBDA" ->
            param_arity_check 2 1;
            annot_arity_check 0 1 0;
            let code, _ = next_mic_arg args in
            Lambda (List.hd dtyps, List.tl dtyps |> List.hd, code)
        | "CREATE_CONTRACT" when (List.length args) = 0 ->
            param_arity_check 0 0;
            annot_arity_check 0 1 0;
            CreateContract (Either.Lft None)
        | "CREATE_CONTRACT" -> (
            param_arity_check 0 1;
            annot_arity_check 0 1 0;
            let const, _ = next_const_arg args in
            match const with
            | Cont c -> CreateContract (Either.Lft (Some c))
            | Str s -> CreateContract (Either.Rgt s)
            | cst -> [
                "while parsing `CREATE_CONTRACT`" ;
                asprintf "expected constant contract, found `%a`" Mic.fmt_const cst
            ] |> Exc.throws
        )

        | "STORAGE_OF" -> (
            param_arity_check 1 0;
            annot_arity_check 0 1 0;
            let dtyp = List.hd dtyps in
            Extension (StorageOf dtyp)
        )
        | "BALANCE_OF" ->
            param_arity_check 0 0;
            annot_arity_check 0 1 0;
            Extension BalanceOf
        | "APPLY_OPERATIONS" ->
            param_arity_check 0 0;
            annot_arity_check 0 0 0;
            Extension ApplyOps
        | "PRINT_STACK" ->
            param_arity_check 0 0;
            annot_arity_check 0 0 0;
            Extension PrintStack

        | _ -> (
            let args = args_to_mic args in
            match Mic.leaf_of_string token with
            | Some leaf -> (
                (* log_1 "token: `%s` (%i)@." token (List.length args); *)
                let typ_arity, var_arity, field_arity = Mic.annot_arity_of_leaf leaf in
                param_arity_check 0 0;
                annot_arity_check typ_arity var_arity field_arity;
                Mic.Leaf leaf
            )
            | None -> (
                match parse_macro token annots.vars annots.fields dtyps args with
                | Some ins -> (
                    Check.Annots.typ_arity_le (fun () -> token) 0 annots.typs;
                    ins
                )
                | None -> sprintf "unknown instruction `%s`" token |> Exc.throw
            )
        )
    in
    Exc.chain_err (
        fun () ->
            asprintf "parsing %s%a%s%a%s%a"
                token
                Annot.fmt annots
                (if dtyps = [] then "" else " ")
                (Fmt.fmt_list Fmt.sep_spc Dtyp.fmt)
                dtyps
                (if args = [] then "" else " ")
                (Fmt.fmt_list Fmt.sep_spc fmt_arg)
                args
    ) (
        inner
    )
    |> Mic.mk ~typs:(annots.typs) ~vars:(annots.vars) ~fields:(annots.fields)

and full_arity_check
    (token : string)
    (dtyps : Dtyp.t list)
    (args : 'a list)
    (annots : Annot.vars)
    (typ_x : int)
    (arg_x : int)
    (annot_x : int)
    : unit
=
    Check.param_arity (fun () -> token) (typ_x, dtyps) (arg_x, args);
    Check.Annots.var_arity_le (fun () -> token) annot_x annots

and macro_annot_arity_check
    (token : string)
    (vars : Annot.vars)
    (fields : Annot.fields)
    (vars_x : int)
    (fields_x : int)
    : unit
=
    Check.Annots.var_arity_le (fun () -> token) vars_x vars;
    Check.Annots.field_arity_le (fun () -> token) fields_x fields

and parse_macro_cmp
    (token : string)
    (vars : Annot.vars)
    (fields: Annot.fields)
    (dtyps : Dtyp.t list)
    (args : Mic.t list)
    : Mic.t Mic.ins option
=
    Macro.prefixed_op
        (
            fun () -> [
                sprintf "token `%s` looks like a CMP-like macro, but is not" token ;
                sprintf "please refer to \
                    https://tezos.gitlab.io/master/whitedoc/michelson.html#compare" ;
            ]
        )
        "CMP"
        (fun op ->
            full_arity_check token dtyps args vars 0 0 0;
            macro_annot_arity_check token vars fields 1 0;
            let expanded = Expand.macro_cmp vars op in
            let macro = Mic.Macro.Cmp op in
            Mic.Macro (expanded, macro)
        )
        token

and parse_macro_if
    (token : string)
    (dtyps : Dtyp.t list)
    (args : Mic.t list)
    (annots : Annot.vars)
    : Mic.t Mic.ins option
=
    Macro.prefixed_op
        (
            fun () -> [
                sprintf "token `%s` looks like an IF-like macro, but is not" token ;
                sprintf "please refer to \
                    https://tezos.gitlab.io/master/whitedoc/michelson.html#compare" ;
            ]
        )
        "IF"
        (fun op ->
            full_arity_check token dtyps args annots 0 2 0;
            let (bt, bf) = (List.hd args, List.tl args |> List.hd) in
            let expanded = Expand.macro_if op bt bf in
            let macro = Mic.Macro.If (op, bt, bf) in
            Mic.Macro (expanded, macro)
        )
        token

and parse_macro_if_cmp
    (token : string)
    (dtyps : Dtyp.t list)
    (args : Mic.t list)
    (annots : Annot.vars)
    : Mic.t Mic.ins option
=
    Macro.prefixed_op
        (
            fun () -> [
                sprintf "token `%s` looks like an IFCMP-like macro, but is not" token ;
                sprintf "please refer to \
                    https://tezos.gitlab.io/master/whitedoc/michelson.html#compare" ;
            ]
        )
        "IFCMP"
        (fun op ->
            full_arity_check token dtyps args annots 0 2 0;
            let (bt, bf) = (List.hd args, List.tl args |> List.hd) in
            let expanded = Expand.macro_if op bt bf in
            let macro = Mic.Macro.IfCmp (op, bt, bf) in
            Mic.Macro (expanded, macro)
        )
        token

and parse_macro_fail
    (token : string)
    (dtyps : Dtyp.t list)
    (args : Mic.t list)
    (annots : Annot.vars)
    : Mic.t Mic.ins option
=
    if token = "FAIL" then (
        full_arity_check token dtyps args annots 0 0 0;
        Some (Mic.Macro (Expand.macro_fail, Mic.Macro.Fail))
    ) else None

(* Parses (variations of) the assert macro. *)
and parse_macro_assert
    (token : string)
    (dtyps : Dtyp.t list)
    (args : Mic.t list)
    (annots : Annot.vars)
    : Mic.t Mic.ins option
=
    (* Parameter is the kind of assert the token looks like: `ASSERT`, `ASSERT_CMP` ... *)
    let bail (branch : string) : (Mic.t list * Mic.t Mic.Macro.t) =
        [
            sprintf "token `%s` looks like an %s-like macro, but is not" token branch ;
            sprintf "please refer to \
                https://tezos.gitlab.io/master/whitedoc/michelson.html#assertion-macros" ;
        ] |> Exc.throws
    in
    (* Calls `Macro.Parse.op` on `s` minus the `n` first characters. *)
    let op_of_suff (s : string) (n : int) : Mic.Macro.op option =
        String.sub s n ((String.length s) - n) |> Macro.op
    in
    (* Performs arity checks and wraps into `Some`. *)
    let terminate (expansion : Mic.t list) (res : Mic.t Mic.Macro.t) : Mic.t Mic.ins option =
        full_arity_check token dtyps args annots 0 0 0;
        Some (Macro (expansion, res))
    in
    match Utils.tail_of_pref ~pref:"ASSERT" token with
    | None -> None
    | Some "" -> terminate Expand.macro_assert Mic.Macro.Assert
    | Some tail -> (
        if String.get tail 0 <> '_' then (
            bail "ASSERT" |> ignore
        );
        let tail = String.sub tail 1 ((String.length tail) - 1) in
        match Macro.op tail with
        | Some op -> Mic.Macro.Assert_ op |> terminate (Expand.macro_assert_ op)
        | None -> (
            let tail_len = String.length tail in
            let (expansion, macro) : (Mic.t list * Mic.t Mic.Macro.t) =
                if tail_len < 3 then (
                    bail "ASSERT_"
                ) else if "CMP" = String.sub tail 0 3 then (
                    match op_of_suff tail 3 with
                    | Some op ->
                        Expand.macro_assert_cmp op, Mic.Macro.AssertCmp op
                    | None -> bail "ASSERT_CMP{CMP}"
                ) else if tail_len < 4 then (
                    bail "ASSERT_"
                ) else if "NONE" = String.sub tail 0 4 then (
                    if tail = "NONE" then
                        Expand.macro_assert_none, Mic.Macro.AssertNone
                    else bail "ASSERT_NONE"
                ) else if "SOME" = String.sub tail 0 4 then (
                    if tail = "SOME" then
                        Expand.macro_assert_some, Mic.Macro.AssertSome
                    else bail "ASSERT_SOME"
                ) else if "LEFT" = String.sub tail 0 4 then (
                    if tail = "LEFT" then
                        Expand.macro_assert_left, Mic.Macro.AssertLeft
                    else bail "ASSERT_LEFT"
                ) else if tail_len < 5 then (
                    bail "ASSERT_"
                ) else if "RIGHT" = String.sub tail 0 5 then (
                    if tail = "RIGHT" then
                        Expand.macro_assert_right, Mic.Macro.AssertRight
                    else bail "ASSERT_RIGHT"
                ) else bail "ASSERT_"
            in
            terminate expansion macro
        )
    )

(* Factors common code between `parse_macro_dip` and `parse_macro_dup`.

    Parses strings of the form `<pref><rep>*<finish>`. Returns the number of times `rep` appears in
    the token.
*)
and parse_macro_diup
    (pref : string)
    (rep : char)
    (finish : char)
    (token : string)
    : int option
=
    let bail () : int =
        [
            sprintf "token `%s` looks like a %s%c+%c-like macro, but is not" token pref rep finish ;
            sprintf "please refer to \
                https://tezos.gitlab.io/master/whitedoc/michelson.html#syntactic-conveniences" ;
        ] |> Exc.throws
    in
    match Utils.tail_of_pref ~pref:pref token with
    | None -> None
    | Some tail -> (
        let tail_len = String.length tail in
        let rec count (n : int) : int =
            if n < tail_len then (
                let next = String.get tail n in
                if next = rep then (
                    n + 1 |> count
                ) else if next = finish then (
                    (* Anything else in the string? *)
                    if n + 1 = tail_len then (
                        n
                    ) else (
                        bail ()
                    )
                ) else (
                    bail ()
                )
            ) else (
                bail ()
            )
        in
        Some (count 0)
    )

and parse_macro_dip
    (token : string)
    (dtyps : Dtyp.t list)
    (args : Mic.t list)
    (annots : Annot.vars)
    : Mic.t Mic.ins option
=
    match parse_macro_diup "DI" 'I' 'P' token with
    | None -> None
    | Some i_count -> (
        if i_count = 0 then (
            Exc.throw "entered unreachable code: encountered `DIP` while parsing a `DII+P`"
        );
        full_arity_check token dtyps args annots 0 1 0;
        let ins = List.hd args in
        let expanded = Expand.macro_dip i_count ins in
        let macro = Mic.Macro.Dip (i_count, ins) in
        Some (Macro (expanded, macro))
    )

and parse_macro_dup
    (token : string)
    (dtyps : Dtyp.t list)
    (args : Mic.t list)
    (annots : Annot.vars)
    : Mic.t Mic.ins option
=
    match parse_macro_diup "DU" 'U' 'P' token with
    | None -> None
    | Some u_count -> (
        if u_count = 0 then (
            Exc.throw "entered unreachable code: encountered `DUP` while parsing a `DUU+P`"
        );
        full_arity_check token dtyps args annots 0 0 1;
        let expanded = Expand.macro_dup annots u_count in
        let macro = Mic.Macro.Dup u_count in
        Some (Macro (expanded, macro))
    )

and parse_macro_pair
    (token : string)
    (vars : Annot.vars)
    (fields : Annot.fields)
    (dtyps : Dtyp.t list)
    (args : Mic.t list)
    : Mic.t Mic.ins option
=
    match Utils.tail_of_pref ~pref:"P" token with
    | None -> None
    | Some tail -> (
        match Macro.pair_ops tail with
        | ops, "R" -> (
            full_arity_check token dtyps args vars 0 0 1;
            Check.Annots.var_arity_le (fun () -> token) 1 vars;
            let expanded = Expand.macro_pair vars fields (Mic.Macro.P :: ops) in
            let macro = Mic.Macro.P ops in
            Some (Macro (expanded, macro))
        )
        | _ ->
            [
                sprintf "token `%s` looks like a P[API]+R-like macro, but is not" token;
                sprintf "please refer to \
                    https://tezos.gitlab.io/master/whitedoc/michelson.html#syntactic-conveniences";
            ] |> Exc.throws
    )

and parse_macro_unpair
    (token : string)
    (vars : Annot.vars)
    (fields : Annot.fields)
    (dtyps : Dtyp.t list)
    (args : Mic.t list)
    : Mic.t Mic.ins option
=
    match Utils.tail_of_pref ~pref:"UNP" token with
    | None -> None
    | Some tail -> (
        match Macro.pair_ops tail with
        | ops, "R" -> (
            Check.typ_arity (fun () -> token) 0 dtyps;
            Check.args_arity (fun () -> token) 0 args;
            Check.Annots.field_arity_le (fun () -> token) 0 fields;
            let expanded = Expand.macro_unpair vars (Mic.Macro.P :: ops) in
            let macro = Mic.Macro.Unp ops in
            Some (Macro (expanded, macro))
        )
        | _ ->
            [
                sprintf "token `%s` looks like a UNP[API]+R-like macro, but is not" token;
                sprintf "please refer to \
                    https://tezos.gitlab.io/master/whitedoc/michelson.html#syntactic-conveniences";
            ] |> Exc.throws
    )

and parse_macro_cadr
    (token : string)
    (vars : Annot.vars)
    (fields : Annot.fields)
    (dtyps : Dtyp.t list)
    (args : Mic.t list)
    : Mic.t Mic.ins option
=
    match Utils.tail_of_pref ~pref:"C" token with
    | None -> None
    | Some tail -> (
        match Macro.unpair_ops tail with
        | ops, "R" -> (
            full_arity_check token dtyps args vars 0 0 1;
            macro_annot_arity_check token vars fields 1 1;
            let expanded = Expand.macro_cadr vars fields ops in
            let macro = Mic.Macro.CadR ops in
            Some (Macro (expanded, macro))
        )
        | _ ->
            [
                sprintf "token `%s` looks like a C[AD]+R-like macro, but is not" token;
                sprintf "please refer to \
                    https://tezos.gitlab.io/master/whitedoc/michelson.html#syntactic-conveniences";
            ] |> Exc.throws
    )

and parse_macro_set_cadr
    (token : string)
    (vars : Annot.vars)
    (fields : Annot.fields)
    (dtyps : Dtyp.t list)
    (args : Mic.t list)
    : Mic.t Mic.ins option
=
    match Utils.tail_of_pref ~pref:"SET_C" token with
    | None -> None
    | Some tail -> (
        match Macro.unpair_ops tail with
        | ops, "R" -> (
            full_arity_check token dtyps args vars 0 0 1;
            macro_annot_arity_check token vars fields 1 1;
            let expanded = Expand.macro_set_cadr vars fields ops in
            let macro = Mic.Macro.SetCadr ops in
            Some (Macro (expanded, macro))
        )
        | _ ->
            [
                sprintf "token `%s` looks like a SET_C[AD]+R-like macro, but is not" token;
                sprintf "please refer to \
                    https://tezos.gitlab.io/master/whitedoc/michelson.html#syntactic-conveniences";
            ] |> Exc.throws
    )

and parse_macro_map_cadr
    (token : string)
    (vars : Annot.vars)
    (fields : Annot.fields)
    (dtyps : Dtyp.t list)
    (args : Mic.t list)
    : Mic.t Mic.ins option
=
    match Utils.tail_of_pref ~pref:"MAP_C" token with
    | None -> None
    | Some tail -> (
        match Macro.unpair_ops tail with
        | ((_ :: _) as ops), "R" -> (
            full_arity_check token dtyps args vars 0 1 1;
            macro_annot_arity_check token vars fields 1 1;
            let ins = List.hd args in
            let expanded = Expand.macro_map_cadr vars fields ops ins in
            let macro = Mic.Macro.MapCadr (ops, ins) in
            Some (Macro (expanded, macro))
        )
        | _ ->
            [
                sprintf "token `%s` looks like a MAP_C[AD]+R-like macro, but is not" token;
                sprintf "please refer to \
                    https://tezos.gitlab.io/master/whitedoc/michelson.html#syntactic-conveniences";
            ] |> Exc.throws
    )

and parse_macro_if_some
    (token : string)
    (dtyps : Dtyp.t list)
    (args : Mic.t list)
    (annots : Annot.vars)
    : Mic.t Mic.ins option
=
    if token = "IF_SOME" then (
        full_arity_check token dtyps args annots 0 2 0;
        let ins_1, ins_2 = List.hd args, List.tl args |> List.hd in
        let expanded = Expand.macro_if_some ins_1 ins_2 in
        let macro = Mic.Macro.IfSome (ins_1, ins_2) in
        Some (Macro (expanded, macro))
    ) else (
        None
    )

and parse_macro_int
    (token : string)
    (dtyps : Dtyp.t list)
    (args : Mic.t list)
    (annots : Annot.vars)
    : Mic.t Mic.ins option
=
    if token = "INT" then (
        full_arity_check token dtyps args annots 0 0 1;
        let expanded = Expand.macro_int in
        let macro = Mic.Macro.Int in
        Some (Macro (expanded, macro))
    ) else (
        None
    )

(* Macro parser. *)
and parse_macro
    (token : string)
    (vars : Annot.vars)
    (fields : Annot.fields)
    (dtyps : Dtyp.t list)
    (args : Mic.t list)
    : Mic.t Mic.ins option
=
    let all : (unit -> Mic.t Mic.ins option) list =
        [
            (fun () -> parse_macro_cmp token vars fields dtyps args) ;
            (fun () -> parse_macro_if_some token dtyps args vars) ;
            (fun () -> parse_macro_if_cmp token dtyps args vars) ;
            (fun () -> parse_macro_if token dtyps args vars) ;
            (fun () -> parse_macro_fail token dtyps args vars) ;
            (fun () -> parse_macro_assert token dtyps args vars) ;
            (fun () -> parse_macro_dip token dtyps args vars) ;
            (fun () -> parse_macro_dup token dtyps args vars) ;
            (fun () -> parse_macro_pair token vars fields dtyps args) ;
            (fun () -> parse_macro_unpair token vars fields dtyps args) ;
            (fun () -> parse_macro_cadr token vars fields dtyps args) ;
            (fun () -> parse_macro_set_cadr token vars fields dtyps args) ;
            (fun () -> parse_macro_map_cadr token vars fields dtyps args) ;
            (fun () -> parse_macro_int token dtyps args vars) ;
        ]
    in
    let rec loop = function
        | [] -> None
        | parse :: tail -> (
            match parse () with
            | Some macro -> Some macro
            | None -> loop tail
        )
    in
    loop all
