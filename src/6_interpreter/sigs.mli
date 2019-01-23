(** Input/output signatures for the functors that create interpreters.

    All primitive datatypes should be formatable (`TFmt`). All other operations are
    domain-specific. The actual signature of the modules the functor of this module takes
    is `Theory`.

    When modifying things here, be very careful of the type constraints in the `Interpreter`
    signature. The abstract types in the conversion modules must be constrained so that the
    compiler knows what they are equal to in the rest of the code.
*)

open Base
open Common

(** Stores the source of something.

    Used to distinguish between interpreting a testcase and interpreting a regular contract. A
    testcase has access to more instructions than a regular contract.
*)
module type SigSrc = sig
    (** Underlying theory. *)
    module Theory : Theo.Sigs.Theory

    (** Representation of a source. *)
    type t =
    | Test of Testcase.t
    (** Operation was initiated by the testcase. *)
    | Contract of Theory.Address.t
    (** Operation was initiated by a contract while running the testcase. *)

    (** Formats an operation source. *)
    val fmt : formatter -> t -> unit

    (** Generates the source corresponding to a test. *)
    val of_test : Testcase.t -> t

    (** Generates the source associated with an address. *)
    val of_address : Theory.Address.t -> t
end

(** Signature of an interpreter.

    This is what the main functor creates.

    Two kinds of interpreters will be created. The interpreter for the testcase, which will be a
    `TestInterpreter`, and interpreters for contracts running in the testcase. This signature is
    for the latter.
*)
module type Interpreter = sig
    (** Underlying theory. *)
    module Theory : Theo.Sigs.Theory

    (** Contract environment module. *)
    module Env : Stack.Sigs.ContractEnv with module Theory = Theory

    (** Stack used by the interpreter. *)
    module Stack : Stack.Sigs.Stack with module Theory = Theory and module Env = Env

    (** Operation source module. *)
    module Src : SigSrc with module Theory = Theory

    (** Type of interpreters. *)
    type t

    (** Initializes an interpreter.

        The last two arguments are *i)* the values to push on the stack during initialization
        (typically a parameter/storage pair when running a contract entry), and a list of
        instructions to run (typically the code of the contract).
    *)
    val init :
        Src.t ->
        balance : Theory.Tez.t ->
        amount : Theory.Tez.t ->
        Env.t ->
        (Theory.value * Dtyp.t * Annot.Var.t option) list ->
        Mic.t list ->
        t

    (** Contract environment of an interpreter. *)
    val contract_env : t -> Env.t

    (** Performs an interpretation step, return true if there are no more instructions to run.

        A step is not the same as "running a single instruction". A step can be, for instance,
        entering a sequence, un-dipping, exiting a block *etc*.
    *)
    val step : t -> bool

    (** Runs `step` until it return true or an exception is raised.

        This is one of the rare places where exceptions do not encode errors. When running a test
        case, this function can throw an `ApplyOpsExc` exception, meaning the interpreter ran into
        the extended `APPLY_OPERATIONS` instruction. (This instruction is only legal in testcases.)
    *)
    val run : t -> unit

    (** Returns the last instruction run, if any.

        This does not necessarily correspond to the last `step`, as steps describe more things than
        just running an instruction. This returns the last actual instruction run.
    *)
    val last_ins : t -> Mic.t option

    (** The next instruction to run. *)
    val next_ins : t -> string list * Mic.t option

    (** The stack of the interpreter. *)
    val stack : t -> Stack.t

    (** Returns true if there is nothing left to run. *)
    val is_done : t -> bool

    (** Returns the source of the interpreter. *)
    val src : t -> Src.t

    (** The balance of the source of the interpreter. *)
    val balance : t -> Theory.Tez.t
end

(** Signature of the testcase interpreter.

    Wraps an `Interpreter` to provide convenience functions for running tests.
*)
module type TestInterpreter = sig
    (** Underlying interpreter. *)
    module Run : Interpreter

    (** Contract environment. *)
    module Env = Run.Env

    (** Underlying theory. *)
    module Theory = Run.Theory

    (** Source module. *)
    module Src = Run.Src

    (** Type of test interpreters. *)
    type t

    (** Contract environment of an interpreter. *)
    val contract_env : t -> Env.t

    (** Constructor. *)
    val mk : Src.t -> Testcase.t -> Env.t -> t

    (** Performs a step.

        The notion of step here is very different for that of an `Interpreter`. A step for the
        **test** interpreter consists in running the instructions in the test until an extended
        instruction (such as `APPLY_OPERATIONS`) asks us to do something, or there are no more
        instructions in the test.
    *)
    val step : t -> Env.operation list option

    (** True if the test has not more instruction to run. *)
    val is_done : t -> bool

    (** Underlying regular interpreter. *)
    val interp : t -> Run.t

    (** Balance of the test interpreter. *)
    val balance : t -> Theory.Tez.t
end

(** A context runs the testcase instructions, deals with operations, and runs contracts.

    This is what the main functor returns.
*)
module type Cxt = sig
    (** Underlying theory. *)
    module Theory : Theo.Sigs.Theory

    (** Underlying contract interpreter. *)
    module Run : Interpreter with module Theory = Theory

    (** Underlying test interpreter. *)
    module TestRun : TestInterpreter with module Run = Run

    (** Type of contexts. *)
    type t
end
