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

    (** Transfer information. *)
    type transfer_info = {
        address : Theory.Address.t ;
        (** Address of the contract running the transfer. *)
        sender : Theory.Address.t ;
        (** Sender of the current transfer. *)
        source : Theory.Address.t ;
        (** Source of the current transfer. *)
    }

    (** Test case information. *)
    type test_info = private {
        test : Testcase.t ;
        (** Actual test case that's running. *)
        mutable address : Theory.Address.t ;
        (** Address of the test case.

            Can be changed with `CHANGE_SOURCE` to control the source of the transfers.
        *)
    }

    (** Representation of a source. *)
    type t =
    | Test of test_info
    (** Operation was initiated by the testcase. *)
    | Contract of transfer_info
    (** Operation was initiated by a contract while running the testcase. *)

    (** Formats an operation source. *)
    val fmt : formatter -> t -> unit

    (** Generates the source corresponding to a test. *)
    val of_test : Testcase.t -> t

    (** Generates the source associated with an address. *)
    val of_address :
        address : Theory.Address.t ->
        sender : Theory.Address.t ->
        source : Theory.Address.t ->
        t
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

    (** Events that can happen when running an interpreter. *)
    type event =
    | Done
    (** The interpreter is done running. *)
    | Step of string option
    (** Asked to stop interpretation. *)
    | Failure of Theory.value * Dtyp.t
    (** A testcase asked to apply some operations. *)
    | PrintStack
    (** The stack should be printed. *)
    | Print of (formatter -> unit)
    (** Something to print. *)
    | Warn of (formatter -> unit)
    (** A warning that something was weird. *)

    (** Event formatter. *)
    val fmt_event : formatter -> event -> unit

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
        now : Theory.TStamp.t ->
        Env.t ->
        (Theory.value * Dtyp.t * Annot.Var.t option) list ->
        Mic.t list ->
        t

    (** Contract environment of an interpreter. *)
    val contract_env : t -> Env.t

    (** Forces the value of the contract environment. *)
    val set_contract_env : Env.t -> t -> unit

    (** Performs an interpretation step, return true if there are no more instructions to run.

        A step is not the same as "running a single instruction". A step can be, for instance,
        entering a sequence.
    *)
    val step : t -> event option

    (** Runs `step` until it returns true or an exception is raised.

        This is one of the rare places where exceptions do not necessarily encode errors. When
        running a test case, this function can throw an `ApplyOpsExc` exception, meaning the
        interpreter ran into the extended `APPLY_OPERATIONS` instruction. (This instruction is only
        legal in testcases.)
    *)
    val run : t -> event

    (** Timestamp of the transfer represented by an interpreter. *)
    val timestamp : t -> Theory.TStamp.t

    (** Terminates a run.

        Retrieves the operation list, the new storage, and the type of the new storage.
    *)
    val terminate : t -> Env.operation list * Theory.value * Dtyp.t

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

    (** Type of normal events. *)
    type event = Run.event

    (** Type of test events. *)
    type test_event =
    | Normal of event
    (** Run resulted in a failure. *)
    | ApplyOps of Env.operation list
    (** Some operations should be applied. *)

    (** Test event formatter. *)
    val fmt_test_event : formatter -> test_event -> unit

    (** Type of test interpreters. *)
    type t

    (** Current timestamp of the test represented by a test interpreter. *)
    val timestamp : t -> Theory.TStamp.t

    (** Contract environment of an interpreter. *)
    val contract_env : t -> Env.t

    (** Forces the value of the contract environment. *)
    val set_contract_env : Env.t -> t -> unit

    (** Constructor. *)
    val mk : Src.t -> Testcase.t -> Env.t -> t

    (** Testcase accessor. *)
    val testcase : t -> Testcase.t

    (** Performs a step. *)
    val step : t -> test_event option

    (** Runs until the next event. *)
    val run : t -> test_event

    (** True if the test has not more instruction to run. *)
    val is_done : t -> bool

    (** Underlying regular interpreter. *)
    val interp : t -> Run.t

    (** Underlying stack. *)
    val stack : t -> Run.Stack.t

    (** Balance of the test interpreter. *)
    val balance : t -> Theory.Tez.t
end
