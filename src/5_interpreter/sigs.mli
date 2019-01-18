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

(** Contract environment.

    Contains a type and some functions to store

    - a user-provided name-contract map (for deployment)
    - a map from addresses to live (deployed) contracts
*)
module type ContractEnv = sig
    (** The underlying theory. *)
    module Theory : Theo.Sigs.Theory

    (** Type of the contract environment. *)
    type t

    (** An operation.

        We do not use `Theory.operation` directly because we need to be able to detect when the
        exact same operation runs twice.
    *)
    type operation

    (** Clones an environment.
    
        This is useful when an operation can fail: it acts as a backtracking mechanism.
    *)
    val clone : t -> t

    (** Functions over operations. *)
    module Op : sig
        (** Operation formatter. *)
        val fmt : formatter -> operation -> unit

        (** Theory operation accessor.

            This function uses the environment to check whether the operation can run. That is,
            it will fail if the uid of the operation is expired.
        *)
        val op : t -> operation -> Theory.operation

        (** MustFail version of an operation.

            The operation is not considered processed: does not invalid the uid of the operation.
            This is used when wrapping an operation into a `MustFail`.
        *)
        val must_fail :
            t ->
            Theory.value option ->
            operation ->
            Theory.value

        (** The unique identifier of the operation. *)
        val uid : operation -> int

        (** Constructor. *)
        val mk : int -> Theory.operation -> operation
    end

    (** Type of a live contract. *)
    type live = private {
        address : Theory.Address.t ;
        (** Address of the live contract. *)
        contract : Contract.t ;
        (** Contract declaration. *)
        mutable balance : Theory.Tez.t ;
        (** Balance of the contract. *)
        mutable storage : Theory.value ;
        (** Value of the storage. *)
        params : Theory.contract_params ;
        (** Parameters the contract was created with. *)
    }

    (** Generates a unique identifier for some operation. *)
    val get_uid : t -> int

    (** An empty contract environment. *)
    val empty : unit -> t

    (** Registers a name/contract binding.

        Throws an exception if the name is already mapped to something.
    *)
    val add : Contract.t -> t -> unit

    (** Retrieves a contract from its name. *)
    val get : string -> t -> Contract.t

    (** Operations dealing with live contracts. *)
    module Live : sig
        (** Creates a live contract. *)
        val create : Theory.contract_params -> Contract.t -> t -> unit

        (** Retrieves a live contract from its address. *)
        val get : Theory.Address.t -> t -> live option

        (** Formats the live contracts of an environment. *)
        val fmt : formatter -> t -> unit

        (** Number of live contracts. *)
        val count : t -> int

        (** Transfers some money to a live contract. *)
        val transfer : Theory.Tez.t -> live -> unit

        (** Updates a live contract.

            Arguments

            - new balance, will replace the old balance
            - new storage and its type
            - address of the contract being updated
            - environment

            The new balance is **not** added to the old one, it's a purely destructive update.
        *)
        val update : Theory.Tez.t -> Theory.value * Dtyp.t -> Theory.Address.t -> t -> unit
    end
end

(** A stack and its most basic operations. *)
module type StackBase = sig
    (** Underlying theory. *)
    module Theory : Theo.Sigs.Theory

    (** Underling contract environment. *)
    module Env : ContractEnv with module Theory = Theory

    (** Type of the stack. *)
    type t

    (** Contract environment. *)
    val contract_env : t -> Env.t

    (** Stack formatter. *)
    val fmt : formatter -> t -> unit

    (** The empty stack. *)
    val empty : Env.t -> t

    (** True if the stack is empty. *)
    val is_empty : t -> bool

    (** Pushes something on the stack. *)
    val push : ?binding : Annot.Var.t option -> Dtyp.t -> Theory.value -> t -> unit

    (** Pops something from the stack. *)
    val pop : t -> Theory.value * Dtyp.t

    (** Clears the stack completely.
    
        Used when running into errors.
    *)
    val clear : t -> unit

    (** Map over the last element on the stack. *)
    val map_last : (Theory.value -> Dtyp.t -> Theory.value) -> t -> unit

    (** Swaps the last two elements of the stack. *)
    val swap : t -> unit

    (** Dips the stack by one level. *)
    val dip : t -> unit

    (** Duplicates the last element on the stack. *)
    val dup : ?binding : Annot.Var.t option -> t -> unit

    (** Undips the stack by one level. *)
    val undip : t -> unit
end

(** Augments `StackBase` with helper functions to pop/push stuff. *)
module type Stack = sig
    include StackBase

    (** Pops a boolean value. *)
    val pop_bool : t -> bool * Dtyp.t

    (** Pops an integer value. *)
    val pop_int : t -> Theory.Int.t * Dtyp.t

    (** Pops a natural value. *)
    val pop_nat : t -> Theory.Nat.t * Dtyp.t

    (** Pops a string value. *)
    val pop_str : t -> Theory.Str.t * Dtyp.t

    (** Pops a key hash. *)
    val pop_key_hash : t -> Theory.KeyH.t * Dtyp.t

    (** Pops a mutez value. *)
    val pop_tez : t -> Theory.Tez.t * Dtyp.t

    (** Pops an address. *)
    val pop_address : t -> Theory.Address.t * Dtyp.t

    (** Pops a contract. *)
    val pop_contract : t -> Theory.Address.t option * Mic.contract

    (** Pops a disjunction. *)
    val pop_either : t -> (Theory.value, Theory.value) Theory.Either.t * Dtyp.t

    (** Pops an option. *)
    val pop_option : t -> Theory.value Theory.Option.t * Dtyp.t

    (** Pops a list. *)
    val pop_list : t -> Theory.value Theory.Lst.t * Dtyp.t

    (** Pops a pair. *)
    val pop_pair : t -> (Theory.value * Dtyp.t) * (Theory.value * Dtyp.t)

    (** Pops an operation. *)
    val pop_operation : t -> Env.operation * Dtyp.t

    (** Pops a list of operation. *)
    val pop_operation_list : t -> Env.operation list * Dtyp.t

    (** Pops the result of a contract call.

        A list of operations and the new storage value. The datatype returned is that of the
        storage value.
    *)
    val pop_contract_res : t -> Env.operation list * Theory.value * Dtyp.t

    (** Pops parameters for a contract creation.

        Takes the address generated by the interpreter for this particular contract creation
        operation.
    *)
    val pop_contract_params : Theory.Address.t -> t -> Theory.contract_params * Dtyp.t

    (** Pushes on the stack `Some` of the last element of the stack. *)
    val some : ?alias : Dtyp.alias -> t -> unit

    (** Pushes a `None` value on the stack. *)
    val none : ?alias : Dtyp.alias -> Dtyp.t -> t -> unit

    (** Pushes a left value for a disjunction. *)
    val left : ?alias : Dtyp.alias -> Dtyp.t -> t -> unit

    (** Pushes a right value for a disjunction. *)
    val right : ?alias : Dtyp.alias -> Dtyp.t -> t -> unit

    (** Prepend operation over lists.

        Pushes on the stack the result of prepending the last element on the stack to the
        second-to-last element on the stack.
    *)
    val cons : t -> unit

    (** Pushes a `nil` value. *)
    val nil : ?binding : Annot.Var.t option -> ?alias : Dtyp.alias -> Dtyp.t -> t -> unit
end

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
    module Env : ContractEnv with module Theory = Theory

    (** Stack used by the interpreter. *)
    module Stack : Stack with module Theory = Theory and module Env = Env

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

    (** The next instruction to run. `None` does **not** mean the interpreter is done.

        When this function returns `None`, it means either that there is nothing else to run or
        that the next step will not consist in running an instruction.
    *)
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
