(** Input/output signatures for the main functor. *)

open Base
open Common

(** Signature of the module returned by the main functor.

    Such a module wraps a `Interpreter.Sigs.Interpreter` and a test interpreter so that it's able
    to run the testcase and contract calls. The goal is to provide ways to

    - run the instruction in the testcase
    - stop when asked to do something (or when done): apply some operations, typically
    - run the contract transfers from the testcases
    - consume more operations, potentially created by contract transfers
    - resume running the testcase when there are no more operations

    # Workflow

    This module essentially encodes a state machine. There's only three states:

    - `run_test`: running the testcase
    - `apply_ops`: applying operations
    - `transfer`: running contract transfers

    State `run_test` is both the initial and the final state. While in this state, the only action
    possible is to run the test, which yields nothing if the test is over, or `apply_ops`.

    Once in the `apply_ops` state, the only action is to apply the operation. Operation application
    applies all the operations that do not creates a transfer, *e.g.*, contract creations, in the
    list of operations. If a transfer operation is encountered, we get in the `transfer` state.
    Otherwise it means there are no more operations and we go back to the `run_test` state.

    In the `transfer` state, the main action is to run the code of the target contract (possibly in
    lockstep) until it is done. Then, run termination puts the system back in the `apply_ops`
    state.

    Each state is encoded as a type and the different transitions as functions in modules `Test`,
    `Ops` and `Transfer` respectively.

    # Invariants

    From the semantics described above, it follows that

    - `run_test` implies there are no pending operations
*)
module type NuTestCxt = sig
    (** Underlying interpreter module. *)
    module Run : Interpreter.Sigs.Interpreter

    (** Test interpreter module. *)
    module RunTest : Interpreter.Sigs.TestInterpreter with module Run = Run

    (** Contract environment module. *)
    module Contracts = Run.Contracts

    (** Underlying theory. *)
    module Theory = Run.Theory

    (** State that runs the testcase. *)
    type run_test

    (** State that applies operations. *)
    type apply_ops

    (** State that runs a transfer. *)
    type transfer

    (** Creates the initial `run_test` state. *)
    val init : Contract.t list -> Testcase.t -> run_test

    (** Contains all the operations related to `run_test` *)
    module Test : sig
        (** Runs a test until it's over or some operations need to be applied.
        
            Returns `None` iff the test is over. *)
        val run : run_test -> apply_ops option

        (** The test interpreter. *)
        val interpreter : run_test -> RunTest.t

        (** The contract environment. *)
        val contract_env : run_test -> Contracts.t

        (** Concise `run_test` formatter.
        
            This formatter only outputs live contracts. For a more verbose output, use the
            accessors.
        *)
        val fmt : formatter -> run_test -> unit
    end

    (** Contains the operations related to `apply_ops`. *)
    module Ops : sig
        (** Applies operations until a transfer is reached or we run out of operations. *)
        val apply : apply_ops -> (run_test, transfer) Either.t

        (** Operations awaiting treatment. *)
        val operations : apply_ops -> Theory.operation list

        (** The contract environment. *)
        val contract_env : apply_ops -> Contracts.t

        (** Concise `run_test` formatter.
        
            This formatter only outputs live contracts and pending operations. For a more verbose
            output, use the accessors.
        *)
        val fmt : formatter -> apply_ops -> unit
    end

    (** Contains the operations related to `transfer`. *)
    module Transfer : sig
        (** Runs a transfer to completion. *)
        val transfer_run : transfer -> apply_ops

        (** Performs a single step in a transfer run. *)
        val transfer_step : transfer -> apply_ops option

        (** Transfer interpreter. *)
        val interpreter : transfer -> Run.t

        (** Operations awaiting treatment. *)
        val operations : transfer -> Theory.operation list

        (** The contract environment. *)
        val contract_env : transfer -> Contracts.t

        (** Concise `run_test` formatter.
        
            This formatter only outputs live contracts and pending operations. For a more verbose
            output, use the accessors.
        *)
        val fmt : formatter -> transfer -> unit
    end
end

module type TestCxt = sig
    (** Underlying interpreter module. *)
    module Run : Interpreter.Sigs.Interpreter

    (** Underlying theory. *)
    module Theory = Run.Theory

    (** Type of contexts. *)
    type t

    (** Empty context constructor. *)
    val mk : Contract.t list -> Testcase.t -> t

    (** Formats the test context. *)
    val fmt : formatter -> t -> unit

    (** True if the test is over. *)
    val is_done : t -> bool

    (** Performs a test step. Returns true if done.
    
        A test step consists in running the testcase until either *i)* it is over or *ii)* the test
        interpreter runs into an extended instruction such as `APPLY_OPERATIONS`. The output of
        this function is `true` iff the testcase is over.        
    *)
    val test_step : t -> bool

    (** The contract environment. *)
    val env : t -> Run.Contracts.t

    (** The contract interpreter.

        This interpreter is not always available. It is only available if the last call to
        `init_contract_run` returned `false` and there was no call to `terminate_contract_run`
        since.
    *)
    val interp : t -> Run.t

    (** The test interpreter.

        This interpreter is always available.
    *)
    val test : t -> Run.t

    (** Applies operations.

        This should be called after a test step. A test step stages operations (except when the
        test is over). Operations that only impact the contract environment (typically, contract
        creation) are applied by this function until a *transfer* operation is reached, if any.

        If it is, this function returns `Some` of the interpreter for this operation. Otherwise, it
        returns `None`. Note that `None` does not mean the testcase is done, it means there are no
        more operations to apply for this test step.
    *)
    val apply_operations : t -> Run.t option

    (** Prepares the next contract run, if any.

        This should be called after a test step. A test step usually stages operations (except when
        it is over).
    *)
    val init_contract_run : t -> bool

    (** Performs a contract step. Returns true if the contract call is over.
    
        A contract step is a step in the sense of `Interpreter.Sigs.Interpreter`. In particular, it
        does not necessarily corresponds to running an instruction.
    *)
    val contract_step : t -> bool

    val is_in_progress : t -> bool
    val terminate_contract_run : t -> unit

    module Contracts : sig
        val add : Contract.t -> t -> unit
        val get : string -> t -> Contract.t

        module Live : sig
            val create : Theory.contract_params -> Contract.t -> t -> unit
            val get : Theory.Address.t -> t -> Run.Contracts.live option
        end
    end
end