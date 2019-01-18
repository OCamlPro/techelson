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

    # Safety

    When changing state, the old state becomes obsolete, meaning that taking a transition from that
    state will fail. (Inspection functions will still work though.)

    # Invariants

    From the semantics described above, it follows that

    - `run_test` implies there are no pending operations
*)
module type TestCxt = sig
    (** Underlying interpreter module. *)
    module Run : Interpreter.Sigs.Interpreter

    (** Test interpreter module. *)
    module RunTest : Interpreter.Sigs.TestInterpreter with module Run = Run

    (** Contract environment module. *)
    module Env = Run.Env

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
        val contract_env : run_test -> Env.t

        (** Concise `run_test` formatter.

            This formatter only outputs live contracts. For a more verbose output, use the
            accessors.
        *)
        val fmt : formatter -> run_test -> unit
    end

    (** Contains the operations related to `apply_ops`. *)
    module Ops : sig
        (** Applies the next operation. *)
        val apply : apply_ops -> (run_test, transfer) Either.t option

        (** Operations awaiting treatment. *)
        val operations : apply_ops -> Env.operation list

        (** The contract environment. *)
        val contract_env : apply_ops -> Env.t

        (** The next operation to apply. *)
        val next_op : apply_ops -> Env.operation option

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
        val operations : transfer -> Env.operation list

        (** The contract environment. *)
        val contract_env : transfer -> Env.t

        (** Concise `run_test` formatter.
        
            This formatter only outputs live contracts and pending operations. For a more verbose
            output, use the accessors.
        *)
        val fmt : formatter -> transfer -> unit
    end
end
