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

    val expired_uids : t -> IntSet.t

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
            (Theory.value * Dtyp.t) option ->
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

    (** Type checks two types. *)
    val unify : t -> Dtyp.t -> Dtyp.t -> unit

    (** Formats the live contracts of an environment. *)
    val fmt : formatter -> t -> unit

    (** Operations dealing with live contracts. *)
    module Live : sig
        (** Creates a live contract. *)
        val create : Theory.contract_params -> Contract.t -> t -> unit

        (** Live contract formatter. *)
        val fmt : formatter -> live -> unit

        (** Retrieves a live contract from its address. *)
        val get : Theory.Address.t -> t -> live option

        (** Number of live contracts. *)
        val count : t -> int

        (** Transfers some money to a live contract. *)
        val transfer : src : string -> Theory.Tez.t -> live -> unit

        (** Collects some money from a live contract. *)
        val collect : tgt : string -> Theory.Tez.t -> live -> unit

        (** Updates a live contract.

            Arguments

            - new balance, will replace the old balance
            - new storage and its type
            - address of the contract being updated
            - environment

            The new balance is **not** added to the old one, it's a purely destructive update.
        *)
        val update : Theory.Tez.t -> Theory.value * Dtyp.t -> Theory.Address.t -> t -> unit

        (** Updates the storage of a live contract. *)
        val update_storage : t -> Theory.value -> Dtyp.t -> live -> unit

        (** Sets the delegate of a live contract.

            This can throw a protocol exception if the contract is not delegatable.
        *)
        val set_delegate : Theory.KeyH.t option -> live -> unit
    end

    (** Account related functions. *)
    module Account : sig
        (** Retrieves an implicit account for some key hash.

            Creates the account with zero mutez if it does not exist.
        *)
        val implicit : Theory.KeyH.t -> t -> live
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

    (** Type checks two types. *)
    val unify : t -> Dtyp.t -> Dtyp.t -> unit

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

    (** Convenience pop helpers. *)
    module Pop : sig

        (** Pops a boolean value. *)
        val bool : t -> bool * Dtyp.t

        (** Pops an integer value. *)
        val int : t -> Theory.Int.t * Dtyp.t

        (** Pops a natural value. *)
        val nat : t -> Theory.Nat.t * Dtyp.t

        (** Pops a string value. *)
        val str : t -> Theory.Str.t * Dtyp.t

        (** Pops some bytes. *)
        val bytes : t -> Theory.Bytes.t * Dtyp.t

        (** Pops a key hash. *)
        val key_hash : t -> Theory.KeyH.t * Dtyp.t

        (** Pops an optional key hash. *)
        val key_hash_option : t -> Theory.KeyH.t option * Dtyp.t

        (** Pops a mutez value. *)
        val tez : t -> Theory.Tez.t * Dtyp.t

        (** Pops a comparable value. *)
        val cmp : t -> Theory.Cmp.t * Dtyp.t

        (** Pops an address. *)
        val address : t -> Theory.Address.t * Dtyp.t

        (** Pops a contract. *)
        val contract : t -> Theory.Address.t option * Mic.contract

        (** Pops a disjunction. *)
        val either : t -> (Theory.value, Theory.value) Theory.Either.t * Dtyp.t

        (** Pops an option. *)
        val option : t -> Theory.value Theory.Option.t * Dtyp.t

        (** Pops a list. *)
        val list : t -> Theory.value Theory.Lst.t * Dtyp.t

        (** Pops a set. *)
        val set : t -> Theory.Set.t * Dtyp.t

        (** Pops a map. *)
        val map : t -> Theory.value Theory.Map.t * Dtyp.t

        (** Pops a pair. *)
        val pair : t -> (Theory.value * Dtyp.t) * (Theory.value * Dtyp.t)

        (** Pops a lambda. *)
        val lambda : t -> Dtyp.t * Dtyp.t * Mic.t

        (** Pops an operation. *)
        val operation : t -> Env.operation * Dtyp.t

        (** Pops a list of operation. *)
        val operation_list : t -> Env.operation list * Dtyp.t

        (** Pops the result of a contract call.

            A list of operations and the new storage value. The datatype returned is that of the
            storage value.
        *)
        val contract_res : t -> Env.operation list * Theory.value * Dtyp.t

        (** Pops parameters for a contract creation.

            Takes the address generated by the interpreter for this particular contract creation
            operation.
        *)
        val contract_params : Theory.Address.t -> t -> Theory.contract_params * Dtyp.t

        (** Pops parameters for a contract creation with the entry given as a lambda. *)
        val contract_params_and_lambda :
            Theory.Address.t -> t -> Theory.contract_params * Mic.contract

        (** Pops parameters for an account creation.

            The difference with `contract_params` is that this one does not retrieve the
            `spendable` flag and initial storage value from the stack. They are set to `true` and
            `unit`.
        *)
        val account_params : Theory.Address.t -> t -> Theory.contract_params
    end

    (** Convenience push operation. *)
    module Push : sig
        (** Pushes on the stack `Some` of the last element of the stack. *)
        val some :
            ?binding : Annot.Var.t option ->
            ?alias : Dtyp.alias ->
            ?field : Annot.Field.t option ->
            t ->
            unit

        (** Pushes a `None` value on the stack. *)
        val none :
            ?binding : Annot.Var.t option ->
            ?alias : Dtyp.alias ->
            ?field : Annot.Field.t option ->
            Dtyp.t ->
            t ->
            unit

        (** Pushes a left value for a disjunction. *)
        val left :
            ?binding : Annot.Var.t option ->
            ?alias : Dtyp.alias ->
            ?field : Annot.Field.t option ->
            Dtyp.t ->
            t ->
            unit

        (** Pushes a right value for a disjunction. *)
        val right :
            ?binding : Annot.Var.t option ->
            ?alias : Dtyp.alias ->
            ?field : Annot.Field.t option ->
            Dtyp.t ->
            t ->
            unit

        (** Pushes a `nil` value. *)
        val nil : ?binding : Annot.Var.t option -> ?alias : Dtyp.alias -> Dtyp.t -> t -> unit

        (** Pushes an empty set value. *)
        val empty_set : ?binding : Annot.Var.t option -> ?alias : Dtyp.alias -> Dtyp.t -> t -> unit

        (** Pushes an empty map value. *)
        val empty_map :
            ?binding : Annot.Var.t option ->
            ?alias : Dtyp.alias ->
            Dtyp.t ->
            Dtyp.t ->
            t ->
            unit

        (** Pushes an address on the stack. *)
        val address :
            ?binding : Annot.Var.t option ->
            ?alias : Dtyp.alias ->
            Theory.Address.t ->
            t ->
            unit
    end

    (** Prepend operation over lists.

        Pushes on the stack the result of prepending the last element on the stack to the
        second-to-last element on the stack.
    *)
    val cons : t -> unit

    (** Renames the element on the top of the stack. *)
    val rename : Annot.Var.t option -> t -> unit
end
