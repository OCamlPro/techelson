(** Contains a functor that creates a contract environment from a theory. *)

(** Creates a contract Environment from a theory.

    This is the default way to create contract environment.
*)
module Contracts : functor (T : Theo.Sigs.Theory) -> Sigs.ContractEnv with module Theory = T