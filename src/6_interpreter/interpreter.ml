(** This module combines a theory with a notion of stack to create interpreters. *)

module Sigs = Sigs
module Make = Make

module Naive = Make.Interpreter (Stack.Naive)
module BigNaive = Make.Interpreter (Stack.BigNaive)
