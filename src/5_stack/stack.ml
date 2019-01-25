(** This module is in charge of constructing stacks and contract environments. *)

module Sigs = Sigs
module Make = Make

module Naive : Sigs.Stack = Make.Stack (Make.StackBase (Theo.Naive))
module BigNaive : Sigs.Stack = Make.Stack (Make.StackBase (Theo.BigNaive))