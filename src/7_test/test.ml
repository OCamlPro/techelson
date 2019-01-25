(** Assembles interpreters to build a full testcase runner. *)

module Testcases = Testcases
module Load = Load
module Sigs = Sigs
module Make = Make

module Naive = Make.TestCxt (Interpreter.Naive)
module BigNaive = Make.TestCxt (Interpreter.BigNaive)

module Default = BigNaive