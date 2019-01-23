(** An interpreter that uses a naive stack and default contract environment. *)
module Interpreter = Make.Interpreter (Stack.Default.Stack)
