(* module Naive = struct
    include Stack.Naive.Theory
    module Stack = struct
        include Stack.Naive
    end
end
 *)

module NaiveCxt = Make.Cxt(Stack.Naive)