open Base

module Make (S : Sigs.SigStack) : Sigs.SigStackOps = struct
    module Stack = S

    let push ?binding:(binding=None) (dtyp : Dtyp.t) (v : Stack.Theory.value) (stack : Stack.t) : unit =
        Stack.push ~binding dtyp v stack
end