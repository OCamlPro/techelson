open Base
open Common

module type SigStack = sig
    module Theory : Theo.Sigs.SigTheory
    type t

    val empty : t
    val is_empty : t -> bool
    val push : ?binding : Annot.Var.t option -> Dtyp.t -> Theory.value -> t -> unit

    val swap : t -> unit
    val dip : t -> unit
    val dup : t -> unit
    val undip : t -> unit

    val pop : t -> Theory.value * Dtyp.t
    val pop_bool : t -> bool * Dtyp.t
    val pop_int : t -> Theory.Cmp.Int.t * Dtyp.t
    val pop_nat : t -> Theory.Cmp.Nat.t * Dtyp.t
    val pop_str : t -> Theory.Cmp.Str.t * Dtyp.t
    val pop_either : t -> (Theory.value, Theory.value) Theory.Either.t * Dtyp.t
    val pop_option : t -> Theory.value Theory.Option.t * Dtyp.t
    val pop_list : t -> Theory.value Theory.Lst.t * Dtyp.t

    val some : t -> unit
    val none : Dtyp.t -> t -> unit

    val left : Dtyp.t -> t -> unit
    val right : Dtyp.t -> t -> unit

    val cons : t -> unit
    val nil : Dtyp.t -> t -> unit

    val fmt : formatter -> t -> unit
end


module type SigCxt = sig
    module Theory : Theo.Sigs.SigTheory
    module Stack : SigStack

    type t

    val init : (Theory.value * Dtyp.t * Annot.Var.t option) list -> Mic.t list -> t
    val step : t -> bool
    val last_ins : t -> Mic.t option
    val next_ins : t -> Mic.t option
    val stack : t -> Stack.t
    val is_done : t -> bool
end

module type SigStackOps = sig
    module Stack : SigStack

    val push : ?binding : Annot.Var.t option -> Dtyp.t -> Stack.Theory.value -> Stack.t -> unit
end