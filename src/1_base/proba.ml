(** Probabilities used in test generation. *)

(** Type of probabilities.

    All probabilities are in **percent**.
*)
type t = int

let _ = Random.init 42

(** Arithmetic-related probabilities. *)
module Arith = struct
    (** Probability to chose zero over other values. *)
    let zero : t = 20
end

(** Option-related probabilities. *)
module Opt = struct
    (** Probability to chose `None`. *)
    let none : t = 50
end

(** Collection-related probabilities. *)
module Coll = struct
    (** Probability to chose the empty collection over other values. *)
    let empty : t = 60

    (** Probability to add one more element when creating a collection. *)
    let add_one : t = 90
end

(** Test-related probabilities. *)
module Test = struct
    (** Probability to add one more transfer when creating a test. *)
    let add_transfer : t = 80
end
