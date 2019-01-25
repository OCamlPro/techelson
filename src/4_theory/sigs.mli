(** Input/output signatures for the functors that create evaluators.

    All primitive datatypes should be formatable (`TFmt`). All other operations are
    domain-specific. The actual signature of the modules the functor of this module takes
    is `Theory`.

    When modifying things here, be very careful of the type constraints in the `Theory` signature.
    The abstract types in the conversion modules must be constrained so that the compiler knows
    what they are equal to in the rest of the code.
*)

open Base
open Base.Common

(** A module with a type `t`. *)
module type SigT = sig
    (** The actual representation of the datatype's values. *)
    type t
end

(** A module that can format the values of its datatype. *)
module type TFmt = sig
    include SigT

    (** Formatter. *)
    val fmt : formatter -> t -> unit

    (** Converts a value to a native string. *)
    val to_string : t -> string
end

(** Signature of arithmetic modules, extended for specific datatypes.


    Note that subtraction is not defined. This is because subtraction for naturals is
    `nat -> nat -> int`, and thus cannot be captured by `t -> t -> t`.
*)
module type Arith = sig
    include TFmt

    (** Conversion from native strings. *)
    val of_string : string -> t

    (** Conversion from native integers. *)
    val of_native : int -> t

    (** Conversion to native integers. *)
    val to_native : t -> int

    (** Addition between values. *)
    val add : t -> t -> t

    (** Multiplication between values. *)
    val mul : t -> t -> t

    (** Division between values. *)
    val div : t -> t -> t

    (** Value comparison. *)
    val compare : t -> t -> int

    (** Absorbing element for addition of the datatype. *)
    val zero : t
end

(** Signature of a module for the integer datatype. *)
module type Int = sig
    include Arith

    (** Subtraction of integers. *)
    val sub : t -> t -> t
end

(** Signature of a module for the natural datatype. *)
module type Nat = sig
    include Arith

    (** Logical shift left. *)
    val lshift_lft : t -> t -> t

    (** Logical shift right. *)
    val lshift_rgt : t -> t -> t

    (** Logical xor. *)
    val xor : t -> t -> t
end

(** Signature of a module for the mutez datatype. *)
module type Tez = sig
    include TFmt

    (** Type of natural numbers in the theory. *)
    type nat

    (** Conversion from native strings. *)
    val of_string : string -> t

    (** Conversion from native 64 bits integers. *)
    val of_native : Int64.t -> t

    (** Conversios to native 64 bits integers. *)
    val to_native : t -> int

    (** Addition over mutez. *)
    val add : t -> t -> t

    (** Subtraction over mutez. *)
    val sub : t -> t -> t

    (** Multiplication over mutez. *)
    val mul_nat : t -> nat -> t

    (** Division between mutez and naturals. *)
    val div_nat : t -> nat -> t

    (** Division over mutez. *)
    val div : t -> t -> t

    (** Mutez comparison. *)
    val compare : t -> t -> int

    (** Absorbing value for addition. *)
    val zero : t

    (** Conversion to natural numbers. *)
    val to_nat : t -> nat

    (** Conversion from natural numbers. *)
    val of_nat : nat -> t
end

(** Natural conversion module. *)
module type NatConv = sig
    (** Type of integers in the theory. *)
    type int

    (** Type of naturals in the theory. *)
    type nat

    (** Integers to natural conversion. *)
    val int_to_nat : int -> nat option

    (** Natural to integer conversion. *)
    val nat_to_int : nat -> int

    (** Natural subtraction. *)
    val nat_sub : nat -> nat -> int

    (** Integer absolute value. *)
    val int_abs : int -> nat

    (** Euclidian division. *)
    val ediv : int -> int -> (int * nat) option
end

(** String module. *)
module type Str = sig
    include TFmt

    (** Conversion from native strings. *)
    val of_native : string -> t

    (** String concatenation. *)
    val concat : t -> t -> t
end

(** String conversion module. *)
module type StrConv = sig
    (** Type of strings in the theory. *)
    type str

    (** Type of naturals in the theory. *)
    type nat

    (** Type of integers in the theory. *)
    type int

    (** String size. *)
    val size : str -> nat

    (** String slice. *)
    val slice : nat -> nat -> str -> str

    (** String comparison. *)
    val compare : str -> str -> int
end

(** Timestamp module. *)
module type TStamp = sig
    include TFmt

    (** Conversion from native strings. *)
    val of_native : string -> t

    (** Current timestamp. *)
    val now : unit -> t

    (** Timestamp comparison. *)
    val compare : t -> t -> int
end

(** Timestamp conversion module. *)
module type TStampConv = sig
    (** Type of timestamps in the theory. *)
    type t_stamp

    (** Type of integers in the theory. *)
    type int

    (** Addition between timestamps and integers. *)
    val add : t_stamp -> int -> t_stamp

    (** Subtraction between timestamps and integers. *)
    val sub_int : t_stamp -> int -> t_stamp

    (** Subtraction over timestamps. *)
    val sub : t_stamp -> t_stamp -> int
end

(** Key module. *)
module type Key = sig
    include TFmt

    (** Conversion from native strings. *)
    val of_native : string -> t
end

(** Key hash module. *)
module type KeyH = sig
    include TFmt

    (** Key hash comparison. *)
    val compare : t -> t -> int
end

(** Key hash conversion module. *)
module type KeyHConv = sig
    (** Type of key hashes in the theory. *)
    type key_h

    (** Type of keys in the tehory. *)
    type key

    (** B58Check hash. *)
    val b58check : key -> key_h

    (** Blake2B hash. *)
    val blake2b : key -> key_h

    (** SHA256 hash. *)
    val sha256 : key -> key_h

    (** SHA512 hash. *)
    val sha512 : key -> key_h
end

(** Address module. *)
module type Address = sig
    (** Type representing addresses. *)
    type t

    (** Generates a fresh address.

        Used when creating contracts. *)
    val fresh : Annot.Var.t option -> t

    (** Address formatter. *)
    val fmt : formatter -> t -> unit

    (** Equality over addresses. *)
    val equal : t -> t -> bool

    (** Unique identifier associated with the address. *)
    val uid : t -> int
end

(** Aggregates all the primitive datatypes.

    This is the signature of the modules accepted by the theory functor.
*)
module type Primitive = sig
    (** Integer module. *)
    module Int : Int

    (** Natural module. *)
    module Nat : Nat

    (** Natural conversion module. *)
    module NatConv : NatConv with type int := Int.t and type nat := Nat.t

    (** String module. *)
    module Str : Str

    (** String conversion module. *)
    module StrConv : StrConv with type str := Str.t and type int := Int.t and type nat := Nat.t

    (** Bytes module. *)
    module Bytes : Str

    (** Bytes conversion module. *)
    module BytesConv : StrConv with type str := Bytes.t and type int := Int.t and type nat := Nat.t

    (** Timestamp module. *)
    module TStamp : TStamp

    (** Timestamp conversion module. *)
    module TStampConv : TStampConv with type t_stamp := TStamp.t and type int := Int.t

    (** Key module. *)
    module Key : Key

    (** Key hash module. *)
    module KeyH : KeyH

    (** Key hash conversion module. *)
    module KeyHConv : KeyHConv with type key := Key.t and type key_h := KeyH.t

    (** Address module. *)
    module Address : Address
end

(** Signature of a theory.

    This is the signature of the modules returned by the theory functor.
*)
module type Theory = sig

    (** Primitive datatype, this is the input of the theory functor. *)
    module Prim : Primitive

    (** Integer module. *)
    module Int : sig
        include Arith with type t = Prim.Int.t

        (** Subtraction over integers. *)
        val sub : t -> t -> t

        (** Conversion to naturals. *)
        val to_nat : t -> Prim.Nat.t option

        (** Conversion from naturals. *)
        val of_nat : Prim.Nat.t -> t

        (** Absolute value. *)
        val abs : t -> Prim.Nat.t

        (** Euclidean division. *)
        val ediv : t -> t -> (t * Prim.Nat.t) option
    end

    (** Natural module. *)
    module Nat : sig
        include Nat with type t = Prim.Nat.t

        (** Subtraction over naturals. *)
        val sub : t -> t -> Int.t

        (** Conversion from integers. *)
        val of_int : Int.t -> t option

        (** Conversion to integers. *)
        val to_int : t -> Int.t
    end

    (** Mutez module. *)
    module Tez : Tez with type nat = Nat.t

    (** String module. *)
    module Str : sig
        include Str with type t = Prim.Str.t

        (** String size. *)
        val size : t -> Nat.t

        (** String slice. *)
        val slice : Nat.t -> Nat.t -> t -> t

        (** String comparison. *)
        val compare : t -> t -> Int.t
    end

    (** Bytes module. *)
    module Bytes : sig
        include Str with type t = Prim.Bytes.t

        (** Bytes size. *)
        val size : t -> Nat.t

        (** Bytes slice. *)
        val slice : Nat.t -> Nat.t -> t -> t

        (** Bytes comparison. *)
        val compare : t -> t -> Int.t
    end

    (** Timestamp module. *)
    module TStamp : sig
        include TStamp with type t = Prim.TStamp.t

        (** Addition over timestamps. *)
        val add : t -> Int.t -> t

        (** Subtraction between timestamps and integers. *)
        val sub_int : t -> Int.t -> t

        (** Subtraction over timestamps. *)
        val sub : t -> t -> Int.t
    end

    (** Key module. *)
    module Key : sig
        include Key with type t = Prim.Key.t

        (** B58Check hash. *)
        val b58check : t -> Prim.KeyH.t

        (** Blake2B hash. *)
        val blake2b : t -> Prim.KeyH.t

        (** SHA256 hash. *)
        val sha256 : t -> Prim.KeyH.t

        (** SHA512 hash. *)
        val sha512 : t -> Prim.KeyH.t
    end

    (** Key hash module. *)
    module KeyH : KeyH with type t = Prim.KeyH.t

    (** Comparable types. *)
    module Cmp : sig

        (** Aggregates comparable types. *)
        type t =
        | B of bool
        (** Booleans. *)
        | I of Int.t
        (** Integers. *)
        | N of Nat.t
        (** Naturals. *)
        | S of Str.t
        (** Strings. *)
        | By of Bytes.t
        (** Bytes. *)
        | Ts of TStamp.t
        (** Timestamps. *)
        | Tz of Tez.t
        (** Mutez. *)
        | KeyH of KeyH.t
        (** Key hash. *)

        (** Comparison between comparable values. *)
        val cmp : t -> t -> int

        (** Comparable value formatter. *)
        val fmt : formatter -> t -> unit

        (** Datatype of a comparable value. *)
        val dtyp : t -> Dtyp.t

        (** Casts a value to some datatype. *)
        val cast : Dtyp.t -> t -> t

        (** Provides functions to unwrap comparable values.

            All functions in this module throw an exception if the unwrap operation asked is not
            possible.
        *)
        module Unwrap : sig
            (** Unwraps a boolean value. *)
            val bool : t -> bool

            (** Unwraps an integer. *)
            val int : t -> Int.t

            (** Unwraps a natural. *)
            val nat : t -> Nat.t

            (** Unwraps a string. *)
            val str : t -> Str.t

            (** Unwraps some bytes. *)
            val bytes : t -> Bytes.t
        end
    end

    (** Address module. *)
    module Address : Address

    (** Set module. *)
    module Set : sig
        (** A set. *)
        type t

        (** The elements stored by the sets can only be values of comparable datatypes. *)
        type elm = Cmp.t

        (** The empty set. *)
        val empty : t

        (** True if the element is in the set. *)
        val mem : elm -> t -> bool

        (** Adds (removes) an element if the flag is true (false). *)
        val update : elm -> bool -> t -> t

        (** Fold over the elements of the set. *)
        val fold : ('acc -> elm -> 'acc) -> 'acc -> t -> 'acc

        (** Fold over the elements of the set, applying some function on each element.

            This is useful when all the elements of the set have a known type and we want to just
            unwrap the actual value using `Cmp.Unwrap`.
        *)
        val fold_as : (elm -> 'a) -> ('acc -> 'a -> 'acc) -> 'acc -> t -> 'acc

        (** Size of a set. *)
        val size : t -> Nat.t

        (** Formats a set. *)
        val fmt : formatter -> t -> unit
    end

    (** Map module. *)
    module Map : sig
        (** A map. *)
        type 'a t

        (** Keys have to be values of comparable datatypes. *)
        type key = Cmp.t

        (** The empty map. *)
        val empty : 'a t

        (** Retrieve the value associated to a key. *)
        val get : key -> 'a t -> 'a option

        (** True if the key appears in the map. *)
        val mem : key -> 'a t -> bool

        (** Adds (removes) a binding if input is `Some` (`None`). *)
        val update : key -> 'a option -> 'a t -> 'a t

        (** Map over maps. *)
        val map : (key -> 'a -> 'b) -> 'a t -> 'b t

        (** Map over maps, with some functions applied to keys and elements.

            This is useful when we know the type of the keys and the values and just want to unwrap
            the actual values using `Cmp.Unwrap`.
        *)
        val map_as : (key -> 'k) -> ('a -> 'b) -> ('k -> 'b -> 'c) -> 'a t -> 'c t

        (** Fold over maps. *)
        val fold : ('acc -> key -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc

        (** Fold over maps, with some functions applied to keys and elements.

            This is useful when we know the type of the keys and the values and just want to unwrap
            the actual values using `Cmp.Unwrap`.
        *)
        val fold_as :
            (key -> 'k) ->
            ('a -> 'b) ->
            ('acc -> 'k -> 'b -> 'acc) ->
            'acc ->
            'a t ->
            'acc

        (** Size of a map. *)
        val size : 'a t -> Nat.t

        (** Map formatter. *)
        val fmt : (formatter -> 'a -> unit) -> formatter -> 'a t -> unit
    end

    (** Big map module. *)
    module BigMap : sig
        (** A map. *)
        type 'a t

        (** Keys have to be values of comparable datatypes. *)
        type key = Cmp.t

        (** The empty map. *)
        val empty : 'a t

        (** Retrieve the value associated to a key. *)
        val get : key -> 'a t -> 'a option

        (** True if the key appears in the map. *)
        val mem : key -> 'a t -> bool

        (** Adds (removes) a binding if input is `Some` (`None`). *)
        val update : key -> 'a option -> 'a t -> 'a t

        (** Map over maps. *)
        val map : (key -> 'a -> 'b) -> 'a t -> 'b t

        (** Map over maps, with some functions applied to keys and elements.

            This is useful when we know the type of the keys and the values and just want to unwrap
            the actual values using `Cmp.Unwrap`.
        *)
        val map_as : (key -> 'k) -> ('a -> 'b) -> ('k -> 'b -> 'c) -> 'a t -> 'c t

        (** Fold over maps. *)
        val fold : ('acc -> key -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc

        (** Fold over maps, with some functions applied to keys and elements.

            This is useful when we know the type of the keys and the values and just want to unwrap
            the actual values using `Cmp.Unwrap`.
        *)
        val fold_as :
            (key -> 'k) ->
            ('a -> 'b) ->
            ('acc -> 'k -> 'b -> 'acc) ->
            'acc ->
            'a t ->
            'acc

        (** Size of a map. *)
        val size : 'a t -> Nat.t

        (** Map formatter. *)
        val fmt : (formatter -> 'a -> unit) -> formatter -> 'a t -> unit
    end

    (** Disjunction module. *)
    module Either : sig
        (** Either one of two things. *)
        type ('l, 'r) t =
        | Lft of 'l
        (** Left version. *)
        | Rgt of 'r
        (** Right version. *)

        (** Formats a disjunction. *)
        val fmt :
            (formatter -> 'l -> unit) ->
            (formatter -> 'r -> unit) ->
            formatter ->
            ('l, 'r) t ->
            unit
    end

    (** Option module. *)
    module Option : sig
        (** Options are just native caml options. *)
        type 'a t = 'a option

        (** Option formatter. *)
        val fmt : (formatter -> 'a -> unit) -> formatter -> 'a t -> unit
    end

    (** List module. *)
    module Lst : sig
        (** A list. *)
        type 'a t

        (** The empty list. *)
        val nil : 'a t

        (** Prepend over lists. *)
        val cons : 'a -> 'a t -> 'a t

        (** True if the list is empty. *)
        val is_nil : 'a t -> bool

        (** Map over lists. *)
        val map : ('a -> 'b) -> 'a t -> 'b t

        (** Fold over lists. *)
        val fold : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc

        (** Size of a list. *)
        val size : 'a t -> Nat.t

        (** List formatter. *)
        val fmt : (formatter -> 'a -> unit) -> formatter -> 'a t -> unit

        (** Inverse of the prepend operation. *)
        val snoc : 'a t -> ('a * 'a t) option
    end

    (** A value in the theory. *)
    type value = private
    | U
    (** Unit. *)
    | C of Cmp.t
    (** A value of a comparable type. *)
    | Key of Key.t
    (** A key. *)
    | Set of Set.t
    (** A set. *)
    | Map of value Map.t
    (** A map. *)
    | BigMap of value BigMap.t
    (** A bigmap. *)
    | Either of (value, value) Either.t
    (** A disjunction. *)
    | Option of value Option.t
    (** An option. *)
    | Lst of value Lst.t
    (** A list. *)
    | Pair of value * value
    (** A pair. *)
    | Contract of Address.t option * Mic.contract
    (** A contract.

        The address is `None` if the value is an anonymous undeployed contract, and stores the
        address of the deployed contract otherwise.
    *)
    | Operation of int * operation
    (** An operation. *)
    | Address of Address.t
    (** An address. *)
    | Lambda of Dtyp.t * Dtyp.t * Mic.t
    (** A lambda. *)

    (** Parameters for contract creation. *)
    and contract_params = private {
        address : Address.t ;
        (** The address of the contract, generated by the interpreter. *)
        manager : KeyH.t ;
        (** The manager for this contract. *)
        mutable delegate : KeyH.t option ;
        (** Optional delegate. *)
        spendable : bool ;
        (** Spendable flag. *)
        delegatable : bool ;
        (** Delegatable flag. *)
        tez : Tez.t ;
        (** Initial amount of mutez. *)
        value : value ;
        (** Storage value. *)
    }

    (** An operation in the (extended) michelson sense. *)
    and operation =
    | MustFail of value option * operation * int
    (** An operation that must fail with some optional value. *)
    | Create of contract_params * Mic.contract
    (** Vanilla contract creation. *)
    | CreateNamed of contract_params * Contract.t
    (** Creation of a contract based on its name.

        The contract must have been given to the environment prior to running the test, with the
        appropriate name.
    *)
    | InitNamed of contract_params * value * string
    (** Creation of a contract based on its name, with an initializer.

        The contract and its initializer must have been given to the environment prior to running
        the test, with the appropriate name.
    *)
    | Transfer of Address.t * Mic.contract * Tez.t * value
    (** A transfer.

        Arguments:

        - address of the contract
        - actual contract
        - amount of mutez transferred
        - entry point's input value

    *)
    | SetDelegate of Address.t * KeyH.t option

    (** Constructor for contract parameters. *)
    val mk_contract_params :
        spendable : bool ->
        delegatable : bool ->
        KeyH.t ->
        KeyH.t option ->
        Tez.t ->
        Address.t ->
        value ->
        contract_params

    (** Value formatter. *)
    val fmt : formatter -> value -> unit

    (** Contract parameters formatter. *)
    val fmt_contract_params : formatter -> contract_params -> unit

    (** Sets the delegate of a contract parameter. *)
    val set_delegate : KeyH.t option -> contract_params -> unit

    (** Operation formatter.
    
        The first argument is the operation's unique identifier.
    *)
    val fmt_operation : int -> formatter -> operation -> unit

    (** Cast between values. *)
    val cast : Dtyp.t -> value -> value

    (** Functions converting stuff to values. *)
    module Of : sig
        (** Integer to value. *)
        val int : Int.t -> value

        (** Natural to value. *)
        val nat : Nat.t -> value

        (** String to value. *)
        val str : Str.t -> value

        (** Bytes to value. *)
        val bytes : Bytes.t -> value

        (** Timestamp to value. *)
        val timestamp : TStamp.t -> value

        (** Key to value. *)
        val key : Key.t -> value

        (** Key hash to value. *)
        val key_hash : KeyH.t -> value

        (** Mutez to value. *)
        val tez : Tez.t -> value

        (** Lambda to value. *)
        val lambda : Dtyp.t -> Dtyp.t -> Mic.t -> value

        (** Address to value. *)
        val address : Address.t -> value

        (** Native string to value. *)
        val primitive_str : Dtyp.t -> string -> value

        (** Unit value. *)
        val unit : value

        (** Boolean to value. *)
        val bool : bool -> value

        (** `Base.Mic` constant to value. *)
        val const : Mic.const -> value

        (** Comparable value to value. *)
        val cmp : Cmp.t -> value

        (** Set to value. *)
        val set : Set.t -> value

        (** Map to value. *)
        val map : value Map.t -> value

        (** Big map to value. *)
        val big_map : value BigMap.t -> value

        (** Disjunction to value. *)
        val either : (value, value) Either.t -> value

        (** List to value. *)
        val list : value Lst.t -> value

        (** Option to value. *)
        val option : value Option.t -> value

        (** Pair to value. *)
        val pair : value -> value -> value

        (** Contract to value. *)
        val contract : Address.t -> Mic.contract -> value

        (** Operation creation functions. *)
        module Operation : sig
            (** Creates a vanilla contract creation operation.
            
                The first parameter is the unique identifier of the operation. It will generated by
                the contract environment.
            *)
            val create : int -> contract_params -> Mic.contract -> value

            (** Creates a named contract creation operation.
            
                The first parameter is the unique identifier of the operation. It will generated by
                the contract environment.
            *)
            val create_named : int -> contract_params -> Contract.t -> value

            (** Creates a named contract creation operation with init.
            
                The first parameter is the unique identifier of the operation. It will generated by
                the contract environment.
            *)
            val init_named : int -> contract_params -> value -> string -> value

            (** Creates a transfer operation.
            
                The first parameter is the unique identifier of the operation. It will generated by
                the contract environment.
            *)
            val transfer : int -> Address.t -> Mic.contract -> Tez.t -> value -> value

            (** Wraps an operation in a `MustFail`.
            
                The first parameter is the unique identifier of the operation. It will generated by
                the contract environment.
            *)
            val must_fail : int -> value option -> (operation * int) -> value

            (** Creates a set delegate operation. *)
            val set_delegate : int -> Address.t -> KeyH.t option -> value
        end
    end

    (** Deconstructors. *)
    module Inspect : sig
        (** Deconstructs a list of values. *)
        val list : value -> value Lst.t

        (** Deconstructs a key. *)
        val key : value -> Key.t
    end

    (** Comparison between values.

        Only legal on comparable values.
    *)
    val cmp : value -> value -> value * Dtyp.t

    (** Value equality.

        This is not michelson's `EQ` which compares a value to zero. That function is `is_zero`.
    *)
    val eq : value -> value -> value

    (** Value inequality.

        This is not michelson's `NEQ` which compares a value to zero. That function is
        `is_not_zero`.
    *)
    val neq : value -> value -> value

    (** True if the value is zero.

        Only legal on integers, naturals and mutez.
    *)
    val is_zero : value -> value

    (** True if the value is not zero.

        Only legal on integers, naturals and mutez.
    *)
    val is_not_zero : value -> value

    (** True if the value less than zero.

        Only legal on integers, naturals and mutez.
    *)
    val lt_zero : value -> value

    (** True if the value is less than or equal to zero.

        Only legal on integers, naturals and mutez.
    *)
    val le_zero : value -> value

    (** True if the value is greater than or equal to zero.

        Only legal on integers, naturals and mutez.
    *)
    val ge_zero : value -> value

    (** True if the value is greater than zero.

        Only legal on integers, naturals and mutez.
    *)
    val gt_zero : value -> value

    (** Absolute value.

        Only legal on integers.
    *)
    val abs : value -> value * Dtyp.t

    (** Polymorphic subtraction. *)
    val add : value -> value -> value * Dtyp.t

    (** Polymorphic addition. *)
    val sub : value -> value -> value * Dtyp.t

    (** Polymorphic multiplication. *)
    val mul : value -> value -> value * Dtyp.t

    (** Arithmetic negation. *)
    val neg : value -> value * Dtyp.t

    (** Euclidean division. *)
    val ediv : value -> value -> value * Dtyp.t

    (** Logical shift left. *)
    val lshift_lft : value -> value -> value * Dtyp.t

    (** Logical shift right. *)
    val lshift_rgt : value -> value -> value * Dtyp.t

    (** Logical `or`. *)
    val disj : value -> value -> value * Dtyp.t

    (** Logical `and`. *)
    val conj : value -> value -> value * Dtyp.t

    (** Logical `xor`. *)
    val xor : value -> value -> value * Dtyp.t

    (** Logical negation. *)
    val not : value -> value * Dtyp.t

    (** First projector over pairs. *)
    val car : value -> value

    (** Second projector over pairs. *)
    val cdr : value -> value

    (** Prepend operation. *)
    val cons : value -> value -> value

    (** Transforms a collection into a list. *)
    val coll_to_list : value -> value list
end
