(** This module combinates datatypes and operations to create a theory.

    A *theory* is a module that provides all the datatypes and associated operations over michelson
    values. It does so in a (arguably) user-friendly way, in particular by creating the illusion of
    forward referencing.

    The end goal is that one only needs to specify the primitive datatypes, and the `Make` functor
    defined in this module will do the rest. It will create list, option, set, map, *etc.*
    datatypes and their relevant operations.

    # Usage

    The module for each primitive datatype is self-contained, in the sense that it must not refer
    to other primitive datatypes. Some operations need to refer to more than one primitive datatype
    however, not to mention that we need conversions, between `int` and `nat` for instance. Such
    operations are given thanks to the `Sig...Conv` module signatures. See how the `Naive` theory
    is constructed for a full example.

    > So-called *conversion modules* are necessary so that the same module for a primitive datatype
    > can be used to create different theories. The only thing that (possibly) changes is the
    > conversion module. The `string` datatype is an example of a primitive datatype module that
    > will probably be always the same regardless of the other datatypes, while its conversion
    > module needs the `nat` datatype (which is bound to change between theories) to express,
    > *e.g.*, `slice`.

    The primitive datatype modules and the conversion modules are aggregated in a module
    implementing the `Sigs.SigPrimitive` signature. This might seem a bit tedious, but once this is
    done the functor in `Make` will build everything. In particular, it will put the functions
    inside conversion modules where they belong, creating the illusion of forward referencing.
    
    # Type Constraints

    Be careful when modifying either signatures or the functor. There are many type constraints all
    over the place making sure the same type appearing in different places is really the same type,
    even when obfuscated. Which the vast majority of datatypes are.
*)

module Sigs = Sigs

module Make = Make

module Naive = Naive
