# techelson

TEst miCHELSON: `techelson`

A test execution engine for Michelson smart contracts.

# Build

We recommend to use the latest version of the OCaml compiler. For example

```bash
> opam switch create techelson 4.07.1
```

`techelson`relies on the [dune] build system and a few other libraries:

```bash
> opam install dune menhir zarith ptime
```

(This list of dependencies might be out-of-date. Check `.travis.sh` for the latest version.) Finally, build `techelson` with `make`. The binary will be `./bin/techelson`.

# Usage

Assuming the binary `techelson` is in you path, you can run it with

```bash
> techelson [ --contract <contract_file> <optional_init_file> ]* -- [ <testcase> ]*
```

Argument `<contract_file>` is a michelson contract (`storage`, `parameter` and `code` fields). `<optional_init_file>` is an optional initializer for the contract. It should contain two fields : `parameter` and `code`. The former is the type of data the initializer takes as input, and the latter is a (sequence of) michelson instruction(s) which, from a stack with a value of type `parameter`, produces a stack with a value of the `storage` type appearing in the `<contract_file>` associated with the initializer.

A `<testcase>` is a (sequence of) michelson instruction(s) which produce(s) a list of `operation`s from an empty stack. Techelson runs all testcases sequentially and reports the errors it runs into.

For example

```bash
> techelson --contract rsc/test1/test1.liq.tz -- rsc/test1/test1.liq.tz.tst
```

Testcases have access to extended Michelson instructions, and contracts can use a subset of these extensions. They are discussed in the [Extensions section](#extensions) below.

# Testcase-only instructions

The following instructions are ([extensions](#extensions), discussed below.

- `SET_SOURCE`
- `APPLY_OPERATIONS`
- `STORAGE_OF`
- `BALANCE_OF`
- `MUST_FAIL`

# Contract-transfer-only instructions

- `SENDER`, this instruction does not make sense in a testcase as a testcase lives outside of the tezos protocol

# Extensions

Techelson testcases have access to extended Michelson instruction to ease the process of writing tests. See `rsc/tests/` for more examples. Note that techelson treats `#>` as whitespace, so you can use extensions in contracts (when legal) while keeping them pure tezos, like in the example below

```
DIP {
    ...
    CONS ;
    #> PRINT_STACK ;
    #> STEP "after list cons" ;
    PUSH int 3 ;
    ...
}
```

The extensions are

- `STEP` and `STEP <string>`:

    `S` `->` `S`

    suspends the evaluator and prints a string, if any.

- `PRINT_STACK`:

    `S` `->` `S`

    prints the current state of the stack

- `APPLY_OPERATIONS`:

    `(list operation) : S` `->` `S`

    - consumes a list of operations
    - suspends the execution of the testcase
    - applies all the operations in the list (these operations can create operations which will be applied too)
    - resumes the execution of the testcase

- `STORAGE_OF 'storage`:

    `contract 'a : S` `->` `(option 'storage) : S`

    - consumes a contract
    - pushes `Some` of the current value of the storage of the contract if its storage has type `'storage`, `None` otherwise

- `BALANCE_OF`:

    `contract 'a : S` `->` `mutez : S`

    same as `STORAGE_OF`, but pushes the balance of the contract instead of its storage

- `MUST_FAIL`:

    `(option 'a) : operation : S` `->` `S`

    specifies that an operation must fail, and optionaly that it `FAIL`ed`WITH` a certain value. More precisely, the whole testcase will fail if the `operation`, *when applied*, either

    - succeeds, or
    - the `(option 'a)` parameter is `(Some value)` and the operation's failure was not caused by a `FAILWITH` on precisely `value`
    
    > Note that if the optional value is `NONE`, then `MUST_FAIL` accepts any kind of *protocol* failure, not just `FAILWITH`. For instance, it will also accept creation/transfer operations that fail because of insufficient balance, because this precise operation already ran (it was `DUP`-ed), *etc*.

- `SET_SOURCE`:

    `address :: S` `->` `S`

    sets the source of the testcase. The source of all transfers can only be the testcase. This allows to pretend the testcase is a live contract.

[dune]:https://github.com/ocaml/dune (Dune project manager's Github page)