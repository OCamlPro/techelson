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
> opam install dune menhir
```

Finally, build `techelson` with `make`. The binary will be `./bin/techelson`.

# Usage

Assuming the binary `techelson` is in you path, you can run it with

```bash
> techelson [ --contract <contract_file> <optional_init_file> ]* -- [ <testcase> ]*
```

Argument `<contract_file>` is a michelson contract (`storage`, `parameter` and `code` fields). `<optional_init_file>` is an optional initializer for the contract. It should contain two fields : `parameter` and `code`. The former is the type of data the initializer takes as input, and the latter is a (sequence of) michelson instruction(s) which, from a stack with a value of type `parameter`, produces a stack with a value of the `storage` type appearing in the `<contract_file>` associated with the initializer.

A `<testcase>` is a (sequence of) michelson instruction(s) which produce(s) a list of `operation`s from an empty stack. `techelson` runs all testcases sequentially and reports the errors it runs into. Testcases have access to extended Michelson instructions. They are discussed in the [Extensions section](#extensions) below.

For example

```bash
> techelson --contract rsc/test1/test1.liq.tz -- rsc/test1/test1.liq.tz.tst
```

# Extensions

Techelson testcases have access to extended Michelson instruction to ease the process of writing tests.

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

[dune]:https://github.com/ocaml/dune (Dune project manager's Github page)