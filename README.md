# micheltest

A test framework for michelson smart contracts.

# Build

We recommend to use the latest version of the OCaml compiler. For example

```bash
> opam switch create micheltest 4.07.1
```

`micheltest` relies on the [`dune`] build system and a few other libraries:

```bash
> opam install dune menhir
```

Finally, build `micheltest` with `make`. The binary will be `bin/micheltest`.

# Usage

Assuming the binary `micheltest` is in you path, you can run it with

```bash
> micheltest [ --contract <contract_file> <optional_init_file> ]* -- [ <testcase> ]*
```

Argument `<contract_file>` is a michelson contract (`storage`, `parameter` and `code` fields). `<optional_init_file>` is an optional initializer for the contract. It should contain two fields : `parameter` and `code`. The former is the type of data the initializer takes as input, and the latter is a (sequence of) michelson instruction(s) which, from a stack with a value of type `parameter`, produces a stack with a value of the `storage` type appearing in the `<contract_file>` associated with the initializer.

A `<testcase>` is a (sequence of) michelson instruction(s) which produce(s) a list of `operation`s from an empty stack. `micheltest` runs all testcases sequentially and reports the errors it runs into.

For example

```bash
> micheltest --contract rsc/test1/test1.liq.tz -- test1.liq.tz.init.tst
```

For more details run

```bash
> micheltest --help
```

[`dune`] : https://github.com/ocaml/dune (Dune project manager's Github page)