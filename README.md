# techelson

TEst miCHELSON: `techelson`

A test execution engine for Michelson smart contracts.

# Features

Techelson is currently in beta. The rough list of (planned) features is

- [x] operation must-fail: specifies that a (chain of) operation(s) should fail
- [x] inspection instruction: break points and stack inspection
- [x] support for all data types (potentially with some liberty taken compared to tezos)
- [ ] semantics as close to the tezos protocol as possible
    - [x] for non-crypto operations (`int`, `nat`, `map`, *etc.*)
    - [ ] for crypto operations
- [x] support import of local michelson contracts
- [ ] support retrieving the storage and code of contracts directly from the tezos blockchain

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

You can also run `make test` to make sure there is no problem with the techelson binary. The user documentation's root is `docs/user_doc/index.html`. You can regenerate it with `make user-doc`, as long as you have [mdbook] installed.

# Usage

Assuming the binary `techelson` is in you path, you can run it with

```bash
> techelson [ --contract <contract_file> <optional_init_file> ]* -- [ <testcase> ]*
```

Argument `<contract_file>` is a michelson contract (`storage`, `parameter` and `code` fields). `<optional_init_file>` is an optional initializer for the contract. It should contain two fields : `parameter` and `code`. The former is the type of data the initializer takes as input, and the latter is a (sequence of) michelson instruction(s) which, from a stack with a value of type `parameter`, produces a stack with a value of the `storage` type appearing in the `<contract_file>` associated with the initializer.

A `<testcase>` is a (sequence of) michelson instruction(s) which produce(s) a list of `operation`s from an empty stack. Techelson runs all testcases sequentially and reports the errors it runs into.

For example

```bash
> techelson --contract rsc/tests/test0/contracts/test0.liq.tz -- rsc/tests/test0/okay/Test0Test1.techel
```

[dune]: https://github.com/ocaml/dune (Dune project manager's Github page)
[mdbook]: https://github.com/rust-lang-nursery/mdBook (mdbook's github repository)