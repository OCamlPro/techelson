# Techelson [![Build Status](https://travis-ci.org/AdrienChampion/techelson.svg?branch=master)](https://travis-ci.org/AdrienChampion/techelson)

TEst miCHELSON: `techelson`.

A test execution engine for Michelson smart contracts.

# Documentation

The user documentation is hosted here [the main repository's github pages]. Finally, there is a blog post by Techelson's main developer on how to use Techelson with [Liquidity]: [adrienchampion.github.io/blog].

# Features

Techelson is currently in beta. The rough list of (planned) features is

- [x] operation must-fail: specifies that a (chain of) operation(s) should fail
- [x] inspection instruction: break points and stack inspection
- [ ] support for all data types (potentially with some liberty taken compared to tezos)
    - [x] everything but `signature`
    - [ ] `signature`
- [ ] semantics as close to the tezos protocol as possible
    - [x] for non-crypto, non-packing-related operations (`int`, `nat`, `map`, *etc.*)
    - [ ] for crypto operations
    - [x] internal packing and unpacking (packed and unpacked by Techelson)
    - [ ] arbitrary byte unpacking
- [x] support import of local michelson contracts
- [ ] support retrieving the storage and code of contracts directly from the tezos blockchain

# Build

We recommend to use the latest version of the OCaml compiler. Make sure you have [opam installed],
and run

```bash
> opam switch create techelson 4.07.1
```

Techelson relies on the [dune] build system and a few other libraries:

```bash
> opam install dune menhir zarith ptime stdint
```

(This list of dependencies might be out-of-date. Check `.travis.sh` for the latest version.) Finally, build Techelson with `make`. The binary will be `./bin/techelson`.

You can also run `make test` to make sure there is no problem with the Techelson binary. The user documentation's root is `docs/user_doc/index.html`. You can regenerate it with `make user-doc`, as long as you have [mdbook] installed.

# Usage

Assuming the binary Techelson is in you path, you can run it with

```bash
> techelson [ --contract <contract_file> ]* -- [ <testcase> ]*
```

Argument `<contract_file>` is a michelson contract (`storage`, `parameter` and `code` fields). `<optional_init_file>` is an optional initializer for the contract. It should contain two fields : `parameter` and `code`. The former is the type of data the initializer takes as input, and the latter is a (sequence of) michelson instruction(s) which, from a stack with a value of type `parameter`, produces a stack with a value of the `storage` type appearing in the `<contract_file>` associated with the initializer.

A `<testcase>` is a (sequence of) michelson instruction(s) which produce(s) a list of `operation`s from an empty stack. Techelson runs all testcases sequentially and reports the errors it runs into.

For example

```bash
> techelson --contract rsc/tests/test0/contracts/test0.liq.tz -- rsc/tests/test0/okay/Test0Test1.techel
```

[opam installed]: https://opam.ocaml.org/doc/Install.html
[dune]: https://github.com/ocaml/dune
[mdbook]: https://github.com/rust-lang-nursery/mdBook
[Liquidity]: http://www.liquidity-lang.org/
[adrienchampion.github.io/blog]: https://adrienchampion.github.io/blog/tezos/techelson/with_liquidity/index.html
[the main repository's github pages]: https://ocamlpro.github.io/techelson/user_doc/