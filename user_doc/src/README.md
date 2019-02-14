# Techelson

**Techelson** is a **T**est **E**xecution **E**nvironment (TEE) for [Michelson] smart contracts.
Michelson is the stack-based language used by the [tezos blockchain].

Techelson emulates just enough of the tezos blockchain protocol to be able to create smart
contracts and make transfers between contracts. Currently, techelson only aims at testing
functional properties of smart contracts. In particular, it does not provide any information about
the *gas* or *burn* of transfers/contracts. This is because computing the *burn*, and especially
the *gas* of a transfer is rather complex and would require techelson to drop some of the
abstractions it makes over the tezos protocol to run tests faster.

Techelson can be used either as a command-line tool or as an [OCaml](http://www.ocaml.org/)
library. This book focuses on the former use case.

Also, this book assumes the reader is fairly familiar with the michelson language. We will discuss
what a contract is and how it behaves, but the reader should know what michelson types and
instructions look like, and their semantics.

[Michelson]: https://tezos.gitlab.io/master/whitedoc/michelson.html (Michelson documentation page)
[tezos blockchain]: (https://tezos.com/) (Tezos official page)
