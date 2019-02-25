# Techelson

**Techelson** is a **T**est **E**xecution **E**nvironment (TEE) for [Michelson smart contracts].
Michelson is the stack-based language used by the [tezos blockchain][tezos]. Techelson is open
source and [hosted on github], where you can find the build instructions.

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

Michelson is a fairly low-level language which makes it difficult to discuss complex contracts.
This book will sometimes give contracts as [Liquidity] contracts. Liquidity is a higher-level,
OCaml-like language for tezos smart contracts which compiles to Michelson.

> **NB**: if you are a Liquidity user, you should probably take a look at [this blog post] on how
> to write tests directly in Liquidity, and run these tests using Techelson.

The chapters of this book are
- [Michelson], provides a very brief introduction to michelson smart contracts.
- [Running Tests], describes techelson's workflow for running tests through examples.
- [Test Generation], discusses techelson's test generation features.
- [Quick Reference], quick reminders of techelson's features, such as extensions.

All examples in this book are available in the [`rsc` directory of the github repository].

[Michelson smart contracts]: https://tezos.gitlab.io/master/whitedoc/michelson.html (Michelson documentation page)
[tezos]: https://tezos.com (Tezos official page)
[Liquidity]: http://www.liquidity-lang.org (Liquidity official page)
[Michelson]: michelson/index.md (Michelson chapter)
[Running Tests]: testing/index.md (Running Tests chapter)
[Test Generation]: testgen/index.md (Test Generation chapter)
[Quick Reference]: quick_ref/index.md (Quick Reference chapter)
[hosted on github]: https://github.com/OCamlPro/techelson (Techelson on github.com)
[`rsc` directory of the github repository]: https://github.com/OCamlPro/techelson/tree/master/user_doc/rsc (rsc folder on techelson's github repository)
[this blog post]: https://adrienchampion.github.io/blog/tezos/techelson/with_liquidity/index.html (Using Techelson with Liquidity)