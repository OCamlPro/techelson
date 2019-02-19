# Michelson

[Michelson] is the stack-based, strongly typed, low-level language supported by the [tezos
blockchain] for smart contracts. We only provide a brief description of michelson here, and refer
the reader to the official documentation for more details.

A michelson contract is similar to transition system. The **storage** of a contract is its current
state; the entry point (**code**) of a contract is a function which takes
- some tezos tokens (of type `mutez`),
- the current storage of the contract, and
- a **parameter** of a certain type provided by the client of the contract.

It returns
- a list of *operations* (contract/account creation, transfers, *etc.*), and
- the new storage of the contract.

In practice, a contract looks as follows:

```michelson,ignore
storage <type> ;
parameter <type> ;
code <instruction> ;
```

Note that tokens are passed implicitely: they are credited to the contract before it starts running
(although the amount of the transfer can be accessed with the `AMOUNT` instruction). Hence the code
of the contract takes two parameters, which are aggregated in a pair `(parameter, storage)`. The
same goes with the operations and the new storage returned by the contract, which are returned as a
pair `(operations, new_storage)`.

In the stack-based context of michelson, "takes `<something>` as argument" means "starts with a
stack containing `<something>`". Likewise, "returns `<something>`" here means "leaves `<something>`
on the stack at the end (and **nothing else than** `<something>`)".

[Michelson]: https://tezos.gitlab.io/master/whitedoc/michelson.html (Michelson documentation page)
[tezos blockchain]: (https://tezos.com/) (Tezos official page)

