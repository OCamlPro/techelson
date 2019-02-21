# Creating and Calling Contracts

When you pass a contract to techelson using `techelson --contract <file> ...`, the contract becomes
a **named** contract in the techelson environment. The name of the contract is the name of the file
- up to its first `.` character,
- with the first letter capitalized.

So

```
techelson \
    --contract my_contract.tz \
    --contract myContract.tz  \
    --contract my.contract.tz \
    ...
```

defines three named contracts: `My_contract`, `MyContract` and `My`.

> Note that the naming convention is the same for testcases, based on the testcase file. The name
> of a testcase might be used in techelson's output to provide information, but it has no practical
> use currently.

## Named Contract Creation

Techelson extends the `CREATE_CONTRACT` michelson instruction with the following rule

| instruction | parameter | stack |
|:---|:---:|:---:|
| `CREATE_CONTRACT` | `<string>` | `:: key_hash : option key_hash : bool : bool : mutez : 'g : 'S` |
|              |            | `-> operation : address : 'S` |

where `<string>` is the name of a contract with storage type `'g` in the environment. The semantics
of the stack parameters is the same as in michelson: manager, optional delegate, the two spendable
and delegatable flags, and the balance and storage of the contract created.

> **NB**: techelson also provides the `SPAWN_CONTRACT` extension which takes the name of the
> contract on the stack. See techelson's [Extensions] for more details.

Say we have the following contract in file [simpleExample.tz].

```mic,ignore
{{#include ../../rsc/simpleExample/contracts/simpleExample.tz}}
```

We can craft a creation operation in file [create1.techel] as follows

```mic,ignore
{{#include ../../rsc/simpleExample/okay/create1.techel}}
```

This produces the following output

```
{{#include ../../rsc/simpleExample/okay/create1.techel.output}}
```

## Applying Operations

Michelson operations (contract/account creation, transfers) cannot be applied directly in a
michelson contract. Instead, a contract produces a list of operation which the tezos runtime
applies after the contract is done running.

A techelson test case needs to be able to apply operations however, which is why the
`APPLY_OPERATIONS` extension exists. This instruction suspends the execution of the testcase to
apply the list of operations on the top of the stack. When all these operations are done running,
techelson resumes the execution of the testcase.

> **Warning**: this instruction is only legal in testcases, not in contracts.

Consider testcase [create2.techel]:

```mic,ignore
{{#include ../../rsc/simpleExample/okay/create2.techel}}
```

Running it yields

```
{{#include ../../rsc/simpleExample/okay/create2.techel.output}}
```

Notice the line `Applying operations...`. We could increase techelson's verbosity to obtain more
information as to which contracts are deployed, but let's see how to inspect the state of a *live*
(deployed) contract instead.

[simpleExample.tz]: ../../rsc/simpleExample/contracts/simpleExample.tz (The SimpleExample contract)
[create1.techel]: ../../rsc/simpleExample/okay/create1.techel (The Create1 testcase)
[create2.techel]: ../../rsc/simpleExample/okay/create2.techel (The Create2 testcase)
[Extensions]: ../quick_ref/extensions.md (Extension section)