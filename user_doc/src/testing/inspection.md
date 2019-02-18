# Live Contract Inspection

Michelson smart contracts cannot access each other's storage. They can only interact through
transfers, during which the client of the contract provides a parameter that the contract runs its
code on.

As a test framework, techelson provides inspection instructions which give access to the balance
and the storage of a *live* (deployed) contract. Both consume a `contract` from the top of the
stack.

| instruction | parameter | stack |
|:---|:---:|:---|
| `STORAGE_OF` | `'storage` | `::       contract 'a : 'S` |
|              |            | `-> (option 'storage) : 'S` |
| `BALANCE_OF` | none       | `:: contract 'a : 'S` |
|              |            | `->       mutez : 'S` |

Let's extend the previous example to [`create3.techel`] which checks that the balance and storage
of the contract deployed are correct.

```mic,ignore
{{#include ../../rsc/simpleExample/okay/create3.techel}}
```

The testcase does not fail and produces the output

```
$ techelson --contract simpleExample.tz -- create3.techel
{{#include ../../rsc/simpleExample/okay/create3.techel.output}}
```

[`create3.techel`]: ../../rsc/simpleExample/okay/create3.techel (A simple contract creation testcase)