# Live Contract Inspection

Michelson smart contracts cannot access each other's storage. They can only interact through
transfers, during which the client of the contract provides a parameter that the contract runs its
code on.

As a test framework, techelson provides inspection instructions which give access to the balance
and the storage of a *live* (deployed) contract. Both consume a `contract` from the top of the
stack.

| instruction | parameter | stack |
|:---|:---:|:---:|
| `GET_STORAGE` | `'storage` | `::       contract 'a : 'S` |
|              |            | `-> (option 'storage) : 'S` |
| `GET_BALANCE` | none       | `:: contract 'a : 'S` |
|              |            | `->       mutez : 'S` |

Let's extend the previous example to [inspection.techel] which checks that the balance and storage
of the contract deployed are correct.

```mic,ignore
{{#include ../../rsc/simpleExample/okay/inspection.techel}}
```

The testcase does not fail and produces the output

```
{{#include ../../rsc/simpleExample/okay/inspection.techel.output}}
```

[inspection.techel]: ../../rsc/simpleExample/okay/inspection.techel (The Inspection testcase)