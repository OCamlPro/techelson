# Usurpation of Identity

The previous section used the liquidity contract [admins.liq] and its techelson version
[admins.tz]. It showcased how to handle expected transfer failures and turn them into test
objectives. The failure used to demonstrate the example was that we tried to add a new
administrator by calling the contract from the testcase, which failed because only administrators
can add other administrators, and the (address of the) testcase was not registered as such.

```mich,ignore
{{#include ../../rsc/admins/okay/preciseMustFail.techel:66:84:}}
```

One solution would be to register the testcase directly, but it would be more natural and more
generic to be able to apply a transfer as someone else. Hence the `SET_SOURCE` extension:

| instruction | parameter | stack |
|:---|:---:|:---:|
| `SET_SOURCE` | `code` | `:: address : 'A` |
|              |        | `-> 'B` |
| | | iff `code :: [ A -> B ]` |

> **Warning**: the `SET_SOURCE` extension is only legal in testcases.

This extension allows to pretend the testcase is a live contract or account from the environment.
More precisely, all operations created in the `code` under the `SET_SOURCE` will have their source
and sender be the address from the stack. Testcase [setSource.techel] uses this instruction to
pretend that `root` is the one adding a new administrator.

```mich,ignore
{{#include ../../rsc/admins/okay/setSource.techel:68:91:}}
```

The testcase now succeeds, and its output is

```
{{#include ../../rsc/admins/okay/setSource.techel.output}}
```

Notice how, in the last `PRINT_STACK`, the sender of the transfer is now `root`:

```
TRANSFER[uid:3] address[1]@root -> address[3]@admins ...
```


[admins.liq]: ../../rsc/admins/admins.liq (The Admin liquidity contract)
[admins.tz]: ../../rsc/admins/contracts/admins.tz (The Admin michelson contract)
[setSource.techel]: ../../rsc/admins/okay/setSource.techel (The SetSource testcase)
