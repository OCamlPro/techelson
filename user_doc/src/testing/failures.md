# Testing for Failures

This section is going to use a slightly more complex contract in order to showcase failures and how
to test them. Even if you are not familiar with [liquidity], the contract's code will most likely
be more readable in liquidity than in michelson. Here is the liquidity version, [admins.liq]:

```ocaml,ignore
{{#include ../../rsc/admins/admins.liq}}
```

Note that the `clients` field of the storage is unused in this example. The `admins` map maps
administrator names to addresses. The only entry point (in this example) is `add_admin` which
allows administrators to add new administrators by registering their name and their address. More
precisely, calling this contract is only legal if the `SENDER` (`Current.sender ()`) of the call is
an administrator (*c.f.* `admin_check`). If the call to the contract is not legal, the transfer
fails:

```ocaml,ignore
{{#include ../../rsc/admins/admins.liq:9:14:}}
```

The parameters of the entry point are
- `admin_name`: name associated with the `SENDER` administrator,
- `nu_name`: name of the new administrator to add,
- `nu_address`: the address of the new administrator.

```ocaml,ignore
{{#include ../../rsc/admins/admins.liq:16:17:}}
```

Using liquidity to compile the contract to michelson (for instance using [liquidity's online
editor]), we obtain [admins.tz]. Here are the `storage` and `parameter` types:

```mic,ignore
{{#include ../../rsc/admins/contracts/admins.tz:1:5:}}
```

We omit the contract's code ([admins.tz]) as *i)* it is not very readable and *ii)*
we do not need to know what the code precisely is to create the contract and call it, as long as we
know the `storage` and `parameter` types.

## Creation

Creating a contract has been covered in previous sections, so let's give ourselves some code to
create the contract with one administrator called `root`. In fact, let's make an account for `root`
and register it as an administrator. The new administrator `new_admin` is also deployed as an
account. Testcase [create.techel] does exactly that:

```mic,ignore
{{#include ../../rsc/admins/okay/create.techel}}
```

Running this test produces the following output

```
{{#include ../../rsc/admins/okay/create.techel.output}}
```

## Transfer Failures

Let's now add `new_admin` as a new administrator. Testcase [addAdmin.techel] only adds the
following few instructions at the end of [create.techel]:

```mic, ignore
{{#include ../../rsc/admins/error/addAdmin.techel:60:85:}}
```

What should the result of applying this transfer be? Remember than before adding an administrator, the contract checks that the sender is an admin.

```ocaml,ignore
{{#include ../../rsc/admins/admins.liq:9:17:}}
...
{{#include ../../rsc/admins/admins.liq:20:21:}}
...
```

So, if everything goes well, the transfer should fail: the sender here is not `root`, but the
testcase. In techelson, the testcase currently running has its own address. It is in particular
not the address of `root`. Hence, the transfer fails as it should and so does the whole testcase.
The (relevant part of the) output is

```
{{#include ../../rsc/admins/error/addAdmin.techel.output:104:108}}
```

You can see in the transfer the sender and the target of the transfer:

```
TRANSFER[uid:3] address[0]@AddAdmin -> address[3]@admins
```

`AddAdmin` is the name of our testcase, and `address[0]@AddAdmin` is its address. Name `"root"`
does not map to this address in the contract and the transfer fails.

## Handling Failures

Before getting into making this transfer work (next section), note that this (failing) testcase is
actually useful. Or at least it should be: the transfer we are trying to make is illegal indeed. We
do want the transfer to fail, but the testcase should
- succeed if the transfer does fail,
- fail if the transfer succeeds: anyone can add admins, which is **bad**.

This is what the `MUST_FAIL` techelson extension does. It takes an operation wraps it in a
construct telling techelson that this operation must fail: either the operation itself or, if it is
a transfer, the operations created by this transfer. Here is its signature:

| instruction | parameter | stack |
|:---|:---:|:---:|
| `MUST_FAIL` | `<type>` | `:: option <type> : operation : 'S` |
|             |          | `-> operation : 'S`                 |

Let's ignore the `<type>` parameter and the first stack argument for now and just use this
instruction right away. Testcase [mustFail.techel] is the same as [addAdmin.techel] except for a
few lines after the transfer:

```mich,ignore
{{#include ../../rsc/admins/okay/mustFail.techel:77:80:}}
```

The test now passes successfully:

```
{{#include ../../rsc/admins/okay/mustFail.techel.output}}
```

## (More) Precise Failure Testing

Now, `MUST_FAIL` (as it is used here) succeeds if the transfer ends in a *tezos protocol* failure.
This include explicit failures in the code, illegal transfers due to insufficient funds, duplicate
operations, *etc.* It does **not** include type-checking errors and internal techelson errors.

This means in particular that if the transfer above fails for a reason different from `"illegal
access to admin account"` then `MUST_FAIL` will consider the test a success. To make sure the cause
for failure is actually the one we want, we must use `MUST_FAIL`'s optional stack parameter. A
failure in michelson code always has a value of some type associated to it. In this case, the type
of this value is `string` and its value is `"illegal access to admin account"`. Testcase
[preciseMustFail.techel] only changes [mustFail.techel] to pass the failure value expected to `MUST_FAIL`:

```mich
{{#include ../../rsc/admins/okay/preciseMustFail.techel:77:80:}}
```

As a consequence, if the transfer fails with anything else than an explicit failure with a value of
type `string` equal to `"illegal access to admin account"`, then the whole testcase will fail.
Everything works fine here, and the output is

```
{{#include ../../rsc/admins/okay/preciseMustFail.techel.output}}
```

Notice that the `MUST_FAIL` operation now mentions the value expected:

```
MUST_FAIL[uid:4] "illegal access to admin account" : string (TRANSFER[uid:3] ...)
```

as opposed to the `_` wildcard from testcase [mustFail.techel], which means no value was given:

```
MUST_FAIL[uid:4] _  (TRANSFER[uid:3] ...)
```

[Liquidity]: http://www.liquidity-lang.org/ (Liquidity's official page)
[liquidity's online editor]: http://www.liquidity-lang.org/zeronet/ (Liquidity's online editor)
[admins.liq]: ../../rsc/admins/admins.liq (The Admin liquidity contract)
[admins.tz]: ../../rsc/admins/contracts/admins.tz (The Admin michelson contract)
[create.techel]: ../../rsc/admins/okay/create.techel (The Create testcase)
[addAdmin.techel]: ../../rsc/admins/error/addAdmin.techel (The AddAdmin testcase)
[mustFail.techel]: ../../rsc/admins/okay/mustFail.techel (The MustFail testcase)
[preciseMustFail.techel]: ../../rsc/admins/okay/preciseMustFail.techel (The PreciseMustFail testcase)