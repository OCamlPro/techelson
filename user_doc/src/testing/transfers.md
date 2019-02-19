# Transfers

At this point creating and applying a transfer should be relatively straightforward. Simply create
the operation using michelson's `TRANSFER_TOKENS`, and apply it with `APPLY_OPERATIONS`. For
instance, [transfer.techel] builds on [inspection.techel]. It creates an instance of
[simpleExample.tz], and creates and applies two operations: the first transfers `7` tokens with a
parameter equal to `False`, and the second transfers `13` tokens with `True`. (Remember that
[simpleExample.tz] will count transfers for which the parameter is `False`.)

```mich,ignore
{{#include ../../rsc/simpleExample/okay/transfer.techel:1:10:}}

    ... # Omitting code creating the contract.

{{#include ../../rsc/simpleExample/okay/transfer.techel:26:45:}}
```

Finally, it checks that the balance and storage are the ones expected:

```mich,ignore
{{#include ../../rsc/simpleExample/okay/transfer.techel:47::}}
```

The test passes and its output is

```
{{#include ../../rsc/simpleExample/okay/transfer.techel.output}}
```

[simpleExample.tz]: ../../rsc/simpleExample/contracts/simpleExample.tz (The SimpleExample contract)
[inspection.techel]: ../../rsc/simpleExample/okay/inspection.techel (The Inspection testcase)
[transfer.techel]: ../../rsc/simpleExample/okay/transfer.techel (The Transfer testcase)