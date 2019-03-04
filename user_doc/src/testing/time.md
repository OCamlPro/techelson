# Timestamp Control

Some contracts need to reason about time, based on the timestamp of the block the computation takes
place in. By default, the timestamp of all blocks in Techelson is `1970-01-01T00:00:00Z`. Testcases
can set this timestamp to anything, the only constraint is that the new timestamp is older than the
previous one. The relevant instruction is

| instruction | parameter | stack |
|:---|:---:|:---:|
| `SET_TIMESTAMP` | none | `:: timestamp : 'S` |
|                 |      | `-> 'S`             |

As an example, consider the following contract [Timestamp] which takes `(or unit (contract unit))`:
it

- receives money once `(Left Unit)`, and
- unlocks it if one week has passed since it receives money.

To unlock the money, someone must call the contract and give it a unit contract to collect the
money `(Right <contract>)`. Unlocking the money is only legal after one week (`604800` seconds) has
passed since the money was received.

The code follows. Its storage is `(option timestamp)` which stores the last time it receives money.
The contract fails if asked to

- receive money but it's already storing money (its storage is not `None`),
- unlock the money but it hasn't received anything (its storage is `None`), and
- unlock the money but one week hasn't passed since it was received.

```mic, ignore
{{#include ../../rsc/timestamp/contracts/timestamp.tz}}
```

Let's go through [TestTimestamp], the testcase for Timestamp. The first step should be unsurprising
by now: deploy the contract and an account (so that we can unlock the money later).

```mic, ignore
{{#include ../../rsc/timestamp/okay/testTimestamp.techel:1:43:}}
```

Next, let's set the timestamp to January 1 2019, 11am, and send some money to the contract.

```mic, ignore
{{#include ../../rsc/timestamp/okay/testTimestamp.techel:45:59:}}
```

We now check the storage is what it should be:

```mic, ignore
{{#include ../../rsc/timestamp/okay/testTimestamp.techel:61:79:}}
```

Let's make sure unlocking the money before one week has passed fails. First, the testcase sets the
timestamp to January 8 2019, **9am**, which is not one week later than the date at which we sent
money to the contract. So this should fail.

```mic, ignore
{{#include ../../rsc/timestamp/okay/testTimestamp.techel:81:97:}}
```

Last, let's set the date to January 8 2019, 11am, at which point unlocking the money should work.

```mic, ignore
{{#include ../../rsc/timestamp/okay/testTimestamp.techel:99:124:}}
```

All set. Running techelson yields the [following output]. It is split in two parts here, first up
to the request to unlock the money on January 8 2019 at 9am, which should fail:

```
{{#include ../../rsc/timestamp/okay/testTimestamp.techel.output:1:49:}}
```

So far so good. Finally, the rest of the output should go smoothly and succeed:

```
{{#include ../../rsc/timestamp/okay/testTimestamp.techel.output:51:77:}}
```

[Timestamp]: ../../rsc/timestamp/contracts/timestamp.tz (Timestamp contract)
[TestTimestamp]: ../../rsc/timestamp/okay/testTimestamp.techel (TestTimestamp testcase)
[following output]: ../../rsc/timestamp/okay/testTimestamp.techel.output (Techelson's output)