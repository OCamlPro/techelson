# A Simple Example

Let us build a contract which counts how many time it was called. We will allow clients to specify
that they do not want to be counted by passing a boolean `ghost`: if it is true, the contract will
not count the transfer.

Based on this description, we already have the *storage* and *parameter* types:

```mic,ignore
{{#include ../../rsc/simpleExample/contracts/simpleExample.tz::2}}
```

The code of this contract should perform the following steps:
- destroy the parameter/storage pair
- branch on the *ghost* parameter: this consumes the parameter, meaning the storage is now on the
    top of the stack
    - do nothing if *ghost* is true: the storage is unchanged
    - add `1` to the storage otherwise
- pair an empty list of operations with the new storage

The complete description of the contract is thus

```mic,ignore
{{#include ../../rsc/simpleExample/contracts/simpleExample.tz}}
```
