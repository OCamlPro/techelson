# Anonymous Contracts

Techelson accepts contracts through its `--contract` option. These contracts are named as discussed
in [Creating and Calling Contracts]. Contracts defined at michelson level in testcases and
contracts however are considered *anonymous*. Anonymous contracts can also be deployed and
inspected. In fact, they are not really different from named contracts apart from their lack of
name, which (currently) prevent techelson from mentioning where they really come from in its debug
output.

The following [`create4.techel`] testcase is similar to the one from the [Live Contract Inspection]
except that the contract deployed is not given to the environment, it is inlined in the testcase.

```mic,ignore
{{#include ../../rsc/simpleExample/okay/create4.techel}}
```

This produces the exact same output (unless we increase verbosity) as for [`create3.techel`]:

```
$ techelson create4.techel
{{#include ../../rsc/simpleExample/okay/create4.techel.output}}
```

[`create3.techel`]: ../../rsc/simpleExample/okay/create3.techel (A simple contract creation testcase)
[`create4.techel`]: ../../rsc/simpleExample/okay/create4.techel (A simple contract creation testcase)
[Creating and Calling Contracts]: contracts.md (Creating and calling contracts in techelson)
[Live Contract Inspection]: inspection.md (Live contract inspection in techelson)