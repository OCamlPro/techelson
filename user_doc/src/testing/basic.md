# First Steps

Let's give ourselves a testcase file [`simple1.techel`]

```mic,ignore
{{#include ../../rsc/simpleExample/okay/simple1.techel}}
```

This testcase does not use any contract. To run it, simply run

```bash,ignore
$ techelson simple1.techel
```

This produces the output

```
{{#include ../../rsc/simpleExample/okay/simple1.techel.output}}
```

## Introspection

This is not very informative, which is why techelson provides [extensions] such as `PRINT_STACK`.
This instruction prints the state of the stack in a readable way. For example, if we change the
example above to [`simple2.techel`] to

```mic,ignore
{{#include ../../rsc/simpleExample/okay/simple2.techel}}
```

the output becomes

```
$ techelson simple2.techel
{{#include ../../rsc/simpleExample/okay/simple2.techel.output}}
```

## Steps

When you run a complex testcase or contract, it can be useful to have break point that stop the
execution. This gives you time to read a `PRINT_STACK` before the next step is actually performed,
make one step, read the state of the stack, *etc.*

The `STEP` techelson extension allows to do just that. You can also provide a string that will be displayed when the `STEP` instruction is reached.

The following example ([`simple3.techel`]) showcases the `STEP` instruction:

```mic,ignore
{{#include ../../rsc/simpleExample/okay/simple3.techel}}
```

Techelson will stop on all `STEP` instructions and ask you to press enter to keep going:

```
$ techelson simple3.techel
{{#include ../../rsc/simpleExample/okay/simple3.techel.output}}
```

[`simple1.techel`]: ../../rsc/simpleExample/okay/simple1.techel (The Simple1 testcase)
[`simple2.techel`]: ../../rsc/simpleExample/okay/simple2.techel (The Simple2 testcase)
[`simple3.techel`]: ../../rsc/simpleExample/okay/simple3.techel (The Simple3 testcase)
[extensions]: extensions.md (Extension Section)
