# First Steps

Let's give ourselves a testcase file [test1.techel]

```mic,ignore
{{#include ../../rsc/no_contract/okay/test1.techel}}
```

This testcase does not use any contract. To run it, simply run

```bash
{{#include ../../rsc/no_contract/okay/test1.techel.output}}
```

## Introspection

This is not very informative, which is why techelson provides [extensions] such as `PRINT_STACK`.
This instruction prints the state of the stack in a readable way. For example, if we change the
example above to [test2.techel] to

```mic,ignore
{{#include ../../rsc/no_contract/okay/test2.techel}}
```

the output becomes

```
{{#include ../../rsc/no_contract/okay/test2.techel.output}}
```

## Steps

When you run a complex testcase or contract, it can be useful to have break point that stop the
execution. This gives you time to read a `PRINT_STACK` before the next step is actually performed,
make one step, read the state of the stack, *etc.*

The `STEP` techelson extension allows to do just that. You can also provide a string that will be displayed when the `STEP` instruction is reached.

The following example ([test3.techel]) showcases the `STEP` instruction:

```mic,ignore
{{#include ../../rsc/no_contract/okay/test3.techel}}
```

Techelson will stop on all `STEP` instructions and ask you to press enter to keep going:

```
{{#include ../../rsc/no_contract/okay/test3.techel.output}}
```

> **Pro tip**: passing `--skip on` to `techelson` will skip (but still display) all the steps. The
> output of the commands reported in this book are all obtained by running techelson with `--skip
> on`.

[test1.techel]: ../../rsc/no_contract/okay/test1.techel (The Test1 testcase)
[test2.techel]: ../../rsc/no_contract/okay/test2.techel (The Test2 testcase)
[test3.techel]: ../../rsc/no_contract/okay/test3.techel (The Test3 testcase)
[extensions]: extensions.md (Extension Section)
