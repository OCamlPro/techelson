# Example

Let's showcase testgeneration on [simpleExample.tz]:

```mic,ignore
{{#include ../../rsc/simpleExample/contracts/simpleExample.tz}}
```

Test generation is activated by passing to techelson a `testgen` argument triggering the test
generation mode. You can read more about modes in the [Usage section]. First, let's generate a
single testcase (`-n 1`, or `--count 1`) and let techelson run it:

```
{{#include ../../rsc/simpleExample/okay/testgen_nodump.techel.output}}
```

While this can be useful for simple contracts, usually you want to retrieve the testcase directly
so that you can modify it to suit your needs. So let's still generate one testcase but this time we
will dump it in the current directory (trailing `.` in the `techelson` command).

```mic
{{#include ../../rsc/simpleExample/okay/testgen_dump.techel.output}}
```

[simpleExample.tz]: ../../rsc/simpleExample/contracts/simpleExample.tz (The SimpleExample contract)
[Usage section]: ../quick_ref/usage.md (Usage section)