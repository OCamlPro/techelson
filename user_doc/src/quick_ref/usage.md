# Command-Line Options

> **Warning**: so-called *contract initializers* are mentioned in the `--help` but are not
> currently supported.

The full list of options (for the nominal mode) is obtained with `--help`:

```
{{#include ../../rsc/no_contract/okay/options.techel.output}}
```

## Modes

Techelson's modes are triggered by simply passing the name of the mode as an argument. For
instance,

```
{{#include ../../rsc/simpleExample/okay/testgen.techel.output}}
```

You can have techelson print the options for mode `<mode>` with `techelson <mode> --help`.

### Test Generation

Given a contract, this mode is in charge of generating a testcase automatically. The relevant
chapter of this book is [Test Generation].

```
{{#include ../../rsc/no_contract/okay/testgenOptions.techel.output}}
```

[Test Generation]: ../testgen/index.html (Test Generation chapter)