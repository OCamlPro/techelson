# Running Tests

Interaction with techelson is currently file-based. There are two kinds of files techelson works
with. *Contract files* are just plain michelson smart contract files, and *testcase files* are
files containing a Michelson instruction, usually a sequence of instructions `{ <instructions> }`.

Omitting options, running techelson looks like (the `--` separator is optional):

```bash
$ techelson --contract <contract_1> .. --contract <contract_n> -- <testcase_1> ... <testcase_m>
```

Techelson will then run the testcases in sequence. All testcases will have access to all the
contracts provided with `--contract`.

This section builds on the small example from the [Michelson Section] to introduce techelson's
workflow and its extended instruction set.

- [First Steps] shows how to run a simple testcase with no contract; it introduces the
    `PRINT_STACK` and `STEP` extensions
- [Creating and Calling Contracts] discusses the contract environment and contract creation and
    transfers; it introduces the `APPLY_OPERATIONS` extension
- [Live Contract Inspection] deals with recovering the balance and the storage of live (deployed)
    contracts; it introduces the `BALANCE_OF` and `STORAGE_OF` extensions

[Michelson Section]: ../michelson/simple_example.md (A simple example in michelson)
[First Steps]: basic.md (First steps in techelson)
[Creating and Calling Contracts]: contracts.md (Creating and calling contracts in techelson)
[Live Contract Inspection]: inspection.md (Live contract inspection)