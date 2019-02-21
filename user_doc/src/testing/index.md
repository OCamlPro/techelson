# Running Tests

Interaction with techelson is currently file-based. There are two kinds of files techelson works
with. *Contract files* are just plain michelson smart contract files, and *testcase files* are
files containing a Michelson instruction, usually a sequence of instructions `{ <instructions> }`.

Omitting options, running techelson looks like (the `--` separator is optional):

```bash
$ techelson \
    --contract <contract_1> .. --contract <contract_n> \
    -- <testcase_1> ... <testcase_m>
```

Techelson will then run the testcases in sequence. All testcases will have access to all the
contracts provided with `--contract`. For more information about command-line refer to the [Usage
section].

This section builds on the small example from the [Michelson section], and a slightly more involved
example later on, to introduce techelson's workflow and its extended instruction set.

- [First Steps] shows how to run a simple testcase with no contract.

    Introduces `PRINT_STACK` and `STEP`.
- [Creating and Calling Contracts] discusses the contract environment and contract creation and
    transfers.

    Introduces `APPLY_OPERATIONS`.
- [Live Contract Inspection] deals with recovering the balance and the storage of live (deployed)
    contracts.

    Introduces `GET_BALANCE` and `GET_STORAGE`.
- [Anonymous Contracts] details how to create anonymous contracts.
- [Transfers] introduces the creation of transfers to a contract.
- [Testing for Failures] shows how to test that an operation fails and how. This section is the
    first to use the slightly more complex example [admins.tz], and it's liquidity version
    [admins.liq].

    Introduces `MUST_FAIL`.
- [Usurpation of Identity] illustrates how to have your testcases pretend they are a specific
    contract/account, and to create operations in their name.

    Introduces `SET_SOURCE`.

[Michelson section]: ../michelson/simple_example.md (A simple example in michelson)
[Usage section]: ../quick_ref/usage.md (Usage section)
[First Steps]: basic.md (First steps section)
[Creating and Calling Contracts]: contracts.md (Creating and calling contracts section)
[Live Contract Inspection]: inspection.md (Live contract inspection section)
[Anonymous Contracts]: anonymous.md (Anonymous contract section)
[Transfers]: transfers.md (Transfers section)
[Testing for Failures]: failures.md (Testing for failures section)
[Usurpation of Identity]: set_source.md (Usurpation of identity section)
[admins.tz]: ../../rsc/admins/contracts/admins.tz (The Admin michelson contract)
[admins.liq]: ../../rsc/admins/admins.liq (The Admin liquidity contract)