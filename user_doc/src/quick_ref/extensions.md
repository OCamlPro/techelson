# Extensions

> **Warning**: the following instructions can only be used in contracts, **not testcases**:
> - `SENDER`
> - `SOURCE`

Techelson testcases have access to an extended instruction set to ease the process of writing
tests. See `rsc/tests/` for more examples. Note that techelson treats `#>` as whitespace, so you
can use extensions in contracts (when [legal](#unrestricted-extensions)) while keeping them pure
michelson, like in the example below

```
DIP {
    ...
    CONS ;
    #> PRINT_STACK ;
    #> STEP "after list cons" ;
    PUSH int 3 ;
    ...
}
```

## Unrestricted Extensions

The following instructions are legal in **testcases and contracts**:

- `STEP` and `STEP <string>`:

    `'S` `->` `'S`

    Relevant section of this book: [First Steps].

    suspends the evaluator and prints a string, if any.

- `PRINT_STACK`:

    `'S` `->` `'S`

    Relevant section of this book: [First Steps].

    prints the current state of the stack

## Testcase-only Extensions

The following instructions are **only** legal in testcases:

- `APPLY_OPERATIONS`:

    `(list operation) : 'S` `->` `'S`

    Relevant section of this book: [Creating and Calling Contracts].

    - consumes a list of operations
    - suspends the execution of the testcase
    - applies all the operations in the list (these operations can create operations which will be
        applied too)
    - resumes the execution of the testcase

- `GET_STORAGE 'storage`:

    `contract 'a : 'S` `->` `(option 'storage) : 'S`

    Relevant section of this book: [Live Contract Inspection].

    - consumes a contract
    - pushes `Some` of the current value of the storage of the contract if its storage has type
        `'storage`, `None` otherwise

- `GET_BALANCE`:

    `contract 'a : 'S` `->` `mutez : 'S`

    Relevant section of this book: [Live Contract Inspection].

    Same as `GET_STORAGE`, but pushes the balance of the contract instead of its storage

- `MUST_FAIL 'a`:

    `(option 'a) : operation : 'S` `->` `'S`

    Relevant section of this book: [Testing for Failures].

    Specifies that an operation (or the operation it creates recursively) must fail, and optionaly
    that it `FAIL`ed`WITH` a certain value. More precisely, the whole testcase will **fail** if the
    `operation` or the operations it creates recursively, *when applied*, either

    - succeeds, or
    - the `(option 'a)` parameter is `(Some value)` and the operation's failure was not caused by a
        `FAILWITH` on precisely `value`
    
    > Note that if the optional value is `NONE`, then `MUST_FAIL` accepts any kind of *protocol*
    > failure, not just `FAILWITH`. For instance, it will also accept creation/transfer operations
    > that fail because of insufficient balance, because this precise operation already ran (it was
    > `DUP`-ed), *etc*.

- `SET_SOURCE code`, with `code :: [ 'A -> 'B ]`

    `address :: 'A` `->` `'B`

    Relevant section of this book: [Usurpation of Identity].

    Sets the source of the testcase. Without this extension, the source of all transfers can only
    be the testcase. This allows to run `code` while pretending the testcase is a different live
    contract.

[First Steps]: ../testing/basic.md (First Steps section)
[Creating and Calling Contracts]: ../testing/contracts.md (Creating and Calling Contracts section)
[Live Contract Inspection]: ../testing/inspection.md (Live Contract Inspection section)
[Testing for Failures]: ../testing/failures.md (Testing for Failures section)
[Usurpation of Identity]: ../testing/set_source.md (Usurpation of Identity section)