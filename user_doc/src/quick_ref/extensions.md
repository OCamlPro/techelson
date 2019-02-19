# Extensions

## Testcase-only instructions

The following instructions can only be used in testcases, not in actual contracts.

- `SET_SOURCE`
- `APPLY_OPERATIONS`
- `MUST_FAIL`

# Contract-transfer-only instructions

- `SENDER`, this instruction does not make sense in a testcase as a testcase lives outside of the tezos protocol

# Extensions

Techelson testcases have access to extended Michelson instruction to ease the process of writing tests. See `rsc/tests/` for more examples. Note that techelson treats `#>` as whitespace, so you can use extensions in contracts (when legal) while keeping them pure tezos, like in the example below

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

The extensions are

- `STEP` and `STEP <string>`:

    `S` `->` `S`

    suspends the evaluator and prints a string, if any.

- `PRINT_STACK`:

    `S` `->` `S`

    prints the current state of the stack

- `APPLY_OPERATIONS`:

    `(list operation) : S` `->` `S`

    - consumes a list of operations
    - suspends the execution of the testcase
    - applies all the operations in the list (these operations can create operations which will be applied too)
    - resumes the execution of the testcase

- `STORAGE_OF 'storage`:

    `contract 'a : S` `->` `(option 'storage) : S`

    - consumes a contract
    - pushes `Some` of the current value of the storage of the contract if its storage has type `'storage`, `None` otherwise

- `BALANCE_OF`:

    `contract 'a : S` `->` `mutez : S`

    same as `STORAGE_OF`, but pushes the balance of the contract instead of its storage

- `MUST_FAIL 'a`:

    `(option 'a) : operation : S` `->` `S`

    specifies that an operation must fail, and optionaly that it `FAIL`ed`WITH` a certain value. More precisely, the whole testcase will fail if the `operation`, *when applied*, either

    - succeeds, or
    - the `(option 'a)` parameter is `(Some value)` and the operation's failure was not caused by a `FAILWITH` on precisely `value`
    
    > Note that if the optional value is `NONE`, then `MUST_FAIL` accepts any kind of *protocol* failure, not just `FAILWITH`. For instance, it will also accept creation/transfer operations that fail because of insufficient balance, because this precise operation already ran (it was `DUP`-ed), *etc*.

- `SET_SOURCE code`:

    `address :: A` `->` `B`

    iff `code :: [ A -> B ]`

    sets the source of the testcase. Without this extension, the source of all transfers can only be the testcase. This allows to run `code` while pretending the testcase is a different live contract.