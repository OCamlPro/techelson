{
    # Final list of operations.
    NIL operation ;
    DUP ;

    PRINT_STACK ;

    # Storage parameter for `Test1` contract deployment.
    NOW ;
    PUSH @v_5 mutez 0 ;
    PAIR ;
    PUSH @v_4 mutez 0 ;
    NOW ;
    PAIR ;
    PAIR ;
    PUSH :UnitContract (contract unit) { storage unit ; parameter unit ; code DROP ; } ;
    PAIR ;
    PUSH :UnitContract @unit_contract (contract unit) { storage unit ; parameter unit ; code DROP ; } ;
    PAIR ;
    PUSH :UnitContract @unit (contract unit) { storage unit ; parameter unit ; code DROP ; } ;
    PAIR ;
    PUSH @v_3 mutez 0 ;
    PUSH @v_2 mutez 0 ;
    PAIR ;
    PAIR ;
    NOW @now ;
    PAIR ;
    PUSH @v_1 string "blah" ;
    PAIR ;
    # PAPAPPAIPAPAPAPPAIPAIR @param ;

    # Additional parameters for contract deployment operation.
    PUSH mutez 42 ;
    PUSH bool True ;
    PUSH bool True ;
    NONE key_hash ;
    PUSH key "some_key" ;
    HASH_KEY ;

    # Spawn contract.
    CREATE_CONTRACT @test1_contract "Test1" ;
    DIP SWAP ;
    CONS ;

    # Duplicate Test1's address.
    DUUP ;
    DUP ;

    # This yields none.
    CONTRACT (pair string
        (pair timestamp
              (pair (pair mutez mutez)
                    (pair (contract :UnitContract unit)
                          (pair (contract :UnitContract unit)
                                (pair (contract :UnitContract unit)
                                      (pair (pair timestamp mutez) (pair mutez timestamp)))))))) ;
    # Checking we got none.
    IF_NONE {} { FAIL } ;

    # Spawning a contract that will run `Test1`.
    PUSH mutez 0 ;
    PUSH bool True ;
    PUSH bool False ;
    NONE key_hash ;
    PUSH key "unit_key" ;
    HASH_KEY ;
    CREATE_CONTRACT @test1_runner {
        storage address;
        parameter unit;
        code {
            DROP ;
            DROP ;
            NIL operation
        } ;
    } ;
    PRINT_STACK ;
    DIP SWAP ;
    CONS ;

    # Apply operations on top of the stack.
    APPLY_OPERATIONS ;

    # Swap so that Test1's address is on top.
    SWAP ;

    # Create call.
    CONTRACT timestamp ;
    IF_SOME {
        PRINT_STACK ;
        PUSH mutez 13 ;
        NOW ;
        TRANSFER_TOKENS ;
        DIP SWAP ;
        PRINT_STACK ;
        CONS
    } {
        PUSH string "failed to resolve Test1" ;
        FAILWITH
    } ;

    # Call.
    APPLY_OPERATIONS
}