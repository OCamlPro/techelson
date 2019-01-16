{
    # Final list of operations.
    NIL @ops operation ;
    DUP @ops_too ;

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

    # Additional parameters for contract deployment operation.
    PUSH mutez 42 ;
    PUSH bool True ;
    PUSH bool True ;
    NONE key_hash ;
    PUSH key "some_key" ;
    HASH_KEY ;

    # Spawn contract.
    CREATE_CONTRACT @test1_contract "Test1" ;
    PRINT_STACK ;
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
    PRINT_STACK ;

    # Apply operations on top of the stack.
    APPLY_OPERATIONS ;

    # Swap so that Test1's address is on top.
    SWAP ;
    DUP ; # Remember Test1's address for later.

    DIP {

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

    } ;

    # Make sure the balance of Test1 is correct.
    DUP ;
    BALANCE_OF ;
    PRINT_STACK ;
    PUSH mutez 55 ;
    SUB ;
    IFNEQ {
        PUSH string "Test1 does not have a balance of 55tz" ;
        FAILWITH
    } {} ;
    STORAGE_OF ;
    PRINT_STACK ;
    CAR ;
    PUSH string "blah" ;
    PRINT_STACK ;
    COMPARE ;
    PRINT_STACK ;
    IFNEQ {
        PUSH string "Test1 does not have the first element of its storage equal to \"blah\"" ;
        FAILWITH
    } {}
}