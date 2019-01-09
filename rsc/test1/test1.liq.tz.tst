{
    # Final list of operations.
    NIL operation ;

    # Test spawning a unit contract explicitely.
    UNIT @unit_storage ;
    PUSH mutez 17 ;
    PUSH bool True ;
    PUSH bool False ;
    NONE key_hash ;
    PUSH key "unit_key" ;
    HASH_KEY ;
    CREATE_CONTRACT @test_unit_contract {
        storage unit;
        parameter unit;
        code { };
    } ;
    SWAP ;
    DROP ;
    CONS ;

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
    CREATE_CONTRACT @test_1_contract "Test1" ;
    SWAP ;
    DIP CONS ;
    DROP
}