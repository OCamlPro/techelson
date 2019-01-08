{
    NIL operation ;
    PUSH mutez 42 ;
    PUSH bool True ;
    PUSH bool True ;
    NONE key_hash ;
    PUSH key "some_key" ;
    HASH_KEY ;
    CREATE_CONTRACT @test_1_contract "Test1" ;
    SWAP ;
    DIP CONS ;
    PUSH @v_1 string "blah" ;
    NOW @now ;
    PUSH @v_2 mutez 0 ;
    PUSH @v_3 mutez 0 ;
    PUSH (contract unit) { storage unit ; parameter unit ; code DROP ; } ;
    PUSH (contract unit) { storage unit ; parameter unit ; code DROP ; } ;
    PUSH (contract unit) { storage unit ; parameter unit ; code DROP ; } ;
    NOW ;
    PUSH @v_4 mutez 0 ;
    PUSH @v_5 mutez 0 ;
    NOW ;
    PAPAPPAIPAPAPAPPAIPAIR @param
}