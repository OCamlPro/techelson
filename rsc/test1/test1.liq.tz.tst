{
    PUSH @v_1 string "blah" ;
    NOW @now ;
    PUSH @v_2 mutez 0 ;
    PUSH @v_3 mutez 0 ;
    CREATE_CONTRACT { storage unit ; parameter unit ; code DROP ; } ;
    CREATE_CONTRACT @unit { storage unit ; parameter unit ; code DROP ; } ;
    CREATE_CONTRACT { storage unit ; parameter unit ; code DROP ; } ;
    NOW ;
    PUSH @v_4 mutez 0 ;
    PUSH @v_5 mutez 0 ;
    NOW ;
    PAPAPPAIPAPAPAPPAIPAIR
}