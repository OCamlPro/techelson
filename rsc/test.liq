parameter signature;
storage (pair :storage (key %key) (pair (bytes %hash) (address %c)));
code { DUP ;
       DIP { CDR @storage_slash_1 } ;
       CAR @sign_slash_2 ;
       DROP ;
       PUSH @x (or :PlusOne.t (unit %PlusOne.A) (unit %PlusOne.B)) (Left Unit) ;
       IF_LEFT { DROP ; UNIT } { DROP ; UNIT ; FAILWITH } ;
       DROP ;
       PAPPAIIR ;
       PAPPAIIR @p %x1 %x2 %x3 %x4 ;
       PPAIIR ;
       PPAIIR @p %x1 %x2 %x3 ;
       CAR %t }
