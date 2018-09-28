: ADD-AMOUNT
    \ get sender zerostring pointer from ENV

    S" SENDER=" ENVLOOKUP
    DUP 0= IF
        ." ᚜smart-contract-error «wrong-sender»᚛" CR
        BYE
    THEN

    \ save sender to stack and convert to lenstr
    DUP STRLEN 2DUP ( len sender-pnt len sender-pnt -- )

    \ get sender amount from storage, or 0 if not exists

    ." ᚜gethash «" TELL ." » node::storage 0᚛᚛" CR
    \ read answer
    WORD NUMBER
    0 <> IF
        ." ᚜smart-contract-error «wrong-amount-from-storage»᚛" CR
        BYE
    THEN


    \ here amount saved to stack ( storage-val len sender-pnt -- )


    \ Get amount from env as string
    S" AMOUNT=" ENVLOOKUP
    DUP 0= IF
        ." ᚜smart-contract-error «wrong-amount-from-env»᚛" CR
        BYE
    THEN

    \ Parse amount to integer
    DUP STRLEN NUMBER
    \ If not correct amount then error
    0 <> IF
        ." ᚜smart-contract-error «wrong-amount-2»᚛" CR
        BYE
    THEN

    \ Add amount from env and amount from storage
    + ( result len sender-pnt )

    >R \ save result

    \ Write result to storage
    ." ᚜setf ᚜gethash «" TELL ." » node::storage᚛" R> . ." ᚛" CR

    WORD 2DROP \ Read and drop response
    BYE
;

9 CONSTANT HEIGHT-FIN
6 CONSTANT HEIGHT-END
3 CONSTANT HEIGHT-START
1000 CONSTANT CAP

: UPDATE-STATE
    ." ᚜gethash «state» node::storage «prepared»᚛᚛" CR

    \ read answer
    WORD SWAP
    2DUP

    Z" fin" SUBSTRCMP

    1 = IF
        2DROP 2DROP EXIT
    THEN

    2DROP 2DUP
    Z" fail" SUBSTRCMP

\ ." ᚜vfm-dbg-die «" .S  ." »᚛" CR

    1 = IF
        2DROP 2DROP
        ." ᚜get-height᚛" CR
        WORD NUMBER
        0 <> IF
            ." ᚜smart-contract-error «wrong-height»᚛" CR
            BYE
        THEN
        \ if height > HEIGHT-FIN then state := fin
        HEIGHT-FIN > IF
            \ TODO: CLEAN-STORAGE
            ." ᚜setf ᚜gethash «state» node::storage᚛ «fin»᚛" CR
            WORD 2DROP \ read and drop answer
            EXIT
        THEN
        EXIT
    THEN

    2DROP 2DUP
    Z" success" SUBSTRCMP

    1 = IF
        2DROP 2DROP
        ." ᚜get-height᚛" CR
        WORD NUMBER
        0 <> IF
            ." ᚜smart-contract-error «wrong-height»᚛" CR
            BYE
        THEN
        \ if height > HEIGHT-FIN then state := fin
        HEIGHT-FIN > IF
            \ TODO: CLEAN-STORAGE
            ." ᚜setf ᚜gethash «state» node::storage᚛ «fin»᚛" CR
            WORD 2DROP \ read and drop answer
            EXIT
        THEN
        EXIT
    THEN

    2DROP 2DUP
    Z" fundraising" SUBSTRCMP

    1 = IF
        2DROP 2DROP
        ." ᚜get-height᚛" CR
        WORD NUMBER
        0 <> IF
            ." ᚜smart-contract-error «wrong-height»᚛" CR
            BYE
        THEN
        \ if height > HEIGHT-FIN then state := fin
        HEIGHT-END > IF
            \ TODO: CLEAN-STORAGE
            ." ᚜gethash «result» node::storage 10᚛" CR
            WORD NUMBER
            0 <> IF
                ." ᚜smart-contract-error «wrong-result»᚛" CR
                BYE
            THEN
            CAP > IF
                ." ᚜setf ᚜gethash «state» node::storage᚛ «success»᚛" CR
                WORD 2DROP \ read and drop answer
            ELSE
                ." ᚜setf ᚜gethash «state» node::storage᚛ «fail»᚛" CR
                WORD 2DROP \ read and drop answer
            THEN
        THEN
        EXIT
    THEN

    2DROP 2DROP
    ." ᚜get-height᚛" CR
    WORD NUMBER
    0 <> IF
        ." ᚜smart-contract-error «wrong-height»᚛" CR
        BYE
    THEN
    \ if height > HEIGHT-START then state := fundraising
    HEIGHT-START > IF
       ." ᚜setf ᚜gethash «state» node::storage᚛ «fundraising»᚛" CR
        WORD 2DROP \ read and drop answer
        EXIT
    THEN

    \ ." ᚜vfm-dbg-die «" .S  ." »᚛" CR

;

: INVEST
    UPDATE-STATE

    ." ᚜gethash «state» node::storage «prepared»᚛᚛" CR

    \ read answer
    WORD SWAP
    2DUP

    Z" fundraising" SUBSTRCMP

    \ if not fundraising time then abort
    1 <> IF
        ." ᚜node::abortvfm᚛" CR
    THEN

    2DROP 2DROP
    ADD-AMOUNT
    BYE
;


: RETURN-PAYMENT
    UPDATE-STATE

    ." ᚜gethash «state» node::storage «prepared»᚛᚛" CR

    \ read answer
    WORD SWAP
    2DUP

    Z" fail" SUBSTRCMP

    \ if not fundraising time then abort
    1 <> IF
        ." ᚜node::abortvfm᚛" CR
    THEN

    2DROP 2DROP

    S" SENDER=" ENVLOOKUP
    DUP 0= IF
        ." ᚜smart-contract-error «wrong-sender»᚛" CR
        BYE
    THEN

    \ save sender to stack and convert to lenstr
    DUP STRLEN 2DUP ( len sender-pnt len sender-pnt -- )

    \ get sender amount from storage, or 0 if not exists

    ." ᚜gethash «" TELL ." » node::storage 0᚛᚛" CR
    \ read answer
    WORD NUMBER
    0 <> IF
        ." ᚜smart-contract-error «wrong-amount-from-storage»᚛" CR
        BYE
    THEN

    ." ᚜send-money :amount " . ." :to «" TELL ." »᚛" CR
    WORD 2DROP \ read and drop answer

    \ ." ᚜vfm-dbg-die «" .S  ." »᚛" CR
;


: RUN
    RETURN-PAYMENT
    BYE
;
