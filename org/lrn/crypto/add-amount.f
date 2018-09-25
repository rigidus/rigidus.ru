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
