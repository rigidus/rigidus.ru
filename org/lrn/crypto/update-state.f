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
    Z" reject" SUBSTRCMP

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
                ." ᚜setf ᚜gethash «state» node::storage᚛ «reject»᚛" CR
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

: RUN
    UPDATE-STATE
    BYE
;
