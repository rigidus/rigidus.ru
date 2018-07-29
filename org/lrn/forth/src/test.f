
( Тест последовательного выполнения )

: SEQUENCE-TEST
    1 . 2 . 3 .
    ." SEQUENCE TEST PASSED "
    CR
;

( Тест арифметики )

: ARITHMETIC-TEST
    2 2 + 4 = IF
        ." ARITHMETIC TEST PASSED "
    ELSE
        ." ARITHMETIC TEST FAILED "
    THEN
    CR
;

( Тест условного выполнения )

: CONDITION-TEST
    ." CONDITION "
    2 1 > IF
        ." TEST "
        7 9 > IF
            ." FAILED "
        ELSE
            ." PASSED "
        THEN
    ELSE
        ." FAILED"
    THEN
    CR
;

( Тест цикла с постусловием )

: DO-WHILE-TEST
    ." DO-WHILE CYCLE: " CR
    3 BEGIN
        DUP ."  ITERATION OF " . CR
        1-
    DUP 0= UNTIL
    0= IF
        ."  =TEST PASSED"
    ELSE
        ."  =TEST FAILED"
    THEN
    CR
;

( Тест цикла с выходом из тела цикла и процедуры )

: WHILE-TRUE
    ." WHILE-TRUE CYCLE: " CR
    0 BEGIN
        1 +
        DUP ."  ITERATION OF " . CR
        DUP 2 > IF
            EXIT
        THEN
    AGAIN
;

( Проверка теста выше и одновременно тест вызова процедур и возвращаемых значений )

: WHILE-TRUE-TEST
    WHILE-TRUE
    3 = IF
        ."  =TEST PASSED"
    ELSE
        ."  =TEST FAILED"
    THEN
    CR
    DROP
;

( Цикл с предусловием )

: WHILE-TEST
    ." WHILE CYCLE: " CR
    0 BEGIN DUP 3 <
    WHILE
        1 +
        DUP ."  ITERATION OF " . CR
    REPEAT
    ."  =TEST PASSED" CR
;


SEQUENCE-TEST
ARITHMETIC-TEST
CONDITION-TEST
DO-WHILE-TEST
WHILE-TRUE-TEST
WHILE-TEST
