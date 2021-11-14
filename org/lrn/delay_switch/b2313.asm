                                ; Disassembly of b2313.bin (avr-gcc style)

    .equ SPL, 0x3d
    .equ SREG, 0x3f
    .text
main:
    rjmp    _reset
    rjmp    _infloop
    rjmp    _infloop
    rjmp    _infloop
    rjmp    _infloop
    rjmp    _timer_1_overflow
    rjmp    _infloop
    rjmp    _infloop
    rjmp    _infloop
    rjmp    _infloop
    rjmp    _infloop
    rjmp    _infloop
    rjmp    _infloop
    rjmp    _timer_0_compare_A
    rjmp    _infloop
    rjmp    _infloop
    rjmp    _infloop
    rjmp    _infloop
    rjmp    _infloop

                                ; Referenced from offset 0x0a by rjmp
_timer_1_overflow:
    push    r17
    push    r18
    in      r17, SREG
    push    r17
    push    r26
    push    r27
    push    r28
    push    r29
    push    r30
    push    r31
    dec     r19
    cpi     r19, 0x7f       ; 127
    brcc    Label2
    ldi     r19, 0xaf       ; 175
Label2:  ; Referenced from offset 0x3e by brcc
    rcall   Function5


    sbis    0x10, 3         ; 0x08 = 8
    rjmp    Label3
    sts     0x007a, r1
    rjmp    Label4

                                ; Referenced from offset 0x46 by rjmp
Label3:
    sts     0x007a, r0

                                ; Referenced from offset 0x4c by rjmp
Label4:
    ldi     r26, 0x7b       ; 123
    ldi     r27, 0x00       ; 0
    ldi     r28, 0x7a       ; 122
    ldi     r29, 0x00       ; 0
    ldi     r30, 0x7c       ; 124
    ldi     r31, 0x00       ; 0
    rcall   Function1
    ldi     r26, 0x7d       ; 125
    ldi     r27, 0x00       ; 0
    ldi     r28, 0x79       ; 121
    ldi     r29, 0x00       ; 0
    ldi     r30, 0x7e       ; 126
    ldi     r31, 0x00       ; 0
    rcall   Function3
    lds     0x007e, r17
    lds     0x007f, r18
    add     r17, r1
    adc     r18, r0
    sts     0x007e, r17
    sts     0x007f, r18
    lds     0x007d, r17
    cp      r17, r0
    breq    Label5
    sbi     0x12, 6         ; 0x40 = 64
    rjmp    Label6

                                ; Referenced from offset 0x88 by breq
Label5:
    cbi     0x12, 6         ; 0x40 = 64

                                ; Referenced from offset 0x8c by rjmp
Label6:
    sts     0x0079, r0
    sbis    0x19, 1         ; 0x02 = 2
    rjmp    Label7
    sts     0x0062, r1
    rjmp    Label8

                                ; Referenced from offset 0x96 by rjmp
Label7:
    sts     0x0062, r0

                                ; Referenced from offset 0x9c by rjmp
Label8:
    ldi     r26, 0x63       ; 99
    ldi     r27, 0x00       ; 0
    ldi     r28, 0x62       ; 98
    ldi     r29, 0x00       ; 0
    ldi     r30, 0x64       ; 100
    ldi     r31, 0x00       ; 0
    rcall   Function1
    ldi     r26, 0x65       ; 101
    ldi     r27, 0x00       ; 0
    ldi     r28, 0x61       ; 97
    ldi     r29, 0x00       ; 0
    ldi     r30, 0x66       ; 102
    ldi     r31, 0x00       ; 0
    rcall   Function3
    lds     0x0066, r17
    lds     0x0067, r18
    add     r17, r1
    adc     r18, r0
    sts     0x0066, r17
    sts     0x0067, r18
    lds     0x0065, r17
    cp      r17, r0
    breq    Label9
    sbi     0x12, 4         ; 0x10 = 16
    rjmp    Label10

                                ; Referenced from offset 0xd8 by breq
Label9:
    cbi     0x12, 4         ; 0x10 = 16

                                ; Referenced from offset 0xdc by rjmp
Label10:
    sts     0x0061, r0
    sbis    0x19, 0         ; 0x01 = 1
    rjmp    Label11
    sts     0x0072, r1
    rjmp    Label12

                                ; Referenced from offset 0xe6 by rjmp
Label11:
    sts     0x0072, r0

                                ; Referenced from offset 0xec by rjmp
Label12:
    ldi     r26, 0x73       ; 115
    ldi     r27, 0x00       ; 0
    ldi     r28, 0x72       ; 114
    ldi     r29, 0x00       ; 0
    ldi     r30, 0x74       ; 116
    ldi     r31, 0x00       ; 0
    rcall   Function1
    ldi     r26, 0x75       ; 117
    ldi     r27, 0x00       ; 0
    ldi     r28, 0x71       ; 113
    ldi     r29, 0x00       ; 0
    ldi     r30, 0x76       ; 118
    ldi     r31, 0x00       ; 0
    rcall   Function3
    lds     0x0076, r17
    lds     0x0077, r18
    add     r17, r1
    adc     r18, r0
    sts     0x0076, r17
    sts     0x0077, r18
    lds     0x0075, r17
    cp      r17, r0
    breq    Label13
    sbi     0x12, 5         ; 0x20 = 32
    rjmp    Label14

                                ; Referenced from offset 0x128 by breq
Label13:
    cbi     0x12, 5         ; 0x20 = 32

                                ; Referenced from offset 0x12c by rjmp
Label14:
    sts     0x0071, r0
    sbis    0x10, 2         ; 0x04 = 4
    rjmp    Label15
    sts     0x006a, r1
    rjmp    Label16

                                ; Referenced from offset 0x136 by rjmp
Label15:
    sts     0x006a, r0

                                ; Referenced from offset 0x13c by rjmp
Label16:
    ldi     r26, 0x6b       ; 107
    ldi     r27, 0x00       ; 0
    ldi     r28, 0x6a       ; 106
    ldi     r29, 0x00       ; 0
    ldi     r30, 0x6c       ; 108
    ldi     r31, 0x00       ; 0
    rcall   Function1
    ldi     r26, 0x6d       ; 109
    ldi     r27, 0x00       ; 0
    ldi     r28, 0x69       ; 105
    ldi     r29, 0x00       ; 0
    ldi     r30, 0x6e       ; 110
    ldi     r31, 0x00       ; 0
    rcall   Function3
    lds     0x006e, r17
    lds     0x006f, r18
    add     r17, r1
    adc     r18, r0
    sts     0x006e, r17
    sts     0x006f, r18
    lds     0x006d, r17
    cp      r17, r0
    breq    Label17
    sbi     0x18, 0         ; 0x01 = 1
    rjmp    Label18

                                ; Referenced from offset 0x178 by breq
Label17:
    cbi     0x18, 0         ; 0x01 = 1

                                ; Referenced from offset 0x17c by rjmp
Label18:
    sts     0x0069, r0
    lds     0x0060, r17
    cpse    r17, r0
    rjmp    Label19
    cbi     0x12, 0         ; 0x01 = 1
    rjmp    Label20

                                ; Referenced from offset 0x18a by rjmp
Label19:
    sbi     0x12, 0         ; 0x01 = 1

                                ; Referenced from offset 0x18e by rjmp
Label20:
    lds     0x0068, r17
    cpse    r17, r0
    rjmp    Label21
    cbi     0x18, 4         ; 0x10 = 16
    rjmp    Label22

                                ; Referenced from offset 0x198 by rjmp
Label21:
    sbi     0x18, 4         ; 0x10 = 16

                                ; Referenced from offset 0x19c by rjmp
Label22:
    lds     0x0070, r17
    cpse    r17, r0
    rjmp    Label23
    cbi     0x12, 1         ; 0x02 = 2
    rjmp    Label24

                                ; Referenced from offset 0x1a6 by rjmp
Label23:
    sbi     0x12, 1         ; 0x02 = 2

                                ; Referenced from offset 0x1aa by rjmp
Label24:
    lds     0x0078, r17
    cpse    r17, r0
    rjmp    Label25
    cbi     0x18, 3         ; 0x08 = 8
    rjmp    Label26

                                ; Referenced from offset 0x1b4 by rjmp
Label25:
    sbi     0x18, 3         ; 0x08 = 8

                                ; Referenced from offset 0x1b8 by rjmp
Label26:
    lds     0x0060, r17
    cp      r17, r0
    brne    Label27
    lds     0x0068, r17
    cp      r17, r0
    brne    Label27
    lds     0x0070, r17
    cp      r17, r0
    brne    Label27
    lds     0x0078, r17
    cp      r17, r0
    brne    Label27
    out     0x33, r0        ; 51
    rjmp    Label28

                                ; Referenced from offset 0x1c2 by brne
                                ; Referenced from offset 0x1ca by brne
                                ; Referenced from offset 0x1d2 by brne
                                ; Referenced from offset 0x1da by brne
Label27:
    out     0x33, r1        ; 51

                                ; Referenced from offset 0x1de by rjmp
Label28:
    sts     0x0060, r0
    sts     0x0068, r0
    sts     0x0070, r0
    sts     0x0078, r0
    ldi     r17, 0x00       ; 0
    ldi     r18, 0xe0       ; 224
    out     0x2d, r18       ; 45
    out     0x2c, r17       ; 44
    pop     r31
    pop     r30
    pop     r29
    pop     r28
    pop     r27
    pop     r26
    pop     r17
    out     SREG, r17
    pop     r18
    pop     r17
    reti

                                ; Referenced from offset 0x1a by rjmp
_timer_0_compare_A:
    out     0x36, r19       ; 54
    reti

                                ; Referenced from offset 0x00 by rjmp
_reset:
    clr     r0
    mov     r1, r0
    inc     r1

    out     SREG, r0

    ldi     r17, 0xdf       ; 223
    out     SPL, r17

    ser     r17
    out     0x17, r17       ; 23

    ldi     r17, 0x73       ; 115
    out     0x11, r17       ; 17

    cbi     0x1a, 1         ; 0x02 = 2
    cbi     0x11, 2         ; 0x04 = 4
    cbi     0x1a, 0         ; 0x01 = 1
    cbi     0x11, 3         ; 0x08 = 8

    ldi     r17, 0x02       ; 2
    out     0x2e, r17       ; 46

    ldi     r17, 0x42       ; 66
    out     0x30, r17       ; 48

    ldi     r17, 0x01       ; 1
    out     0x33, r17       ; 51

    out     0x32, r0        ; 50

    ser     r17
    out     0x36, r17       ; 54

    out     0x38, r0        ; 56

    ldi     r17, 0x81       ; 129
    out     0x39, r17       ; 57

    ldi     r19, 0xaf       ; 175

    sei

    out     0x33, r0        ; 51

                                ; Referenced from offset 0x24e by rjmp
_mainloop:
    rjmp    _mainloop

                                ; Referenced from offset 0x5e by rcall
                                ; Referenced from offset 0xae by rcall
                                ; Referenced from offset 0xfe by rcall
                                ; Referenced from offset 0x14e by rcall
Function1:
    rcall   Function2

                                ; Referenced from offset 0x250 by rcall
Function2:
    pop     r25
    pop     r24
    ldi     r23, 0x0b       ; 11
    add     r24, r23
    adc     r25, r0
    ld      r23, X
    add     r24, r23
    adc     r25, r0
    push    r24
    push    r25
    ret

    rjmp    Label32
    rjmp    Label39
    rjmp    Label55

                                ; Referenced from offset 0x268 by rjmp
Label32:
    ld      r25, Y
    cp      r25, r0
    brne    Label33
    mov     r16, r1
    rjmp    Label34

                                ; Referenced from offset 0x272 by brne
Label33:
    mov     r16, r0

                                ; Referenced from offset 0x276 by rjmp
Label34:
    cp      r16, r0
    breq    Label35
    ldi     r25, 0x00       ; 0
    st      X, r25

                                ; Referenced from offset 0x27c by breq
Label35:
    ld      r25, Y
    cp      r25, r1
    brne    Label36
    mov     r16, r1
    rjmp    Label37

                                ; Referenced from offset 0x286 by brne
Label36:
    mov     r16, r0

                                ; Referenced from offset 0x28a by rjmp
Label37:
    cp      r16, r0
    breq    Label38
    st      Z, r0
    ldi     r25, 0x01       ; 1
    st      X, r25

                                ; Referenced from offset 0x290 by breq
Label38:
    rjmp    Label62

                                ; Referenced from offset 0x26a by rjmp
Label39:
    ld      r25, Y
    cp      r25, r0
    brne    Label40
    mov     r16, r1
    rjmp    Label41

                                ; Referenced from offset 0x29e by brne
Label40:
    mov     r16, r0

                                ; Referenced from offset 0x2a2 by rjmp
Label41:
    cp      r16, r0
    breq    Label42
    ldi     r25, 0x00       ; 0
    st      X, r25

                                ; Referenced from offset 0x2a8 by breq
Label42:
    ld      r25, Y
    cp      r25, r1
    brne    Label43
    mov     r16, r1
    rjmp    Label44

                                ; Referenced from offset 0x2b2 by brne
Label43:
    mov     r16, r0

                                ; Referenced from offset 0x2b6 by rjmp
Label44:
    cp      r16, r0
    breq    Label47
    ld      r25, Z
    ldi     r24, 0x02       ; 2
    cp      r25, r24
    brcc    Label45
    mov     r16, r1
    rjmp    Label46

                                ; Referenced from offset 0x2c4 by brcc
Label45:
    mov     r16, r0

                                ; Referenced from offset 0x2c8 by rjmp
Label46:
    cp      r16, r0
    breq    Label47
    mov     r16, r1

                                ; Referenced from offset 0x2bc by breq
                                ; Referenced from offset 0x2ce by breq
Label47:
    cp      r16, r0
    breq    Label48
    ld      r24, Z
    inc     r24
    st      Z, r24
    ldi     r24, 0x01       ; 1
    st      X, r24

                                ; Referenced from offset 0x2d4 by breq
Label48:
    ld      r24, Y
    cp      r24, r1
    brne    Label49
    mov     r16, r1
    rjmp    Label50

                                ; Referenced from offset 0x2e4 by brne
Label49:
    mov     r16, r0

                                ; Referenced from offset 0x2e8 by rjmp
Label50:
    cp      r16, r0
    breq    Label53
    ld      r24, Z
    ldi     r25, 0x02       ; 2
    cp      r24, r25
    brcs    Label51
    mov     r16, r1
    rjmp    Label52

                                ; Referenced from offset 0x2f6 by brcs
Label51:
    mov     r16, r0

                                ; Referenced from offset 0x2fa by rjmp
Label52:
    cp      r16, r0
    breq    Label53
    mov     r16, r1

                                ; Referenced from offset 0x2ee by breq
                                ; Referenced from offset 0x300 by breq
Label53:
    cp      r16, r0
    breq    Label54
    st      -Y, r1
    ldi     r25, 0x02       ; 2
    st      X, r25

                                ; Referenced from offset 0x306 by breq
Label54:
    rjmp    Label62

                                ; Referenced from offset 0x26c by rjmp
Label55:
    ld      r25, Y
    cp      r25, r1
    brne    Label56
    mov     r16, r1
    rjmp    Label57

                                ; Referenced from offset 0x314 by brne
Label56:
    mov     r16, r0

                                ; Referenced from offset 0x318 by rjmp
Label57:
    cp      r16, r0
    breq    Label58
    ldi     r25, 0x02       ; 2
    st      X, r25

                                ; Referenced from offset 0x31e by breq
Label58:
    ld      r25, Y
    cp      r25, r0
    brne    Label59
    mov     r16, r1
    rjmp    Label60

                                ; Referenced from offset 0x328 by brne
Label59:
    mov     r16, r0

                                ; Referenced from offset 0x32c by rjmp
Label60:
    cp      r16, r0
    breq    Label61
    ldi     r25, 0x00       ; 0
    st      X, r25

                                ; Referenced from offset 0x332 by breq
Label61:
    rjmp    Label62

                                ; Referenced from offset 0x298 by rjmp
                                ; Referenced from offset 0x30e by rjmp
                                ; Referenced from offset 0x338 by rjmp
Label62:
    ret


                                ; Referenced from offset 0x6c by rcall
                                ; Referenced from offset 0xbc by rcall
                                ; Referenced from offset 0x10c by rcall
                                ; Referenced from offset 0x15c by rcall
Function3:
    rcall   Function4

                                ; Referenced from offset 0x33c by rcall
Function4:
    pop     r25
    pop     r24
    ldi     r23, 0x0b       ; 11
    add     r24, r23
    adc     r25, r0
    ld      r23, X
    add     r24, r23
    adc     r25, r0
    push    r24
    push    r25
    ret

    rjmp    Label63
    rjmp    Label67

                                ; Referenced from offset 0x354 by rjmp
Label63:
    ld      r25, Y
    cp      r25, r1
    brne    Label64
    mov     r16, r1
    rjmp    Label65

                                ; Referenced from offset 0x35c by brne
Label64:
    mov     r16, r0

                                ; Referenced from offset 0x360 by rjmp
Label65:
    cp      r16, r0
    breq    Label66
    push    r30
    push    r31
    st      Z+, r0
    st      Z, r0
    pop     r31
    pop     r30
    ldi     r25, 0x01       ; 1
    st      X, r25

                                ; Referenced from offset 0x366 by breq
Label66:
    rjmp    Label82

                                ; Referenced from offset 0x356 by rjmp
Label67:
    ld      r25, Y
    cp      r25, r1
    brne    Label68
    mov     r16, r1
    rjmp    Label69

                                ; Referenced from offset 0x37e by brne
Label68:
    mov     r16, r0

                                ; Referenced from offset 0x382 by rjmp
Label69:
    cp      r16, r0
    breq    Label70
    ldi     r25, 0x00       ; 0
    st      X, r25

                                ; Referenced from offset 0x388 by breq
Label70:
    push    r31
    push    r30
    ld      r25, Z+
    ld      r24, Z
    pop     r30
    pop     r31
    cpi     r25, 0x00       ; 0
    ldi     r23, 0x02       ; 2
    cpc     r24, r23
    brcs    Label71
    mov     r16, r1
    rjmp    Label72

                                ; Referenced from offset 0x3a0 by brcs
Label71:
    mov     r16, r0

                                ; Referenced from offset 0x3a4 by rjmp
Label72:
    cp      r16, r0
    breq    Label73
    ldi     r23, 0x00       ; 0
    st      X, r23

                                ; Referenced from offset 0x3aa by breq
Label73:
    push    r31
    push    r30
    ld      r23, Z+
    ld      r24, Z
    pop     r30
    pop     r31
    cpi     r23, 0x10       ; 16
    ldi     r25, 0x00       ; 0
    cpc     r24, r25
    brcs    Label74
    mov     r16, r1
    rjmp    Label75

                                ; Referenced from offset 0x3c2 by brcs
Label74:
    mov     r16, r0

                                ; Referenced from offset 0x3c6 by rjmp
Label75:
    cp      r16, r0
    breq    Label80
    push    r31
    push    r30
    ld      r25, Z+
    ld      r24, Z
    pop     r30
    pop     r31
    cpi     r25, 0x00       ; 0
    ldi     r23, 0x02       ; 2
    cpc     r24, r23
    brcs    Label76
    mov     r16, r1
    rjmp    Label77

                                ; Referenced from offset 0x3e0 by brcs
Label76:
    mov     r16, r0

                                ; Referenced from offset 0x3e4 by rjmp
Label77:
    cp      r16, r0
    breq    Label78
    mov     r16, r0
    rjmp    Label79

                                ; Referenced from offset 0x3ea by breq
Label78:
    mov     r16, r1

                                ; Referenced from offset 0x3ee by rjmp
Label79:
    cp      r16, r0
    breq    Label80
    mov     r16, r1

                                ; Referenced from offset 0x3cc by breq
                                ; Referenced from offset 0x3f4 by breq
Label80:
    cp      r16, r0
    breq    Label81
    st      -Y, r1

                                ; Referenced from offset 0x3fa by breq
Label81:
    rjmp    Label82

                                ; Referenced from offset 0x378 by rjmp
                                ; Referenced from offset 0x3fe by rjmp
Label82:
    ret


                                ; Referenced from offset 0x02 by rjmp
                                ; Referenced from offset 0x04 by rjmp
                                ; Referenced from offset 0x06 by rjmp
                                ; Referenced from offset 0x08 by rjmp
                                ; Referenced from offset 0x0c by rjmp
                                ; Referenced from offset 0x0e by rjmp
                                ; Referenced from offset 0x10 by rjmp
                                ; Referenced from offset 0x12 by rjmp
                                ; Referenced from offset 0x14 by rjmp
                                ; Referenced from offset 0x16 by rjmp
                                ; Referenced from offset 0x18 by rjmp
                                ; Referenced from offset 0x1c by rjmp
                                ; Referenced from offset 0x1e by rjmp
                                ; Referenced from offset 0x20 by rjmp
                                ; Referenced from offset 0x22 by rjmp
                                ; Referenced from offset 0x24 by rjmp
                                ; Referenced from offset 0x402 by rjmp
_infloop:
    rjmp    _infloop

                                ; Referenced from offset 0x42 by rcall
Function5:
    sbic    0x18, 1         ; 0x02 = 2
    rjmp    Label85
    sbi     0x18, 1         ; 0x02 = 2

                                ; Referenced from offset 0x40e by rjmp
Label84:
    ret


                                ; Referenced from offset 0x406 by rjmp
Label85:
    cbi     0x18, 1         ; 0x02 = 2
    rjmp    Label84
