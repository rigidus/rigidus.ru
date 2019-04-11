; Disassembly of delay_switch6.bin (avr-gcc style)

.equ SPL, 0x3d
.equ SREG, 0x3f
    .text
    .global main
    .org 0
main:

; Referenced from offset 0x20 by rjmp
Label1:
rjmp    Label2
rjmp    Label3
rjmp    Label3
rjmp    Label4
rjmp    Label3
rjmp    Label3
rjmp    Label3
rjmp    Label3
rjmp    Label3
rjmp    Label3

; Referenced from offset 0x00 by rjmp
Label2:
clr     r1
out     SREG, r1
ldi     r28, 0x9f       ; 159
out     SPL, r28
rcall   Function1
rjmp    Label6

; Referenced from offset 0x02 by rjmp
; Referenced from offset 0x04 by rjmp
; Referenced from offset 0x08 by rjmp
; Referenced from offset 0x0a by rjmp
; Referenced from offset 0x0c by rjmp
; Referenced from offset 0x0e by rjmp
; Referenced from offset 0x10 by rjmp
; Referenced from offset 0x12 by rjmp
Label3:
rjmp    Label1

; Referenced from offset 0x06 by rjmp
Label4:
push    r1
push    r0
in      r0, SREG
push    r0
clr     r1
push    r24
push    r25
in      r25, 0x18       ; 24
ldi     r24, 0x01       ; 1
eor     r24, r25
out     0x18, r24       ; 24
pop     r25
pop     r24
pop     r0
out     SREG, r0
pop     r0
pop     r1
reti

; Referenced from offset 0x1c by rcall
Function1:
ldi     r24, 0x01       ; 1
out     0x17, r24       ; 23
out     0x18, r1        ; 24
in      r24, 0x33       ; 51
ori     r24, 0x05       ; 5
out     0x33, r24       ; 51
in      r24, 0x39       ; 57
ori     r24, 0x02       ; 2
out     0x39, r24       ; 57
sei

; Referenced from offset 0x5a by rjmp
Label5:
rjmp    Label5

; Referenced from offset 0x1e by rjmp
Label6:
cli

; Referenced from offset 0x5e by rjmp
Label7:
rjmp    Label7
