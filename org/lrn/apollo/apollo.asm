; Disassembly of apollo.bin (avr-gcc style)

.equ SPL, 0x3d
.equ SREG, 0x3f
.text
main:

; Referenced from offset 0x42 by rjmp
Label1:
rjmp    Label2
rjmp    Label5
rjmp    Label5
rjmp    Label5
rjmp    Label6
rjmp    Label5
rjmp    Label5
rjmp    Label5
rjmp    Label5
rjmp    Label5
rjmp    Label5
rjmp    Label5
rjmp    Label5
rjmp    Label5
rjmp    Label5
rjmp    Label5
rjmp    Label5
rjmp    Label5
rjmp    Label5

; Referenced from offset 0x00 by rjmp
Label2:
clr     r1
out     SREG, r1
ldi     r28, 0xdf       ; 223
out     SPL, r28
ldi     r18, 0x00       ; 0
ldi     r26, 0x60       ; 96
ldi     r27, 0x00       ; 0
rjmp    Label4

; Referenced from offset 0x3c by brne
Label3:
st      X+, r1

; Referenced from offset 0x34 by rjmp
Label4:
cpi     r26, 0x61       ; 97
cpc     r27, r18
brne    Label3
rcall   Function2
rjmp    Label11

; Referenced from offset 0x02 by rjmp
; Referenced from offset 0x04 by rjmp
; Referenced from offset 0x06 by rjmp
; Referenced from offset 0x0a by rjmp
; Referenced from offset 0x0c by rjmp
; Referenced from offset 0x0e by rjmp
; Referenced from offset 0x10 by rjmp
; Referenced from offset 0x12 by rjmp
; Referenced from offset 0x14 by rjmp
; Referenced from offset 0x16 by rjmp
; Referenced from offset 0x18 by rjmp
; Referenced from offset 0x1a by rjmp
; Referenced from offset 0x1c by rjmp
; Referenced from offset 0x1e by rjmp
; Referenced from offset 0x20 by rjmp
; Referenced from offset 0x22 by rjmp
; Referenced from offset 0x24 by rjmp
Label5:
rjmp    Label1

; Referenced from offset 0x9c by rcall
Function1:
sbi     0x17, 0         ; 0x01 = 1
cli
out     0x2f, r1        ; 47
out     0x2e, r1        ; 46
ldi     r24, 0xcf       ; 207
ldi     r25, 0x03       ; 3
out     0x2b, r25       ; 43
out     0x2a, r24       ; 42
in      r24, 0x2e       ; 46
ori     r24, 0x08       ; 8
out     0x2e, r24       ; 46
in      r24, 0x2e       ; 46
ori     r24, 0x01       ; 1
out     0x2e, r24       ; 46
in      r24, 0x2e       ; 46
ori     r24, 0x04       ; 4
out     0x2e, r24       ; 46
in      r24, 0x39       ; 57
ori     r24, 0x40       ; 64
out     0x39, r24       ; 57
sei
ret


; Referenced from offset 0x08 by rjmp
Label6:
push    r1
push    r0
in      r0, SREG
push    r0
clr     r1
push    r24
lds     0x0060, r24
tst     r24
breq    Label7
sts     0x0060, r1
rjmp    Label8

; Referenced from offset 0x82 by breq
Label7:
ldi     r24, 0x01       ; 1
sts     0x0060, r24

; Referenced from offset 0x88 by rjmp
Label8:
pop     r24
pop     r0
out     SREG, r0
pop     r0
pop     r1
reti

; Referenced from offset 0x3e by rcall
Function2:
rcall   Function1

; Referenced from offset 0xa8 by rjmp
; Referenced from offset 0xac by rjmp
Label9:
lds     0x0060, r24
tst     r24
breq    Label10
cbi     0x18, 0         ; 0x01 = 1
rjmp    Label9

; Referenced from offset 0xa4 by breq
Label10:
sbi     0x18, 0         ; 0x01 = 1
rjmp    Label9

; Referenced from offset 0x40 by rjmp
Label11:
cli

; Referenced from offset 0xb0 by rjmp
Label12:
rjmp    Label12
