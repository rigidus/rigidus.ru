; Disassembly of delay_switch6.bin (avr-gcc style)

.equ SPL, 0x3d
.equ SREG, 0x3f
.text
main:

; Referenced from offset 0x20 by rjmp
Label1:
   0:   09 c0           rjmp    Label2
   2:   0e c0           rjmp    Label3
   4:   0d c0           rjmp    Label3
   6:   0d c0           rjmp    Label4
   8:   0b c0           rjmp    Label3
   a:   0a c0           rjmp    Label3
   c:   09 c0           rjmp    Label3
   e:   08 c0           rjmp    Label3
  10:   07 c0           rjmp    Label3
  12:   06 c0           rjmp    Label3

; Referenced from offset 0x00 by rjmp
Label2:
  14:   11 24           clr     r1
  16:   1f be           out     SREG, r1
  18:   cf e9           ldi     r28, 0x9f       ; 159
  1a:   cd bf           out     SPL, r28
  1c:   14 d0           rcall   Function1
  1e:   1e c0           rjmp    Label6

; Referenced from offset 0x02 by rjmp
; Referenced from offset 0x04 by rjmp
; Referenced from offset 0x08 by rjmp
; Referenced from offset 0x0a by rjmp
; Referenced from offset 0x0c by rjmp
; Referenced from offset 0x0e by rjmp
; Referenced from offset 0x10 by rjmp
; Referenced from offset 0x12 by rjmp
Label3:
  20:   ef cf           rjmp    Label1

; Referenced from offset 0x06 by rjmp
Label4:
  22:   1f 92           push    r1
  24:   0f 92           push    r0
  26:   0f b6           in      r0, SREG
  28:   0f 92           push    r0
  2a:   11 24           clr     r1
  2c:   8f 93           push    r24
  2e:   9f 93           push    r25
  30:   98 b3           in      r25, 0x18       ; 24
  32:   81 e0           ldi     r24, 0x01       ; 1
  34:   89 27           eor     r24, r25
  36:   88 bb           out     0x18, r24       ; 24
  38:   9f 91           pop     r25
  3a:   8f 91           pop     r24
  3c:   0f 90           pop     r0
  3e:   0f be           out     SREG, r0
  40:   0f 90           pop     r0
  42:   1f 90           pop     r1
  44:   18 95           reti

; Referenced from offset 0x1c by rcall
Function1:
  46:   81 e0           ldi     r24, 0x01       ; 1
  48:   87 bb           out     0x17, r24       ; 23
  4a:   18 ba           out     0x18, r1        ; 24
  4c:   83 b7           in      r24, 0x33       ; 51
  4e:   85 60           ori     r24, 0x05       ; 5
  50:   83 bf           out     0x33, r24       ; 51
  52:   89 b7           in      r24, 0x39       ; 57
  54:   82 60           ori     r24, 0x02       ; 2
  56:   89 bf           out     0x39, r24       ; 57
  58:   78 94           sei

; Referenced from offset 0x5a by rjmp
Label5:
  5a:   ff cf           rjmp    Label5

; Referenced from offset 0x1e by rjmp
Label6:
  5c:   f8 94           cli

; Referenced from offset 0x5e by rjmp
Label7:
  5e:   ff cf           rjmp    Label7
