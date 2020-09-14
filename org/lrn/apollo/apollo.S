.equ DDRB, 0x17
.equ PB0, 0x00
.equ PORTB, 0x18
.org 0x00 ; начало
        sbi     DDRB, PB0;   порт PB0 на передачу
        ldi     r25, 0x01;   r25=1
; главный цикл
loop:
        in      r24, PORTB;  r24=PORTB
        eor     r24, r25;    r24 = r24 xor r25
        out     PORTB, r24;  PORTB=r25
        ldi     r18, 0x3F;   r18=0x3F
        ldi     r19, 0x0D;   r19=0x0D
        ldi     r24, 0x03;   r24=0x03
sleep:
        subi    r18, 0x01;  (r18r19r24)-1  вычитание трехбайтного целого числа
        sbci    r19, 0x00
        sbci    r24, 0x00
        brne    sleep;     если значение в r24 не равно нулю, то переход на начало операции вычитания
        rjmp    loop;      возврат на начало главного цикла
