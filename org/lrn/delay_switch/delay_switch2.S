#define F_CPU 1200000UL
#define PORTB 0x18
#define DDRB  0x17
#define SREG 0x3f
#define SPL  0x3d
#define RAMEND 0x9F
#define RELAY_1 4
#define BTN_1 3

    .section .text
    .org 0
    .global main

vectors:
    rjmp	main
    rjmp	bad_interrupt
    rjmp	bad_interrupt
    rjmp	bad_interrupt
    rjmp	bad_interrupt
    rjmp	bad_interrupt
    rjmp	bad_interrupt
    rjmp	bad_interrupt
    rjmp	bad_interrupt
    rjmp	bad_interrupt

bad_interrupt:
    ldi r16, 0b00000001
    ldi r17, 0b00000001 ; using R17 for toggle mask (XOR)
    out DDRB, r16
    eor r16, r17
    out PORTB, r16
    eor r16, r17
    out PORTB, r16
    rjmp bad_interrupt

main:
    eor	r1, r1
    out	SREG, r1
    ldi	r28, RAMEND
    out	SPL, r28
    ;; _____                        +--v--+
    ;; RESET ADC0 5/A0 PCINT5 PB5  1|o    |8  VCC
    ;; CLKI  ADC3 3/A3 PCINT3 PB3  2|     |7  PB2 PCINT2 2/A1 SCK  ADC1
    ;;       ADC2 4/A2 PCINT4 PB4  3|     |6  PB1 PCINT1 1    MISO OC0B INT0
    ;;                        GND  4|     |5  PB0 PCINT0 0    MOSI OC0A
    ;;                              +-----+
    ;;           +------- (PB5) [1-pin]
    ;;           |+------ (PB4) [3-pin] RELAY_1 (INPUT)
    ;;           ||+----- (PB3) [2-pin] BTN_1
    ;;           |||+---- (PB2) [7-pin]
    ;;           ||||+--- (PB1) [6-pin] WIDE-LED
    ;;           |||||+-- (PB0) [5-pin] LED
    ;;           ||||||
    ldi r16, 0b00010011
    out DDRB, r16
    sei
loop:
    in r16, PORTB
    ori r16, 0b00010010
    out PORTB, r16

    ldi r24, 0xFF
    ldi r25, 0
    rcall delay

    in  r16, PORTB
    andi r16, 0b11101101
    out PORTB, r16

    ldi r24, 0xFF
    ldi r25, 0
    rcall delay

    rjmp loop


delay:
    sbiw	r24, 0x00 ; b6:
    breq	delay_return ; b8:
    ldi	r30, 0x2B ; ba:
    ldi	r31, 0x01 ; bc:
delay_loop:
    sbiw	r30, 0x01  ; be:
    brne	delay_loop ; c0:
    rjmp	delay_nop ; c2:
delay_nop:
    nop ; c4:
    sbiw	r24, 0x01 ; c6:
    rjmp	delay ; c8:
delay_return:
    ret ; ca:
