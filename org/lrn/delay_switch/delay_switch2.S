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
    rjmp	bad_interrupt ; vector_3
    rjmp	bad_interrupt
    rjmp	bad_interrupt
    rjmp	bad_interrupt
    rjmp	bad_interrupt
    rjmp	bad_interrupt
    rjmp	bad_interrupt

bad_interrupt:
    push r20
    push r24
    push r25
    in  r20, PORTB
          ;; 0b00010111
    ori r20, 0b00010111 ; RED-LED (PB2) - switch on
    out PORTB, r20

    ldi r24, 0xFF
    ldi r25, 0
    rcall delay

    in  r20, PORTB
           ;; 0b00010111
    andi r20, 0b11101000 ; RED-LED (PB2) - switch off
    out PORTB, r20

    ldi r24, 0xFF
    ldi r25, 0
    rcall delay

    pop r25
    pop r24
    pop r20

    reti

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
    ;;           |||+---- (PB2) [7-pin] RED-LED
    ;;           ||||+--- (PB1) [6-pin] WIDE-LED
    ;;           |||||+-- (PB0) [5-pin] LED (status blink)
    ;;           ||||||
    ldi r18, 0b00010111
    out DDRB, r18

    ;;  TCCR0B - Timer/Counter Control Register B
#define TCCR0B 0x33
;;  TCCR0B |= _BV(CS00);
    ldi	r26, 0b101  ; Clock Select : 100 : CLK/1024 prescale
    out	TCCR0B, r26 ;

    ;; Output Compare Register A
#define OCR0A 0x36
    ldi r26, 116    ; 1200000/(1024*(1+116)) = 10.01 per second
    out OCR0A, r26

    ;;  TCCR0A - Timer/Counter Control Register A
#define TCCR0A 0x2f
    ;; TCCR0A |= _BV(WGM00)|_BV(WGM01);
    ldi	r26, 0b10   ; 10 - сброс таймера при совпадении (CTC)
    out	TCCR0A, r26 ;

    ;; TIMSK0 Timer/Counter Interrupt Mask Register
#define TIMSK0 0x39
    ;; TIMSK0 |= 2;
    ldi	r26, 0b100  ; Set OCIE1A прерывание по Совпадению А
    out	TIMSK0, r26 ;

    ;; Timer/Counter - Устанавливаем нулевое начальное значение
#define TCNT0 0x32
    ;; TCNT0=0;
    out	TCNT0, r1 ; de:

    ;; Разрешаем прерывания
    sei

loop:
    in r16, PORTB
    ori r16, 0b00000001
    out PORTB, r16

    ldi r24, 0xFF
    ldi r25, 0
    rcall delay

    in  r16, PORTB
    andi r16, 0b11111110
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


vector_3:
    ;; push	r1
    ;; push	r0
    ;; in	r0, 0x3f
    ;; push	r0
    ;; eor	r1, r1
    ;; push	r24
    ;; push	r25
    ;; push	r26
    ;; push	r27

    ;; ;; lds	r24, 0x0078
    ;; ;; lds	r25, 0x0079
    ;; ;; lds	r26, 0x007A
    ;; ;; lds	r27, 0x007B
    ;; ;; adiw	r24, 0x01
    ;; ;; adc	r26, r1
    ;; ;; adc	r27, r1
    ;; ;; sts	0x0078, r24
    ;; ;; sts	0x0079, r25
    ;; ;; sts	0x007A, r26
    ;; ;; sts	0x007B, r27

    ;; pop	r27
    ;; pop	r26
    ;; pop	r25
    ;; pop	r24
    ;; pop	r0
    ;; out	0x3f, r0
    ;; pop	r0
    ;; pop	r1

    reti
