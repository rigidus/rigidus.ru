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
    rjmp	vector_3
    rjmp	bad_interrupt
    rjmp	bad_interrupt
    rjmp	bad_interrupt
    rjmp	bad_interrupt
    rjmp	bad_interrupt
    rjmp	bad_interrupt

bad_interrupt:
    in  r16, PORTB
    ori r16, 0b11111111
    out PORTB, r16

    rjmp bad_interrupt

main:
    eor	r1, r1
    out	SREG, r1
    ldi	r28, RAMEND
    out	SPL, r28


    ;;  TCCR0B - Timer/Counter Control Register B
#define TCCR0B 0x33
#define CS00    = 0 ; Clock Select:
#define CS01    = 1 ; Clock Select:
#define CS02    = 2 ; Clock Select:
#define WGM02   = 3 ; Waveform Generation Mode
#define FOC0B   = 6 ; Force Output Compare B
#define FOC0A   = 7 ; Force Output Compare A

;; ;;  TCCR0B |= _BV(CS00);
;;     in	r24, TCCR0B ; cc: NonNeeded?
;;     ori	r24, 0x01   ; ce: Clock Select : 001,
;;     out	TCCR0B, r24 ; d0:

;;     ;;  TCCR0A - Timer/Counter Control Register A
;; #define TCCR0A 0x2f

;;     ;; TCCR0A |= _BV(WGM00)|_BV(WGM01);
;;     in	r24, 0x2f ; d2: NonNeeded?
;;     ori	r24, 0x03 ; d4: Set WGM01 & WGM02
;;     out	TCCR0A, r24 ; d6:

;;     ;; TIMSK0 Timer/Counter Interrupt Mask Register
;;     ;; TIMSK0 |= 2;
;;     in	r24, 0x39 ; d8:
;;     ori	r24, 0x02 ; da: Set OCIE1A & TOIE1 (зачем совпадение A?)
;;     out	0x39, r24 ; dc:

;;     ;; Timer/Counter - Устанавливаем нулевое начальное значение
;;     ;; TCNT0=0;
;;     out	0x32, r1 ; de:




    ;; TCCR0B |= _BV(CS00);
	in	r24, 0x33	; 51
	ori	r24, 0x01	; 1
	out	0x33, r24	; 51
    ;; TCCR0A |= _BV(WGM00)|_BV(WGM01);
	in	r24, 0x2f	; 47
	ori	r24, 0x03	; 3
	out	0x2f, r24	; 47
    ;; TIMSK0 |= 2;
	in	r24, 0x39	; 57
	ori	r24,    2    ; 0x02
	out	0x39, r24	; 57
    ;; TCNT0=0;
	out	0x32, r1	; 50
    ;; ;; sei();
	sei
    ;; ;; ADMUX=0;
	out	0x07, r1	; 7

	in	r24, 0x06	; 6
	ori	r24, 0x82	; 130
	out	0x06, r24	; 6

    ;; ;; DDRB |=  (1 << relay_1);    // pinMode(relay_1, OUTPUT);
	sbi	0x17, 4	; 23
    ;; ;; DDRB &= ~(1 << btn_1);      // pinMode(btn_1, INPUT);
	cbi	0x17, 3	; 23


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
    ldi r16, 0b00010111
    out DDRB, r16
    sei

loop:
    in r16, PORTB
    ori r16, 0b00010100
    ;; ori r16, 0b11111111
    out PORTB, r16

    ldi r24, 0xFF
    ldi r25, 0
    rcall delay

    in  r16, PORTB
    andi r16, 0b11101011
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
    push	r1
    push	r0
    in	r0, 0x3f
    push	r0
    eor	r1, r1
    push	r24
    push	r25
    push	r26
    push	r27

    ;; lds	r24, 0x0078
    ;; lds	r25, 0x0079
    ;; lds	r26, 0x007A
    ;; lds	r27, 0x007B
    ;; adiw	r24, 0x01
    ;; adc	r26, r1
    ;; adc	r27, r1
    ;; sts	0x0078, r24
    ;; sts	0x0079, r25
    ;; sts	0x007A, r26
    ;; sts	0x007B, r27

    pop	r27
    pop	r26
    pop	r25
    pop	r24
    pop	r0
    out	0x3f, r0
    pop	r0
    pop	r1

    reti
