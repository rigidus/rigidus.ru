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
	rjmp	ctors_end
	rjmp	bad_interrupt
	rjmp	bad_interrupt
	rjmp	vector_3
	rjmp	bad_interrupt
	rjmp	bad_interrupt
	rjmp	bad_interrupt
	rjmp	bad_interrupt
	rjmp	bad_interrupt
	rjmp	bad_interrupt

ctors_end:
    eor	r1, r1
    out	0x3f, r1
    ldi	r28, 0x9F
    out	0x3d, r28

do_clear_bss:
	ldi	r18, 0x00
	ldi	r26, 0x60
	ldi	r27, 0x00
	rjmp	do_clear_bss_start

do_clear_bss_loop:
    st	X+, r1

do_clear_bss_start:
	cpi	r26, 0x64	; 100
	cpc	r27, r18
	brne	do_clear_bss_loop
	rcall	main
	rjmp	exit

bad_interrupt:

    in r24, PORTB
    ori r24, 0b00010000
    out PORTB, r24

    rjmp	bad_interrupt

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

    in r24, PORTB
    ori r24, 0b00000100
    out PORTB, r24

    ;; ovrf++; //Increment counter every 256 clock cycles
	lds	r24, 0x0060
	lds	r25, 0x0061
	lds	r26, 0x0062
	lds	r27, 0x0063
	adiw	r24, 0x01
	adc	r26, r1
	adc	r27, r1
	sts	0x0060, r24
	sts	0x0061, r25
	sts	0x0062, r26
	sts	0x0063, r27
    ;; }
	pop	r27
	pop	r26
	pop	r25
	pop	r24
	pop	r0
	out	0x3f, r0
	pop	r0
	pop	r1
	reti

millis:
    cli
	lds	r22, 0x0060    ;; x = ovrf / 5;
	lds	r23, 0x0061
	lds	r24, 0x0062
	lds	r25, 0x0063
    sei
	ldi	r18, 0x05	; 5
	ldi	r19, 0x00	; 0
	ldi	r20, 0x00	; 0
	ldi	r21, 0x00	; 0
	rcall	udivmodsi4
	movw	r24, r20
	movw	r22, r18
	ret

delay:
	sbiw	r24, 0x00
	breq	delay_ret
    ldi	r30, 0x2B
    ldi	r31, 0x01
delay_a8:
	sbiw	r30, 0x01
	brne	delay_a8
	rjmp	delay_ae
delay_ae:
	nop
	sbiw	r24, 0x01
	rjmp	delay
delay_ret:
	ret

main:
    //Setup timer interrupt and PWM pins
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
	ori	r24, 0x02	; 2
	out	0x39, r24	; 57
    ;; TCNT0=0;
	out	0x32, r1	; 50

    ;; sei();
    sei

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

    ;; DDRB |=  (1 << relay_1);    // pinMode(relay_1, OUTPUT);
    sbi	0x17, 4	; 23
    ;; DDRB &= ~(1 << btn_1);      // pinMode(btn_1, INPUT);
    cbi	0x17, 3	; 23

while1:
    ;; unsigned long cur_mils = millis();
    ;; rcall	millis

    in r16, PORTB
    ori r16, 0b00000001
    out PORTB, r16
    ldi r24, 0xFF
    ldi r25, 0
    rcall delay

    in r16, PORTB
    ori r16, 0b00000010
    out PORTB, r16
    ldi r24, 0xFF
    ldi r25, 0
    rcall delay

    ;; ---

    in  r16, PORTB
    andi r16, 0b11111110
    out PORTB, r16
    ldi r24, 0xFF
    ldi r25, 0
    rcall delay

    in  r16, PORTB
    andi r16, 0b11111101
    out PORTB, r16
    ldi r24, 0xFF
    ldi r25, 0
    rcall delay

    rjmp	while1

udivmodsi4:
	ldi	r26, 0x21
	mov	r1, r26
	sub	r26, r26
	sub	r27, r27
	movw	r30, r26
	rjmp	udivmodsi4_ep

udivmodsi4_loop:
	adc	r26, r26
	adc	r27, r27
	adc	r30, r30
	adc	r31, r31
	cp	r26, r18
	cpc	r27, r19
	cpc	r30, r20
	cpc	r31, r21
	brcs	udivmodsi4_ep
	sub	r26, r18
	sbc	r27, r19
	sbc	r30, r20
	sbc	r31, r21

udivmodsi4_ep:
	adc	r22, r22
	adc	r23, r23
	adc	r24, r24
	adc	r25, r25
	dec	r1
	brne	udivmodsi4_loop
	com	r22
	com	r23
	com	r24
	com	r25
	movw	r18, r22
	movw	r20, r24
	movw	r22, r26
	movw	r24, r30
	ret

exit:
    cli

stop_program:
    rjmp	stop_program
