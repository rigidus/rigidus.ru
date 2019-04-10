#define F_CPU 1200000UL

    .include "tn13def.inc"

    .set WRK , 0b000001
    .set WID , 0b000010
    .set RED , 0b000100
    .set BTN , 0b001000
    .set REL , 0b010000
    .set RST , 0b100000

    ;; define r16 as temporary register
#define zero r1
#define temp r16
#define ovrf r28
#define crit r29

    .global main

    ;;  interrupt vectors
    .org 0x0000
    rjmp main                    ; INT0addr - External Interrupt 0
    rjmp badint                  ; PCI0addr - External Interrupt Request 0
    rjmp isr_ovf0                ; OVF0addr - Timer/Counter0 Overflow
    rjmp badint                  ; ERDYaddr - EEPROM Ready
    rjmp badint                  ; ACIaddr - Analog Comparator
    rjmp isr_oc0a                ; OC0Aaddr - Timer/Counter Compare Match A
    rjmp badint                  ; OC0Baddr - Timer/Counter Compare Match B
    rjmp badint                  ; WDTaddr - Watchdog Time-out
    rjmp badint                  ; ADCCaddr - ADC Conversion Complete

    ;; main program
main:
    ;; stack initialization
    eor	zero, zero
    out	SREG, zero
    ldi	temp, RAMEND
    out	SPL, temp

    ;; _____                        +--v--+
    ;; RESET ADC0 5/A0 PCINT5 PB5  1|o    |8  VCC
    ;; CLKI  ADC3 3/A3 PCINT3 PB3  2|     |7  PB2 PCINT2 2/A1 SCK  ADC1
    ;;       ADC2 4/A2 PCINT4 PB4  3|     |6  PB1 PCINT1 1    MISO OC0B INT0
    ;;                        GND  4|     |5  PB0 PCINT0 0    MOSI OC0A
    ;;                              +-----+
    ;;            +------- (PB5) [1-pin]
    ;;            |+------ (PB4) [3-pin] RELAY_1 (INPUT)
    ;;            ||+----- (PB3) [2-pin] BTN_1
    ;;            |||+---- (PB2) [7-pin] RED
    ;;            ||||+--- (PB1) [6-pin] WID
    ;;            |||||+-- (PB0) [5-pin] WRK (status blink)
    ;;            ||||||
    ldi temp, 0b00010111
    out DDRB, temp

    ;; ovrf initialization
    mov ovrf, zero
    mov crit, zero

    ;; Timer initializtion
    ldi temp, (1<<WGM01) ; set configuration bits to temprary register
    out TCCR0A, temp     ; set timer counter mode to CTC
    ldi temp, (1<<CS02)|(1<<CS00) ; set configuration bits to temprary register
    out TCCR0B, temp              ; set prescaler (F=1200000Hz/1024/256=4Hz, 0.25s)
    ldi temp, 255                 ; set value to temporary register
    out OCR0A, temp               ; set OCR0A value (256 - 1)
    ldi temp, (1<<OCIE0A)         ; set configuration bits to temprary register
    out TIMSK0, temp              ; enable Timer Compare Match A Interrupt

    ;; ldi temp, (1<<CS02)|(1<<CS00) ; set configuration bits to temprary register
    ;; out TCCR0B, temp              ; set prescaler (F=1200000Hz/1024/256=4.58Hz, 0.25s)
    ;; ldi temp, (1<<TOIE0)          ; set configuration bits to temprary register
    ;; out TIMSK0, temp              ; enable Timer Overflow Interrupt

    ; enable global interrupts
    sei
loop:
    sbic    PINB, PB0       ; if bit of LED pin is clear, skip next line
    cbi     PORTB, PB0      ; if 1, turn the LED off
    sbis    PINB, PB0       ; if bit of LED pin is set, skip next line
    sbi     PORTB, PB0      ; if 0, light the LED up

    sbic    PINB, PB1       ; if bit of LED pin is clear, skip next line
    cbi     PORTB, PB1      ; if 1, turn the LED off
    sbis    PINB, PB1       ; if bit of LED pin is set, skip next line
    sbi     PORTB, PB1      ; if 0, light the LED up

    sbrc    ovrf, 0         ; if 7-bit of OVRF is clear, skip next line
    sbi     PORTB, PB2      ; if 1, light the LED up
    sbrs    ovrf, 0         ; if 7-bit of OVRF pin is set, skip next line
    cbi     PORTB, PB2      ; if 0, turn the LED off

    sbrc    crit, 0         ; if bit of LED pin is clear, skip next line
    sbi     PORTB, PB4      ; if 1, turn the LED off
    sbrs    crit, 0         ; if bit of LED pin is set, skip next line
    cbi     PORTB, PB4      ; if 0, light the LED up

    ldi r24, 0xFF
    ldi r25, 0
    rcall delay

    rjmp    loop

    ;;  timer overfow interrupt service routine
isr_ovf0:
    inc     ovrf
    reti

    ;; timer compare match A interrupt service routine
isr_oc0a:
    inc     ovrf
    reti

    ;; badint
badint:
    inc     crit
    rjmp badint

;; delay
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
