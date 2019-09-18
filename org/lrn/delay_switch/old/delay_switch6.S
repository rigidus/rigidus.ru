#define F_CPU 1200000UL

    .include "tn13def.inc"

    .text

    .global main
    .org 0

    .set    ZERO    , r1
    .set    TEMP    , r16
    .set    OVRF    , r28
    .set    SPL     , 0x3d
    .set    SREG    , 0x3f
    .set    RAMEND  , 0x9f
    .set	DDRB	, 0x17
    .set	PORTB	, 0x18
    .set	TCCR0A	, 0x2f
    .set	TCCR0B	, 0x33
    .set	TIMSK0	, 0x39
    .set	TCNT0	, 0x32
    .set	TOIE0	, 1	; Timer/Counter0 Overflow Interrupt Enable
    .set	CS00	, 0	; Clock Select
    .set	CS02	, 2	; Clock Select

; Referenced from offset 0x20 by rjmp
main:
    rjmp    start               ; INT0addr - External Interrupt 0
    rjmp    bad_int             ; PCI0addr - External Interrupt Request 0
    rjmp    bad_int             ; OVF0addr - Timer/Counter0 Overflow
    rjmp    vector_3            ; ERDYaddr - EEPROM Ready
    rjmp    bad_int             ; ACIaddr - Analog Comparator
    rjmp    bad_int             ; OC0Aaddr - Timer/Counter Compare Match A
    rjmp    bad_int             ; OC0Baddr - Timer/Counter Compare Match B
    rjmp    bad_int             ; WDTaddr - Watchdog Time-out
    rjmp    bad_int             ; ADCCaddr - ADC Conversion Complete
    rjmp    bad_int             ; ?

bad_int:
    rjmp    Label1

; Referenced from offset 0x06 by rjmp
vector_3:
    push    r1
    push    r0
    in      r0, SREG
    push    r0
    clr     r1
    push    r24
    push    r25

    in      r25, PORTB
    ldi     r24, 0x01       ; 1
    eor     r24, r25
    out     PORTB, r24      ; 24

    pop     r25
    pop     r24
    pop     r0
    out     SREG, r0
    pop     r0
    pop     r1
    reti

; Referenced from offset 0x00 by rjmp
start:
    clr     ZERO
    out     SREG, ZERO
    ldi     TEMP, RAMEND
    out     SPL, TEMP
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
    ldi TEMP, 0b00000001
    out DDRB, temp

    ;; ovrf initialization
    mov OVRF, ZERO

    ;; in      TEMP, TCCR0B
    ;; ori     TEMP, 0x05
    ;; out     TCCR0B, TEMP
    ;; in      TEMP, TIMSK0
    ;; ori     TEMP, 0x02
    ;; out     TIMSK0, TEMP

    ;; ldi temp, (1<<WGM01) ; set configuration bits to temprary register
    ;; out TCCR0A, temp     ; set timer counter mode to CTC


    ldi TEMP, (1<<CS00)         ; (no prescaler)
    out TCCR0B, TEMP            ; set prescaler

    ldi TEMP, (1<<WGM01)|(1<<WGM00)
    out TCCR0A, TEMP            ; PWM-mode

    ldi TEMP, 0x02
    out TIMSK0, TEMP            ; enable Timer Overflow Interrupt

    out TCNT0, ZERO             ; clear Timer Counter

    ;; enable global interrupts
    sei


; Referenced from offset 0x1c by rcall
loop:
    rjmp loop
