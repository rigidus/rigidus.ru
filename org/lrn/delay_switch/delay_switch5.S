#define F_CPU 1200000UL
#define SREG 0x3f
#define RAMEND 0x9F
#define SPL 0x3d
#define PINB 0x16
#define PB0 0
#define WRK 0b000001
#define PB1 1
#define WID 0b000010
#define PB2 2
#define RED 0b000100
#define PB3 3
#define BTN 0b001000
#define PB4 4
#define REL 0b010000
#define PB5 5
#define RST 0b100000

;; define r16 as temporary register
#define tmp r16
;; Timer/Counter0 Overflow
#define OVF0addr 0x0003
;; Data Direction Register, Port B
#define DDRB 0x17
#define CS02 2
#define CS00 0
;; TCCR0B - Timer/Counter Control Register B
#define TCCR0B 0x33
;; Timer/Counter0 Interrupt Mask Register
#define TIMSK0 0x39
;; Timer/Counter0 Overflow Interrupt Enable
#define TOIE0 1

#define PORTB 0x18
#define PINB 0x16

    .global main

    ;;  interrupt vectors
    .org    0x0000
    rjmp main                    ; INT0addr - External Interrupt 0
    rjmp badint                  ; PCI0addr - External Interrupt Request 0
    rjmp vector3                 ; OVF0addr - Timer/Counter0 Overflow
    rjmp badint                  ; ERDYaddr - EEPROM Ready
    rjmp badint                  ; ACIaddr - Analog Comparator
    rjmp badint                  ; OC0Aaddr - Timer/Counter Compare Match A
    rjmp badint                  ; OC0Baddr - Timer/Counter Compare Match B
    rjmp badint                  ; WDTaddr - Watchdog Time-out
    rjmp badint                  ; ADCCaddr - ADC Conversion Complete

    ;; main program
main:
    ;; stack initialization
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
    ;;           |||+---- (PB2) [7-pin] RED
    ;;           ||||+--- (PB1) [6-pin] WID
    ;;           |||||+-- (PB0) [5-pin] WRK (status blink)
    ;;           ||||||
    ldi tmp, 0b00010111
    out DDRB, tmp

    ldi tmp, (1<<CS02)|(1<<CS00) ; set configuration bits to temprary register
    out TCCR0B, tmp              ; set prescaler (F=1200000Hz/1024/256=4Hz, 0.25s)
    ldi tmp, (1<<TOIE0)          ; set configuration bits to temprary register
    out TIMSK0, tmp              ; enable Timer Overflow Interrupt
    sei                          ; enable global interrupts
loop:
    sbic    PINB, PB0       ; if bit of LED pin is clear, skip next line
    cbi     PORTB, PB0      ; if 1, turn the LED off
    sbis    PINB, PB0       ; if bit of LED pin is set, skip next line
    sbi     PORTB, PB0      ; if 0, light the LED up

    sbic    PINB, PB1       ; if bit of LED pin is clear, skip next line
    cbi     PORTB, PB1      ; if 1, turn the LED off
    sbis    PINB, PB1       ; if bit of LED pin is set, skip next line
    sbi     PORTB, PB1      ; if 0, light the LED up

    rjmp    loop

    ;;  timer overfow interrupt service routine
vector3:
    sbic    PINB, PB2       ; if bit of LED pin is clear, skip next line
    cbi     PORTB, PB2      ; if 1, turn the LED off
    sbis    PINB, PB2       ; if bit of LED pin is set, skip next line
    sbi     PORTB, PB2      ; if 0, light the LED up
    reti
badint:
    sbic    PINB, PB4       ; if bit of LED pin is clear, skip next line
    cbi     PORTB, PB4      ; if 1, turn the LED off
    sbis    PINB, PB4       ; if bit of LED pin is set, skip next line
    sbi     PORTB, PB4      ; if 0, light the LED up
    rjmp badint
