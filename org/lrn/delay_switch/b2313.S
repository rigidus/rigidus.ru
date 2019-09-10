    .equ SPL, 0x3D
    .equ SREG, 0x3F
    .equ RAMEND, 0xDF
.text
.global main
main:

_vectors:
	rjmp    _reset              ;; Reset Handler
	rjmp    _infloop            ;; External Interrupt0 Handler
	rjmp    _infloop            ;; External Interrupt1 Handler
	rjmp    _infloop            ;; Timer1 Capture Handler
	rjmp    _infloop            ;; Timer1 CompareA Handler
	rjmp    _infloop            ;; Timer1 Overflow Handler
	rjmp    _infloop            ;; Timer0 Overflow Handler
	rjmp    _infloop            ;; USART0 RX Complete Handler
	rjmp    _infloop            ;; USART0,UDR Empty Handler
	rjmp    _infloop            ;; USART0 TX Complete Handler
	rjmp    _infloop            ;; Analog Comparator Handler
	rjmp    _infloop            ;; Pin Change Interrupt
	rjmp    _infloop            ;; Timer1 Compare B Handler
	rjmp    _infloop            ;; Timer0 Compare A Handler
	rjmp    _infloop            ;; Timer0 Compare B Handler
	rjmp    _infloop            ;; USI Start Handler
	rjmp    _infloop            ;; USI Overflow Handler
	rjmp    _infloop            ;; EEPROM Ready Handler
	rjmp    _infloop            ;; Watchdog Overflow Handler

_reset:
    ;; Запретить прерывания
	clr     r1
	out     SREG, r1
    ;; Настроить Stack
	ldi     r28, RAMEND
	out     SPL, r28
    ;; Настроить Timer

    ;; CLI                         ; Запрещаем прерывания, в обязательном порядке!
    ;; OUT TCNT1H,R16              ; Старшей байт записался вначале в TEMP
    ;; OUT TCNT1L,R17              ; А теперь записалось и в старший и младший!
    ;; SEI

	rcall   _setup

_halt:
    cli

_infloop:
    rjmp    _infloop

; Referenced from offset 0x2e by rcall
_setup:
	ser     r24                 ; r24 := 0xFF
	out     0x17, r24           ; port-0x17 := 0xFF

; Referenced from offset 0x50 by rjmp
_mainloop:
	in      r24, 0x18           ; r24 := 0x18
	com     r24                 ; invert(r24)
	out     0x18, r24           ; port-0x18 := r24

	ldi     r18, 0x7f           ; r18 := 7F
	ldi     r24, 0xA9           ; r24 := A9
	ldi     r25, 0x03           ; r25 := 03

; Referenced from offset 0x4a by brne
_delay:
	subi    r18, 0x01       ; 1
	sbci    r24, 0x00       ; 0
	sbci    r25, 0x00       ; 0
	brne    _delay
	rjmp    _mainloop

    .data
time:
    db
