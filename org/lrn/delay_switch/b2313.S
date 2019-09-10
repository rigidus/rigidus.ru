    #define tmp0 r16
    #define tmp1 r17
    #define cnt0 r18
    #define par1 r23
    #define par2 r24
    #define par3 r25

    .equ SPL, 0x3D
    .equ SREG, 0x3F
    .equ RAMEND, 0xDF
    .equ DDRB, 0x17
    .equ PORTB, 0x18
    .equ TCCR0, 0x33
    .equ TCNT0, 0x32
    .equ TIFR, 0x38
    .equ TIMSK, 0x39

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
	rjmp    _timer0_overflow    ;; Timer0 Overflow Handler
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

_timer0_overflow:
    ;; Cбрасываем счетный регистр таймера/счетчика T0
    out     TCNT0, r1
    ;; Инкрементируем регистр-счетчик
    inc     cnt0
    ;; Проверяем на переполнение
    cpi     cnt0, 30
    brlo    _over
    reti
_over:
	in      tmp0, PORTB
	com     tmp0                ; invert(r24)
	out     PORTB, tmp0
    reti


_reset:

	;; Запретить прерывания
	clr     r1
	out     SREG, r1

    ;; Настроить Stack
	ldi     r28, RAMEND
	out     SPL, r28

    ;; Настроить порт на выход
    ser     r24
	out     DDRB, r24

    ;; Настроить Timer
    ;; ...

    ;; Разрешить прерывания
    sei

_mainloop:
    rcall    _blink

	ldi     par1, 0x7f           ; par1 := 7F
	ldi     par2, 0xA9           ; par2 := A9
	ldi     par3, 0x03           ; par3 := 03
_delay:
	subi    par1, 0x01       ; 1
	sbci    par2, 0x00       ; 0
	sbci    par3, 0x00       ; 0
	brne    _delay
	rjmp    _mainloop


_infloop:
	rjmp    _infloop

_blink:
    in      tmp0, PORTB
	com     tmp0
	out     PORTB, tmp0
    ret
