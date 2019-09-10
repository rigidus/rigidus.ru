    #define tmp0 r16
    #define tmp1 r17
    #define cnt  r18
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
  	rjmp    _reset              ; Reset Handler
	rjmp    _infloop            ; External Interrupt0 Handler
	rjmp    _infloop            ; External Interrupt1 Handler
	rjmp    _infloop            ; Timer1 Capture Handler
	rjmp    _infloop            ; Timer1 CompareA Handler
	rjmp    _infloop            ; Timer1 Overflow Handler
	rjmp    _timer0_overflow    ; Timer0 Overflow Handler
	rjmp    _infloop            ; USART0 RX Complete Handler
	rjmp    _infloop            ; USART0,UDR Empty Handler
	rjmp    _infloop            ; USART0 TX Complete Handler
	rjmp    _infloop            ; Analog Comparator Handler
	rjmp    _infloop            ; Pin Change Interrupt
	rjmp    _infloop            ; Timer1 Compare B Handler
	rjmp    _infloop            ; Timer0 Compare A Handler
	rjmp    _infloop            ; Timer0 Compare B Handler
	rjmp    _infloop            ; USI Start Handler
	rjmp    _infloop            ; USI Overflow Handler
	rjmp    _infloop            ; EEPROM Ready Handler
	rjmp    _infloop            ; Watchdog Overflow Handler

_timer0_overflow:
    ;; Cбрасываем счетный регистр таймера/счетчика T0
    out     TCNT0, r1

    inc     cnt
    cpi     cnt, 5
    brsh    _overstep           ; Переход если больше или равно
    reti
_overstep:
    mov     cnt, r1
    rcall _blink
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

    ;; Выставляем частоту предделителя
    ldi     tmp0, 0b100
	out     TCCR0, tmp0

    ;;  Обнуляем регистр-счетчки таймера-0
    out     TCNT0, r1

    ;; Этот регистр нужно сбросить в 0:
    out     TIFR, r1

    ;; Разрешаем прерывания по таймеру 0
    ldi     tmp0, 0b10
    out     TIMSK, tmp0

    ;; Разрешить прерывания
    sei

    mov     cnt, r1

_mainloop:
	rjmp    _mainloop


_infloop:
	rjmp    _infloop

_blink:
    in      tmp0, PORTB
	com     tmp0
	out     PORTB, tmp0
    ret
