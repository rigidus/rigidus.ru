;;; b2313 delay switch for 4 buttons

    #define tmp0 r16
    #define tmp1 r17
    #define cnt  r18

    .equ SPL, 0x3D
    .equ SREG, 0x3F
    .equ RAMEND, 0xDF
    .equ DDRB, 0x17
    .equ PORTB, 0x18
    .equ PINB, 0x16
    .equ TCCR0A, 0x30
    .equ TCCR0B, 0x33
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
        ;; out     TCNT0, r1
        ;; Увеличиваем и проверяем счетчик переполнений
        ;; inc     cnt
        ;; cpi     cnt, 1
        ;; brsh    _overstep           ; Переход если больше или равно
        ;; reti
    _overstep:
    ;;     ;; Читаем выводы PB0-PB3
    ;;     in      tmp0, PINB
    ;;     ldi     tmp1, 0b00000001    ; NB! - Пока только нулевой
    ;;     and     tmp0, tmp1          ;
    ;;     ;; Есть ли нажатие?
    ;;     cpi     tmp0, 0
    ;;     breq    _not_press          ; Перейти если равно
    ;;     ;; Включить светодиод
    ;;     ldi     tmp0, 0b00010000
    ;; 	out     PORTB, tmp0
    ;;     rjmp    _timer0_overflow_ret
    ;; _not_press:
        ;; Пока ничего не нажато - мигаем
        rcall _blink
    _timer0_overflow_ret:
        ;; Очищаем счетчик переполнений
        ;; mov     cnt, r1
    
        ldi tmp0, 0
        out TCNT0, tmp0
        ;; out TCNT0, cnt
        ;; inc cnt
    
        reti

_reset:
    ;; Запретить прерывания
    clr     r1
    out     SREG, r1
    
    ;; Настроить Stack
    ldi     tmp0, RAMEND
    out     SPL, tmp0
    ;; Настроить для PB4 на выход, остальные на вход
    ldi     tmp0, 0b00010000
    out     DDRB, tmp0
    
    ;; Для пинов PB0-PB3 подключенных на вход (кнопки)
    ;; установить подтяжку к питанию, чтобы не было hi-z
    ;; Однако их надо подтянуть к минусу питания (!)
    ;; PB4 подтягиваем к земле, потому что начальное
    ;; его состояние - выключен
    ldi     tmp0, 0b00001111
    out     PORTB, tmp0
    
    ;; Настроить Timer
    
    ldi     tmp0, 0b101
    out     TCCR0B, tmp0
    
    out     TCNT0, r1
    
    ;; OCR0A – Output Compare Register A
    ;; The Output Compare Register A contains an 8-bit value that is continuously compared with the
    ;; counter value (TCNT0). A match can be used to generate an Output Compare interrupt, or to
    ;; generate a waveform output on the OC0A pin.
    ;; [TODO]
    
    ;; OCR0B – Output Compare Register B
    ;; The Output Compare Register B contains an 8-bit value that is continuously compared with the
    ;; counter value (TCNT0). A match can be used to generate an Output Compare interrupt, or to
    ;; generate a waveform output on the OC0B pin.
    ;; [TODO]
    
    ;; Разрешаем прерывания по переполнению таймера 0
    ldi     tmp0, 0b10
    out     TIMSK, tmp0
    
    out     TIFR, r1
    ;; Начальная инициализация счетчика переполнений
    mov     cnt, r1
    
    ;; Разрешить прерывания
    sei

_mainloop:
    rjmp    _mainloop

_infloop:
    rjmp    _infloop

_blink:
    in      tmp0, PORTB
    com     tmp0
    ldi     tmp1, 0b00010000    ; Мигаем только PB4
    and     tmp0, tmp1
    out     PORTB, tmp0
    ret
