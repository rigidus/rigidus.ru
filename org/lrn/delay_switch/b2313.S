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
    .equ TCCR1B, 0x2E
    .equ OCR0A, 0x36
    .equ OCR0B, 0x3C
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
    rjmp    _timer_1_overflow   ; Timer1 Overflow Handler
    rjmp    _infloop            ; Timer0 Overflow Handler
    rjmp    _infloop            ; USART0 RX Complete Handler
    rjmp    _infloop            ; USART0,UDR Empty Handler
    rjmp    _infloop            ; USART0 TX Complete Handler
    rjmp    _infloop            ; Analog Comparator Handler
    rjmp    _infloop            ; Pin Change Interrupt
    rjmp    _infloop            ; Timer1 Compare B Handler
    rjmp    _timer_0_compare_A  ; Timer0 Compare A Handler
    rjmp    _infloop            ; Timer0 Compare B Handler
    rjmp    _infloop            ; USI Start Handler
    rjmp    _infloop            ; USI Overflow Handler
    rjmp    _infloop            ; EEPROM Ready Handler
    rjmp    _infloop            ; Watchdog Overflow Handler

_timer_1_overflow:
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
    ;;     rjmp    _timer_0_overflow_ret
    ;; _not_press:
        ;; Пока ничего не нажато - мигаем
        dec     cnt
        rcall   _blink
    _timer_0_overflow_ret:
        ;; Очищаем счетчик переполнений
        ;; mov     cnt, r1
        ;; ldi tmp0, 0
        ;; out TCNT0, tmp0
        ;; out TCNT0, cnt
        ;; inc cnt
        reti

_timer_0_compare_A:
    ;;; Записываем cnt в интервал сравнения
        out     OCR0A, cnt
        reti

_reset:
    ;; Запретить прерывания
    clr     r1
    out     SREG, r1
    
    ;; Настроить Stack
    ldi     tmp0, RAMEND
    out     SPL, tmp0
    
    ;; Инициализируем выводы
    ;; Настроить PB4(blink) и PB2(OC0A) на выход, остальные на вход
    ;;                 |          |
    ;;                 | +--------+
    ;;                 | |
    ;;                 v v
    ldi     tmp0, 0b00010100
    out     DDRB, tmp0
    
    ;; Для пинов подключенных на вход (кнопки)
    ;; установить подтяжку к питанию (записав 1) чтобы не было hi-z
    ;; Однако их надо подтянуть к минусу питания (!)
    ;; PB4 подтягиваем к земле, потому что начальное
    ;; его состояние - выключен
    ;; PB2(0C0A) - тоже к земле, потому что он выход
    ldi     tmp0, 0b00001011
    out     PORTB, tmp0
    
    ;; Инициализация таймера-1
    ;; Выставляем предделитель
    ldi     tmp0, 0b0010
    out     TCCR1B, tmp0
    
    ;; Инициализация таймера-0
    ;; TCCR0A
    ldi tmp0, 0b01000010
    out TCCR0A, tmp0
    ;; TCCR0B
    ldi tmp0, 0b01
    out TCCR0B, tmp0
    ;; Clear TCNT0
    out TCNT0, r1
    ;; OCR0A & OCR0B
    ldi tmp0, 0xFF
    out OCR0A, tmp0
    ;; Очищаем флаги прерывания таймера
    out TIFR, r1
    
    ;; Настройка прерываний таймеров
    ;; TOEI1(ovfl-1) & OCIE0A(cmpA-0)
    ldi     tmp0, 0b10000001
    out     TIMSK, tmp0
    
    ;; Разрешить прерывания
    sei

    ;; DISABLE TIMER-0
    ;; Временно остановим таймер-0 чтобы не щелкал
    ldi tmp0, 0
    out TCCR0B, tmp0

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
