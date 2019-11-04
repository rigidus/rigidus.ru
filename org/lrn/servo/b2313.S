;;; b2313 delay switch for 4 buttons
    #define TRUE 1
    #define FALSE 0
    #define MAX 2
    #define result r16
    #define tmp0 r17
    #define tmp1 r18
    #define freq r19
    
    #defune FCPU 8000000
    #define ICR_MAX F_CPU/50
    #define OCR_MIN ICR_MAX/20
    #define OCR_MAX ICR_MAX/10
    #define OCR_CENTER (ICR_MAX/4/10)*3
    
    .equ DELAY_MAX, 0x0200
    .equ SIGNAL_TIME, 0x0010
    .equ START_FREQ, 0xAF
    .equ END_FREQ, 0x7F

    .equ SPL, 0x3D
    .equ SREG, 0x3F
    .equ RAMEND, 0xDF
    .equ DDRA, 0x1A
    .equ DDRB, 0x17
    .equ DDRD, 0x11
    .equ PORTA, 0x1B
    .equ PORTB, 0x18
    .equ PORTD, 0x12
    .equ PINA, 0x19
    .equ PINB, 0x16
    .equ PIND, 0x10
    .equ TCCR0A, 0x30
    .equ TCCR0B, 0x33
    .equ TCCR1B, 0x2E
    .equ OCR0A, 0x36
    .equ OCR0B, 0x3C
    .equ TCNT0, 0x32
    .equ TCNT1H, 0x2D
    .equ TCNT1L, 0x2C
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
    ;; Сохраняем регистры
    push    tmp0
    push    tmp1
    ;; Сохраняем регистр флагов
    in      tmp0, SREG
    push    tmp0
    ;; Сохраняем индексные регистры
    push    xl
    push    xh
    push    yl
    push    yh
    push    zl
    push    zh

    ;; [[[=== Высота звука ===]]]
    ;; Уменьшаем freq для частоты buzzer-а
    dec     freq
;;     cpi     freq, END_FREQ
;;     brsh    1f                  ; Перейти если больше или равно
;;     ldi     freq, START_FREQ
;; 1:

    ;; [[[=== Мигание зеленым светодиодом ===]]]
    ;; Переключим состояние зеленого светодиода
    rcall   _blink_green



    ;; Ускорим процесс:
    ;; Set TCNT1 to 0xE000
    ldi     tmp0, 0x00
    ldi     tmp1, 0xE0
    out     TCNT1H, tmp1
    out     TCNT1L, tmp0

_timer_0_overflow_ret:
    ;; Восстанавливаем индексные регистры
    pop     zh
    pop     zl
    pop     yh
    pop     yl
    pop     xh
    pop     xl
    ;; Восстанавливаем регистр флагов
    pop     tmp0
    out     SREG, tmp0
    ;; Восстанавливем регистры
    pop     tmp1
    pop     tmp0
    ;; Выходим
    reti

_timer_0_compare_A:
    ;; Записываем freq в регистр сравнения
    out     OCR0A, freq
    reti

_reset:
_init:
    ;; Выделяем регистр r0 под значение нуля и r1 под значение единицы
    clr     r0
    mov     r1, r0
    inc     r1

    ;; Запретить прерывания
    out     SREG, r0

    ;; Настроить Stack
    ldi     tmp0, RAMEND
    out     SPL, tmp0

    ;; Инициализируем выводы
_init_pins:
    ;; Настроить PB7:PB0 на выход
    ;;              76543210
    ldi     tmp0, 0b11111111
    out     DDRB, tmp0
    ;; Настроить на выход
    ;; BLUE:pd1, WHITE:pd0,
    ;; OUT-IP:pd4, OUT-PP:pd5
    ;; OUT-PM:pd6
    ;; остальные - на вход
    ;;              76543210
    ldi     tmp0, 0b01110011
    out     DDRD, tmp0
    ;; Настроить A1


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
    out TCNT0, r0
    ;; OCR0A & OCR0B
    ldi tmp0, 0xFF
    out OCR0A, tmp0
    ;; Очищаем флаги прерывания таймера
    out TIFR, r0

    ;; Настройка прерываний таймеров
    ;; TOEI1(ovfl-1) & OCIE0A(cmpA-0)
    ldi     tmp0, 0b10000001
    out     TIMSK, tmp0

    ;; Начальное значение
    ldi     freq, START_FREQ

    ;; Разрешить прерывания
    sei


    ;; DISABLE TIMER-0
    ;; Временно остановим таймер-0 чтобы не щелкал
    out TCCR0B, r1

_mainloop:
    rjmp    _mainloop

_infloop:
    rjmp    _infloop

_blink_green:
    sbic    PORTB, 1
    rjmp    _bg_clean
    sbi     PORTB, 1
_bg_ret:
    ret
_bg_clean:
    cbi     PORTB, 1
    rjmp    _bg_ret
