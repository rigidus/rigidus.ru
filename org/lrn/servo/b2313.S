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
    .equ ICR1L, 0x24
    .equ ICR1H, 0x25
    .equ TCCR1A, 0x2F
    .equ OCR1AH, 0x2B
    .equ OCR1AL, 0x2A

    

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
    rjmp    _infloop            ; Timer0 Overflow Handler
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

_timer_0_compare_A:
    

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
    ;; Fast PWM с очисткой по достижении значения регистра сравнения
    ;; WGM = 1110
    ldi     tmp0, 0b10000010
    out     TCCR1A, tmp0
    ;; Выставляем предделитель
    ldi     tmp0, 0b00011010
    out     TCCR1B, tmp0
    ldi     tmp0, hi8(2499)
    out     ICR1H, tmp0
    ldi     tmp0, lo8(2499)
    out     ICR1L, tmp0
    ; Частота ШИМ считается как Fpwm = Fcpu / (N*(1+TOP)) = 8000000 / (64*(1+2499)) = 50 Гц
    ; Устанавливаем начальный коэффициент заполнения - 50%
    LDI 	tmp0, hi8(165)
    OUT 	OCR1AH, tmp0
    LDI 	tmp0, lo8(165)
    OUT 	OCR1AL, tmp0
      ; Обнуление счетчика таймера 1
      OUT 	TCNT1H, r0
      OUT 	TCNT1L, r0

    ;; Инициализация таймера-0
    

    ;; Настройка прерываний таймеров
    ;; TOEI1(ovfl-1) & OCIE0A(cmpA-0)
    ldi     tmp0, 0b00000000
    out     TIMSK, tmp0

    ;; Начальное значение
    ldi     freq, START_FREQ

    ;; Разрешить прерывания
    sei


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
