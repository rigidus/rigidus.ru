;;; b2313 delay switch for 4 buttons
    #define TRUE 1
    #define FALSE 0
    #define HIGH 1
    #define LOW 0
    #define MAX 2
    #define DELAY_MAX 100
    #define result r16
    #define tmp0 r17
    #define tmp1 r18
    #define freq r19

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

    .macro IFFALSE to
        cp      result, r0
        breq    \to
    .endm
    
    .macro SETRESULT val
        .ifc FALSE,\val
        mov     result, r0
        .else
        mov     result, r1
        .endif
    .endm
    
    .macro ON port pin
        sbi     \port, \pin
    .endm
    
    .macro OFF port pin
        cbi     \port, \pin
    .endm
    
    .macro RED_ON
        ON      PORTB, 3
    .endm
    
    .macro RED_OFF
        OFF     PORTB, 3
    .endm
    
    .macro YELLOW_ON
        ON      PORTB, 1
    .endm
    
    .macro YELLOW_OFF
        OFF     PORTB, 1
    .endm

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

    ;; [[[=== Мигание зеленым светодиодом ===]]]
    ;; Переключим состояние зеленого светодиода
    rcall   _blink_green


    ;; [[[==== Получаем состояние вывода в NEW_@ ===]]]
    sbis    PINB, 0
    rjmp    _not_press_IP
    sts     NEW_IP, r1
    rjmp    _end_press_handler_IP
_not_press_IP:
    sts     NEW_IP, r0
_end_press_handler_IP:

    sbis    PINA, 1
    rjmp    _not_press_SP
    sts     NEW_SP, r1
    rjmp    _end_press_handler_SP
_not_press_SP:
    sts     NEW_SP, r0
_end_press_handler_SP:

    sbis    PINA, 0
    rjmp    _not_press_PP
    sts     NEW_PP, r1
    rjmp    _end_press_handler_PP
_not_press_PP:
    sts     NEW_PP, r0
_end_press_handler_PP:

    sbis    PIND, 2
    rjmp    _not_press_PM
    sts     NEW_PM, r1
    rjmp    _end_press_handler_PM
_not_press_PM:
    sts     NEW_PM, r0
_end_press_handler_PM:


    ldi     xl, lo8(STATE_IP)
    ldi     xh, hi8(STATE_IP)
    ldi     yl, lo8(NEW_IP)
    ldi     yh, hi8(NEW_IP)
    ldi     zl, lo8(CNT_IP)
    ldi     zh, hi8(CNT_IP)
    rcall   _fsmbtn

    ldi     xl, lo8(STATE_SP)
    ldi     xh, hi8(STATE_SP)
    ldi     yl, lo8(NEW_SP)
    ldi     yh, hi8(NEW_SP)
    ldi     zl, lo8(CNT_SP)
    ldi     zh, hi8(CNT_SP)
    rcall   _fsmbtn

    ldi     xl, lo8(STATE_PP)
    ldi     xh, hi8(STATE_PP)
    ldi     yl, lo8(NEW_PP)
    ldi     yh, hi8(NEW_PP)
    ldi     zl, lo8(CNT_PP)
    ldi     zh, hi8(CNT_PP)
    rcall   _fsmbtn

    ldi     xl, lo8(STATE_PM)
    ldi     xh, hi8(STATE_PM)
    ldi     yl, lo8(NEW_PM)
    ldi     yh, hi8(NEW_PM)
    ldi     zl, lo8(CNT_PM)
    ldi     zh, hi8(CNT_PM)
    rcall   _fsmbtn


    ;; Передаем адрес переменной состояния нового автомата
    ldi     xl, lo8(MODE_IP)
    ldi     xh, hi8(MODE_IP)
    ;; Передаем адрес переменной связи автоматов
    ldi     yl, lo8(LINK_IP)
    ldi     yh, hi8(LINK_IP)
    ;; Передаем адрес переменной задержки
    ldi     zl, lo8(DELAY_IP)
    ldi     zh, hi8(DELAY_IP)
    ;; Вызываем конечный автомат обработки интервалов
    rcall   _fsmprg
    ;; Увеличиваем счетчик задержки
    lds     tmp0, DELAY_IP
    inc     tmp0
    sts     DELAY_IP, tmp0
    ;; Проверяем выход
    lds     tmp0, MODE_IP
    cp      tmp0, r0
    breq    _switch_off_IP
    ON PORTD 6
    rjmp    _switch_end_IP
_switch_off_IP:
    OFF PORTD 6
_switch_end_IP:
    ;; Очищаем SWITCH и LINK
    sts     SWITCH_IP, r0
    sts     LINK_IP, r0

    ;; Передаем адрес переменной состояния нового автомата
    ldi     xl, lo8(MODE_SP)
    ldi     xh, hi8(MODE_SP)
    ;; Передаем адрес переменной связи автоматов
    ldi     yl, lo8(LINK_SP)
    ldi     yh, hi8(LINK_SP)
    ;; Передаем адрес переменной задержки
    ldi     zl, lo8(DELAY_SP)
    ldi     zh, hi8(DELAY_SP)
    ;; Вызываем конечный автомат обработки интервалов
    rcall   _fsmprg
    ;; Увеличиваем счетчик задержки
    lds     tmp0, DELAY_SP
    inc     tmp0
    sts     DELAY_SP, tmp0
    ;; Проверяем выход
    lds     tmp0, MODE_SP
    cp      tmp0, r0
    breq    _switch_off_SP
    ON PORTD 5
    rjmp    _switch_end_SP
_switch_off_SP:
    OFF PORTD 5
_switch_end_SP:
    ;; Очищаем SWITCH и LINK
    sts     SWITCH_SP, r0
    sts     LINK_SP, r0

    ;; Передаем адрес переменной состояния нового автомата
    ldi     xl, lo8(MODE_PP)
    ldi     xh, hi8(MODE_PP)
    ;; Передаем адрес переменной связи автоматов
    ldi     yl, lo8(LINK_PP)
    ldi     yh, hi8(LINK_PP)
    ;; Передаем адрес переменной задержки
    ldi     zl, lo8(DELAY_PP)
    ldi     zh, hi8(DELAY_PP)
    ;; Вызываем конечный автомат обработки интервалов
    rcall   _fsmprg
    ;; Увеличиваем счетчик задержки
    lds     tmp0, DELAY_PP
    inc     tmp0
    sts     DELAY_PP, tmp0
    ;; Проверяем выход
    lds     tmp0, MODE_PP
    cp      tmp0, r0
    breq    _switch_off_PP
    ON PORTD 4
    rjmp    _switch_end_PP
_switch_off_PP:
    OFF PORTD 4
_switch_end_PP:
    ;; Очищаем SWITCH и LINK
    sts     SWITCH_PP, r0
    sts     LINK_PP, r0

    ;; Передаем адрес переменной состояния нового автомата
    ldi     xl, lo8(MODE_PM)
    ldi     xh, hi8(MODE_PM)
    ;; Передаем адрес переменной связи автоматов
    ldi     yl, lo8(LINK_PM)
    ldi     yh, hi8(LINK_PM)
    ;; Передаем адрес переменной задержки
    ldi     zl, lo8(DELAY_PM)
    ldi     zh, hi8(DELAY_PM)
    ;; Вызываем конечный автомат обработки интервалов
    rcall   _fsmprg
    ;; Увеличиваем счетчик задержки
    lds     tmp0, DELAY_PM
    inc     tmp0
    sts     DELAY_PM, tmp0
    ;; Проверяем выход
    lds     tmp0, MODE_PM
    cp      tmp0, r0
    breq    _switch_off_PM
    ON PORTD 3
    rjmp    _switch_end_PM
_switch_off_PM:
    OFF PORTD 3
_switch_end_PM:
    ;; Очищаем SWITCH и LINK
    sts     SWITCH_PM, r0
    sts     LINK_PM, r0


    ;; Ускорим процесс:
    ;; Set TCNT1 to 0x00E0
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
    ;; Настроить PB4(blink) и PB2(OC0A) на выход, остальные на вход
    ;;                 |          |
    ;;                 | +--------+
    ;;                 | |
    ;;                 v v
    ldi     tmp0, 0b00011110;;<--pb0 - вход
    ;;                  ^ ^
    ;;                  | |
    ;;                  | +PB1
    ;;                  +PB3
    out     DDRB, tmp0

    .data

SWITCH_IP:
    .byte 0x0
LINK_IP:
    .byte 0x0
NEW_IP:
    .byte 0x0
STATE_IP:
    .byte 0x0
CNT_IP:
    .byte 0x0
MODE_IP:
    .byte 0x0
DELAY_IP:
    .byte 0x0

    .text

_init_IP:
    cbi    DDRB, 0

    .data

SWITCH_SP:
    .byte 0x0
LINK_SP:
    .byte 0x0
NEW_SP:
    .byte 0x0
STATE_SP:
    .byte 0x0
CNT_SP:
    .byte 0x0
MODE_SP:
    .byte 0x0
DELAY_SP:
    .byte 0x0

    .text

_init_SP:
    cbi    DDRA, 1

    .data

SWITCH_PP:
    .byte 0x0
LINK_PP:
    .byte 0x0
NEW_PP:
    .byte 0x0
STATE_PP:
    .byte 0x0
CNT_PP:
    .byte 0x0
MODE_PP:
    .byte 0x0
DELAY_PP:
    .byte 0x0

    .text

_init_PP:
    cbi    DDRA, 0

    .data

SWITCH_PM:
    .byte 0x0
LINK_PM:
    .byte 0x0
NEW_PM:
    .byte 0x0
STATE_PM:
    .byte 0x0
CNT_PM:
    .byte 0x0
MODE_PM:
    .byte 0x0
DELAY_PM:
    .byte 0x0

    .text

_init_PM:
    cbi    DDRD, 2


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

    ;; Разрешить прерывания
    sei

    ;; DISABLE TIMER-0
    ;; Временно остановим таймер-0 чтобы не щелкал
    out TCCR0B, r0

    YELLOW_OFF

_mainloop:
    rjmp    _mainloop

_fsmbtn:
    rcall   _case_next_204
_case_next_204:
    pop     r25
    pop     r24
    ldi     r23, 11
    add     r24, r23
    adc     r25, r0
    ld      r23, X
    add     r24, r23
    adc     r25, r0
    push    r24
    push    r25
    ret 
    rjmp    _label_case_0206
    rjmp    _label_case_1217
    rjmp    _label_case_2244
_label_case_0206:
    ;; progn-open
    ld      r25, Y
    cp      r25, r0
    brne    _equal_ind2const_not_209
    SETRESULT TRUE
    rjmp    _equal_ind2const_end_210
_equal_ind2const_not_209:
    SETRESULT FALSE
_equal_ind2const_end_210:
    IFFALSE _when_end_207
    ldi     r25, 0
    st      X, r25
_when_end_207:
    ld      r25, Y
    cp      r25, r1
    brne    _equal_ind2const_not_214
    SETRESULT TRUE
    rjmp    _equal_ind2const_end_215
_equal_ind2const_not_214:
    SETRESULT FALSE
_equal_ind2const_end_215:
    IFFALSE _when_end_212
    st      Z, r0
    ldi     r25, 1
    st      X, r25
_when_end_212:
    ;; progn-close
    rjmp    _case_end_205
_label_case_1217:
    ;; progn-open
    ld      r25, Y
    cp      r25, r0
    brne    _equal_ind2const_not_220
    SETRESULT TRUE
    rjmp    _equal_ind2const_end_221
_equal_ind2const_not_220:
    SETRESULT FALSE
_equal_ind2const_end_221:
    IFFALSE _when_end_218
    ldi     r25, 0
    st      X, r25
_when_end_218:
    ld      r25, Y
    cp      r25, r1
    brne    _equal_ind2const_not_226
    SETRESULT TRUE
    rjmp    _equal_ind2const_end_227
_equal_ind2const_not_226:
    SETRESULT FALSE
_equal_ind2const_end_227:
    IFFALSE _and_end_224
    ld      r25, Z
    ldi     r24, MAX
    cp      r25, r24
    brsh    _lt_ind2const_not_230
    SETRESULT TRUE
    rjmp    _lt_ind2const_end_231
_lt_ind2const_not_230:
    SETRESULT FALSE
_lt_ind2const_end_231:
    IFFALSE _and_end_224
    SETRESULT TRUE
_and_end_224:
    IFFALSE _when_end_223
    ld      r24, Z
    inc     r24
    st      Z, r24
    ldi     r24, 1
    st      X, r24
_when_end_223:
    ld      r24, Y
    cp      r24, r1
    brne    _equal_ind2const_not_237
    SETRESULT TRUE
    rjmp    _equal_ind2const_end_238
_equal_ind2const_not_237:
    SETRESULT FALSE
_equal_ind2const_end_238:
    IFFALSE _and_end_235
    ld      r24, Z
    ldi     r25, MAX
    cp      r24, r25
    brlo    _ge_ind2const_not_241
    SETRESULT TRUE
    rjmp    _ge_ind2const_end_242
_ge_ind2const_not_241:
    SETRESULT FALSE
_ge_ind2const_end_242:
    IFFALSE _and_end_235
    SETRESULT TRUE
_and_end_235:
    IFFALSE _when_end_234
    st      -Y, r1
    ldi     r25, 2
    st      X, r25
_when_end_234:
    ;; progn-close
    rjmp    _case_end_205
_label_case_2244:
    ;; progn-open
    ld      r25, Y
    cp      r25, r1
    brne    _equal_ind2const_not_247
    SETRESULT TRUE
    rjmp    _equal_ind2const_end_248
_equal_ind2const_not_247:
    SETRESULT FALSE
_equal_ind2const_end_248:
    IFFALSE _when_end_245
    ldi     r25, 2
    st      X, r25
_when_end_245:
    ld      r25, Y
    cp      r25, r0
    brne    _equal_ind2const_not_252
    SETRESULT TRUE
    rjmp    _equal_ind2const_end_253
_equal_ind2const_not_252:
    SETRESULT FALSE
_equal_ind2const_end_253:
    IFFALSE _when_end_250
    ldi     r25, 0
    st      X, r25
_when_end_250:
    ;; progn-close
    rjmp    _case_end_205
_case_end_205:

    ret

_fsmprg:
    rcall   _case_next_404
_case_next_404:
    pop     r25
    pop     r24
    ldi     r23, 11
    add     r24, r23
    adc     r25, r0
    ld      r23, X
    add     r24, r23
    adc     r25, r0
    push    r24
    push    r25
    ret 
    rjmp    _label_case_0406
    rjmp    _label_case_1412
_label_case_0406:
    ;; progn-open
    ld      r25, Y
    cp      r25, r1
    brne    _equal_ind2const_not_409
    SETRESULT TRUE
    rjmp    _equal_ind2const_end_410
_equal_ind2const_not_409:
    SETRESULT FALSE
_equal_ind2const_end_410:
    IFFALSE _when_end_407
    st      Z, r0
    ldi     r25, 1
    st      X, r25
_when_end_407:
    ;; progn-close
    rjmp    _case_end_405
_label_case_1412:
    ;; progn-open
    ld      r25, Y
    cp      r25, r1
    brne    _equal_ind2const_not_415
    SETRESULT TRUE
    rjmp    _equal_ind2const_end_416
_equal_ind2const_not_415:
    SETRESULT FALSE
_equal_ind2const_end_416:
    IFFALSE _when_end_413
    ldi     r25, 0
    st      X, r25
_when_end_413:
    ld      r25, Z
    ldi     r24, DELAY_MAX
    cp      r25, r24
    brlo    _ge_ind2const_not_421
    SETRESULT TRUE
    rjmp    _ge_ind2const_end_422
_ge_ind2const_not_421:
    SETRESULT FALSE
_ge_ind2const_end_422:
    IFFALSE _when_end_418
    ldi     r24, 0
    st      X, r24
_when_end_418:
    ;; progn-close
    rjmp    _case_end_405
_case_end_405:

    ret

_infloop:
    rjmp    _infloop

_blink_green:
    sbic    PORTB, 4
    rjmp    _bg_clean
    sbi     PORTB, 4
_bg_ret:
    ret
_bg_clean:
    cbi     PORTB, 4
    rjmp    _bg_ret
