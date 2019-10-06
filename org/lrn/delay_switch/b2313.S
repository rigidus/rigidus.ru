;;; b2313 delay switch for 4 buttons
    #define TRUE 1
    #define FALSE 0
    #define HIGH 1
    #define LOW 0
    #define MAX 5
    #define result r17
    #define tmp0 r18
    #define freq r19

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
    ;; Сохраняем регистр флагов
    in      tmp0, SREG
    push    tmp0


    ;; [[[=== Высота звука ===]]]
    ;; Уменьшаем freq для частоты buzzer-а
    dec     freq


    ;; [[[=== Мигание зеленым светодиодом ===]]]
    ;; Переключим состояние зеленого светодиода
    rcall   _blink_green


    ;; [[[==== Получаем состояние вывода в NEW_IP ===]]]
    sbis    PINB, 0
    rjmp    _not_press_IP
    sts     NEW_IP, r1
    rjmp    _end_press_handler_IP
_not_press_IP:
    sts     NEW_IP, r0
_end_press_handler_IP:


    ;; [[[=== Защита от дребезга ===]]]
    ;; На входе: STATE, CNT, NEW
    ;; На выходе: STATE
    ;; Перед вызовом конечного автомата
    ;; Сохраняем индексные регистры
    push    xl
    push    xh
    push    yl
    push    yh
    push    zl
    push    zh
    ;; Передаем адрес переменной состояния
    ldi     xl, lo8(STATE_IP)
    ldi     xh, hi8(STATE_IP)
    ;; Передаем адрес переменной сигнала
    ldi     yl, lo8(NEW_IP)
    ldi     yh, hi8(NEW_IP)
    ;; Передаем адрес переменной счетчика сигналов
    ldi     zl, lo8(CNT_IP)
    ldi     zh, hi8(CNT_IP)
    ;; Вызываем конечный автомат подавления дребезга
    rcall   _fsmbtn
    ;; После вызова конечного автомата
    ;; Восстанавливаем индексные регистры
    pop     zh
    pop     zl
    pop     yh
    pop     yl
    pop     xh
    pop     xl

    ;; Здесь у нас есть STATE, но нам нужно отслеживать
    ;; изменение STATE. Поэтому нам нужно PREV_STATE
    ;; Тогда мы можем переключать другой автомат когда
    ;; STATE меняется.
    ;; Теперь если PREV_STATE и STATE не совпадают -
    ;; мы можем выполнить какие-то действия (переключить
    ;; состояние, выставить задержку итп)

_timer_0_overflow_ret:
    ;; Восстанавливаем регистр флагов
    pop     tmp0
    out     SREG, tmp0
    ;; Восстанавливем регистры
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

STATE_IP:
    .byte 0x0
NEW_IP:
    .byte 0x0
CNT_IP:
    .byte 0x0

    .text

_init_IP:
    cbi    DDRB, 0



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

    rcall   _red_off
    rcall   _yellow_off

_mainloop:
    lds     tmp0, STATE_IP
    cpi     tmp0, 0             ; Проверяем на состояние
    brne    _not_one            ;--+
    rcall   _red_on             ;  |
    rcall   _yellow_off         ;  |
    rjmp    _end                ;--|-+
_not_one:                       ;<-+ |
    cpi     tmp0, 1             ;    |
    brne    _not_two            ;--+ |
    rcall   _red_off            ;  | |
    rcall   _yellow_on          ;  | |
    rjmp    _end                ;--|-+
_not_two:                       ;<-+ |
    rcall   _red_on             ;    |
    rcall   _yellow_on          ;    |
_end:                           ;<---+
    rjmp    _mainloop

_fsmbtn:
    push    tmp0
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

    pop     tmp0
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

_red_on:
    sbi     PORTB, 3
    ret
_red_off:
    cbi     PORTB, 3
    ret
_yellow_on:
    sbi     PORTB, 1
    ret
_yellow_off:
    cbi     PORTB, 1
    ret
