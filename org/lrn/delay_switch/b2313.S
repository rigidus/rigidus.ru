;;; b2313 delay switch for 4 buttons
    #define TRUE 1
    #define FALSE 0
    #define HIGH 1
    #define LOW 0
    #define MAX 5
    #define result r16
    #define tmp0 r17
    #define tmp1 r18
    #define cnt  r19

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

    .macro  IFFALSE to
        cpi     result, FALSE
        breq    \to
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
    in      tmp1, SREG
    push    tmp1

    ;; [[[=== Высота звука ===]]]
    ;; Уменьшаем cnt для частоты buzzer-а
    dec     cnt

    ;; [[[=== Мигание зеленым светодиодом ===]]]
    ;; Переключим состояние зеленого светодиода
    rcall   _blink_green

    ;; [[[=== Защита от дребезга ===]]]
    ;; Перед вызовом конечного автомата
    ;; Сохраняем индексный регистр X
    push    xl
    push    xh
    ;; Заготовка чтобы передавать адрес
    ldi     xl, lo8(STATE)
    ldi     xh, hi8(STATE)
    ;; Вызываем конечный автомат подавления дребезга
    rcall   _fsmbtn
    ;; После вызова конечного автомата
    ;; Восстанавливаем индексный регистр X
    pop     xh
    pop     xl

    ;; [[[=== Обработка нажатий клавиш ===]]]
    ;; Читаем вывод PB0
    in      tmp0, PINB
    ldi     tmp1, 0b00000001
    and     tmp0, tmp1
    ;; Есть ли нажатие?
    breq    _not_press ; Перейти если ноль
    ;; Если нажатие зарегистрировано
    ;; rcall   _yellow_on
    ldi     tmp0, HIGH
    sts     NEW, tmp0
    rjmp    _timer_0_overflow_ret
_not_press:
    ;; В противном случае
    ;; rcall   _yellow_off
    ldi     tmp0, LOW
    sts     NEW, tmp0

_timer_0_overflow_ret:
    ;; Восстанавливаем регистр флагов
    pop     tmp1
    out     SREG, tmp1
    ;; Восстанавливем регистры
    pop     tmp1
    pop     tmp0
    ;; Выходим
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
    ldi     tmp0, 0b00011110;;<--pb0 - вход
    ;;                  ^ ^
    ;;                  | |
    ;;                  | +PB1
    ;;                  +PB3
    out     DDRB, tmp0
    
    ;; Для пинов подключенных на вход (кнопки)
    ;; установить подтяжку к питанию (записав 1) чтобы не было hi-z
    ;; Однако их надо подтянуть к минусу питания (!)
    ;; PB4 и PB3 подтягиваем к земле, потому что начальное
    ;; его состояние - выключен
    ;; PB2(0C0A) - тоже к земле, потому что он выход
    ldi     tmp0, 0b00000001
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

    ;; Устанавливаем начальные значения
    ldi     tmp0, 1
    sts     STATE, tmp0

    ldi     tmp0, LOW
    sts     NEW, tmp0

    ldi     tmp0, 0
    sts     CNT, tmp0

    rcall   _red_off
    rcall   _yellow_off

_mainloop:
    lds     tmp0, STATE
    ldi     tmp1, 1             ; Проверяем на состояние
    cp      tmp0, tmp1
    brne    _not_one            ;--+
    rcall   _red_on             ;  |
    rcall   _yellow_off         ;  |
    rjmp    _end                ;--|-+
_not_one:                       ;<-+ |
    ldi     tmp1, 2             ;    |
    cp      tmp0, tmp1          ;    |
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
    push    tmp1
    rcall   _next_205
_next_205:
    pop     r25
    pop     r24
    clr     r23
    ldi     r22, 11
    add     r24, r22
    adc     r25, r23
    ld      r22, X
    add     r24, r22
    adc     r25, r23
    push    r24
    push    r25
    ret 
    rjmp    _label_case_1207
    rjmp    _label_case_2223
    rjmp    _label_case_3258
_label_case_1207:
    ;; progn-open
    lds     r25, NEW
    ldi     r24, LOW
    cp      r25, r24
    brne    _not_equal_eq2_212
    ldi     result, TRUE
    rjmp    _end_eq2_213
_not_equal_eq2_212:
    ldi     result, FALSE
_end_eq2_213:
    IFFALSE _when_false_ret_208
    ldi     r24, 1
    st      X, r24
    rjmp    _when_end_209
_when_false_ret_208:
    ldi     result, FALSE
_when_end_209:
    lds     r24, NEW
    ldi     r25, HIGH
    cp      r24, r25
    brne    _not_equal_eq2_219
    ldi     result, TRUE
    rjmp    _end_eq2_220
_not_equal_eq2_219:
    ldi     result, FALSE
_end_eq2_220:
    IFFALSE _when_false_ret_215
    ldi     r25, 0
    sts     CNT, r25
    ldi     r25, 2
    st      X, r25
    rjmp    _when_end_216
_when_false_ret_215:
    ldi     result, FALSE
_when_end_216:
    ;; progn-close
    rjmp    _end_case_206
_label_case_2223:
    ;; progn-open
    lds     r25, NEW
    ldi     r24, LOW
    cp      r25, r24
    brne    _not_equal_eq2_228
    ldi     result, TRUE
    rjmp    _end_eq2_229
_not_equal_eq2_228:
    ldi     result, FALSE
_end_eq2_229:
    IFFALSE _when_false_ret_224
    ldi     r24, 1
    st      X, r24
    rjmp    _when_end_225
_when_false_ret_224:
    ldi     result, FALSE
_when_end_225:
    lds     r24, NEW
    ldi     r25, HIGH
    cp      r24, r25
    brne    _not_equal_eq2_237
    ldi     result, TRUE
    rjmp    _end_eq2_238
_not_equal_eq2_237:
    ldi     result, FALSE
_end_eq2_238:
    IFFALSE _and_false_ret_233
    lds     r25, CNT
    ldi     r24, MAX
    cp      r25, r24
    brsh    _not_lt_eq2_241
    ldi     result, TRUE
    rjmp    _end_lt2_242
_not_lt_eq2_241:
    ldi     result, FALSE
_end_lt2_242:
    IFFALSE _and_false_ret_233
    ldi     result, TRUE
    rjmp    _and_end_234
_and_false_ret_233:
    ldi     result, FALSE
_and_end_234:
    IFFALSE _when_false_ret_231
    lds     r24, CNT
    inc     r24
    sts     CNT, r24
    ldi     r24, 2
    st      X, r24
    rjmp    _when_end_232
_when_false_ret_231:
    ldi     result, FALSE
_when_end_232:
    lds     r24, NEW
    ldi     r25, HIGH
    cp      r24, r25
    brne    _not_equal_eq2_251
    ldi     result, TRUE
    rjmp    _end_eq2_252
_not_equal_eq2_251:
    ldi     result, FALSE
_end_eq2_252:
    IFFALSE _and_false_ret_247
    lds     r25, CNT
    ldi     r24, MAX
    cp      r25, r24
    brlo    _not_ge_eq2_255
    ldi     result, TRUE
    rjmp    _end_ge2_256
_not_ge_eq2_255:
    ldi     result, FALSE
_end_ge2_256:
    IFFALSE _and_false_ret_247
    ldi     result, TRUE
    rjmp    _and_end_248
_and_false_ret_247:
    ldi     result, FALSE
_and_end_248:
    IFFALSE _when_false_ret_245
    ldi     r24, 3
    st      X, r24
    rjmp    _when_end_246
_when_false_ret_245:
    ldi     result, FALSE
_when_end_246:
    ;; progn-close
    rjmp    _end_case_206
_label_case_3258:
    ;; progn-open
    lds     r24, NEW
    ldi     r25, HIGH
    cp      r24, r25
    brne    _not_equal_eq2_263
    ldi     result, TRUE
    rjmp    _end_eq2_264
_not_equal_eq2_263:
    ldi     result, FALSE
_end_eq2_264:
    IFFALSE _when_false_ret_259
    ldi     r25, 3
    st      X, r25
    rjmp    _when_end_260
_when_false_ret_259:
    ldi     result, FALSE
_when_end_260:
    lds     r25, NEW
    ldi     r24, LOW
    cp      r25, r24
    brne    _not_equal_eq2_270
    ldi     result, TRUE
    rjmp    _end_eq2_271
_not_equal_eq2_270:
    ldi     result, FALSE
_end_eq2_271:
    IFFALSE _when_false_ret_266
    ldi     r24, 1
    st      X, r24
    rjmp    _when_end_267
_when_false_ret_266:
    ldi     result, FALSE
_when_end_267:
    ;; progn-close
    rjmp    _end_case_206
_end_case_206:

    pop     tmp1
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

    .data

STATE:
    .byte 0x1
NEW:
    .byte 0x0
CNT:
    .byte 0x0
