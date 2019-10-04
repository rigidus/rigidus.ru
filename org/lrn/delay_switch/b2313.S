;;; b2313 delay switch for 4 buttons
    #define TRUE 1
    #define FALSE 0
    #define HIGH 1
    #define LOW 0
    #define MAX 5
    #define result r16
    #define tmp0 r17
    #define tmp1 r18
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
    in      tmp0, SREG
    push    tmp0


    ;; [[[=== Высота звука ===]]]
    ;; Уменьшаем freq для частоты buzzer-а
    dec     freq


    ;; [[[=== Мигание зеленым светодиодом ===]]]
    ;; Переключим состояние зеленого светодиода
    rcall   _blink_green

    sbis    PINB, 0
    rjmp    _not_press_IP
    sts     NEW_IP, r2
    rjmp    _end_press_handler_IP
_not_press_IP:
    sts     NEW_IP, r1
_end_press_handler_IP:


    ;; [[[=== Защита от дребезга ===]]]
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



_timer_0_overflow_ret:
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
    ;; Выделяем регистр r1 под значение нуля и r2 под значение единицы
    clr     r1
    mov     r2, r1
    inc     r2
    
    ;; Запретить прерывания
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
    
    .data
    
    STATE_IP:
        .byte 0x1
    NEW_IP:
        .byte 0x0
    CNT_IP:
        .byte 0x0
    
    .text
    
    _init_IP:
       cbi    DDRB, 0
       sts    STATE_IP, r2
    
    
    
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
    out TCCR0B, r1

    rcall   _red_off
    rcall   _yellow_off

_mainloop:
    lds     tmp0, STATE_IP
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
    rjmp    _label_case_2221
    rjmp    _label_case_3253
_label_case_1207:
    ;; progn-open
    ld      r25, Y
    cp      r25, r1
    brne    _not_equal_eq2_211
    ldi     result, TRUE
    rjmp    _end_eq2_212
_not_equal_eq2_211:
    ldi     result, FALSE
_end_eq2_212:
    IFFALSE _when_false_ret_208
    ldi     r25, 1
    st      X, r25
    rjmp    _when_end_209
_when_false_ret_208:
    ldi     result, FALSE
_when_end_209:
    ld      r25, Y
    cp      r25, r2
    brne    _not_equal_eq2_217
    ldi     result, TRUE
    rjmp    _end_eq2_218
_not_equal_eq2_217:
    ldi     result, FALSE
_end_eq2_218:
    IFFALSE _when_false_ret_214
    ldi     r25, 0
    sts     CNT_IP, r25
    ldi     r25, 2
    st      X, r25
    rjmp    _when_end_215
_when_false_ret_214:
    ldi     result, FALSE
_when_end_215:
    ;; progn-close
    rjmp    _end_case_206
_label_case_2221:
    ;; progn-open
    ld      r25, Y
    cp      r25, r1
    brne    _not_equal_eq2_225
    ldi     result, TRUE
    rjmp    _end_eq2_226
_not_equal_eq2_225:
    ldi     result, FALSE
_end_eq2_226:
    IFFALSE _when_false_ret_222
    ldi     r25, 1
    st      X, r25
    rjmp    _when_end_223
_when_false_ret_222:
    ldi     result, FALSE
_when_end_223:
    ld      r25, Y
    cp      r25, r2
    brne    _not_equal_eq2_233
    ldi     result, TRUE
    rjmp    _end_eq2_234
_not_equal_eq2_233:
    ldi     result, FALSE
_end_eq2_234:
    IFFALSE _and_false_ret_230
    lds     r25, CNT_IP
    ldi     r24, MAX
    cp      r25, r24
    brsh    _not_lt_eq2_237
    ldi     result, TRUE
    rjmp    _end_lt2_238
_not_lt_eq2_237:
    ldi     result, FALSE
_end_lt2_238:
    IFFALSE _and_false_ret_230
    ldi     result, TRUE
    rjmp    _and_end_231
_and_false_ret_230:
    ldi     result, FALSE
_and_end_231:
    IFFALSE _when_false_ret_228
    lds     r24, CNT_IP
    inc     r24
    sts     CNT_IP, r24
    ldi     r24, 2
    st      X, r24
    rjmp    _when_end_229
_when_false_ret_228:
    ldi     result, FALSE
_when_end_229:
    ld      r24, Y
    cp      r24, r2
    brne    _not_equal_eq2_246
    ldi     result, TRUE
    rjmp    _end_eq2_247
_not_equal_eq2_246:
    ldi     result, FALSE
_end_eq2_247:
    IFFALSE _and_false_ret_243
    lds     r24, CNT_IP
    ldi     r25, MAX
    cp      r24, r25
    brlo    _not_ge_eq2_250
    ldi     result, TRUE
    rjmp    _end_ge2_251
_not_ge_eq2_250:
    ldi     result, FALSE
_end_ge2_251:
    IFFALSE _and_false_ret_243
    ldi     result, TRUE
    rjmp    _and_end_244
_and_false_ret_243:
    ldi     result, FALSE
_and_end_244:
    IFFALSE _when_false_ret_241
    ldi     r25, 3
    st      X, r25
    rjmp    _when_end_242
_when_false_ret_241:
    ldi     result, FALSE
_when_end_242:
    ;; progn-close
    rjmp    _end_case_206
_label_case_3253:
    ;; progn-open
    ld      r25, Y
    cp      r25, r2
    brne    _not_equal_eq2_257
    ldi     result, TRUE
    rjmp    _end_eq2_258
_not_equal_eq2_257:
    ldi     result, FALSE
_end_eq2_258:
    IFFALSE _when_false_ret_254
    ldi     r25, 3
    st      X, r25
    rjmp    _when_end_255
_when_false_ret_254:
    ldi     result, FALSE
_when_end_255:
    ld      r25, Y
    cp      r25, r1
    brne    _not_equal_eq2_263
    ldi     result, TRUE
    rjmp    _end_eq2_264
_not_equal_eq2_263:
    ldi     result, FALSE
_end_eq2_264:
    IFFALSE _when_false_ret_260
    ldi     r25, 1
    st      X, r25
    rjmp    _when_end_261
_when_false_ret_260:
    ldi     result, FALSE
_when_end_261:
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
