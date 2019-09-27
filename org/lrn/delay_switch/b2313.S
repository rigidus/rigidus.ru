;;; b2313 delay switch for 4 buttons
    #define TRUE 1
    #define FALSE 0
    #define HIGH 1
    #define LOW 0
    #define tmp0 r16
    #define tmp1 r17
    #define cnt  r18
    #define result r16

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

    rcall   _fsmbtn

    rjmp    _mainloop

_fsmbtn:
    rcall    _next_205
_next_205:
    pop     r25
    pop     r24
    clr     r23
    ldi     r22, 13
    add     r24, r22
    adc     r25, r23
    lds     r22, STATE
    add     r24, r22
    adc     r25, r23
    push    r24
    push    r25
    ret
    rjmp    _label_case_1206
    rjmp    _label_case_2229
    rjmp    _label_case_3259
_label_case_1206:
    ;; progn-open
    lds     r25, PREV
    lds     r24, NEXT
    lds     r23, HIGH
    cp      r25, r24
    brne    _not_equal_eq3_212
    cp      r24, r23
    brne    _not_equal_eq3_212
    ldi     result, TRUE
    rjmp    _end_eq3_213
_not_equal_eq3_212:
    ldi     result, FALSE
_end_eq3_213:
    IFFALSE _when_false_ret_207
    ldi     r23, 1
    sts      STATE, r23
    rjmp    _when_end_208
_when_false_ret_207:
    ldi     result, FALSE
_when_end_208:
    lds     r23, PREV
    lds     r24, HIGH
    cp      r23, r24
    brne    _not_equal_eq2_221
    ldi     result, TRUE
    rjmp    _end_eq2_222
_not_equal_eq2_221:
    ldi     result, FALSE
_end_eq2_222:
    IFFALSE _and_false_ret_217
    lds     r24, NEXT
    lds     r23, LOW
    cp      r24, r23
    brne    _not_equal_eq2_225
    ldi     result, TRUE
    rjmp    _end_eq2_226
_not_equal_eq2_225:
    ldi     result, FALSE
_end_eq2_226:
    IFFALSE _and_false_ret_217
    ldi     result, TRUE
    rjmp    _and_end_218
_and_false_ret_217:
    ldi     result, FALSE
_and_end_218:
    IFFALSE _when_false_ret_215
    ldi     r23, 0
    sts      CNT, r23
    ldi     r23, 3
    sts      STATE, r23
    rjmp    _when_end_216
_when_false_ret_215:
    ldi     result, FALSE
_when_end_216:
    ;; progn-close
_label_case_2229:
    ;; progn-open
    lds     r23, PREV
    lds     r24, NEXT
    lds     r25, HIGH
    cp      r23, r24
    brne    _not_equal_eq3_237
    cp      r24, r25
    brne    _not_equal_eq3_237
    ldi     result, TRUE
    rjmp    _end_eq3_238
_not_equal_eq3_237:
    ldi     result, FALSE
_end_eq3_238:
    IFFALSE _and_false_ret_232
    lds     r25, CNT
    lds     r24, MAX
    cp      r25, r24
    brsh    _not_lt_eq2_241
    ldi     result, TRUE
    rjmp    _end_lt2_242
_not_lt_eq2_241:
    ldi     result, FALSE
_end_lt2_242:
    IFFALSE _and_false_ret_232
    ldi     result, TRUE
    rjmp    _and_end_233
_and_false_ret_232:
    ldi     result, FALSE
_and_end_233:
    IFFALSE _when_false_ret_230
    ldi     r24, CNT
    inc     r24
    sts      CNT, r24
    ldi     r24, 2
    sts      STATE, r24
    rjmp    _when_end_231
_when_false_ret_230:
    ldi     result, FALSE
_when_end_231:
    lds     r24, PREV
    lds     r25, NEXT
    cp      r24, r25
    brne    _not_equal_eq2_251
    ldi     result, TRUE
    rjmp    _end_eq2_252
_not_equal_eq2_251:
    ldi     result, FALSE
_end_eq2_252:
    IFFALSE _and_false_ret_247
    ldi     r25, CNT
    ldi     r24, MAX
    cp      r25, r24
    brlt    _not_ge_eq2_255
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
    ldi     r24, 0
    sts      CNT, r24
    ldi     r24, 1
    sts      STATE, r24
    rjmp    _when_end_246
_when_false_ret_245:
    ldi     result, FALSE
_when_end_246:
    ;; progn-close
_label_case_3259:
    ;; progn-open
    lds     r24, PREV
    lds     r25, LOW
    cp      r24, r25
    brne    _not_equal_eq2_266
    ldi     result, TRUE
    rjmp    _end_eq2_267
_not_equal_eq2_266:
    ldi     result, FALSE
_end_eq2_267:
    IFFALSE _and_false_ret_262
    lds     r25, NEXT
    lds     r24, HIGH
    cp      r25, r24
    brne    _not_equal_eq2_270
    ldi     result, TRUE
    rjmp    _end_eq2_271
_not_equal_eq2_270:
    ldi     result, FALSE
_end_eq2_271:
    IFFALSE _and_false_ret_262
    ldi     result, TRUE
    rjmp    _and_end_263
_and_false_ret_262:
    ldi     result, FALSE
_and_end_263:
    IFFALSE _when_false_ret_260
    ldi     r24, 0
    sts      CNT, r24
    ldi     r24, 2
    sts      STATE, r24
    rjmp    _when_end_261
_when_false_ret_260:
    ldi     result, FALSE
_when_end_261:
    lds     r24, PREV
    lds     r25, NEXT
    lds     r23, LOW
    cp      r24, r25
    brne    _not_equal_eq3_281
    cp      r25, r23
    brne    _not_equal_eq3_281
    ldi     result, TRUE
    rjmp    _end_eq3_282
_not_equal_eq3_281:
    ldi     result, FALSE
_end_eq3_282:
    IFFALSE _and_false_ret_276
    ldi     result, TRUE
    rjmp    _and_end_277
_and_false_ret_276:
    ldi     result, FALSE
_and_end_277:
    IFFALSE _when_false_ret_274
    ldi     r23, 3
    sts      STATE, r23
    rjmp    _when_end_275
_when_false_ret_274:
    ldi     result, FALSE
_when_end_275:
    ;; progn-close

    ret

_infloop:
    rjmp    _infloop

_blink:
    in      tmp0, PORTB
    com     tmp0
    ldi     tmp1, 0b00010000    ; Мигаем только PB4
    and     tmp0, tmp1
    out     PORTB, tmp0
    ret

    .data

STATE:
    .byte 0x1
PREV:
    .byte 0x0
NEXT:
    .byte 0x0
NEW:
    .byte 0x0
CNT:
    .byte 0x0
MAX:
    .byte 0x0
