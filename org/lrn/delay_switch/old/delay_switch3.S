#define F_CPU 1200000UL
#include <avr/io.h>

    .section .text
    .org 0

vectors:
    rjmp   reset                ; Reset Handler
    rjmp   vectors              ; IRQ0 Handler
    rjmp   vectors              ; PCINT0 Handler
    rjmp   timer_0_overflow     ; Timer0 Overflow Handler
    rjmp   vectors              ; EEPROM Ready Handler
    rjmp   vectors              ; Analog Comparator Handler
    rjmp   vectors              ; Timer0 CompareA Handler
    rjmp   vectors              ; Timer0 CompareB Handler
    rjmp   vectors              ; Watchdog Interrupt Handler
    rjmp   vectors              ; ADC Conversion Handler

reset:
    ;; Очищаем SREG
    eor	r1, r1                  ; Обнуляем r1 - дальше он будет всегда нулевым
    out	0x3f, r1                ; Обнуляем SREG (0x3f)
    ;; Устанавливаем стек
    ldi	r28, 0x9F  	            ; Загружаем 0x9F (RAMEND) в R28
    out	0x3d, r28               ; Загружаем RAMEND в SPL (0x3d)


main:
    .global main

    rjmp main

    ;; ovrf разместим в регистрах r27:r26:r25:r25
timer_0_overflow:
    adiw r24, 0x01	            ; r25:r25 + 1
    adc	r26, r1                 ; r26:r25:r25 + 1
    adc	r27, r1                 ; r27:r26:r25:r25 + 1
    ;; Обнуляем SREG (0x3f)
    out	0x3f, r1                ; Обнуляем SREG (0x3f)
    ;; Выходим из прерывания
    reti

millis:
    ;; Перегрузить из ovrf в 24:23:22:21, запретив прерывания
    cli
    mov  r24, r27
    mov  r22, r25
    sei
    ;; Разделить на пять (делитель в 20:19:18:17)
    ldi r17, 0x05
    mov r18, r1
    mov r19, r1
    mov r20, r1
    rcall udivmodsi4
    ;; Забрать результат в 24:23:22:21 (TODO)
    movw	r24, r20
    movw	r22, r18
    ret



    ;; "si" - 32 Bit (single integer),
    ;; "hi" - 16 Bit ("half int")
    ;; "qi" - 8 Bit ("quarter of an int").
    ;; "di" - "double int" (64),
    ;; "df" - double (double float),
    ;; "sf" - "single float" (32), etc.

#define m1 r22
#define m2 r23
#define m3 r24
#define m4 r25

#define n1 r18
#define n2 r19
#define n3 r20
#define n4 r21

#define x1 r26
#define x2 r27
#define x3 r30
#define x4 r31

    ;;; 22-25 / 18-21
udivmodsi4:
    ldi	x1, 0x21 ;
    mov	r1, x1   ; r1 = x1 = 0x21 - зачем?
    sub	x1, x1   ; Очищаем X:Z - частное
    sub	x2, x2   ;
    movw x3, x1  ;
    rjmp udivmodsi4_ep ; -------+
    ;; ----------;              |
udivmodsi4_loop: ; <----------------+
    adc	x1, x1   ; X <<= 1      |   |
    adc	x2, x2   ;              |   |
    adc	x3, x3   ;              |   |
    adc	x4, x4   ;              |   |
    cp	x1, n1   ; [X > N] ?    |   |
    cpc	x2, n2   ;              |   |
    cpc	x3, n3   ;              |   |
    cpc	x4, n4   ;         !Yes |   |
    brcs udivmodsi4_ep ; -------+   |
    sub	x1, n1   ; X -= N       |   |
    sbc	x2, n2   ;              |   |
    sbc	x3, n3   ;              |   |
    sbc	x4, n4   ;              |   |
udivmodsi4_ep:   ; <--------=---+   |
    adc	m1, m1   ; m1 += CF         |
    adc	m2, m2   ;                  |
    adc	m3, m3   ;                  |
    adc	m4, m4   ;                  |
    dec	r1       ; r1--             |
    brne udivmodsi4_loop ; ---------+
    com	m1       ; INVERT M
    com	m2       ;
    com	m3       ;
    com	m4       ;
    movw n1, m1  ; N = M
    movw n3, m3  ;
    movw m1, x1  ; M = X
    movw m3, x3  ;
    ret
