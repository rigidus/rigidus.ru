#define F_CPU 1200000UL
#include <avr/io.h>

    .section .text
    .org 0

vectors:
    rjmp   reset                ; Reset Handler
    rjmp   vectors              ; IRQ0 Handler
    rjmp   vectors              ; PCINT0 Handler
    rjmp   vectors              ; Timer0 Overflow Handler
    rjmp   vectors              ; EEPROM Ready Handler
    rjmp   vectors              ; Analog Comparator Handler
    rjmp   vectors              ; Timer0 CompareA Handler
    rjmp   vectors              ; Timer0 CompareB Handler
    rjmp   vectors              ; Watchdog Interrupt Handler
    rjmp   vectors              ; ADC Conversion Handler

reset:
    ;; Очищаем SREG
    eor	r1, r1                  ; Обнуляем r1
    out	0x3f, r1                ; Обнуляем SREG (0x3f)
    ;; Устанавливаем стек
    ldi	r28, 0x9F  	            ; Загружаем 0x9F (RAMEND) в R28
    out	0x3d, r28               ; Загружаем RAMEND в SPL (0x3d)

main:
    .global main

    rjmp main
