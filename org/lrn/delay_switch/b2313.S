    #define tmp0 r16
    #define tmp1 r17
    #define cnt  r18
    #define par1 r23
    #define par2 r24
    #define par3 r25

    .equ SPL, 0x3D
    .equ SREG, 0x3F
    .equ RAMEND, 0xDF
    .equ DDRB, 0x17
    .equ PORTB, 0x18
    .equ PINB, 0x16
    .equ TCCR0A, 0x30
    .equ TCCR0B, 0x33
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
	rjmp    _infloop            ; Timer1 Overflow Handler
	rjmp    _timer0_overflow    ; Timer0 Overflow Handler
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

_timer0_overflow:
    ;; Cбрасываем счетный регистр таймера/счетчика T0
    out     TCNT0, r1
    ;; Увеличиваем и проверяем счетчик переполнений
    inc     cnt
    cpi     cnt, 3
    brsh    _overstep           ; Переход если больше или равно
    reti
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
;;     rjmp    _timer0_overflow_ret
;; _not_press:
    ;; Пока ничего не нажато - мигаем
    rcall _blink
_timer0_overflow_ret:
    ;; Очищаем счетчик переполнений
    mov     cnt, r1
    reti

_reset:

	;; Запретить прерывания
	clr     r1
	out     SREG, r1

    ;; Настроить Stack
	ldi     tmp0, RAMEND
	out     SPL, tmp0

    ;; Настроить для PB4 на выход, остальные на вход
    ldi     tmp0, 0b00010000
	out     DDRB, tmp0

    ;; Для пинов PB0-PB3 подключенных на вход (кнопки)
    ;; установить подтяжку к питанию, чтобы не было hi-z
    ;; Однако их надо подтянуть к минусу питания (!)
    ;; PB4 подтягиваем к земле, потому что начальное
    ;; его состояние - выключен
    ldi     tmp0, 0b00001111
    out     PORTB, tmp0

    ;; Настроить Timer

    ;; TCCR0A – Timer/Counter Control Register A
    ;; | COM0A1 | COM0A0 | COM0B1 | COM0B0 | – | – | WGM01 | WGM00 |
    ;;
    ;; Bits 7:6 – COM0A1:0: Compare Match Output A Mode
    ;; Эти биты управляют поведением вывода сравнения выхода OC0A.
    ;; Если хотя бы один установлен, выход OC0A переопределяет
    ;; нормальную функциональность порта контакта ввода-вывода,
    ;; к которому он подключен.
    ;;
    ;; Bits 5:4 – COM0B1:0: Compare Match Output B Mode
    ;; То же самое но для вывода OC0B
    ;;
    ;; Bits 3, 2 – Res: Reserved Bits
    ;;
    ;; Bits 1:0 – WGM01:0: Waveform Generation Mode
    ;;
    ;; В сочетании с битом WGM02, из регистра TCCR0B, эти биты управляют:
    ;; - последовательностью подсчета счетчика,
    ;; - источником максимального значения (TOP) счетчика и
    ;; - типом генерируемого сигнала, который будет использоваться
    ;;
    ;; | WGM2 | WGM1 | WGM0 | Mode     | TOP   | Update of OCRx | TOV Flag |
    ;; |------+------+------+----------+-------+----------------+----------|
    ;; |    0 |    0 |    0 | Normal   | 0xFF  | Immediate      | MAX      |
    ;; |    0 |    0 |    1 | PWM, PC  | 0xFF  | TOP            | BOTTOM   |
    ;; |    0 |    1 |    0 | CTC      | OCR0A | Immediate      | MAX      |
    ;; |    0 |    1 |    1 | Fast PWM | 0xFF  | TOP            | MAX      |
    ;; |    1 |    0 |    0 | Reserved | –     | –              | –        |
    ;; |    1 |    0 |    1 | PWM, PC  | OCR0A | TOP            | BOTTOM   |
    ;; |    1 |    1 |    0 | Reserved | –     | –              | –        |
    ;; |    1 |    1 |    1 | Fast PWM | OCR0A | TOP            | TOP      |
    ;; PC = Phase Correct
    ;; MAX = 0xFF
    ;; BOTTOM = 0x00
    ;;
    ;; Таким образом, для режима Normal мы ничего не делаем с этим регистром
    ;; оставляя все его биты нулевыми.


    ;; TCCR0B Timer/Counter Control Register B
    ;; | FOC0A | FOC0B | – | – | WGM02 | CS02 | CS01 | CS00 |
    ;;
    ;; Bit 7 – FOC0A: Force Output Compare A
    ;; Когда записывается логическая единица в бит FOC0A, немедленно
    ;; принудительно вызывается сравнение сопоставления  на Waveform Generation Unit.
    ;; Мне пока непонятно назначеиние этого бита, но в примерах он выставляется в ноль.
    ;;
    ;; Bit 6 – FOC0B: Force Output Compare B
    ;; Аналогично предыдущему
    ;;
    ;; Bits 5:4 – Res: Reserved Bits
    ;;
    ;; Bit 3 – WGM02: Waveform Generation Mode
    ;; Детально описаны в таблице чуть выше
    ;;
    ;; Bits 2:0 – CS02:0: Clock Select
    ;; | CS02 | CS01 | CS00 | Description                                             |
    ;; |    0 |    0 |    0 | No clock source (Timer/Counter stopped)                 |
    ;; |    0 |    0 |    1 | clk I/O /(No prescaling)                                |
    ;; |    0 |    1 |    0 | clk I/O /8 (From prescaler)                             |
    ;; |    0 |    1 |    1 | clk I/O /64 (From prescaler)                            |
    ;; |    1 |    0 |    0 | clk I/O /256 (From prescaler)                           |
    ;; |    1 |    0 |    1 | clk I/O /1024 (From prescaler)                          |
    ;; |    1 |    1 |    0 | External clock source on T0 pin. Clock on falling edge. |
    ;; |    1 |    1 |    1 | External clock source on T0 pin. Clock on rising edge.  |
    ;;
    ;; Наша тактовая частота 8.000.000 Герц
    ;; Если мы поделим ее 1024, то тик таймера будет происходить
    ;; 8000000/1024=7812.5 раз в секунду.
    ;; В Normal Mode cчетчик таймера переполняется
    ;; 7812.5/256 = 30.517578125 раз в сек.

    ldi     tmp0, 0b101
	out     TCCR0B, tmp0

    ;;  Обнуляем регистр-счетчик TCNT0 – Timer/Counter Register
    out     TCNT0, r1

    ;; OCR0A – Output Compare Register A
    ;; The Output Compare Register A contains an 8-bit value that is continuously compared with the
    ;; counter value (TCNT0). A match can be used to generate an Output Compare interrupt, or to
    ;; generate a waveform output on the OC0A pin.
    ;; [TODO]

    ;; OCR0B – Output Compare Register B
    ;; The Output Compare Register B contains an 8-bit value that is continuously compared with the
    ;; counter value (TCNT0). A match can be used to generate an Output Compare interrupt, or to
    ;; generate a waveform output on the OC0B pin.
    ;; [TODO]


    ;; TIMSK – Timer/Counter Interrupt Mask Register
    ;; | TOIE1 | OCIE1A | OCIE1B | – | ICIE1 | OCIE0B | TOIE0 | OCIE0A |
    ;;
    ;; Bit 0 – OCIE0A: Timer/Counter0 Output Compare Match A Interrupt Enable
    ;; Когда бит OCIE0A установлен в единицу, и бит I в Status Register установлен,
    ;; разрешается прерывание Compare Match.
    ;; Оно возникает, если происходит совпадение значения Timer/Counter0,
    ;; т.е. когда бит OCF0A установлен в TIFR.
    ;; 8-битный компаратор непрерывно сравнивает TCNT0 с
    ;; Output Compare Registers (OCR0A and OCR0B)
    ;; Всякий раз, когда TCNT0 равен OCR0A или OCR0B, компаратор
    ;; сигнализирует о совпадении. Совпадение установит
    ;; Output Compare Flag (OCF0A or OCF0B) в следующем тактовом цикле таймера.
    ;; Если соответствующее прерывание включено, Output Compare Flag генерирует прерывание
    ;; Output Compare interrupt.
    ;; Output Compare Flag автоматически сбрасывается при выполнении прерывания.

    ;; Bit 1 – TOIE0: Timer/Counter0 Overflow Interrupt Enable
    ;; Когда бит TOIE0 установлен и бит I в Status Register установлен,
    ;; прерывание Timer/Counter0 Overflow разрешается.
    ;; Соответствующее прерывание выполняется, если происходит
    ;; переполнение Timer/Counter0, то есть когда бит TOV0
    ;; установлен в [Timer/Counter 0 Interrupt Flag Register] – TIFR.
    ;; В режиме Normal TOV0 будет установлен в том же тактовом цикле таймера,
    ;; когда TCNT0 становится равным нулю.

    ;; Bit 2 – OCIE0B: Timer/Counter0 Output Compare Match B Interrupt Enable
    ;; Полностью аналогично биту 0 но для прерывания Timer/Counter Compare Match B

    ;; Bit 3 – ICIE1: Timer/Counter1, Input Capture Interrupt Enable
    ;; Когда этот бит установлен в единицу и установлен I-флаг в регистре
    ;; состояния (глобальные прерывания разрешены),
    ;; прерывание [Timer/Counter1 Input Capture interrupt] разрешено.
    ;; Соответствующий вектор прерывания выполняется, если установлен
    ;; флаг ICF1, расположенный в TIFR.

    ;; Bit 4 – Res: Reserved Bit

    ;; Bit 5 – OCIE1B: Timer/Counter1, Output Compare B Match Interrupt Enable
    ;; Аналог OCIE0B

    ;; Bit 7 – TOIE1: Timer/Counter1, Overflow Interrupt Enable
    ;; Аналог TOIE0

    ;; Разрешаем прерывания по переполнению таймера 0
    ldi     tmp0, 0b10
    out     TIMSK, tmp0

    ;; TIFR – Timer/Counter Interrupt Flag Register
    ;; | TOV1 | OCF1A | OCF1B | – | ICF1 | OCF0B | TOV0 | OCF0A |
    ;;
    ;; Bit 0 – OCF0A: Output Compare Flag 0 A
    ;; Бит OCF0A устанавливается, когда происходит Compare Match
    ;; между Timer/Counter0  и данными в OCR0A
    ;; Он сбрасывается аппаратно при выполнении соответствующего
    ;; вектора обработки прерываний (или можно вручную)
    ;; Когда I-бит в SREG,
    ;; OCIE0A (Timer/Counter0 Compare Match Interrupt Enable), и
    ;; OCF0A установлены, выполняется
    ;; прерывание [Timer/Counter0 Compare Match Interrupt].

    ;; Bit 1 – TOV0: Timer/Counter0 Overflow Flag
    ;; Бит TOV0 устанавливается при переполнении Timer/Counter0.
    ;; TOV0 очищается аппаратно при выполнении соответствующего
    ;; вектора обработки прерываний (или вручную). Когда I-бит SREG,
    ;; TOIE0 (Timer/Counter0 Overflow Interrupt Enable) и TOV0
    ;; установлены, выполняется
    ;; прерывание [Timer/Counter0 Overflow interrupt]

    ;; Bit 2 – OCF0B: Output Compare Flag 0 B
    ;; Аналог OCF0A

    ;; Bit 3 - Input Capture Flag
    ;; Когда происходит изменение логического уровня (событие)
    ;; на выводе  Input Capture pin (ICP1) или на выходе
    ;; аналогового компаратора  Analog Comparator output (ACO),
    ;; и это изменение подтверждается настройкой детектора фронта,
    ;; захват будет инициирован.
    ;; Когда происходит захват, 16-битное значение счетчика (TCNT1)
    ;; записывается в регистр ввода ввода (ICR1).
    ;; Input Capture Flag (ICF1) устанавливается в том же такте
    ;; что и значение TCNT1, которое копируется в регистр ICR1.
    ;; Если включено (ICIE1 = 1),  Input Capture Flag генерирует
    ;; прерывание Input Capture interrupt.
    ;; Флаг ICF1 автоматически сбрасывается при выполнении
    ;; прерывания, и может быть сброшен программно

    ;; Bit 4 – Res: Reserved Bit

    ;; Bits 5-6: OCF1A и OCF1B
    ;; см аналог OCF0A

    ;; Bit 7: TOV1
    ;; см аналог TOV0

    ;; TIFR-регистр нужно сбросить в 0:
    out     TIFR, r1


    ;; Разрешить прерывания
    sei

    ;; Начальная инициализация счетчика переполнений
    mov     cnt, r1

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
