#define F_CPU 1200000UL
#define PORTB 0x18
#define DDRB  0x17

    .section .text
    .org 0
    .global main

vectors:
    rjmp	reset
    rjmp	bad_interrupt
    rjmp	bad_interrupt
    rjmp	vector_3
    rjmp	bad_interrupt
    rjmp	bad_interrupt
    rjmp	bad_interrupt
    rjmp	bad_interrupt
    rjmp	bad_interrupt
    rjmp	bad_interrupt

#define SREG 0x3f
#define SPL  0x3d
#define RAMEND 0x9F
bad_interrupt:
    ldi r16, 0b00000001
    ldi r17, 0b00000001 ; using R17 for toggle mask (XOR)
    out DDRB, r16
    eor r16, r17
    out PORTB, r16
    eor r16, r17
    out PORTB, r16
    rjmp bad_interrupt

reset:
    eor	r1, r1
    out	SREG, r1
    ldi	r28, RAMEND
    out	SPL, r28
    ;; _____                        +--v--+
    ;; RESET ADC0 5/A0 PCINT5 PB5  1|o    |8  VCC
    ;; CLKI  ADC3 3/A3 PCINT3 PB3  2|     |7  PB2 PCINT2 2/A1 SCK  ADC1
    ;;       ADC2 4/A2 PCINT4 PB4  3|     |6  PB1 PCINT1 1    MISO OC0B INT0
    ;;                        GND  4|     |5  PB0 PCINT0 0    MOSI OC0A
    ;;                              +-----+
    ;;           +------- (PB5) [1-pin]
    ;;           |+------ (PB4) [3-pin] RELAY_1 (INPUT)
    ;;           ||+----- (PB3) [2-pin] BTN_1
    ;;           |||+---- (PB2) [7-pin]
    ;;           ||||+--- (PB1) [6-pin] WIDE-LED
    ;;           |||||+-- (PB0) [5-pin] LED
    ;;           ||||||
    ldi r16, 0b00010011
    ;; ldi r17, 0b00010001 ; using R17 for toggle mask (XOR)
    out DDRB, r16

    in r16, PORTB
    ori r16, 0b00000010
    out PORTB, r16

    ldi r24, 0xFF
    rcall delay


#define rX1 r26
#define rX2 r27
#define rY1 r28
#define rY2 r29
#define rZ1 r30
#define rZ2 r31

do_copy_data:
    ;; from 0x0:0x60 - 0x0:0x6A
    ;;   to 0x0:0x8C - ...
    ldi	r17, 0x00 ; 1c:
    ldi	rX1, 0x60 ; 1e:
    ldi	rX2, 0x00 ; 20:
    ldi	rZ1, 0x8C ; 22:
    ldi	rZ2, 0x02 ; 24:
    rjmp	_0x2c ; 26:
_0x28:
    lpm	r0, Z+    ; 28:
    st	X+, r0    ; 2a:
_0x2c:
    cpi	rX1, 0x6A ; 2c:
    cpc	rX2, r17  ; 2e:
    brne	_0x28 ; 30:

_do_clear_bss:
    ldi	r18, 0x00 ; 32:
    ldi	rX1, 0x6A ; 34:
    ldi	rX2, 0x00 ; 36:
    rjmp do_clear_bss_start ; 38:

do_clear_bss_loop:
    st	X+, r1    ; 3a:

do_clear_bss_start:
    cpi	rX1, 0x7C ; 3c:
    cpc	rX1, r18  ; 3e:
    brne	do_clear_bss_loop ; 40:
    rcall	main  ; 42:
    rjmp	exit  ; 44:


    ;; void delay (unsigned ms) {
    ;; 	while (ms--) {
    ;; 		_delay_ms(1); // Using the libc routine over
    ;;                    // and over is non-optimal
    ;;                    // but it works and is close enough
    ;; 	} // Note, I may have to reimplement this because
    ;;    // the avr-libc delay is too slow *todo*
    ;; }
    ;;
    ;; param: r24
    ;; destroy: r30:r31
delay:
    ;; while(ms--) {
    sbiw	r24, 0x00 ; b6:
    breq	delay_return ; b8:
	;; #else
	;; 	//round up by default
	;; 	__ticks_dc = (uint32_t)(ceil(fabs(__tmp)));
	;; #endif
	;; __builtin_avr_delay_cycles(__ticks_dc);
    ldi	r30, 0x2B ; ba:
    ldi	r31, 0x01 ; bc:
delay_loop:
    sbiw	r30, 0x01  ; be:
    brne	delay_loop ; c0:
    rjmp	delay_nop ; c2:
delay_nop:
    nop ; c4:
    sbiw	r24, 0x01 ; c6:
    rjmp	delay ; c8:
delay_return:
    ret ; ca:


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
main:

    ;;  // Setup timer interrupt and PWM pins

    ;;  TCCR0B - Timer/Counter Control Register B
#define TCCR0B 0x33
    ;; 7 - FOC0 (w)
    ;; 6 - WGM00 (r/w)
    ;; 5 - COM01 (r/w)
    ;; 4 - COM00 (r/w)
    ;; 3 - WGM01 (r/w)
    ;; 2 - CS02 (r/w)
    ;; 1 - CS01 (r/w)
    ;; 0 - CS00 (rw)
#define CS00    = 0 ; Clock Select:
#define CS01    = 1 ; Clock Select:
#define CS02    = 2 ; Clock Select:
    ;; 000 - таймер/счетчик T0 остановлен
    ;; 001 - тактовый генератор CLK
    ;; 010 - CLK/8
    ;; 011 - CLK/64
    ;; 100 - CLK/256
    ;; 101 - CLK/1024
    ;; 110 - внешний источник на выводе T0 (6 ножка) по спаду сигнала
    ;; 111 - внешний источник на выводе T0 (6 ножка) по возрастанию сигнала
#define WGM02   = 3 ; Waveform Generation Mode
    ;; Биты WGM10, WGM00 (Wave Generator Mode) - определяют режим
    ;; работы таймера-счетчика Т0. Два из них находятся в регистре
    ;; TCCR0A(00,01), а третий(02) в TCCR0B. 0-й бит включает ШИМ,
    ;; 1-й — включает быстрый ШИМ, 2-й означает что верхнее значение
    ;; указано в OCRA. Не очень удобно, конечно, что 2-й бит вынесен
    ;; в TCCRB (т.к. добавлен позднее), но он и нужен нечасто.
    ;; 00 - нормальный режим (normal)
    ;; 01 - Phase Correct PWM
    ;; 10 - сброс таймера при совпадении (CTC)
    ;; 11 - FastPWM
#define FOC0B   = 6 ; Force Output Compare B
#define FOC0A   = 7 ; Force Output Compare A
    ;; Биты FOC0A (7) и FOC0B (6) регистра TCCR0B принудительно
    ;; устанавливают значение на выводах OC0A и OC0B.

;;  TCCR0B |= _BV(CS00);
    in	r24, TCCR0B ; cc: NonNeeded?
    ori	r24, 0x01   ; ce: Clock Select : 001,
    out	TCCR0B, r24 ; d0:

    ;;  TCCR0A - Timer/Counter Control Register A
#define TCCR0A 0x2f
    ;; 7 - COM0A1 - определяет состояние вывода OC0A при Совпадении
    ;; 6 - COM0A0 - определяет состояние вывода OC0A при Совпадении
    ;; 5 - COM0B1 - определяет состояние вывода OC0B при Совпадении
    ;; 4 - COM0B0 - Флаг определяет состояние вывода OC0B при Совпадении
    ;; 3 -
    ;; 2 -
    ;; 1 - WGM01 - Флаг определяет режим работы таймера/счетчика
    ;; 0 - WGM00 - Флаг определяет режим работы таймера/счетчика

    ;; TCCR0A |= _BV(WGM00)|_BV(WGM01);
    in	r24, 0x2f ; d2: NonNeeded?
    ori	r24, 0x03 ; d4: Set WGM01 & WGM02
    out	TCCR0A, r24 ; d6:

    ;; TIMSK0 Timer/Counter Interrupt Mask Register
#define TIMSK0 0x39
    ;; 7 -
    ;; 6 -
    ;; 5 - ICIE1 - разрешает прерывание по захвату события
    ;; 4 -
    ;; 3 - OCIE1B - Флаг разрешения прерывания Совпадению B
    ;; 2 - OCIE1A - Флаг разрешения прерывания Совпадению А
    ;; 1 - TOIE1 - Флаг разрешения прерывания по переполнению
    ;; 0 -
    ;; TIMSK0 |= 2;
    in	r24, TIMSK0 ; d8:
    ori	r24, 0x02 ; da: Set TOIE1
    out	TIMSK0, r24 ; dc:

    ;; Timer/Counter - Устанавливаем нулевое начальное значение
    ;; TCNT0=0;
    out	0x32, r1 ; de:
/*
    ;; Разрешаем прерывания
    sei ; e0:

    ;; ADMUX - Регистр выбора входа мультиплексора АЦП - управляет
    ;; выбором опорного напряжения, а также входа мультиплексора.
    ;; 7 - REFS1 - Reference U Selector
    ;; 6 - REFS0 - Reference U Selector
    ;; 5 - ADLAR - Если бит ADLAR установлен, то результат преобразования
    ;;      размещается в регистровой паре ADCH-ADCL с выравниванием влево,
    ;;      в противном случае - с выравниванием вправо.
    ;; 4 -
    ;; 3 - MUX3
    ;; 2 - MUX2
    ;; 1 - MUX1
    ;; 0 - MUX0
    ;; Биты REFS1 (7) и REFS0 (6) устанавливают какой источник
    ;; опорного напряжения будет выбран:
    ;; 00 - опорное напряжение на входе AREF
    ;; 01 - Vпитания (вход AREF должен быть отключен или к нему можно
    ;; подключить фильтрующий конденсатор)
    ;; 10 - резерв
    ;; 11 - внутренний ИОН 1.1 В (к входу AREF можно подключить
    ;; фильтрующий конденсатор)

    ;; ADMUX=0;
    out	0x07, r1 ; e2:


    ;; ADCSRA:
    ;; 7 - ADEN - включает или выключает АЦП (1-включен).
    ;; 6 - ADSC - запускает преобразование если в него записать 1
    ;;            (для многоразового режима запуск первого преобразования)
    ;; 5 - ADATE - позволяет запускать преобразование по прерыванию от
    ;;           переферийных устройств микроконтроллера если установить в 1
    ;; 4 - ADIF - флаг прерывания от АЦП.
    ;; 3 - ADIE - разрешает прерывания от АЦП если установлен в 1.
    ;; 2 - ADPS2
    ;; 1 - ADPS1
    ;; 0 - ADPS0
    ;; Биты ADPS2 - ADPS0 (2 - 0) регистра ADCSRA выбирают режим работы
    ;; предделителя тактовой частоты:
    ;; 000 - CLK/2
    ;; 001 - CLK/2
    ;; 010 - CLK/4
    ;; 011 - CLK/8
    ;; 100 - CLK/16
    ;; 101 - CLK/32
    ;; 110 - CLK/64
    ;; 111 - CLK/128

    ;; // Set up ADC clock depending on F_CPU
;;  #if F_CPU <= 200000
;;      ADCSRA |= _BV(ADEN);
;;  #elif F_CPU <= 1200000 && F_CPU > 200000
;;      ADCSRA |= _BV(ADEN) | _BV(ADPS1);
    in	r24, 0x06 ; e4:
    ori	r24, 0x82 ; e6:
    out	0x06, r24 ; e8:
;;  ADCSRA |= _BV(ADEN) | _BV(ADPS2);
;;  #else
;;     ADCSRA |= _BV(ADEN) | _BV(ADPS1) | _BV(ADPS0) | _BV(ADPS2);
;;  #endif
*/
    ;; _____                        +--v--+
    ;; RESET ADC0 5/A0 PCINT5 PB5  1|o    |8  VCC
    ;; CLKI  ADC3 3/A3 PCINT3 PB3  2|     |7  PB2 PCINT2 2/A1 SCK  ADC1
    ;;       ADC2 4/A2 PCINT4 PB4  3|     |6  PB1 PCINT1 1    MISO OC0B INT0
    ;;                        GND  4|     |5  PB0 PCINT0 0    MOSI OC0A
    ;;                              +-----+
#define RELAY_1 4
#define BTN_1 3
    ;;           +------- (PB5) [1-pin]
    ;;           |+------ (PB4) [3-pin] RELAY_1 (INPUT)
    ;;           ||+----- (PB3) [2-pin] BTN_1
    ;;           |||+---- (PB2) [7-pin]
    ;;           ||||+--- (PB1) [6-pin]
    ;;           |||||+-- (PB0) [5-pin] LED
    ;;           ||||||
    ldi r16, 0b00010001
    ;; ldi r17, 0b00010001 ; using R17 for toggle mask (XOR)
    out DDRB, r16

loop:
    in r16, PORTB
    ori r16, 0b00010000
    out PORTB, r16

    ldi r24, 0xFF
    rcall delay

    in  r16, PORTB
    andi r16, 0b11101111
    out PORTB, r16

    ldi r24, 0xFF
    rcall delay


goback:
    rjmp loop ; ad infinitum.



vector_3:
;; #include <avr/io.h>
;; #include <util/delay.h>
;; #include <avr/interrupt.h>

;; volatile unsigned long ovrf=0;
;; ISR(TIM0_OVF_vect){
;; 48:	1f 92
    push	r1
;; 4a:	0f 92
    push	r0
;; 4c:	0f b6
    in	r0, 0x3f	; 63
;; 4e:	0f 92
    push	r0
;; 50:	11 24
    eor	r1, r1
;; 52:	8f 93
    push	r24
;; 54:	9f 93
    push	r25
;; 56:	af 93
    push	r26
;; 58:	bf 93
    push	r27
    ;; ovrf++; //Increment counter every 256 clock cycles
;; 5a:	80 91 78 00
    lds	r24, 0x0078
;; 5e:	90 91 79 00
    lds	r25, 0x0079
;; 62:	a0 91 7a 00
    lds	r26, 0x007A
;; 66:	b0 91 7b 00
    lds	r27, 0x007B
;; 6a:	01 96
    adiw	r24, 0x01	; 1
;; 6c:	a1 1d
    adc	r26, r1
;; 6e:	b1 1d
    adc	r27, r1
;; 70:	80 93 78 00
    sts	0x0078, r24
;; 74:	90 93 79 00
    sts	0x0079, r25
;; 78:	a0 93 7a 00
    sts	0x007A, r26
;; 7c:	b0 93 7b 00
    sts	0x007B, r27
;; }
;; 80:	bf 91
    pop	r27
;; 82:	af 91
    pop	r26
;; 84:	9f 91
    pop	r25
;; 86:	8f 91
    pop	r24
;; 88:	0f 90
    pop	r0
;; 8a:	0f be
    out	0x3f, r0	; 63
;; 8c:	0f 90
    pop	r0
;; 8e:	1f 90
    pop	r1
;; 90:	18 95
    reti
/*
   ;; 00000092 <
millis:

;; unsigned long millis(){
;;     unsigned long x;
;;     asm("cli");
;;   92:	f8 94       	cli
;; #elif F_CPU == 600000
;;     x = ovrf / 2;
;; #elif F_CPU == 1000000
;;     x = ovrf / 4;
;; #elif F_CPU == 1200000
;;     x = ovrf / 5;
;; 94:	60 91 78 00
    lds	r22, 0x0078
;; 98:	70 91 79 00
    lds	r23, 0x0079
;; 9c:	80 91 7a 00
    lds	r24, 0x007A
;; a0:	90 91 7b 00
    lds	r25, 0x007B
;; #elif F_CPU == 16000000
;;     x = ovrf / 63;
;; #else
;; #error This CPU frequency is not defined
;; #endif
;;     asm("sei");
;; a4:	78 94
    sei
    ;; return x;
;; a6:	25 e0
    ldi	r18, 0x05	; 5
;; a8:	30 e0
    ldi	r19, 0x00	; 0
;; aa:	40 e0
    ldi	r20, 0x00	; 0
;; ac:	50 e0
    ldi	r21, 0x00	; 0
;; ae:	ca d0
    rcall	__udivmodsi4
;; }
;; b0:	ca 01
    movw	r24, r20
;; b2:	b9 01
    movw	r22, r18
;; b4:	08 95
    ret


   ;; 000000cc <
main:

        ;;     }

        ;; } else {
        ;;     if (!btn_1_released) {
        ;;         delay(100);
        ;;         btn_1_released = true;
;; ee:	c1 e0
    ldi	r28, 0x01	; 1
;; f0:	d0 e0
    ldi	r29, 0x00	; 0
                ;; if (HIGH == state_1) {
                ;;     state_1 = LOW;
                ;;     duration_1 = 0;
                ;; } else {
                ;;     state_1 = HIGH;
                ;;     duration_1 = period_1;
;; f2:	80 ea
    ldi	r24, 0xA0	; 160
;; f4:	c8 2e
    mov	r12, r24
;; f6:	8f e0
    ldi	r24, 0x0F	; 15
;; f8:	d8 2e
    mov	r13, r24
;; fa:	e1 2c
    mov	r14, r1
;; fc:	f1 2c
    mov	r15, r1
    ;; DDRB |=  (1 << relay_1);    // pinMode(relay_1, OUTPUT);
    ;; DDRB &= ~(1 << btn_1);      // pinMode(btn_1, INPUT);

    ;; while (1)
    ;; {
    ;;     unsigned long cur_mils = millis();

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

theloop:

    ;; SET BIT with REG
.macro  SETB
    .if @0 < 0x20               ; Low IO
        sbi @0,@1
    .else
        .if @0 < 0x40             ; High IO
            in  @2, @0
            ori @2, 1 << @1
            out @0, @2
        .else                   ; Memory
            lds @2, @0
            ori @2, 1 << @1
            sts @0, @2
        .endif
    .endif
.endm

    ;; Clear BIT with REG
.macro  CLRB
    .if @0 < 0x20               ; low io
        cbi @0, @1
    .else
        .if @0 < 0x40             ; high io
            in   @2, @0
            andi @2, ~(1 << @1)
            out  @0,@2
        .else                   ; memory
            lds  @2,@0
            andi @2, ~(1 << @1)
            sts  @0,@2
        .endif
    .endif
.endm


    ldi	r24, 0xE8
    ldi	r25, 0x03
    rcall	delay

    in  r26, PORTB
    andi r26, 0 ; ~(1 << PB4)
    out PORTB, r26

;; 11e:	84 e6
    ldi	r24, 0xE8
;; 120:	90 e0
    ldi	r25, 0x03
;; 122:	c9 df
    rcall	delay

    in  r26, PORTB
    ori r26, 255 ;  (1 << PB4)
    out PORTB, r26

    ;; SETB    DDRD,4,R16          ; DDRD.4 = 1


    rjmp theloop









_0xfe:
;; fe:	c9 df
    rcall	millis
;; 100:	4b 01
    movw	r8, r22
;; 102:	5c 01
    movw	r10, r24
;; 104:	80 91 60 00
    lds	r24, 0x0060
;; 108:	90 91 61 00
    lds	r25, 0x0061

        ;; if(PINB & (1 << btn_1)) // if (digitalRead(bnt_1) == HIGH)
;; 10c:	b3 9b
    sbis	0x16, 3	; 22
;; 10e:	2a c0
    rjmp	_0x164
        ;; {
        ;;     if (btn_1_released) {
;; 110:	89 2b
    or	r24, r25
;; 112:	09 f4
    brne	_0x116
;; 114:	7d c0
    rjmp	_0x210
                ;; btn_1_released = false;
_0x116:
;; 116:	10 92 61 00
    sts	0x0061, r1
;; 11a:	10 92 60 00
    sts	0x0060, r1
                ;; delay(100);
;; 11e:	84 e6
    ldi	r24, 0x64	; 100
;; 120:	90 e0
    ldi	r25, 0x00	; 0
;; 122:	c9 df
    rcall	delay
                ;; if (HIGH == state_1) {
;; 124:	80 91 76 00
    lds	r24, 0x0076
;; 128:	90 91 77 00
    lds	r25, 0x0077
;; 12c:	01 97
    sbiw	r24, 0x01	; 1
;; 12e:	69 f4
    brne	_0x14a
                    ;; state_1 = LOW;
;; 130:	10 92 77 00
    sts	0x0077, r1
;; 134:	10 92 76 00
    sts	0x0076, r1
                    ;; duration_1 = 0;
;; 138:	10 92 72 00
    sts	0x0072, r1
;; 13c:	10 92 73 00
    sts	0x0073, r1
;; 140:	10 92 74 00
    sts	0x0074, r1
;; 144:	10 92 75 00
    sts	0x0075, r1
;; 148:	63 c0
    rjmp	_0x210
                ;; } else {
                ;;     state_1 = HIGH;
_0x14a:
;; 14a:	d0 93 77 00
    sts	0x0077, r29
;; 14e:	c0 93 76 00
    sts	0x0076, r28
                    ;; duration_1 = period_1;
;; 152:	c0 92 72 00
    sts	0x0072, r12
;; 156:	d0 92 73 00
    sts	0x0073, r13
;; 15a:	e0 92 74 00
    sts	0x0074, r14
;; 15e:	f0 92 75 00
    sts	0x0075, r15
;; 162:	56 c0
    rjmp	_0x210
        ;;         }
        ;;     }

        ;; } else {
        ;;     if (!btn_1_released) {
_0x164:
;; 164:	89 2b
    or	r24, r25
;; 166:	39 f4
    brne	_0x176
                ;; delay(100);
;; 168:	84 e6
    ldi	r24, 0x64	; 100
;; 16a:	90 e0
    ldi	r25, 0x00	; 0
;; 16c:	a4 df
    rcall	delay
                ;; btn_1_released = true;
;; 16e:	d0 93 61 00
    sts	0x0061, r29
;; 172:	c0 93 60 00
    sts	0x0060, r28
            ;; }
            ;; if (state_1 == HIGH) {
_0x176:
;; 176:	80 91 76 00
    lds	r24, 0x0076
;; 17a:	90 91 77 00
    lds	r25, 0x0077
;; 17e:	01 97
    sbiw	r24, 0x01	; 1
;; 180:	09 f0
    breq	_0x184
;; 182:	46 c0
    rjmp	_0x210
                ;; if (cur_mils < prev_mils) {
_0x184:
;; 184:	80 91 6e 00
    lds	r24, 0x006E
;; 188:	90 91 6f 00
    lds	r25, 0x006F
;; 18c:	a0 91 70 00
    lds	r26, 0x0070
;; 190:	b0 91 71 00
    lds	r27, 0x0071
;; 194:	88 16
    cp	r8, r24
;; 196:	99 06
    cpc	r9, r25
;; 198:	aa 06
    cpc	r10, r26
;; 19a:	bb 06
    cpc	r11, r27
;; 19c:	48 f4
    brcc	_0x1b0
                    ;; prev_mils = cur_mils;
;; 19e:	80 92 6e 00
    sts	0x006E, r8
;; 1a2:	90 92 6f 00
    sts	0x006F, r9
;; 1a6:	a0 92 70 00
    sts	0x0070, r10
;; 1aa:	b0 92 71 00
    sts	0x0071, r11
;; 1ae:	30 c0
    rjmp	_0x210
                ;; } else {
                ;;     unsigned long last_time = cur_mils - prev_mils;
                ;;     if ( last_time >= interval ) {
_0x1b0:
;; 1b0:	a5 01
    movw	r20, r10
;; 1b2:	94 01
    movw	r18, r8
;; 1b4:	28 1b
    sub	r18, r24
;; 1b6:	39 0b
    sbc	r19, r25
;; 1b8:	4a 0b
    sbc	r20, r26
;; 1ba:	5b 0b
    sbc	r21, r27
;; 1bc:	da 01
    movw	r26, r20
;; 1be:	c9 01
    movw	r24, r18
;; 1c0:	88 3e
    cpi	r24, 0xE8	; 232
;; 1c2:	93 40
    sbci	r25, 0x03	; 3
;; 1c4:	a1 05
    cpc	r26, r1
;; 1c6:	b1 05
    cpc	r27, r1
;; 1c8:	18 f1
    brcs	_0x210
                        ;; prev_mils = cur_mils;
;; 1ca:	80 92 6e 00
    sts	0x006E, r8
;; 1ce:	90 92 6f 00
    sts	0x006F, r9
;; 1d2:	a0 92 70 00
    sts	0x0070, r10
;; 1d6:	b0 92 71 00
    sts	0x0071, r11
                        ;; long decremented = duration_1 - interval;
;; 1da:	80 91 72 00
    lds	r24, 0x0072
;; 1de:	90 91 73 00
    lds	r25, 0x0073
;; 1e2:	a0 91 74 00
    lds	r26, 0x0074
;; 1e6:	b0 91 75 00
    lds	r27, 0x0075
;; 1ea:	88 5e
    subi	r24, 0xE8	; 232
;; 1ec:	93 40
    sbci	r25, 0x03	; 3
;; 1ee:	a1 09
    sbc	r26, r1
;; 1f0:	b1 09
    sbc	r27, r1
                        ;; if ( decremented < 0 ) {
;; 1f2:	b7 ff
    sbrs	r27, 7
;; 1f4:	05 c0
    rjmp	_0x200
                            ;; state_1 = LOW;
;; 1f6:	10 92 77 00
    sts	0x0077, r1
;; 1fa:	10 92 76 00
    sts	0x0076, r1
;; 1fe:	08 c0
    rjmp	_0x210
                        ;; } else {
                        ;;     duration_1 = decremented;
_0x200:
;; 200:	80 93 72 00
    sts	0x0072, r24
;; 204:	90 93 73 00
    sts	0x0073, r25
;; 208:	a0 93 74 00
    sts	0x0074, r26
;; 20c:	b0 93 75 00
    sts	0x0075, r27
        ;;             }
        ;;         }
        ;;     }
        ;; }

        ;; latch_1 = state_1;
_0x210:
;; 210:	80 91 76 00
    lds	r24, 0x0076
;; 214:	90 91 77 00
    lds	r25, 0x0077
;; 218:	90 93 6b 00
    sts	0x006B, r25
;; 21c:	80 93 6a 00
    sts	0x006A, r24
        ;; if ( latch_1 != prev_latch_1 ) {
;; 220:	20 91 6c 00
    lds	r18, 0x006C
;; 224:	30 91 6d 00
    lds	r19, 0x006D
;; 228:	82 17
    cp	r24, r18
;; 22a:	93 07
    cpc	r25, r19
;; 22c:	09 f4
    brne	_0x230
;; 22e:	67 cf
    rjmp	_0xfe
            ;; prev_latch_1 = latch_1;
_0x230:
;; 230:	90 93 6d 00
    sts	0x006D, r25
;; 234:	80 93 6c 00
    sts	0x006C, r24
            ;; if (latch_1) {
;; 238:	89 2b
    or	r24, r25
;; 23a:	11 f0
    breq	_0x240
                ;; PORTB |= (1 << relay_1);    // digitalWrite(relay_1, HIGH);
;; 23c:	c4 9a
    sbi	0x18, 4	; 24
;; 23e:	5f cf
    rjmp	_0xfe
            ;; } else {
                ;; PORTB &= ~(1 << relay_1);   // digitalWrite(relay_1, LOW);
_0x240:
;; 240:	c4 98
    cbi	0x18, 4	; 24
;; 242:	5d cf
    rjmp	_0xfe

   ;; 00000244 <
__udivmodsi4:
;; 244:	a1 e2
    ldi	r26, 0x21	; 33
;; 246:	1a 2e
    mov	r1, r26
;; 248:	aa 1b
    sub	r26, r26
;; 24a:	bb 1b
    sub	r27, r27
;; 24c:	fd 01
    movw	r30, r26
;; 24e:	0d c0
    rjmp	__udivmodsi4_ep

   ;; 00000250 <
__udivmodsi4_loop:
;; 250:	aa 1f
    adc	r26, r26
;; 252:	bb 1f
    adc	r27, r27
;; 254:	ee 1f
    adc	r30, r30
;; 256:	ff 1f
    adc	r31, r31
;; 258:	a2 17
    cp	r26, r18
;; 25a:	b3 07
    cpc	r27, r19
;; 25c:	e4 07
    cpc	r30, r20
;; 25e:	f5 07
    cpc	r31, r21
;; 260:	20 f0
    brcs	__udivmodsi4_ep
;; 262:	a2 1b
    sub	r26, r18
;; 264:	b3 0b
    sbc	r27, r19
;; 266:	e4 0b
    sbc	r30, r20
;; 268:	f5 0b
    sbc	r31, r21

   ;; 0000026a <
__udivmodsi4_ep:
;; 26a:	66 1f
    adc	r22, r22
;; 26c:	77 1f
    adc	r23, r23
;; 26e:	88 1f
    adc	r24, r24
;; 270:	99 1f
    adc	r25, r25
;; 272:	1a 94
    dec	r1
;; 274:	69 f7
    brne	.-38     	; 0x250 <__udivmodsi4_loop>
;; 276:	60 95
    com	r22
;; 278:	70 95
    com	r23
;; 27a:	80 95
    com	r24
;; 27c:	90 95
    com	r25
;; 27e:	9b 01
    movw	r18, r22
;; 280:	ac 01
    movw	r20, r24
;; 282:	bd 01
    movw	r22, r26
;; 284:	cf 01
    movw	r24, r30
;; 286:	08 95
    ret

    */

exit:
    cli ; 288:

stop_program:
    rjmp	stop_program ; 28a:
