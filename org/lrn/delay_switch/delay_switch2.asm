
delay_switch2.elf:     file format elf32-avr


Disassembly of section .text:

00000000 <__vectors>:
   0:	09 c0       	rjmp	.+18     	; 0x14 <__ctors_end>
   2:	21 c0       	rjmp	.+66     	; 0x46 <__bad_interrupt>
   4:	20 c0       	rjmp	.+64     	; 0x46 <__bad_interrupt>
   6:	20 c0       	rjmp	.+64     	; 0x48 <__vector_3>
   8:	1e c0       	rjmp	.+60     	; 0x46 <__bad_interrupt>
   a:	1d c0       	rjmp	.+58     	; 0x46 <__bad_interrupt>
   c:	1c c0       	rjmp	.+56     	; 0x46 <__bad_interrupt>
   e:	1b c0       	rjmp	.+54     	; 0x46 <__bad_interrupt>
  10:	1a c0       	rjmp	.+52     	; 0x46 <__bad_interrupt>
  12:	19 c0       	rjmp	.+50     	; 0x46 <__bad_interrupt>

00000014 <__ctors_end>:
  14:	11 24       	eor	r1, r1
  16:	1f be       	out	0x3f, r1	; 63
  18:	cf e9       	ldi	r28, 0x9F	; 159
  1a:	cd bf       	out	0x3d, r28	; 61

0000001c <__do_copy_data>:
  1c:	10 e0       	ldi	r17, 0x00	; 0
  1e:	a0 e6       	ldi	r26, 0x60	; 96
  20:	b0 e0       	ldi	r27, 0x00	; 0
  22:	ec e8       	ldi	r30, 0x8C	; 140
  24:	f2 e0       	ldi	r31, 0x02	; 2
  26:	02 c0       	rjmp	.+4      	; 0x2c <__do_copy_data+0x10>
  28:	05 90       	lpm	r0, Z+
  2a:	0d 92       	st	X+, r0
  2c:	aa 36       	cpi	r26, 0x6A	; 106
  2e:	b1 07       	cpc	r27, r17
  30:	d9 f7       	brne	.-10     	; 0x28 <__do_copy_data+0xc>

00000032 <__do_clear_bss>:
  32:	20 e0       	ldi	r18, 0x00	; 0
  34:	aa e6       	ldi	r26, 0x6A	; 106
  36:	b0 e0       	ldi	r27, 0x00	; 0
  38:	01 c0       	rjmp	.+2      	; 0x3c <.do_clear_bss_start>

0000003a <.do_clear_bss_loop>:
  3a:	1d 92       	st	X+, r1

0000003c <.do_clear_bss_start>:
  3c:	ac 37       	cpi	r26, 0x7C	; 124
  3e:	b2 07       	cpc	r27, r18
  40:	e1 f7       	brne	.-8      	; 0x3a <.do_clear_bss_loop>
  42:	44 d0       	rcall	.+136    	; 0xcc <main>
  44:	21 c1       	rjmp	.+578    	; 0x288 <_exit>

00000046 <__bad_interrupt>:
  46:	dc cf       	rjmp	.-72     	; 0x0 <__vectors>

00000048 <__vector_3>:
#include <avr/io.h>
#include <util/delay.h>
#include <avr/interrupt.h>

volatile unsigned long ovrf=0;
ISR(TIM0_OVF_vect){
  48:	1f 92       	push	r1
  4a:	0f 92       	push	r0
  4c:	0f b6       	in	r0, 0x3f	; 63
  4e:	0f 92       	push	r0
  50:	11 24       	eor	r1, r1
  52:	8f 93       	push	r24
  54:	9f 93       	push	r25
  56:	af 93       	push	r26
  58:	bf 93       	push	r27
	ovrf++; //Increment counter every 256 clock cycles
  5a:	80 91 78 00 	lds	r24, 0x0078
  5e:	90 91 79 00 	lds	r25, 0x0079
  62:	a0 91 7a 00 	lds	r26, 0x007A
  66:	b0 91 7b 00 	lds	r27, 0x007B
  6a:	01 96       	adiw	r24, 0x01	; 1
  6c:	a1 1d       	adc	r26, r1
  6e:	b1 1d       	adc	r27, r1
  70:	80 93 78 00 	sts	0x0078, r24
  74:	90 93 79 00 	sts	0x0079, r25
  78:	a0 93 7a 00 	sts	0x007A, r26
  7c:	b0 93 7b 00 	sts	0x007B, r27
}
  80:	bf 91       	pop	r27
  82:	af 91       	pop	r26
  84:	9f 91       	pop	r25
  86:	8f 91       	pop	r24
  88:	0f 90       	pop	r0
  8a:	0f be       	out	0x3f, r0	; 63
  8c:	0f 90       	pop	r0
  8e:	1f 90       	pop	r1
  90:	18 95       	reti

00000092 <millis>:

unsigned long millis(){
	unsigned long x;
	asm("cli");
  92:	f8 94       	cli
#elif F_CPU == 600000
	x = ovrf / 2;
#elif F_CPU == 1000000
	x = ovrf / 4;
#elif F_CPU == 1200000
	x = ovrf / 5;
  94:	60 91 78 00 	lds	r22, 0x0078
  98:	70 91 79 00 	lds	r23, 0x0079
  9c:	80 91 7a 00 	lds	r24, 0x007A
  a0:	90 91 7b 00 	lds	r25, 0x007B
#elif F_CPU == 16000000
	x = ovrf / 63;
#else
#error This CPU frequency is not defined
#endif
	asm("sei");
  a4:	78 94       	sei
	return x;
  a6:	25 e0       	ldi	r18, 0x05	; 5
  a8:	30 e0       	ldi	r19, 0x00	; 0
  aa:	40 e0       	ldi	r20, 0x00	; 0
  ac:	50 e0       	ldi	r21, 0x00	; 0
  ae:	ca d0       	rcall	.+404    	; 0x244 <__udivmodsi4>
}
  b0:	ca 01       	movw	r24, r20
  b2:	b9 01       	movw	r22, r18
  b4:	08 95       	ret

000000b6 <delay>:

void delay(unsigned ms) {
	while(ms--){
  b6:	00 97       	sbiw	r24, 0x00	; 0
  b8:	41 f0       	breq	.+16     	; 0xca <delay+0x14>
	#else
		//round up by default
		__ticks_dc = (uint32_t)(ceil(fabs(__tmp)));
	#endif

	__builtin_avr_delay_cycles(__ticks_dc);
  ba:	eb e2       	ldi	r30, 0x2B	; 43
  bc:	f1 e0       	ldi	r31, 0x01	; 1
  be:	31 97       	sbiw	r30, 0x01	; 1
  c0:	f1 f7       	brne	.-4      	; 0xbe <delay+0x8>
  c2:	00 c0       	rjmp	.+0      	; 0xc4 <delay+0xe>
  c4:	00 00       	nop
  c6:	01 97       	sbiw	r24, 0x01	; 1
  c8:	f6 cf       	rjmp	.-20     	; 0xb6 <delay>
		_delay_ms(1);
        //Using the libc routine over and over is non-optimal but it works and is close enough
	} //Note, I may have to reimplement this because the avr-libc delay is too slow *todo*
}
  ca:	08 95       	ret

000000cc <main>:
boolean latch = LOW;

int main()
{
	//Setup timer interrupt and PWM pins
	TCCR0B |= _BV(CS00);
  cc:	83 b7       	in	r24, 0x33	; 51
  ce:	81 60       	ori	r24, 0x01	; 1
  d0:	83 bf       	out	0x33, r24	; 51
	TCCR0A |= _BV(WGM00)|_BV(WGM01);
  d2:	8f b5       	in	r24, 0x2f	; 47
  d4:	83 60       	ori	r24, 0x03	; 3
  d6:	8f bd       	out	0x2f, r24	; 47
	TIMSK0 |= 2;
  d8:	89 b7       	in	r24, 0x39	; 57
  da:	82 60       	ori	r24, 0x02	; 2
  dc:	89 bf       	out	0x39, r24	; 57
	TCNT0=0;
  de:	12 be       	out	0x32, r1	; 50
	sei();
  e0:	78 94       	sei
	ADMUX=0;
  e2:	17 b8       	out	0x07, r1	; 7
	//Set up ADC clock depending on F_CPU
#if F_CPU <= 200000
	ADCSRA |= _BV(ADEN);
#elif F_CPU <= 1200000 && F_CPU > 200000
	ADCSRA |= _BV(ADEN) | _BV(ADPS1);
  e4:	86 b1       	in	r24, 0x06	; 6
  e6:	82 68       	ori	r24, 0x82	; 130
  e8:	86 b9       	out	0x06, r24	; 6
#else
	ADCSRA |= _BV(ADEN) | _BV(ADPS1) | _BV(ADPS0) | _BV(ADPS2);
#endif


    DDRB |=  (1 << relay_1);    // pinMode(relay_1, OUTPUT);
  ea:	bc 9a       	sbi	0x17, 4	; 23
    DDRB &= ~(1 << btn_1);      // pinMode(btn_1, INPUT);
  ec:	bb 98       	cbi	0x17, 3	; 23
                }
            }
        } else {
            if (!btn_released) {
                delay(100);
                btn_released = true;
  ee:	c1 e0       	ldi	r28, 0x01	; 1
  f0:	d0 e0       	ldi	r29, 0x00	; 0
                if (HIGH == state) {
                    state = LOW;
                    duration = 0;
                } else {
                    state = HIGH;
                    duration = period;
  f2:	80 ea       	ldi	r24, 0xA0	; 160
  f4:	c8 2e       	mov	r12, r24
  f6:	8f e0       	ldi	r24, 0x0F	; 15
  f8:	d8 2e       	mov	r13, r24
  fa:	e1 2c       	mov	r14, r1
  fc:	f1 2c       	mov	r15, r1
    DDRB |=  (1 << relay_1);    // pinMode(relay_1, OUTPUT);
    DDRB &= ~(1 << btn_1);      // pinMode(btn_1, INPUT);

    while (1)
    {
        unsigned long cur_mils = millis();
  fe:	c9 df       	rcall	.-110    	; 0x92 <millis>
 100:	4b 01       	movw	r8, r22
 102:	5c 01       	movw	r10, r24
 104:	80 91 60 00 	lds	r24, 0x0060
 108:	90 91 61 00 	lds	r25, 0x0061

        // Чтение состояния(лог. 1) на порту ввода - вывода (3 вывод порта B):
        if(PINB & (1 << PINB3)) // if (digitalRead(3) == HIGH)
 10c:	b3 9b       	sbis	0x16, 3	; 22
 10e:	2a c0       	rjmp	.+84     	; 0x164 <main+0x98>
        {
            if (btn_released) {
 110:	89 2b       	or	r24, r25
 112:	09 f4       	brne	.+2      	; 0x116 <main+0x4a>
 114:	7d c0       	rjmp	.+250    	; 0x210 <main+0x144>
                btn_released = false;
 116:	10 92 61 00 	sts	0x0061, r1
 11a:	10 92 60 00 	sts	0x0060, r1
                delay(100);
 11e:	84 e6       	ldi	r24, 0x64	; 100
 120:	90 e0       	ldi	r25, 0x00	; 0
 122:	c9 df       	rcall	.-110    	; 0xb6 <delay>
                if (HIGH == state) {
 124:	80 91 76 00 	lds	r24, 0x0076
 128:	90 91 77 00 	lds	r25, 0x0077
 12c:	01 97       	sbiw	r24, 0x01	; 1
 12e:	69 f4       	brne	.+26     	; 0x14a <main+0x7e>
                    state = LOW;
 130:	10 92 77 00 	sts	0x0077, r1
 134:	10 92 76 00 	sts	0x0076, r1
                    duration = 0;
 138:	10 92 72 00 	sts	0x0072, r1
 13c:	10 92 73 00 	sts	0x0073, r1
 140:	10 92 74 00 	sts	0x0074, r1
 144:	10 92 75 00 	sts	0x0075, r1
 148:	63 c0       	rjmp	.+198    	; 0x210 <main+0x144>
                } else {
                    state = HIGH;
 14a:	d0 93 77 00 	sts	0x0077, r29
 14e:	c0 93 76 00 	sts	0x0076, r28
                    duration = period;
 152:	c0 92 72 00 	sts	0x0072, r12
 156:	d0 92 73 00 	sts	0x0073, r13
 15a:	e0 92 74 00 	sts	0x0074, r14
 15e:	f0 92 75 00 	sts	0x0075, r15
 162:	56 c0       	rjmp	.+172    	; 0x210 <main+0x144>
                }
            }
        } else {
            if (!btn_released) {
 164:	89 2b       	or	r24, r25
 166:	39 f4       	brne	.+14     	; 0x176 <main+0xaa>
                delay(100);
 168:	84 e6       	ldi	r24, 0x64	; 100
 16a:	90 e0       	ldi	r25, 0x00	; 0
 16c:	a4 df       	rcall	.-184    	; 0xb6 <delay>
                btn_released = true;
 16e:	d0 93 61 00 	sts	0x0061, r29
 172:	c0 93 60 00 	sts	0x0060, r28
            }
            if (state == HIGH) {
 176:	80 91 76 00 	lds	r24, 0x0076
 17a:	90 91 77 00 	lds	r25, 0x0077
 17e:	01 97       	sbiw	r24, 0x01	; 1
 180:	09 f0       	breq	.+2      	; 0x184 <main+0xb8>
 182:	46 c0       	rjmp	.+140    	; 0x210 <main+0x144>
                if (cur_mils < prev_mils) {
 184:	80 91 6e 00 	lds	r24, 0x006E
 188:	90 91 6f 00 	lds	r25, 0x006F
 18c:	a0 91 70 00 	lds	r26, 0x0070
 190:	b0 91 71 00 	lds	r27, 0x0071
 194:	88 16       	cp	r8, r24
 196:	99 06       	cpc	r9, r25
 198:	aa 06       	cpc	r10, r26
 19a:	bb 06       	cpc	r11, r27
 19c:	48 f4       	brcc	.+18     	; 0x1b0 <main+0xe4>
                    prev_mils = cur_mils;
 19e:	80 92 6e 00 	sts	0x006E, r8
 1a2:	90 92 6f 00 	sts	0x006F, r9
 1a6:	a0 92 70 00 	sts	0x0070, r10
 1aa:	b0 92 71 00 	sts	0x0071, r11
 1ae:	30 c0       	rjmp	.+96     	; 0x210 <main+0x144>
                } else {
                    unsigned long last_time = cur_mils - prev_mils;
                    if ( last_time >= interval ) {
 1b0:	a5 01       	movw	r20, r10
 1b2:	94 01       	movw	r18, r8
 1b4:	28 1b       	sub	r18, r24
 1b6:	39 0b       	sbc	r19, r25
 1b8:	4a 0b       	sbc	r20, r26
 1ba:	5b 0b       	sbc	r21, r27
 1bc:	da 01       	movw	r26, r20
 1be:	c9 01       	movw	r24, r18
 1c0:	88 3e       	cpi	r24, 0xE8	; 232
 1c2:	93 40       	sbci	r25, 0x03	; 3
 1c4:	a1 05       	cpc	r26, r1
 1c6:	b1 05       	cpc	r27, r1
 1c8:	18 f1       	brcs	.+70     	; 0x210 <main+0x144>
                        prev_mils = cur_mils;
 1ca:	80 92 6e 00 	sts	0x006E, r8
 1ce:	90 92 6f 00 	sts	0x006F, r9
 1d2:	a0 92 70 00 	sts	0x0070, r10
 1d6:	b0 92 71 00 	sts	0x0071, r11
                        long decremented = duration - interval;
 1da:	80 91 72 00 	lds	r24, 0x0072
 1de:	90 91 73 00 	lds	r25, 0x0073
 1e2:	a0 91 74 00 	lds	r26, 0x0074
 1e6:	b0 91 75 00 	lds	r27, 0x0075
 1ea:	88 5e       	subi	r24, 0xE8	; 232
 1ec:	93 40       	sbci	r25, 0x03	; 3
 1ee:	a1 09       	sbc	r26, r1
 1f0:	b1 09       	sbc	r27, r1
                        if ( decremented < 0 ) {
 1f2:	b7 ff       	sbrs	r27, 7
 1f4:	05 c0       	rjmp	.+10     	; 0x200 <main+0x134>
                            state = LOW;
 1f6:	10 92 77 00 	sts	0x0077, r1
 1fa:	10 92 76 00 	sts	0x0076, r1
 1fe:	08 c0       	rjmp	.+16     	; 0x210 <main+0x144>
                        } else {
                            duration = decremented;
 200:	80 93 72 00 	sts	0x0072, r24
 204:	90 93 73 00 	sts	0x0073, r25
 208:	a0 93 74 00 	sts	0x0074, r26
 20c:	b0 93 75 00 	sts	0x0075, r27
                        }
                    }
                }
            }
        }
        latch = state;
 210:	80 91 76 00 	lds	r24, 0x0076
 214:	90 91 77 00 	lds	r25, 0x0077
 218:	90 93 6b 00 	sts	0x006B, r25
 21c:	80 93 6a 00 	sts	0x006A, r24
        if ( latch != prev_latch ) {
 220:	20 91 6c 00 	lds	r18, 0x006C
 224:	30 91 6d 00 	lds	r19, 0x006D
 228:	82 17       	cp	r24, r18
 22a:	93 07       	cpc	r25, r19
 22c:	09 f4       	brne	.+2      	; 0x230 <main+0x164>
 22e:	67 cf       	rjmp	.-306    	; 0xfe <main+0x32>
            prev_latch = latch;
 230:	90 93 6d 00 	sts	0x006D, r25
 234:	80 93 6c 00 	sts	0x006C, r24
            if (latch) {
 238:	89 2b       	or	r24, r25
 23a:	11 f0       	breq	.+4      	; 0x240 <main+0x174>
                PORTB |= (1 << relay_1);    // digitalWrite(relay_1, HIGH);
 23c:	c4 9a       	sbi	0x18, 4	; 24
 23e:	5f cf       	rjmp	.-322    	; 0xfe <main+0x32>
            } else {
                PORTB &= ~(1 << relay_1);   // digitalWrite(relay_1, LOW);
 240:	c4 98       	cbi	0x18, 4	; 24
 242:	5d cf       	rjmp	.-326    	; 0xfe <main+0x32>

00000244 <__udivmodsi4>:
 244:	a1 e2       	ldi	r26, 0x21	; 33
 246:	1a 2e       	mov	r1, r26
 248:	aa 1b       	sub	r26, r26
 24a:	bb 1b       	sub	r27, r27
 24c:	fd 01       	movw	r30, r26
 24e:	0d c0       	rjmp	.+26     	; 0x26a <__udivmodsi4_ep>

00000250 <__udivmodsi4_loop>:
 250:	aa 1f       	adc	r26, r26
 252:	bb 1f       	adc	r27, r27
 254:	ee 1f       	adc	r30, r30
 256:	ff 1f       	adc	r31, r31
 258:	a2 17       	cp	r26, r18
 25a:	b3 07       	cpc	r27, r19
 25c:	e4 07       	cpc	r30, r20
 25e:	f5 07       	cpc	r31, r21
 260:	20 f0       	brcs	.+8      	; 0x26a <__udivmodsi4_ep>
 262:	a2 1b       	sub	r26, r18
 264:	b3 0b       	sbc	r27, r19
 266:	e4 0b       	sbc	r30, r20
 268:	f5 0b       	sbc	r31, r21

0000026a <__udivmodsi4_ep>:
 26a:	66 1f       	adc	r22, r22
 26c:	77 1f       	adc	r23, r23
 26e:	88 1f       	adc	r24, r24
 270:	99 1f       	adc	r25, r25
 272:	1a 94       	dec	r1
 274:	69 f7       	brne	.-38     	; 0x250 <__udivmodsi4_loop>
 276:	60 95       	com	r22
 278:	70 95       	com	r23
 27a:	80 95       	com	r24
 27c:	90 95       	com	r25
 27e:	9b 01       	movw	r18, r22
 280:	ac 01       	movw	r20, r24
 282:	bd 01       	movw	r22, r26
 284:	cf 01       	movw	r24, r30
 286:	08 95       	ret

00000288 <_exit>:
 288:	f8 94       	cli

0000028a <__stop_program>:
 28a:	ff cf       	rjmp	.-2      	; 0x28a <__stop_program>
