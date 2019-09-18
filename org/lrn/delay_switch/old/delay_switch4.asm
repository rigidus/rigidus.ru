
delay_switch4.elf:     file format elf32-avr


Disassembly of section .text:

00000000 <__vectors>:
   0:	09 c0       	rjmp	.+18     	; 0x14 <__ctors_end>
   2:	16 c0       	rjmp	.+44     	; 0x30 <__bad_interrupt>
   4:	15 c0       	rjmp	.+42     	; 0x30 <__bad_interrupt>
   6:	15 c0       	rjmp	.+42     	; 0x32 <__vector_3>
   8:	13 c0       	rjmp	.+38     	; 0x30 <__bad_interrupt>
   a:	12 c0       	rjmp	.+36     	; 0x30 <__bad_interrupt>
   c:	11 c0       	rjmp	.+34     	; 0x30 <__bad_interrupt>
   e:	10 c0       	rjmp	.+32     	; 0x30 <__bad_interrupt>
  10:	0f c0       	rjmp	.+30     	; 0x30 <__bad_interrupt>
  12:	0e c0       	rjmp	.+28     	; 0x30 <__bad_interrupt>

00000014 <__ctors_end>:
  14:	11 24       	eor	r1, r1
  16:	1f be       	out	0x3f, r1	; 63
  18:	cf e9       	ldi	r28, 0x9F	; 159
  1a:	cd bf       	out	0x3d, r28	; 61

0000001c <__do_clear_bss>:
  1c:	20 e0       	ldi	r18, 0x00	; 0
  1e:	a0 e6       	ldi	r26, 0x60	; 96
  20:	b0 e0       	ldi	r27, 0x00	; 0
  22:	01 c0       	rjmp	.+2      	; 0x26 <.do_clear_bss_start>

00000024 <.do_clear_bss_loop>:
  24:	1d 92       	st	X+, r1

00000026 <.do_clear_bss_start>:
  26:	a4 36       	cpi	r26, 0x64	; 100
  28:	b2 07       	cpc	r27, r18
  2a:	e1 f7       	brne	.-8      	; 0x24 <.do_clear_bss_loop>
  2c:	44 d0       	rcall	.+136    	; 0xb6 <main>
  2e:	74 c0       	rjmp	.+232    	; 0x118 <_exit>

00000030 <__bad_interrupt>:
  30:	e7 cf       	rjmp	.-50     	; 0x0 <__vectors>

00000032 <__vector_3>:
#include <avr/io.h>
#include <util/delay.h>
#include <avr/interrupt.h>

volatile unsigned long ovrf=0;
ISR(TIM0_OVF_vect){
  32:	1f 92       	push	r1
  34:	0f 92       	push	r0
  36:	0f b6       	in	r0, 0x3f	; 63
  38:	0f 92       	push	r0
  3a:	11 24       	eor	r1, r1
  3c:	8f 93       	push	r24
  3e:	9f 93       	push	r25
  40:	af 93       	push	r26
  42:	bf 93       	push	r27
    ovrf++; //Increment counter every 256 clock cycles
  44:	80 91 60 00 	lds	r24, 0x0060
  48:	90 91 61 00 	lds	r25, 0x0061
  4c:	a0 91 62 00 	lds	r26, 0x0062
  50:	b0 91 63 00 	lds	r27, 0x0063
  54:	01 96       	adiw	r24, 0x01	; 1
  56:	a1 1d       	adc	r26, r1
  58:	b1 1d       	adc	r27, r1
  5a:	80 93 60 00 	sts	0x0060, r24
  5e:	90 93 61 00 	sts	0x0061, r25
  62:	a0 93 62 00 	sts	0x0062, r26
  66:	b0 93 63 00 	sts	0x0063, r27
}
  6a:	bf 91       	pop	r27
  6c:	af 91       	pop	r26
  6e:	9f 91       	pop	r25
  70:	8f 91       	pop	r24
  72:	0f 90       	pop	r0
  74:	0f be       	out	0x3f, r0	; 63
  76:	0f 90       	pop	r0
  78:	1f 90       	pop	r1
  7a:	18 95       	reti

0000007c <millis>:

unsigned long millis(){
    unsigned long x;
    asm("cli");
  7c:	f8 94       	cli
    /*Scale number of timer overflows to milliseconds*/
    x = ovrf / 5;
  7e:	60 91 60 00 	lds	r22, 0x0060
  82:	70 91 61 00 	lds	r23, 0x0061
  86:	80 91 62 00 	lds	r24, 0x0062
  8a:	90 91 63 00 	lds	r25, 0x0063
    asm("sei");
  8e:	78 94       	sei
    return x;
  90:	25 e0       	ldi	r18, 0x05	; 5
  92:	30 e0       	ldi	r19, 0x00	; 0
  94:	40 e0       	ldi	r20, 0x00	; 0
  96:	50 e0       	ldi	r21, 0x00	; 0
  98:	1d d0       	rcall	.+58     	; 0xd4 <__udivmodsi4>
}
  9a:	ca 01       	movw	r24, r20
  9c:	b9 01       	movw	r22, r18
  9e:	08 95       	ret

000000a0 <delay>:

void delay(unsigned ms) {
    while(ms--){
  a0:	00 97       	sbiw	r24, 0x00	; 0
  a2:	41 f0       	breq	.+16     	; 0xb4 <delay+0x14>
	#else
		//round up by default
		__ticks_dc = (uint32_t)(ceil(fabs(__tmp)));
	#endif

	__builtin_avr_delay_cycles(__ticks_dc);
  a4:	eb e2       	ldi	r30, 0x2B	; 43
  a6:	f1 e0       	ldi	r31, 0x01	; 1
  a8:	31 97       	sbiw	r30, 0x01	; 1
  aa:	f1 f7       	brne	.-4      	; 0xa8 <delay+0x8>
  ac:	00 c0       	rjmp	.+0      	; 0xae <delay+0xe>
  ae:	00 00       	nop
  b0:	01 97       	sbiw	r24, 0x01	; 1
  b2:	f6 cf       	rjmp	.-20     	; 0xa0 <delay>
        _delay_ms(1);
    }
}
  b4:	08 95       	ret

000000b6 <main>:

int main()
{
    //Setup timer interrupt and PWM pins
    TCCR0B |= _BV(CS00);
  b6:	83 b7       	in	r24, 0x33	; 51
  b8:	81 60       	ori	r24, 0x01	; 1
  ba:	83 bf       	out	0x33, r24	; 51
    TCCR0A |= _BV(WGM00)|_BV(WGM01);
  bc:	8f b5       	in	r24, 0x2f	; 47
  be:	83 60       	ori	r24, 0x03	; 3
  c0:	8f bd       	out	0x2f, r24	; 47
    TIMSK0 |= 2;
  c2:	89 b7       	in	r24, 0x39	; 57
  c4:	82 60       	ori	r24, 0x02	; 2
  c6:	89 bf       	out	0x39, r24	; 57
    TCNT0=0;
  c8:	12 be       	out	0x32, r1	; 50
    sei();
  ca:	78 94       	sei

    DDRB |=  (1 << relay_1);    // pinMode(relay_1, OUTPUT);
  cc:	bc 9a       	sbi	0x17, 4	; 23
    DDRB &= ~(1 << btn_1);      // pinMode(btn_1, INPUT);
  ce:	bb 98       	cbi	0x17, 3	; 23

    while (1)
    {
        unsigned long cur_mils = millis();
  d0:	d5 df       	rcall	.-86     	; 0x7c <millis>
  d2:	fe cf       	rjmp	.-4      	; 0xd0 <main+0x1a>

000000d4 <__udivmodsi4>:
  d4:	a1 e2       	ldi	r26, 0x21	; 33
  d6:	1a 2e       	mov	r1, r26
  d8:	aa 1b       	sub	r26, r26
  da:	bb 1b       	sub	r27, r27
  dc:	fd 01       	movw	r30, r26
  de:	0d c0       	rjmp	.+26     	; 0xfa <__udivmodsi4_ep>

000000e0 <__udivmodsi4_loop>:
  e0:	aa 1f       	adc	r26, r26
  e2:	bb 1f       	adc	r27, r27
  e4:	ee 1f       	adc	r30, r30
  e6:	ff 1f       	adc	r31, r31
  e8:	a2 17       	cp	r26, r18
  ea:	b3 07       	cpc	r27, r19
  ec:	e4 07       	cpc	r30, r20
  ee:	f5 07       	cpc	r31, r21
  f0:	20 f0       	brcs	.+8      	; 0xfa <__udivmodsi4_ep>
  f2:	a2 1b       	sub	r26, r18
  f4:	b3 0b       	sbc	r27, r19
  f6:	e4 0b       	sbc	r30, r20
  f8:	f5 0b       	sbc	r31, r21

000000fa <__udivmodsi4_ep>:
  fa:	66 1f       	adc	r22, r22
  fc:	77 1f       	adc	r23, r23
  fe:	88 1f       	adc	r24, r24
 100:	99 1f       	adc	r25, r25
 102:	1a 94       	dec	r1
 104:	69 f7       	brne	.-38     	; 0xe0 <__udivmodsi4_loop>
 106:	60 95       	com	r22
 108:	70 95       	com	r23
 10a:	80 95       	com	r24
 10c:	90 95       	com	r25
 10e:	9b 01       	movw	r18, r22
 110:	ac 01       	movw	r20, r24
 112:	bd 01       	movw	r22, r26
 114:	cf 01       	movw	r24, r30
 116:	08 95       	ret

00000118 <_exit>:
 118:	f8 94       	cli

0000011a <__stop_program>:
 11a:	ff cf       	rjmp	.-2      	; 0x11a <__stop_program>
