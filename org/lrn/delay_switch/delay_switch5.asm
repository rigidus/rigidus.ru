
delay_switch5.elf:     file format elf32-avr


Disassembly of section .text:

00000000 <__vectors>:
   0:	09 c0       	rjmp	.+18     	; 0x14 <__ctors_end>
   2:	0e c0       	rjmp	.+28     	; 0x20 <__bad_interrupt>
   4:	0d c0       	rjmp	.+26     	; 0x20 <__bad_interrupt>
   6:	0d c0       	rjmp	.+26     	; 0x22 <__vector_3>
   8:	0b c0       	rjmp	.+22     	; 0x20 <__bad_interrupt>
   a:	0a c0       	rjmp	.+20     	; 0x20 <__bad_interrupt>
   c:	09 c0       	rjmp	.+18     	; 0x20 <__bad_interrupt>
   e:	08 c0       	rjmp	.+16     	; 0x20 <__bad_interrupt>
  10:	07 c0       	rjmp	.+14     	; 0x20 <__bad_interrupt>
  12:	06 c0       	rjmp	.+12     	; 0x20 <__bad_interrupt>

00000014 <__ctors_end>:
  14:	11 24       	eor	r1, r1
  16:	1f be       	out	0x3f, r1	; 63
  18:	cf e9       	ldi	r28, 0x9F	; 159
  1a:	cd bf       	out	0x3d, r28	; 61
  1c:	14 d0       	rcall	.+40     	; 0x46 <main>
  1e:	1e c0       	rjmp	.+60     	; 0x5c <_exit>

00000020 <__bad_interrupt>:
  20:	ef cf       	rjmp	.-34     	; 0x0 <__vectors>

00000022 <__vector_3>:


#define LED_PIN PB0

ISR(TIM0_OVF_vect)
{
  22:	1f 92       	push	r1
  24:	0f 92       	push	r0
  26:	0f b6       	in	r0, 0x3f	; 63
  28:	0f 92       	push	r0
  2a:	11 24       	eor	r1, r1
  2c:	8f 93       	push	r24
  2e:	9f 93       	push	r25

    PORTB ^= _BV(LED_PIN); // toggle LED pin
  30:	98 b3       	in	r25, 0x18	; 24
  32:	81 e0       	ldi	r24, 0x01	; 1
  34:	89 27       	eor	r24, r25
  36:	88 bb       	out	0x18, r24	; 24
}
  38:	9f 91       	pop	r25
  3a:	8f 91       	pop	r24
  3c:	0f 90       	pop	r0
  3e:	0f be       	out	0x3f, r0	; 63
  40:	0f 90       	pop	r0
  42:	1f 90       	pop	r1
  44:	18 95       	reti

00000046 <main>:
int
main(void)
{

    /* setup */
    DDRB = 0b00000001; // set LED pin as OUTPUT
  46:	81 e0       	ldi	r24, 0x01	; 1
  48:	87 bb       	out	0x17, r24	; 23
    PORTB = 0b00000000; // set all pins to LOW
  4a:	18 ba       	out	0x18, r1	; 24
    TCCR0B |= _BV(CS02)|_BV(CS00); // set prescaler to 1024 (CLK=1200000Hz/1024/256=4Hz, 0.25s)
  4c:	83 b7       	in	r24, 0x33	; 51
  4e:	85 60       	ori	r24, 0x05	; 5
  50:	83 bf       	out	0x33, r24	; 51
    TIMSK0 |= _BV(TOIE0); // enable Timer Overflow interrupt
  52:	89 b7       	in	r24, 0x39	; 57
  54:	82 60       	ori	r24, 0x02	; 2
  56:	89 bf       	out	0x39, r24	; 57
    sei(); // enable global interrupts
  58:	78 94       	sei
  5a:	ff cf       	rjmp	.-2      	; 0x5a <main+0x14>

0000005c <_exit>:
  5c:	f8 94       	cli

0000005e <__stop_program>:
  5e:	ff cf       	rjmp	.-2      	; 0x5e <__stop_program>
