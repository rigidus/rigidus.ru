
    .text
    .global main
    .org 0

vectors:
	rjmp	ctors_end
	rjmp	bad_interrupt
	rjmp	bad_interrupt
	rjmp	vector_3
	rjmp	bad_interrupt
	rjmp	bad_interrupt
	rjmp	bad_interrupt
	rjmp	bad_interrupt
	rjmp	bad_interrupt
	rjmp	bad_interrupt

ctors_end:
	eor	r1, r1
	out	0x3f, r1	; 63
	ldi	r28, 0x9F	; 159
	out	0x3d, r28	; 61

do_copy_data:
	ldi	r17, 0x00	; 0
	ldi	r26, 0x60	; 96
	ldi	r27, 0x00	; 0
	ldi	r30, 0x8C	; 140
	ldi	r31, 0x02	; 2
	rjmp	0x2c
_0x28:
	lpm	r0, Z+
	st	X+, r0
_0x2c:
	cpi	r26, 0x6A	; 106
	cpc	r27, r17
	brne	_0x28

do_clear_bss:
	ldi	r18, 0x00	; 0
	ldi	r26, 0x6A	; 106
	ldi	r27, 0x00	; 0
	rjmp	do_clear_bss_start

do_clear_bss_loop:
	st	X+, r1

do_clear_bss_start:
	cpi	r26, 0x7C	; 124
	cpc	r27, r18
	brne	do_clear_bss_loop
	rcall	main
	rjmp	exit

bad_interrupt:
	rjmp	vectors

vector_3:
	push	r1
	push	r0
	in	r0, 0x3f	; 63
	push	r0
	eor	r1, r1
	push	r24
	push	r25
	push	r26
	push	r27
	lds	r24, 0x0078
	lds	r25, 0x0079
	lds	r26, 0x007A
	lds	r27, 0x007B
	adiw	r24, 0x01	; 1
	adc	r26, r1
	adc	r27, r1
	sts	0x0078, r24
	sts	0x0079, r25
	sts	0x007A, r26
	sts	0x007B, r27
	pop	r27
	pop	r26
	pop	r25
	pop	r24
	pop	r0
	out	0x3f, r0	; 63
	pop	r0
	pop	r1
	reti

millis:
	cli
	lds	r22, 0x0078
	lds	r23, 0x0079
	lds	r24, 0x007A
	lds	r25, 0x007B
	sei
	ldi	r18, 0x05	; 5
	ldi	r19, 0x00	; 0
	ldi	r20, 0x00	; 0
	ldi	r21, 0x00	; 0
	rcall	udivmodsi4
	movw	r24, r20
	movw	r22, r18
	ret

delay:
	sbiw	r24, 0x00	; 0
	breq	_0xca
	ldi	r30, 0x2B	; 43
	ldi	r31, 0x01	; 1
_0xbe:
	sbiw	r30, 0x01	; 1
	brne	_0xbe
	rjmp	_0xc4
_0xc4:
	nop
	sbiw	r24, 0x01	; 1
	rjmp	delay
_0xca:
    ret

main:
	in	r24, 0x33	; 51
	ori	r24, 0x01	; 1
	out	0x33, r24	; 51
	in	r24, 0x2f	; 47
	ori	r24, 0x03	; 3
	out	0x2f, r24	; 47
	in	r24, 0x39	; 57
	ori	r24, 0x02	; 2
	out	0x39, r24	; 57
	out	0x32, r1	; 50
	sei
	out	0x07, r1	; 7
	in	r24, 0x06	; 6
	ori	r24, 0x82	; 130
	out	0x06, r24	; 6
	sbi	0x17, 4	; 23
	cbi	0x17, 3	; 23
	ldi	r28, 0x01	; 1
	ldi	r29, 0x00	; 0
	ldi	r24, 0xA0	; 160
	mov	r12, r24
	ldi	r24, 0x0F	; 15
	mov	r13, r24
	mov	r14, r1
	mov	r15, r1
_0xfe:
	rcall	millis
	movw	r8, r22
	movw	r10, r24
	lds	r24, 0x0060
	lds	r25, 0x0061
	sbis	0x16, 3	; 22
	rjmp	_0x164
	or	r24, r25
	brne	_0x116
	rjmp	_0x210
_0x116:
	sts	0x0061, r1
	sts	0x0060, r1
	ldi	r24, 0x64	; 100
	ldi	r25, 0x00	; 0
	rcall	delay
	lds	r24, 0x0076
	lds	r25, 0x0077
	sbiw	r24, 0x01	; 1
	brne	0x14a
	sts	0x0077, r1
	sts	0x0076, r1
	sts	0x0072, r1
	sts	0x0073, r1
	sts	0x0074, r1
	sts	0x0075, r1
	rjmp	_0x210
_0x14a:
	sts	0x0077, r29
	sts	0x0076, r28
	sts	0x0072, r12
	sts	0x0073, r13
	sts	0x0074, r14
	sts	0x0075, r15
	rjmp	_0x210
_0x164:
	or	r24, r25
	brne	_0x176
	ldi	r24, 0x64	; 100
	ldi	r25, 0x00	; 0
	rcall	delay
	sts	0x0061, r29
	sts	0x0060, r28
_0x176:
	lds	r24, 0x0076
	lds	r25, 0x0077
	sbiw	r24, 0x01	; 1
	breq	_0x184
	rjmp	_0x210
_0x184:
	lds	r24, 0x006E
	lds	r25, 0x006F
	lds	r26, 0x0070
	lds	r27, 0x0071
	cp	r8, r24
	cpc	r9, r25
	cpc	r10, r26
	cpc	r11, r27
	brcc	_0x1b0
	sts	0x006E, r8
	sts	0x006F, r9
	sts	0x0070, r10
	sts	0x0071, r11
	rjmp	_0x210
_0x1b0:
	movw	r20, r10
	movw	r18, r8
	sub	r18, r24
	sbc	r19, r25
	sbc	r20, r26
	sbc	r21, r27
	movw	r26, r20
	movw	r24, r18
	cpi	r24, 0xE8	; 232
	sbci	r25, 0x03	; 3
	cpc	r26, r1
	cpc	r27, r1
	brcs	_0x210
	sts	0x006E, r8
	sts	0x006F, r9
	sts	0x0070, r10
	sts	0x0071, r11
	lds	r24, 0x0072
	lds	r25, 0x0073
	lds	r26, 0x0074
	lds	r27, 0x0075
	subi	r24, 0xE8	; 232
	sbci	r25, 0x03	; 3
	sbc	r26, r1
	sbc	r27, r1
	sbrs	r27, 7
	rjmp	_0x200
	sts	0x0077, r1
	sts	0x0076, r1
	rjmp	_0x210
_0x200:
	sts	0x0072, r24
	sts	0x0073, r25
	sts	0x0074, r26
	sts	0x0075, r27
_0x210:
	lds	r24, 0x0076
	lds	r25, 0x0077
	sts	0x006B, r25
	sts	0x006A, r24
	lds	r18, 0x006C
	lds	r19, 0x006D
	cp	r24, r18
	cpc	r25, r19
	brne	_0x230
	rjmp	_0xfe
_0x230:
	sts	0x006D, r25
	sts	0x006C, r24
	or	r24, r25
	breq	_0x240
	sbi	0x18, 4	; 24
	rjmp	_0xfe
_0x240:
	cbi	0x18, 4	; 24
	rjmp	_0xfe

udivmodsi4:
	ldi	r26, 0x21	; 33
	mov	r1, r26
	sub	r26, r26
	sub	r27, r27
	movw	r30, r26
	rjmp	udivmodsi4_ep

udivmodsi4_loop:
	adc	r26, r26
	adc	r27, r27
	adc	r30, r30
	adc	r31, r31
	cp	r26, r18
	cpc	r27, r19
	cpc	r30, r20
	cpc	r31, r21
	brcs	udivmodsi4_ep
	sub	r26, r18
	sbc	r27, r19
	sbc	r30, r20
	sbc	r31, r21

udivmodsi4_ep:
	adc	r22, r22
	adc	r23, r23
	adc	r24, r24
	adc	r25, r25
	dec	r1
	brne	udivmodsi4_loop
	com	r22
	com	r23
	com	r24
	com	r25
	movw	r18, r22
	movw	r20, r24
	movw	r22, r26
	movw	r24, r30
	ret

exit:
    cli

stop_program:
    rjmp	stop_program
