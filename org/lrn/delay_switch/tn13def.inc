;***** THIS IS A MACHINE GENERATED FILE - DO NOT EDIT ********************
;***** Created: 2011-02-09 12:03 ******* Source: ATtiny13.xml ************
;*************************************************************************
;* A P P L I C A T I O N   N O T E   F O R   T H E   A V R   F A M I L Y
;*
;* Number            : AVR000
;* File Name         : "tn13def.inc"
;* Title             : Register/Bit Definitions for the ATtiny13
;* Date              : 2011-02-09
;* Version           : 2.35
;* Support E-mail    : avr@atmel.com
;* Target MCU        : ATtiny13
;*
;* DESCRIPTION
;* When including this file in the assembly program file, all I/O register
;* names and I/O register bit names appearing in the data book can be used.
;* In addition, the six registers forming the three data pointers X, Y and
;* Z have been assigned names XL - ZH. Highest RAM address for Internal
;* SRAM is also defined
;*
;* The Register names are represented by their hexadecimal address.
;*
;* The Register Bit names are represented by their bit number (0-7).
;*
;* Please observe the difference in using the bit names with instructions
;* such as "sbr"/"cbr" (set/clear bit in register) and "sbrs"/"sbrc"
;* (skip if bit in register set/cleared). The following example illustrates
;* this:
;*
;* in    r16,PORTB             ;read PORTB latch
;* sbr   r16,(1<<PB6)+(1<<PB5) ;set PB6 and PB5 (use masks, not bit#)
;* out   PORTB,r16             ;output to PORTB
;*
;* in    r16,TIFR              ;read the Timer Interrupt Flag Register
;* sbrc  r16,TOV0              ;test the overflow flag (use bit#)
;* rjmp  TOV0_is_set           ;jump if set
;* ...                         ;otherwise do something else
;*************************************************************************

#ifndef _TN13DEF_INC_
#define _TN13DEF_INC_


#pragma partinc 0

; ***** SPECIFY DEVICE ***************************************************
.set	SIGNATURE_000	, 0x1e
.set	SIGNATURE_001	, 0x90
.set	SIGNATURE_002	, 0x07

; ***** I/O REGISTER DEFINITIONS *****************************************
; NOTE:
; Definitions marked "MEMORY MAPPED"are extended I/O ports
; and cannot be used with IN/OUT instructions
.set	SREG	, 0x3f
.set	SPL	, 0x3d
.set	GIMSK	, 0x3b
.set	GIFR	, 0x3a
.set	TIMSK0	, 0x39
.set	TIFR0	, 0x38
.set	SPMCSR	, 0x37
.set	OCR0A	, 0x36
.set	MCUCR	, 0x35
.set	MCUSR	, 0x34
.set	TCCR0B	, 0x33
.set	TCNT0	, 0x32
.set	OSCCAL	, 0x31
.set	TCCR0A	, 0x2f
.set	DWDR	, 0x2e
.set	OCR0B	, 0x29
.set	GTCCR	, 0x28
.set	CLKPR	, 0x26
.set	WDTCR	, 0x21
.set	EEAR	, 0x1e
.set	EEDR	, 0x1d
.set	EECR	, 0x1c
.set	PORTB	, 0x18
.set	DDRB	, 0x17
.set	PINB	, 0x16
.set	PCMSK	, 0x15
.set	DIDR0	, 0x14
.set	ACSR	, 0x08
.set	ADMUX	, 0x07
.set	ADCSRA	, 0x06
.set	ADCH	, 0x05
.set	ADCL	, 0x04
.set	ADCSRB	, 0x03


; ***** BIT DEFINITIONS **************************************************

; ***** AD_CONVERTER *****************
; ADMUX - The ADC multiplexer Selection Register
.set	MUX0	, 0	; Analog Channel and Gain Selection Bits
.set	MUX1	, 1	; Analog Channel and Gain Selection Bits
.set	ADLAR	, 5	; Left Adjust Result
.set	REFS0	, 6	; Reference Selection Bit 0

; ADCSRA - The ADC Control and Status register
.set	ADPS0	, 0	; ADC  Prescaler Select Bits
.set	ADPS1	, 1	; ADC  Prescaler Select Bits
.set	ADPS2	, 2	; ADC  Prescaler Select Bits
.set	ADIE	, 3	; ADC Interrupt Enable
.set	ADIF	, 4	; ADC Interrupt Flag
.set	ADATE	, 5	; ADC Auto Trigger Enable
.set	ADSC	, 6	; ADC Start Conversion
.set	ADEN	, 7	; ADC Enable

; ADCH - ADC Data Register High Byte
.set	ADCH0	, 0	; ADC Data Register High Byte Bit 0
.set	ADCH1	, 1	; ADC Data Register High Byte Bit 1
.set	ADCH2	, 2	; ADC Data Register High Byte Bit 2
.set	ADCH3	, 3	; ADC Data Register High Byte Bit 3
.set	ADCH4	, 4	; ADC Data Register High Byte Bit 4
.set	ADCH5	, 5	; ADC Data Register High Byte Bit 5
.set	ADCH6	, 6	; ADC Data Register High Byte Bit 6
.set	ADCH7	, 7	; ADC Data Register High Byte Bit 7

; ADCL - ADC Data Register Low Byte
.set	ADCL0	, 0	; ADC Data Register Low Byte Bit 0
.set	ADCL1	, 1	; ADC Data Register Low Byte Bit 1
.set	ADCL2	, 2	; ADC Data Register Low Byte Bit 2
.set	ADCL3	, 3	; ADC Data Register Low Byte Bit 3
.set	ADCL4	, 4	; ADC Data Register Low Byte Bit 4
.set	ADCL5	, 5	; ADC Data Register Low Byte Bit 5
.set	ADCL6	, 6	; ADC Data Register Low Byte Bit 6
.set	ADCL7	, 7	; ADC Data Register Low Byte Bit 7

; ADCSRB - ADC Control and Status Register B
.set	ADTS0	, 0	; ADC Auto Trigger Source 0
.set	ADTS1	, 1	; ADC Auto Trigger Source 1
.set	ADTS2	, 2	; ADC Auto Trigger Source 2

; DIDR0 - Digital Input Disable Register 0
.set	ADC1D	, 2	; ADC2 Digital input Disable
.set	ADC3D	, 3	; ADC3 Digital input Disable
.set	ADC2D	, 4	; ADC2 Digital input Disable
.set	ADC0D	, 5	; ADC0 Digital input Disable


; ***** ANALOG_COMPARATOR ************
; ADCSRB - ADC Control and Status Register B
.set	ACME	, 6	; Analog Comparator Multiplexer Enable

; ACSR - Analog Comparator Control And Status Register
.set	ACIS0	, 0	; Analog Comparator Interrupt Mode Select bit 0
.set	ACIS1	, 1	; Analog Comparator Interrupt Mode Select bit 1
.set	ACIE	, 3	; Analog Comparator Interrupt Enable
.set	ACI	, 4	; Analog Comparator Interrupt Flag
.set	ACO	, 5	; Analog Compare Output
.set	ACBG	, 6	; Analog Comparator Bandgap Select
.set	AINBG	, ACBG	; For compatibility
.set	ACD	, 7	; Analog Comparator Disable

; DIDR0 -
.set	AIN0D	, 0	; AIN0 Digital Input Disable
.set	AIN1D	, 1	; AIN1 Digital Input Disable


; ***** EEPROM ***********************
; EEAR - EEPROM Read/Write Access
.set	EEARL	, EEAR	; For compatibility
.set	EEAR0	, 0	; EEPROM Read/Write Access bit 0
.set	EEAR1	, 1	; EEPROM Read/Write Access bit 1
.set	EEAR2	, 2	; EEPROM Read/Write Access bit 2
.set	EEAR3	, 3	; EEPROM Read/Write Access bit 3
.set	EEAR4	, 4	; EEPROM Read/Write Access bit 4
.set	EEAR5	, 5	; EEPROM Read/Write Access bit 5

; EEDR - EEPROM Data Register
.set	EEDR0	, 0	; EEPROM Data Register bit 0
.set	EEDR1	, 1	; EEPROM Data Register bit 1
.set	EEDR2	, 2	; EEPROM Data Register bit 2
.set	EEDR3	, 3	; EEPROM Data Register bit 3
.set	EEDR4	, 4	; EEPROM Data Register bit 4
.set	EEDR5	, 5	; EEPROM Data Register bit 5
.set	EEDR6	, 6	; EEPROM Data Register bit 6
.set	EEDR7	, 7	; EEPROM Data Register bit 7

; EECR - EEPROM Control Register
.set	EERE	, 0	; EEPROM Read Enable
.set	EEWE	, 1	; EEPROM Write Enable
.set	EEPE	, EEWE	; For compatibility
.set	EEMWE	, 2	; EEPROM Master Write Enable
.set	EEMPE	, EEMWE	; For compatibility
.set	EERIE	, 3	; EEProm Ready Interrupt Enable
.set	EEPM0	, 4	;
.set	EEPM1	, 5	;


; ***** CPU **************************
; SREG - Status Register
.set	SREG_C	, 0	; Carry Flag
.set	SREG_Z	, 1	; Zero Flag
.set	SREG_N	, 2	; Negative Flag
.set	SREG_V	, 3	; Two-s Complement Overflow Flag
.set	SREG_S	, 4	; Sign Bit
.set	SREG_H	, 5	; Half Carry Flag
.set	SREG_T	, 6	; Bit Copy Storage
.set	SREG_I	, 7	; Global Interrupt Enable

; SPL - Stack Pointer Low Byte
.set	SP0	, 0	; Stack Pointer Bit 0
.set	SP1	, 1	; Stack Pointer Bit 1
.set	SP2	, 2	; Stack Pointer Bit 2
.set	SP3	, 3	; Stack Pointer Bit 3
.set	SP4	, 4	; Stack Pointer Bit 4
.set	SP5	, 5	; Stack Pointer Bit 5
.set	SP6	, 6	; Stack Pointer Bit 6
.set	SP7	, 7	; Stack Pointer Bit 7

; MCUCR - MCU Control Register
.set	ISC00	, 0	; Interrupt Sense Control 0 bit 0
.set	ISC01	, 1	; Interrupt Sense Control 0 bit 1
.set	SM0	, 3	; Sleep Mode Select Bit 0
.set	SM1	, 4	; Sleep Mode Select Bit 1
.set	SE	, 5	; Sleep Enable
.set	PUD	, 6	; Pull-up Disable

; MCUSR - MCU Status register
.set	PORF	, 0	; Power-On Reset Flag
.set	EXTRF	, 1	; External Reset Flag
.set	BORF	, 2	; Brown-out Reset Flag
.set	WDRF	, 3	; Watchdog Reset Flag

; OSCCAL - Oscillator Calibration Register
.set	CAL0	, 0	; Oscillatro Calibration Value Bit 0
.set	CAL1	, 1	; Oscillatro Calibration Value Bit 1
.set	CAL2	, 2	; Oscillatro Calibration Value Bit 2
.set	CAL3	, 3	; Oscillatro Calibration Value Bit 3
.set	CAL4	, 4	; Oscillatro Calibration Value Bit 4
.set	CAL5	, 5	; Oscillatro Calibration Value Bit 5
.set	CAL6	, 6	; Oscillatro Calibration Value Bit 6

; CLKPR - Clock Prescale Register
.set	CLKPS0	, 0	; Clock Prescaler Select Bit 0
.set	CLKPS1	, 1	; Clock Prescaler Select Bit 1
.set	CLKPS2	, 2	; Clock Prescaler Select Bit 2
.set	CLKPS3	, 3	; Clock Prescaler Select Bit 3
.set	CLKPCE	, 7	; Clock Prescaler Change Enable

; DWDR - Debug Wire Data Register
.set	DWDR0	, 0	; Debug Wire Data Register Bit 0
.set	DWDR1	, 1	; Debug Wire Data Register Bit 1
.set	DWDR2	, 2	; Debug Wire Data Register Bit 2
.set	DWDR3	, 3	; Debug Wire Data Register Bit 3
.set	DWDR4	, 4	; Debug Wire Data Register Bit 4
.set	DWDR5	, 5	; Debug Wire Data Register Bit 5
.set	DWDR6	, 6	; Debug Wire Data Register Bit 6
.set	DWDR7	, 7	; Debug Wire Data Register Bit 7

; SPMCSR - Store Program Memory Control and Status Register
.set	SPMEN	, 0	; Store program Memory Enable
.set	PGERS	, 1	; Page Erase
.set	PGWRT	, 2	; Page Write
.set	RFLB	, 3	; Read Fuse and Lock Bits
.set	CTPB	, 4	; Clear Temporary Page Buffer


; ***** PORTB ************************
; PORTB - Data Register, Port B
.set	PORTB0	, 0	;
.set	PB0	, 0	; For compatibility
.set	PORTB1	, 1	;
.set	PB1	, 1	; For compatibility
.set	PORTB2	, 2	;
.set	PB2	, 2	; For compatibility
.set	PORTB3	, 3	;
.set	PB3	, 3	; For compatibility
.set	PORTB4	, 4	;
.set	PB4	, 4	; For compatibility
.set	PORTB5	, 5	;
.set	PB5	, 5	; For compatibility

; DDRB - Data Direction Register, Port B
.set	DDB0	, 0	;
.set	DDB1	, 1	;
.set	DDB2	, 2	;
.set	DDB3	, 3	;
.set	DDB4	, 4	;
.set	DDB5	, 5	;

; PINB - Input Pins, Port B
.set	PINB0	, 0	;
.set	PINB1	, 1	;
.set	PINB2	, 2	;
.set	PINB3	, 3	;
.set	PINB4	, 4	;
.set	PINB5	, 5	;


; ***** EXTERNAL_INTERRUPT ***********
; MCUCR - MCU Control Register
;.set	ISC00	, 0	; Interrupt Sense Control 0 Bit 0
;.set	ISC01	, 1	; Interrupt Sense Control 0 Bit 1

; GIMSK - General Interrupt Mask Register
.set	GICR	, GIMSK	; For compatibility
.set	PCIE	, 5	; Pin Change Interrupt Enable
.set	INT0	, 6	; External Interrupt Request 0 Enable

; GIFR - General Interrupt Flag register
.set	PCIF	, 5	; Pin Change Interrupt Flag
.set	INTF0	, 6	; External Interrupt Flag 0

; PCMSK - Pin Change Enable Mask
.set	PCINT0	, 0	; Pin Change Enable Mask Bit 0
.set	PCINT1	, 1	; Pin Change Enable Mask Bit 1
.set	PCINT2	, 2	; Pin Change Enable Mask Bit 2
.set	PCINT3	, 3	; Pin Change Enable Mask Bit 3
.set	PCINT4	, 4	; Pin Change Enable Mask Bit 4
.set	PCINT5	, 5	; Pin Change Enable Mask Bit 5


; ***** TIMER_COUNTER_0 **************
; TIMSK0 - Timer/Counter0 Interrupt Mask Register
.set	TOIE0	, 1	; Timer/Counter0 Overflow Interrupt Enable
.set	OCIE0A	, 2	; Timer/Counter0 Output Compare Match A Interrupt Enable
.set	OCIE0B	, 3	; Timer/Counter0 Output Compare Match B Interrupt Enable

; TIFR0 - Timer/Counter0 Interrupt Flag register
.set	TOV0	, 1	; Timer/Counter0 Overflow Flag
.set	OCF0A	, 2	; Timer/Counter0 Output Compare Flag 0A
.set	OCF0B	, 3	; Timer/Counter0 Output Compare Flag 0B

; OCR0A - Timer/Counter0 Output Compare Register
.set	OCR0A_0	, 0	;
.set	OCR0A_1	, 1	;
.set	OCR0A_2	, 2	;
.set	OCR0A_3	, 3	;
.set	OCR0A_4	, 4	;
.set	OCR0A_5	, 5	;
.set	OCR0A_6	, 6	;
.set	OCR0A_7	, 7	;

; TCCR0A - Timer/Counter  Control Register A
.set	WGM00	, 0	; Waveform Generation Mode
.set	WGM01	, 1	; Waveform Generation Mode
.set	COM0B0	, 4	; Compare Match Output B Mode
.set	COM0B1	, 5	; Compare Match Output B Mode
.set	COM0A0	, 6	; Compare Match Output A Mode
.set	COM0A1	, 7	; Compare Match Output A Mode

; TCNT0 - Timer/Counter0
.set	TCNT0_0	, 0	;
.set	TCNT0_1	, 1	;
.set	TCNT0_2	, 2	;
.set	TCNT0_3	, 3	;
.set	TCNT0_4	, 4	;
.set	TCNT0_5	, 5	;
.set	TCNT0_6	, 6	;
.set	TCNT0_7	, 7	;

; TCCR0B - Timer/Counter Control Register B
.set	CS00	, 0	; Clock Select
.set	CS01	, 1	; Clock Select
.set	CS02	, 2	; Clock Select
.set	WGM02	, 3	; Waveform Generation Mode
.set	FOC0B	, 6	; Force Output Compare B
.set	FOC0A	, 7	; Force Output Compare A

; OCR0B - Timer/Counter0 Output Compare Register
.set	OCR0B_0	, 0	;
.set	OCR0B_1	, 1	;
.set	OCR0B_2	, 2	;
.set	OCR0B_3	, 3	;
.set	OCR0B_4	, 4	;
.set	OCR0B_5	, 5	;
.set	OCR0B_6	, 6	;
.set	OCR0B_7	, 7	;

; GTCCR - General Timer Conuter Register
.set	PSR10	, 0	; Prescaler Reset Timer/Counter0
.set	TSM	, 7	; Timer/Counter Synchronization Mode


; ***** WATCHDOG *********************
; WDTCR - Watchdog Timer Control Register
.set	WDP0	, 0	; Watch Dog Timer Prescaler bit 0
.set	WDP1	, 1	; Watch Dog Timer Prescaler bit 1
.set	WDP2	, 2	; Watch Dog Timer Prescaler bit 2
.set	WDE	, 3	; Watch Dog Enable
.set	WDCE	, 4	; Watchdog Change Enable
.set	WDP3	, 5	; Watchdog Timer Prescaler Bit 3
.set	WDTIE	, 6	; Watchdog Timeout Interrupt Enable
.set	WDTIF	, 7	; Watchdog Timeout Interrupt Flag



; ***** LOCKSBITS ********************************************************
.set	LB1	, 0	; Lockbit
.set	LB2	, 1	; Lockbit


; ***** FUSES ************************************************************
; LOW fuse bits
.set	CKSEL0	, 0	; Select Clock Source
.set	CKSEL1	, 1	; Select Clock Source
.set	SUT0	, 2	; Select start-up time
.set	SUT1	, 3	; Select start-up time
.set	CKDIV8	, 4	; Start up with system clock divided by 8
.set	WDTON	, 5	; Watch dog timer always on
.set	EESAVE	, 6	; Keep EEprom contents during chip erase
.set	SPIEN	, 7	; SPI programming enable

; HIGH fuse bits
.set	RSTDISBL	, 0	; Disable external reset
.set	BODLEVEL0	, 1	; Enable BOD and select level
.set	BODLEVEL1	, 2	; Enable BOD and select level
.set	DWEN	, 3	; DebugWire Enable
.set	SELFPRGEN	, 4	; Self Programming Enable



; ***** CPU REGISTER DEFINITIONS *****************************************
.set	XH	, r27
.set	XL	, r26
.set	YH	, r29
.set	YL	, r28
.set	ZH	, r31
.set	ZL	, r30



; ***** DATA MEMORY DECLARATIONS *****************************************
.set	FLASHEND	, 0x01ff	; Note: Word address
.set	IOEND	, 0x003f
.set	SRAM_START	, 0x0060
.set	SRAM_SIZE	, 64
.set	RAMEND	, 0x009f
.set	XRAMEND	, 0x0000
.set	E2END	, 0x003f
.set	EEPROMEND	, 0x003f
.set	EEADRBITS	, 6
#pragma AVRPART MEMORY PROG_FLASH 1024
#pragma AVRPART MEMORY EEPROM 64
#pragma AVRPART MEMORY INT_SRAM SIZE 64
#pragma AVRPART MEMORY INT_SRAM START_ADDR 0x60



; ***** BOOTLOADER DECLARATIONS ******************************************
.set	PAGESIZE	, 16



; ***** INTERRUPT VECTORS ************************************************
.set	INT0addr	, 0x0001	; External Interrupt 0
.set	PCI0addr	, 0x0002	; External Interrupt Request 0
.set	OVF0addr	, 0x0003	; Timer/Counter0 Overflow
.set	ERDYaddr	, 0x0004	; EEPROM Ready
.set	ACIaddr	, 0x0005	; Analog Comparator
.set	OC0Aaddr	, 0x0006	; Timer/Counter Compare Match A
.set	OC0Baddr	, 0x0007	; Timer/Counter Compare Match B
.set	WDTaddr	, 0x0008	; Watchdog Time-out
.set	ADCCaddr	, 0x0009	; ADC Conversion Complete

.set	INT_VECTORS_SIZE	, 10	; size in words

#endif  /* _TN13DEF_INC_ */

; ***** END OF FILE ******************************************************
