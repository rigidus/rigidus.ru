ARDUINO_PATH = /home/${USER}/build/arduino-1.8.9
PROJECT_PATH = ./
AVRTOOLS_PATH = hardware/tools/avr
PROGRAM = delay_switch5
MCU = attiny13
CC = $(ARDUINO_PATH)/$(AVRTOOLS_PATH)/bin/avr-gcc
OBJCOPY = avr-objcopy
CFLAGS += -Wall -g -Os -mmcu=$(MCU) -I$(ARDUINO_PATH)/$(AVRTOOLS_PATH)/avr/include
LDFLAGS +=
OBJS = $(PROGRAM).o

all: $(PROGRAM).hex

$(PROGRAM).hex: $(PROGRAM).elf
	$(OBJCOPY) -O ihex $< $@

$(PROGRAM).elf: $(PROGRAM).o
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $^

%.o: %.c
	$(CC) $(CFLAGS) -o $@ -c $<

flash: $(PROGRAM).hex
	$(ARDUINO_PATH)/$(AVRTOOLS_PATH)/bin/avrdude \
	-C$(ARDUINO_PATH)/$(AVRTOOLS_PATH)/etc/avrdude.conf \
	-v               \
	-pattiny13       \
	-carduino        \
	-P/dev/ttyUSB0   \
	-b19200 \
    -Uflash:w:$(PROJECT_PATH)/$(PROGRAM).hex:i

clean:
	rm -f $(OBJS)
	rm -f *.elf
	rm -f *.elfS
	rm -f *.hex
	rm -f *.hexS


objdump:
	avr-objdump -dS $(PROGRAM).elf > $(PROGRAM).asm

extract:
	$(ARDUINO_PATH)/$(AVRTOOLS_PATH)/bin/avrdude \
	-C$(ARDUINO_PATH)/$(AVRTOOLS_PATH)/etc/avrdude.conf \
	-v               \
	-pattiny13       \
	-carduino        \
	-P/dev/ttyUSB0   \
	-b19200 \
	-Uflash:r:$(PROJECT_PATH)/$(PROGRAM).bin:r

disasm:
	avrdisas -a1 -o1 -s1 $(PROJECT_PATH)/$(PROGRAM).bin


asm: $(PROGRAM).hexS

$(PROGRAM).hexS: $(PROGRAM).elfS
	$(OBJCOPY) -O ihex $< $@

$(PROGRAM).elfS: $(PROGRAM).S
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $^

%.oS: %.S
	$(CC) -mmcu=$(MCU) -xassembler-with-cpp $(PROGRAM).S -nostdlib -o $@ $^

asmflash: $(PROGRAM).hexS
	$(ARDUINO_PATH)/$(AVRTOOLS_PATH)/bin/avrdude \
	-C$(ARDUINO_PATH)/$(AVRTOOLS_PATH)/etc/avrdude.conf \
	-v               \
	-pattiny13       \
	-carduino        \
	-P/dev/ttyUSB0   \
	-b19200 \
    -Uflash:w:$(PROJECT_PATH)/$(PROGRAM).hexS:i
