ARDUINO_PATH = /home/${USER}/build/arduino-1.8.9
AVRTOOLS_PATH = hardware/tools/avr
PROGRAM = delay_switch5
MCU = attiny13
CC = $(ARDUINO_PATH)/$(AVRTOOLS_PATH)/bin/avr-gcc
OBJCOPY = avr-objcopy
CFLAGS += -Wall -g -Os -mmcu=$(MCU) -I$(ARDUINO_PATH)/$(AVRTOOLS_PATH)/avr/include
LDFLAGS +=
OBJS = $(PROGRAM).o

all: $(PROGRAM).hex

flash: $(PROGRAM).hex
	$(ARDUINO_PATH)/$(AVRTOOLS_PATH)/bin/avrdude \
	-C$(ARDUINO_PATH)/$(AVRTOOLS_PATH)/etc/avrdude.conf \
	-v               \
	-pattiny13       \
	-carduino        \
	-P/dev/ttyUSB0   \
	-b19200 \
    -Uflash:w:$(PROGRAM).hex:i

$(PROGRAM).hex: $(PROGRAM).elf
	$(OBJCOPY) -O ihex $< $@

$(PROGRAM).elf: $(PROGRAM).c
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $^

obj: $(PROGRAM).c
	$(CC) $(CFLAGS) $(LDFLAGS) -o $(PROGRAM).o $^


# Получение ассемблерного листинга из elf
objdump: $(PROGRAM).elf
	avr-objdump -d $^ > $(PROGRAM).asm


# Удаление объектных и целевых файлов
# Удаляем не все .o файлы, а только те что в списке $(OBJS)
clean:
	rm -f $(OBJS)
	rm -f *.elf
	rm -f *.elfS
	rm -f *.hex
	rm -f *.hexS
	rm -f *.bin

# Ассеблерная компиляция
asmflash: $(PROGRAM).hexS
	$(ARDUINO_PATH)/$(AVRTOOLS_PATH)/bin/avrdude \
	-C$(ARDUINO_PATH)/$(AVRTOOLS_PATH)/etc/avrdude.conf \
	-v               \
	-pattiny13       \
	-carduino        \
	-P/dev/ttyUSB0   \
	-b19200 \
    -Uflash:w:$(PROGRAM).hexS:i


asm: $(PROGRAM).hexS

$(PROGRAM).hexS: $(PROGRAM).elfS
	$(OBJCOPY) -O ihex $< $@

$(PROGRAM).elfS: $(PROGRAM).S
	$(CC) $(CFLAGS) -nostartfiles $(LDFLAGS) -o $@ $^

# Получение ассемблерного листинга из elfS
objdumpS: $(PROGRAM).elfS
	avr-objdump -dS $^ > $(PROGRAM).asmS



# Извлечение прошивки в файл HEX
extract:
	$(ARDUINO_PATH)/$(AVRTOOLS_PATH)/bin/avrdude \
	-C$(ARDUINO_PATH)/$(AVRTOOLS_PATH)/etc/avrdude.conf \
	-v               \
	-pattiny13       \
	-carduino        \
	-P/dev/ttyUSB0   \
	-b19200 \
	-Uflash:r:$(PROGRAM).hexbin:i

# Преобразование файла HEX в BIN
hex2bin: $(PROGRAM).hexbin
	objcopy -I ihex $^ -O binary $(PROGRAM).bin

# Дизассемблирование BIN-файла
disasm: $(PROGRAM).bin
	avrdisas -a0 -o0 -s1 -p0 -l1 -m$(MCU) $^ > $(PROGRAM).asm
