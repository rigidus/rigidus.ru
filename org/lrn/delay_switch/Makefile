ARDUINO_PATH = /home/${USER}/build/arduino-1.8.9
PROJECT_PATH = ./
AVRTOOLS_PATH = hardware/tools/avr
PROGRAM = b2313
MCU = attiny2313
PORT = /dev/ttyACM0
BAUDRATE = 19200
# Пока без фьюзов
# FUSES = -U lfuse:w:0x64:m -U hfuse:w:0xdd:m -U efuse:w:0xff:m
CC = $(ARDUINO_PATH)/$(AVRTOOLS_PATH)/bin/avr-gcc
OBJCOPY = avr-objcopy
CFLAGS += -Wall -g -Os -mmcu=$(MCU) -I$(ARDUINO_PATH)/$(AVRTOOLS_PATH)/avr/include
LDFLAGS +=
OBJS = $(PROGRAM).o $(PROGRAM).oS

all: $(PROGRAM).hex

flash: $(PROGRAM).hex
	$(ARDUINO_PATH)/$(AVRTOOLS_PATH)/bin/avrdude \
	-C$(ARDUINO_PATH)/$(AVRTOOLS_PATH)/etc/avrdude.conf \
	-v				 \
	-p$(MCU)		 \
	-carduino		 \
	-P$(PORT)		 \
	-b$(BAUDRATE)	 \
	-Uflash:w:$<:i

$(PROGRAM).hex: $(PROGRAM).elf
	$(OBJCOPY) -O ihex $< $@

$(PROGRAM).elf: $(PROGRAM).o
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


asmflash: $(PROGRAM).hexS
	$(ARDUINO_PATH)/$(AVRTOOLS_PATH)/bin/avrdude \
	-C$(ARDUINO_PATH)/$(AVRTOOLS_PATH)/etc/avrdude.conf \
	-v				 \
	-p$(MCU)		 \
	-carduino		 \
	-P$(PORT)		 \
	-b$(BAUDRATE)	 \
	-Uflash:w:$(PROGRAM).hexS:i

$(PROGRAM).hexS: $(PROGRAM).elfS
	$(OBJCOPY) -O ihex $< $@

$(PROGRAM).elfS: $(PROGRAM).oS
	$(CC) \
	$(CFLAGS) \
	$(LDFLAGS) \
	-nostartfiles \
	$^ \
	-o $@

$(PROGRAM).oS: $(PROGRAM).S
	$(CC) \
	$(CFLAGS) \
	$(LDFLAGS) \
	-nostartfiles \
	$^ \
	-o $@

# Получение ассемблерного листинга из elfS
objdumpS: $(PROGRAM).elfS
	avr-objdump -d $^ > $(PROGRAM).asmS



# Извлечение прошивки в файл HEX
extract:
	$(ARDUINO_PATH)/$(AVRTOOLS_PATH)/bin/avrdude \
	-C$(ARDUINO_PATH)/$(AVRTOOLS_PATH)/etc/avrdude.conf \
	-v				 \
	-p$(MCU)		 \
	-carduino		 \
	-P$(PORT)		 \
	-b$(BAUDRATE)	 \
	-Uflash:r:$(PROGRAM).hexbin:i

# Преобразование файла HEX в BIN
hex2bin: $(PROGRAM).hexbin
	objcopy -I ihex $^ -O binary $(PROGRAM).bin

# Дизассемблирование BIN-файла
disasm: $(PROGRAM).bin
	# avrdisas -a1 -o1 -s1 $(PROJECT_PATH)/$(PROGRAM).bin
	avrdisas -a0 -o0 -s1 -p0 -l1 -m$(MCU) $^ > $(PROGRAM).asm

%.o: %.c
	$(CC) $(CFLAGS) -o $@ -c $<
