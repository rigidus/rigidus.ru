ARDUINO_PATH = /home/${USER}/build/arduino-1.8.9
PROJECT_PATH = ./
AVRTOOLS_PATH = hardware/tools/avr
PROGRAM = apollo
MCU = atmega168
AVRDUDE_CONF = -C$(ARDUINO_PATH)/$(AVRTOOLS_PATH)/etc/avrdude.conf
AVRDUDE_CONF =
# PORT = /dev/ttyACM0
PORT = /dev/ttyUSB0
BAUDRATE = 19200
# Пока без фьюзов
# FUSES = -U lfuse:w:0x64:m -U hfuse:w:0xdd:m -U efuse:w:0xff:m
CC = avr-gcc
OBJCOPY = avr-objcopy
CFLAGS += -Wall -g -Os -mmcu=$(MCU) -I$(ARDUINO_PATH)/$(AVRTOOLS_PATH)/avr/include
LDFLAGS +=
OBJS = $(PROGRAM).o $(PROGRAM).oS

.PHONY: all
all: $(PROGRAM).hex

.PHONY: flash
flash: $(PROGRAM).hex
	avrdude          \
	$(AVRDUDE_CONF)  \
	-v               \
	-p$(MCU)         \
	-carduino        \
	-P$(PORT)        \
	-b$(BAUDRATE)    \
	-Uflash:w:$<:i

$(PROGRAM).hex: $(PROGRAM).elf
	$(OBJCOPY) -O ihex $< $@

$(PROGRAM).elf: $(PROGRAM).o
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $^

obj: $(PROGRAM).c
	$(CC) $(CFLAGS) $(LDFLAGS) -o $(PROGRAM).o $^


# Получение ассемблерного листинга из elf
.PHONY: objdump
objdump: $(PROGRAM).elf
	avr-objdump -d $^ > $(PROGRAM).asm

# Удаление объектных и целевых файлов
# Удаляем не все .o файлы, а только те что в списке $(OBJS)
.PHONY: clean
clean:
	rm -f $(OBJS)
	rm -f *.elf
	rm -f *.elfS
	rm -f *.hex
	rm -f *.hexS
	rm -f *.bin

# c
.PHONY: cflash
cflash: $(PROGRAM).hexC
	avrdude          \
	$(AVRDUDE_CONF)  \
	-v               \
	-p$(MCU)         \
	-carduino        \
	-P$(PORT)        \
	-b$(BAUDRATE)    \
	-Uflash:w:$(PROGRAM).hexC:i


$(PROGRAM).hexC: $(PROGRAM).elfC
	$(OBJCOPY) -O ihex $< $@

$(PROGRAM).elfC: $(PROGRAM).c
	$(CC)         \
	$(CFLAGS)     \
	$(LDFLAGS)    \
	$^            \
	-o $@

# asm
.PHONY: asmflash
asmflash: $(PROGRAM).hexS
	avrdude          \
	$(AVRDUDE_CONF)  \
	-v               \
	-p$(MCU)         \
	-carduino        \
	-P$(PORT)        \
	-b$(BAUDRATE)    \
	-Uflash:w:$(PROGRAM).hexS:i

$(PROGRAM).hexS: $(PROGRAM).elfS
	$(OBJCOPY) -O ihex $< $@

$(PROGRAM).elfS: $(PROGRAM).oS
	$(CC)         \
	$(CFLAGS)     \
	$(LDFLAGS)    \
	-nostartfiles \
	-g            \
	$^            \
	-o $@

$(PROGRAM).oS: $(PROGRAM).S
	$(CC)         \
	$(CFLAGS)     \
	$(LDFLAGS)    \
	-nostartfiles \
	-g            \
	$^            \
	-o $@

# Получение ассемблерного листинга из elfS
objdumpS: $(PROGRAM).elfS
	avr-objdump -D $^ > $(PROGRAM).asmS

asmsize: $(PROGRAM).elfS
	avr-size --mcu=attiny2313 --format=avr $^

# Извлечение прошивки в файл HEX
.PHONY: extract
extract:
	avrdude \
	$(AVRDUDE_PATH)  \
	-v               \
	-p$(MCU)         \
	-carduino        \
	-P$(PORT)        \
	-b$(BAUDRATE)    \
	-Uflash:r:$(PROGRAM).hexbin:i

# Преобразование файла HEX в BIN
.PHONY: hex2bin
hex2bin: $(PROGRAM).hexbin
	objcopy -I ihex $^ -O binary $(PROGRAM).bin

# Дизассемблирование BIN-файла
.PHONY: disasm
disasm: $(PROGRAM).bin
	# avrdisas -a1 -o1 -s1 $(PROJECT_PATH)/$(PROGRAM).bin
	avrdisas -a0 -o0 -s1 -p0 -l1 -m$(MCU) $^ > $(PROGRAM).asm

.PHONY: fulldisasm
fulldisasm:
	make extract
	make hex2bin
	make disasm

%.o: %.c
	$(CC) $(CFLAGS) -o $@ -c $<
