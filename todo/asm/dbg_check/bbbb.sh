
as --32 bbbb.S -o bbbb.O

ld -static                              \
   -m elf_i386                          \
   -o bbbb                              \
   -L/usr/lib/gcc/i686-linux-gnu/5      \
   /usr/lib/i386-linux-gnu/crt1.o       \
   /usr/lib/i386-linux-gnu/crti.o       \
   bbbb.O                               \
   /usr/lib/i386-linux-gnu/crtn.o       \
   --start-group -lc -lgcc -lgcc_eh --end-group
