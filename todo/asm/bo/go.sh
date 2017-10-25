
as --32 aaaa.S -o aaaa.O

ld -static                              \
   -m elf_i386                          \
   -o aaaa                              \
   -L/usr/lib32                         \
   -L/lib/i386-linux-gnu                \
   -L/usr/lib/gcc/x86_64-linux-gnu/6/32 \
   /usr/lib32/crt1.o                    \
   /usr/lib32/crti.o                    \
   aaaa.O                               \
   /usr/lib32/crtn.o                    \
   --start-group -lc -lgcc -lgcc_eh --end-group
