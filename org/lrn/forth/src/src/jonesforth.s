.set F_IMMED,0x80
.set F_HIDDEN,0x20
.set F_LENMASK,0x1f  # length mask

.macro NEXT
    lodsl
    jmp *(%eax)
.endm

.macro PUSHRSP reg
    lea -4(%ebp), %ebp      # push reg в стек возвратов
    movl \reg, (%ebp)
.endm

.macro POPRSP reg
    mov (%ebp),\reg         # pop вершину стека возвратов в reg
    lea 4(%ebp), %ebp
.endm

    .set link,0   #  Store the chain of links.
.macro defword name, namelen, flags=0, label
    .section .rodata
    .align 4
    .globl name_\label
    name_\label :
    .int link               # link
    .set link,name_\label
    .byte \flags+\namelen   # flags + байт длины
    .ascii "\name"          # имя
    .align 4                # выравнивание на 4-х байтовую границу
    .globl \label
    \label :
    .int DOCOL              # codeword - указатель на функцию-интепретатор
    # list of word pointers follow
.endm

.macro defcode name, namelen, flags=0, label
    .section .rodata
    .align 4
    .globl name_\label
name_\label :
    .int    link               # link
    .set    link,name_\label
    .byte   \flags+\namelen    # flags + байт длины
    .ascii  "\name"            # имя
    .align  4                  # выравнивание на 4-х байтовую границу
    .globl  \label
\label :
    .int    code_\label        # codeword
    .text
    //.align 4
    .globl  code_\label
    code_\label :              # далее следует ассемблерный код
.endm

.macro defvar name, namelen, flags=0, label, initial=0
    defcode \name,\namelen,\flags,\label
    push    $var_\name
    NEXT
    .data
    .align 4
    var_\name :
    .int \initial
.endm

.macro defconst name, namelen, flags=0, label, value
    defcode \name,\namelen,\flags,\label
    push $\value
    NEXT
.endm

defvar "STATE",5,,STATE
defvar "HERE",4,,HERE
defvar "LATEST",6,,LATEST,name_SYSCALL0   # SYSCALL0 must be last in built-in dictionary
defvar "S0",2,,SZ
defvar "BASE",4,,BASE,10

.set JONES_VERSION,47

defconst "VERSION",7,,VERSION,JONES_VERSION
defconst "R0",2,,RZ,return_stack_top
defconst "DOCOL",5,,__DOCOL,DOCOL
defconst "F_IMMED",7,,__F_IMMED,F_IMMED
defconst "F_HIDDEN",8,,__F_HIDDEN,F_HIDDEN
defconst "F_LENMASK",9,,__F_LENMASK,F_LENMASK

.set sys_exit,1
.set sys_read,3
.set sys_write,4
.set sys_open,5
.set sys_close,6
.set sys_creat,8
.set sys_unlink,0xA
.set sys_lseek,0x13
.set sys_truncate,0x5C

.set stdin, 2

.set __NR_exit,  93
.set __NR_open,  1024
.set __NR_close, 57
.set __NR_read,  4
.set __NR_write, 64
.set __NR_creat, 1064
.set __NR_brk,   214

defconst "SYS_EXIT",8,,SYS_EXIT,__NR_exit
defconst "SYS_OPEN",8,,SYS_OPEN,__NR_open
defconst "SYS_CLOSE",9,,SYS_CLOSE,__NR_close
defconst "SYS_READ",8,,SYS_READ,__NR_read
defconst "SYS_WRITE",9,,SYS_WRITE,__NR_write
defconst "SYS_CREAT",9,,SYS_CREAT,__NR_creat
defconst "SYS_BRK",7,,SYS_BRK,__NR_brk

defconst "O_RDONLY",8,,__O_RDONLY,0
defconst "O_WRONLY",8,,__O_WRONLY,1
defconst "O_RDWR",6,,__O_RDWR,2
defconst "O_CREAT",7,,__O_CREAT,0100
defconst "O_EXCL",6,,__O_EXCL,0200
defconst "O_TRUNC",7,,__O_TRUNC,01000
defconst "O_APPEND",8,,__O_APPEND,02000
defconst "O_NONBLOCK",10,,__O_NONBLOCK,04000

    .text
    .align 4
DOCOL:
    PUSHRSP %esi            # push %esi on to the return stack
    addl    $4, %eax        # %eax points to codeword, so make
    movl    %eax, %esi      # %esi point to first data word
    NEXT

defcode ">R",2,,TOR
    popl    %eax            # pop parameter stack into %eax
    PUSHRSP %eax            # push it on to the return stack
    NEXT

defcode "R>",2,,FROMR
    POPRSP  %eax            # pop return stack on to %eax
    pushl   %eax            # and push on to parameter stack
    NEXT

defcode "RSP@",4,,RSPFETCH
    pushl    %ebp
    NEXT

defcode "RSP!",4,,RSPSTORE
    popl    %ebp
    NEXT

defcode "RDROP",5,,RDROP
    addl    $4, %ebp        # pop return stack and throw away
    NEXT



defcode "CHAR",4,,CHAR
    call    _WORD           # Returns %ecx = length, %edi = pointer to word.
    xor     %eax, %eax
    movb    (%edi), %al     # Get the first character of the word.
    push    %eax            # Push it onto the stack.
    NEXT

defcode "EXECUTE",7,,EXECUTE
    pop     %eax            # Get xt into %eax
    jmp     *(%eax)         # and jump to it.
    # After xt runs its NEXT will continue executing the current word.

defcode "SYSCALL3",8,,SYSCALL3
    pop     %eax            # System call number (see <asm/unistd.h>)
    pop     %ebx            # First parameter.
    pop     %ecx            # Second parameter
    pop     %edx            # Third parameter
    int     $0x80
    push    %eax            # Result (negative for -errno)
    NEXT

defcode "SYSCALL2",8,,SYSCALL2
    pop     %eax            # System call number (see <asm/unistd.h>)
    pop     %ebx            # First parameter.
    pop     %ecx            # Second parameter
    int     $0x80
    push    %eax            # Result (negative for -errno)
    NEXT

defcode "SYSCALL1",8,,SYSCALL1
    pop     %eax            # System call number (see <asm/unistd.h>)
    pop     %ebx            # First parameter.
    int     $0x80
    push    %eax            # Result (negative for -errno)
    NEXT

defcode "SYSCALL0",8,,SYSCALL0
    pop     %eax            # System call number (see <asm/unistd.h>)
    int     $0x80
    push    %eax            # Result (negative for -errno)
    NEXT

    defcode "KEY",3,,KEY
    call _KEY
    push    %eax            #       # push возвращенне значение на стек
    NEXT                    #
_KEY:                       # <--+
    mov     (currkey), %ebx #    |  # Берем указатель currkey в %ebx
    cmp     (bufftop), %ebx #    |  # (bufftop >= currkey)?
    jge     1f              #-+  |  # ?-Да, переходим вперед
    xor     %eax, %eax      # |  |  # ?-Нет,
    mov     (%ebx), %al     # |  |  #        переносим указатель смещешения в начало (на ноль)
    inc     %ebx            # |  |  #        и инкрементируем
    mov     %ebx, (currkey) # |  |  #        записываем в переменную
    ret                     # |  |  #        и выходим (в %eax лежит 0)
    # ---------------- RET    |  |
1:  #                     <---+  |  # Буфер ввода пуст, сделаем read из stdin
    mov     $sys_read, %eax #    |  # param1: SYSCALL #3 (read)
    mov     $stdin, %ebx    #    |  # param2: Дескриптор #2 (stdin)
    mov     $input_buffer, %ecx #|  # param3: Кладем адрес буфера ввода в %ecx
    mov     %ecx, currkey   #    |  # Сохраняем адрес буфера ввода в currkey
    mov     INPUT_BUFFER_SIZE, %edx # Максимальная длина ввода
    int     $0x80           #    |  # SYSCALL
    # Проверяем возвращенное     |
    test    %eax, %eax      #    |  # (%eax <= 0)?
    jbe     2f              #-+  |  # ?-Да, это ошибка, переходим вперед
    addl    %eax, %ecx      # |  |  # ?-Нет, добавляем в %ecx кол-во прочитанных байт
    mov     %ecx, bufftop   # |  |  #        записываем %ecx в bufftop
    jmp     _KEY            # |  |
    # ------------------------|--+
2:  #                     <---+     # Ошибка или конец потока ввода - выходим
    mov     $sys_exit, %eax         # param1: SYSCALL #1 (exit)
    xor     %ebx, %ebx              # param2: код возврата
    int     $0x80                   # SYSCALL
    # --------------- EXIT
    .data
    .align 4
currkey:
    # Хранит смещение на текущее положение в буфере ввода (следующий символ будет прочитан по нему)
    .int input_buffer
bufftop:
    # Хранит вершину буфера ввода (последние валидные данные + 1)
    .int input_buffer

    defcode "WORD",4,,WORD
    call    _WORD
    push    %edi            # push base address
    push    %ecx            # push length
    NEXT
_WORD:
    # Ищем первый непробельный символ, пропуская комменты, начинающиеся с обратного слэша
1:                      # <---+
    call    _KEY            # |     # Получаем следующую букву, возвращаемую в %eax
    cmpb    $'\\', %al      # |     # (Это начало комментария)?
    je      3f              #-|---+ # ?-Да, переходим вперед
    cmpb    $' ', %al       # |   | # ?-Нет. (Это пробел, возрат каретки, перевод строки)?
    jbe     1b              #-+   | # ?-Да, переходим назад
    #                             |
    # Ищем конец слова, сохраняя символы по мере продвижения
    mov     $word_buffer, %edi  # | # Указатель на возвращаемы буфер
2:                      # <---+   |
    stosb                   # |   | # Добавляем символ в возвращаемый буфер
    call    _KEY            # |   | # Вызываем KEY символ будет возвращен в %al
    cmpb    $' ', %al       # |   | # (Это пробел)?
    ja      2b              #-+   | # Если нет, повторим
    #                       #     |
    # Вернем слово (указатель на статический буфер черех %ecx) и его длину (через %edi)
    sub     $word_buffer, %edi  # |
    mov     %edi, %ecx      #     | # return: длина слова
    mov     $word_buffer, %edi  # | # return: адрес буфера
    ret                     #     |
    # ----------------- RET       |
    #                             |
    # Это комментарий, пропускаем | его до конца строки
3:                      # <---+ <-+
    call    _KEY            # |
    cmpb    $'\n', %al      # |     # KEY вернул конец строки?
    jne     3b              #-+     # Нет, повторим
    jmp     1b              #
    # ---------------- to 1

    .data
    # Статический буфер, в котором возвращается WORD.
    # Последующие вызовы перезаписывают этот буфер.
    # Максимальная длина слова - 32 символа.
word_buffer:
    .space 32

    defcode "FIND",4,,FIND
    pop     %ecx            # %ecx = length
    pop     %edi            # %edi = address
    call    _FIND
    push    %eax            # %eax = address of dictionary entry (or NULL)
    NEXT

_FIND:
    push    %esi            # Save %esi so we can use it in string comparison.

    # Now we start searching backwards through the dictionary for this word.
    mov     var_LATEST, %edx # LATEST points to name header of the latest word in the dictionary
1:
    test    %edx, %edx      # NULL pointer?  (end of the linked list)
    je      4f

    # Compare the length expected and the length of the word.
    # Note that if the F_HIDDEN flag is set on the word, then by a bit of trickery
    # this won't pick the word (the length will appear to be wrong).
    xor     %eax, %eax
    movb    4(%edx), %al    # %al = flags+length field
    andb    $(F_HIDDEN|F_LENMASK), %al # %al = name length
    cmpb    %cl, %al        # Length is the same?
    jne     2f

    # Compare the strings in detail.
    push    %ecx            # Save the length
    push    %edi            # Save the address (repe cmpsb will move this pointer)
    lea     5(%edx), %esi   # Dictionary string we are checking against.
    repe    cmpsb           # Compare the strings.
    pop     %edi
    pop     %ecx
    jne     2f              # Not the same.

    # The strings are the same - return the header pointer in %eax
    pop     %esi
    mov     %edx, %eax
    ret
2:
    mov     (%edx), %edx    # Move back through the link field to the previous word
    jmp     1b              # .. and loop.
4:
    # Not found.
    pop     %esi
    xor     %eax, %eax      # Return zero to indicate not found.
    ret

    defcode ">CFA",4,,TCFA
    pop     %edi
    call    _TCFA
    push    %edi
    NEXT
_TCFA:
    xor     %eax, %eax
    add     $4, %edi        # Skip link pointer.
    movb    (%edi), %al     # Load flags+len into %al.
    inc     %edi            # Skip flags+len byte.
    andb    $F_LENMASK, %al # Just the length, not the flags.
    add     %eax, %edi      # Skip the name.
    addl    $3, %edi        # The codeword is 4-byte aligned.
    andl    $~3, %edi
    ret

    defcode "NUMBER",6,,NUMBER
    pop     %ecx            # length of string
    pop     %edi            # start address of string
    call    _NUMBER
    push    %eax            # parsed number
    push    %ecx            # number of unparsed characters (0 = no error)
    NEXT

_NUMBER:
    xor     %eax, %eax
    xor     %ebx, %ebx

    test    %ecx, %ecx      # trying to parse a zero-length string is an error, but will return 0.
    jz      5f

    movl    var_BASE, %edx  # get BASE (in %dl)

    # Check if first character is '-'.
    movb    (%edi), %bl     # %bl = first character in string
    inc     %edi
    push    %eax            # push 0 on stack
    cmpb    $'-', %bl       # negative number?
    jnz     2f
    pop     %eax
    push    %ebx            # push <> 0 on stack, indicating negative
    dec     %ecx
    jnz     1f
    pop     %ebx            # error: string is only '-'.
    movl    $1, %ecx
    ret
    # Loop reading digits.
1:
    imull   %edx, %eax      # %eax *= BASE
    movb    (%edi), %bl     # %bl = next character in string
    inc     %edi
    # Convert 0-9, A-Z to a number 0-35.
2:
    subb    $'0', %bl       # < '0'?
    jb      4f
    cmp     $10, %bl        # <= '9'?
    jb      3f
    subb    $17, %bl        # < 'A'? (17 is 'A'-'0')
    jb      4f
    addb    $10, %bl
3:
    cmp     %dl, %bl        # >= BASE?
    jge     4f
    # OK, so add it to %eax and loop.
    add     %ebx, %eax
    dec     %ecx
    jnz     1b
    # Negate the result if first character was '-' (saved on the stack).
4:
    pop     %ebx
    test    %ebx, %ebx
    jz      5f
    neg     %eax
5:
    ret
defcode "CREATE",6,,CREATE

    # Get the name length and address.
    pop     %ecx            # %ecx = length
    pop     %ebx            # %ebx = address of name

    # Link pointer.
    movl    var_HERE, %edi  # %edi is the address of the header
    movl    var_LATEST, %eax    # Get link pointer
    stosl                   # and store it in the header.

    # Length byte and the word itself.
    mov     %cl,%al         # Get the length.
    stosb                   # Store the length/flags byte.
    push    %esi
    mov     %ebx, %esi      # %esi = word
    rep     movsb           # Copy the word
    pop     %esi
    addl    $3, %edi        # Align to next 4 byte boundary.
    andl    $~3, %edi

    # Update LATEST and HERE.
    movl    var_HERE, %eax
    movl    %eax, var_LATEST
    movl    %edi, var_HERE
    NEXT

defcode "LIT",3,,LIT
    # %esi указывает на следующую команду, но в этом случае это указатель на следующий
    # литерал, представляющий собой 4 байтовое значение. Получение этого литерала в %eax
    # и инкремент %esi на x86 -  это удобная однобайтовая инструкция! (см. NEXT macro)
    lodsl
    # push literal в стек
    push %eax
    NEXT
defcode "LITSTRING",9,,LITSTRING
    lodsl                   # get the length of the string
    push    %esi            # push the address of the start of the string
    push    %eax            # push it on the stack
    addl    %eax,%esi       # skip past the string
    addl    $3,%esi         # but round up to next 4 byte boundary
    andl    $~3,%esi
    NEXT

defcode "TELL",4,,TELL
    mov     $1,%ebx         # 1st param: stdout
    pop     %edx            # 3rd param: length of string
    pop     %ecx            # 2nd param: address of string
    mov     $__NR_write,%eax    # write syscall
    int     $0x80
    NEXT

defcode ",",1,,COMMA
    pop     %eax        # Code pointer to store.
    call    _COMMA
    NEXT
_COMMA:
    movl    var_HERE, %edi  # HERE
    stosl                   # Store it.
    movl    %edi, var_HERE  # Update HERE (incremented)
    ret

defcode "INTERPRET",9,,INTERPRET
    call    _WORD           # Returns %ecx = length, %edi = pointer to word.

    # Is it in the dictionary?
    xor     %eax, %eax
    movl    %eax, interpret_is_lit  # Not a literal number (not yet anyway ...)
    call    _FIND           # Returns %eax = pointer to header or 0 if not found.
    test    %eax, %eax      # Found?
    jz  1f

    # In the dictionary.  Is it an IMMEDIATE codeword?
    mov     %eax, %edi      # %edi = dictionary entry
    movb    4(%edi), %al    # Get name+flags.
    push    %ax             # Just save it for now.
    call    _TCFA           # Convert dictionary entry (in %edi) to codeword pointer.
    pop     %ax
    andb    $F_IMMED, %al   # Is IMMED flag set?
    mov     %edi, %eax
    jnz     4f              # If IMMED, jump straight to executing.

    jmp 2f

1:
    # Not in the dictionary (not a word) so assume it's a literal number.
    incl    interpret_is_lit
    call    _NUMBER         # Returns the parsed number in %eax, %ecx > 0 if error
    test    %ecx, %ecx
    jnz     6f
    mov     %eax, %ebx
    mov     $LIT, %eax      # The word is LIT

2:
    # Are we compiling or executing?
    movl    var_STATE, %edx
    test    %edx, %edx
    jz  4f                  # Jump if executing.

    # Compiling - just append the word to the current dictionary definition.
    call    _COMMA
    mov     interpret_is_lit, %ecx # Was it a literal?
    test    %ecx, %ecx
    jz  3f
    mov     %ebx, %eax      # Yes, so LIT is followed by a number.
    call    _COMMA
3:
    NEXT

4:
    # Executing - run it!
    mov     interpret_is_lit, %ecx # Literal?
    test    %ecx, %ecx      # Literal?
    jnz     5f

    # Not a literal, execute it now.  This never returns, but the codeword will
    # eventually call NEXT which will reenter the loop in QUIT.
    jmp     *(%eax)

5:
    # Executing a literal, which means push it on the stack.
    push    %ebx
    NEXT

6:
    # Parse error (not a known word or a number in the current BASE).
    # Print an error message followed by up to 40 characters of context.
    mov     $2, %ebx        # 1st param: stderr
    mov     $errmsg, %ecx   # 2nd param: error message
    mov     $errmsgend-errmsg, %edx # 3rd param: length of string
    mov     $__NR_write, %eax    # write syscall
    int     $0x80

    mov     (currkey), %ecx # the error occurred just before currkey position
    mov     %ecx, %edx
    sub     $input_buffer, %edx   # %edx = currkey - buffer (length in buffer before currkey)
    cmp     $40, %edx       # if > 40, then print only 40 characters
    jle     7f
    mov     $40, %edx
7:
    sub     %edx, %ecx      # %ecx = start of area to print, %edx = length
    mov     $__NR_write, %eax    # write syscall
    int     $0x80

    mov     $errmsgnl, %ecx # newline
    mov     $1, %edx
    mov     $__NR_write, %eax    # write syscall
    int     $0x80

    NEXT

    .section .rodata
errmsg:  .ascii "PARSE ERROR: "
errmsgend:
errmsgnl:    .ascii "\n"

    .data                   # NB: easier to fit in the .data section
    .align 4
interpret_is_lit:
    .int 0                  # Flag used to record if reading a literal

defcode "BRANCH",6,,BRANCH
    add     (%esi),%esi     # add the offset to the instruction pointer
    NEXT

defcode "0BRANCH",7,,ZBRANCH
    pop     %eax
    test    %eax, %eax      # top of stack is zero?
    jz      code_BRANCH     # if so, jump back to the branch function above
    lodsl                   # otherwise we need to skip the offset
    NEXT

# QUIT must not return (ie. must not call EXIT).
defword "QUIT",4,,QUIT
    .int RZ,RSPSTORE    # R0 RSP!, clear the return stack
    .int INTERPRET      # interpret the next word
    .int BRANCH,-8      # and loop (indefinitely)

    /* Assembler entry point. */

    .text
    .globl  forth_asm_start
    .type   forth_asm_start, @function
forth_asm_start:
    # Сбрасываем флаг направления
    cld
    # Записываем вершину стека %esp параметров в переменную S0
    mov     %esp, var_S0
    # Устанавливаем стек возвратов %ebp
    mov     $return_stack_top, %ebp
    # Устанавливаем указатель HERE на начало области данных.
    mov     $data_buffer, %eax
    mov     %eax, var_HERE
    # Инициализируем IP
    mov     $cold_start, %esi
    # Запускаем интерпретатор
    NEXT

    .section .rodata
cold_start:                             # High-level code without a codeword.
    .int QUIT

    .bss

    /* Forth return stack. */
    .set RETURN_STACK_SIZE,8192
    .align 4096
return_stack:
    .space RETURN_STACK_SIZE
return_stack_top:           # Initial top of return stack.

    /* This is used as a temporary input buffer when reading from files or the terminal. */
    .set INPUT_BUFFER_SIZE,4096
    .align 4096
input_buffer:
    .space INPUT_BUFFER_SIZE

    .set INITIAL_DATA_SEGMENT_SIZE,65536
    .align 4096
data_buffer:
    .space INITIAL_DATA_SEGMENT_SIZE
