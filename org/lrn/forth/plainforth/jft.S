## Compile time vars

    .set wordsize,8
    .set datastacksize, 1024*16
    .set F_IMMED,0x80
    .set F_HIDDEN,0x20
    .set F_LENMASK,0x1f  # length mask

    .text

## Macro

.macro NEXT
    # %rsi - absolute forth instruction pointer
    lods (%rsi),%rax                # %rax = (%rsi) , %rsi++
    # now %rax relatively points to the codeword callee, let's get an absolute address
    lea  (%r15, %rax), %rax
    # now %rax absolutely points to the codeword callee, let's get relative codeword
    mov  (%rax), %r13
    # now %r13 contains the relative codeword callee, let's get the absolute address of the codeword
    lea  (%r15, %r13), %r13
    # jmp to indirect absolute addr, %rax contains absolute pointer to the codeword callee
    jmp  *%r13
.endm

.macro PUSHRSP reg
    lea     -wordsize(%rbp),%rbp    # decrement %rbp by wordsize
    mov     \reg,(%rbp)             # push reg to return stack
.endm

.macro POPRSP reg
    mov (%rbp),\reg                 # pop top return stack to reg
    lea wordsize(%rbp),%rbp         # increment %rbp by wordsize
.endm

.set link,0 # initial link (compile time variable)

.macro defword name, namelen, flags=0, label
    .align wordsize
    .globl name_\label
name_\label :
    .quad   link
    .set    link,name_\label - v0    # relative link
    .byte   \flags+\namelen     # flags/len
    .ascii  "\name"
    .align  wordsize
    .globl  \label
\label :
    .quad  DOCOL-v0           # relative DOCOL addr as codeword
    # here will be pointers to words
.endm

.macro defcode name, namelen, flags=0, label
    .align wordsize
    .globl name_\label
name_\label :
    .quad   link
    .set    link,name_\label - v0  # relative link
    .byte   \flags+\namelen   # flags/len
    .ascii  "\name"
    .align  wordsize
    .globl  \label
\label :
    .quad   code_\label-v0    # relative codeword
    .align  wordsize
    .globl  code_\label
code_\label :
    # here will be asm code
.endm

.macro defvar name, namelen, flags=0, label, initial=0
    defcode \name,\namelen,\flags,\label
    push    var_\name-v0(%r15)
    NEXT
    .align wordsize
var_\name :
    .quad \initial
.endm

.macro defconst name, namelen, flags=0, label, value
    defcode \name,\namelen,\flags,\label
    push \value
    NEXT
.endm


## Assembler entry point

    .globl  _start
    .type   _start, @function
_start:

    .globl  forth_asm_start
    .type   forth_asm_start, @function
    .align  8
forth_asm_start:
    nop             # short call занимает 8 байт поэтому, чтобы нужны nops
    nop             # чтобы v0-адрес команды следующей за call был выровнен на 8
    nop             # и TCFA работал правильно
    call    forth_entry
v0:
    .quad   0
argc:
    .quad   0
argv:
    .quad   0
cold_start:
    .quad   0

    ## Build-in variables

defvar "STATE",5,,STATE
defvar "HERE",4,,HERE
defvar "LATEST",6,,LATEST,name_SYSCALL0-v0  # SYSCALL0 должен быть последним встроенным словом
defvar "S0",2,,SZ
defvar "BASE",4,,BASE,10

    ## Forth Entry

forth_entry:
    pop     %r15                            # v0 - base of data in text segment

    mov     %rsp, var_S0-v0(%r15)           # save initial stack poiner to var_S0

    mov     (%rsp), %r14                    # argc
    mov     %r14, argc-v0(%r15)             # save argc
    mov     wordsize(%rsp), %r14            # argv
    mov     %r14, argv-v0(%r15)             # save argv
    cld                                     # clear direction flag

    lea     return_stack_top-v0(%r15), %rbp # set return stack
    lea     data_buffer-v0(%r15), %rax
    mov     %rax, var_HERE-v0(%r15)         # set HERE to DATA BUFFER

    mov     $QUIT-v0, %rax                  # lea TARGET
    mov     %rax, cold_start-v0(%r15)       # set TARGET addr to cold_start

    lea     cold_start-v0(%r15), %rsi       # %rsi points to cold_start
                                            # cold_start points to TARGET
    # Run interpret
    NEXT
exit:
    mov     $60, %rax                       # sys_exit
    mov     $0, %rdi                        # retcode 0
    syscall


## Build-in constants

.set JONES_VERSION,47

defconst "VERSION",7,,VERSION,$JONES_VERSION
defconst "R0",2,,RZ,return_stack_top-v0(%r15)
/*
defconst "DOCOL",5,,__DOCOL,DOCOL-v0(%r15)
defconst "F_IMMED",7,,__F_IMMED,F_IMMED
defconst "F_HIDDEN",8,,__F_HIDDEN,F_HIDDEN
defconst "F_LENMASK",9,,__F_LENMASK,F_LENMASK
*/
.set sys_exit,60
.set sys_read,0
.set sys_write,1
/*
.set sys_open,5
.set sys_close,6
.set sys_creat,8
.set sys_unlink,0xA
.set sys_lseek,0x13
.set sys_truncate,0x5C
*/
.set stdin,0
/*
.set stdout,1
*/
.set stderr,2
/*
defconst "SYS_EXIT",8,,SYS_EXIT,$sys_exit
defconst "SYS_OPEN",8,,SYS_OPEN,$sys_open
defconst "SYS_CLOSE",9,,SYS_CLOSE,$sys_close
defconst "SYS_READ",8,,SYS_READ,$sys_read
defconst "SYS_WRITE",9,,SYS_WRITE,$sys_write
defconst "SYS_CREAT",9,,SYS_CREAT,$sys_creat

defconst "O_RDONLY",8,,__O_RDONLY,$0
defconst "O_WRONLY",8,,__O_WRONLY,$1
defconst "O_RDWR",6,,__O_RDWR,$2
defconst "O_CREAT",7,,__O_CREAT,$0100
defconst "O_EXCL",6,,__O_EXCL,$0200
defconst "O_TRUNC",7,,__O_TRUNC,$01000
defconst "O_APPEND",8,,__O_APPEND,$02000
defconst "O_NONBLOCK",10,,__O_NONBLOCK,$04000

defconst "WORDSIZE",8,,WORDSIZE,$wordsize
*/
## DOCOL

    .align wordsize
DOCOL:
    # after NEXT %eax pointed to codeword of current (callee) word
    # and %esi pointed to next ceall after prevous (caller) word
    PUSHRSP %rsi                 # save %rsi to return stack
    lea    wordsize(%rax), %rsi  # %rsi = %rax++ | now %rsi pointed to param-field callee word
    NEXT
/*
## RetStack

defcode ">R",2,,TOR
    pop     %rax            # pop со стека данных в %rax
    PUSHRSP %rax            # push %rax на стек возвратов
    NEXT

defcode "R>",2,,FROMR
    POPRSP  %rax            # pop со стека возвратов в %rax
    push    %rax            # push %rax на стек параметров
    NEXT

defcode "RSP@",4,,RSPFETCH
    push    %rbp
    NEXT
*/
defcode "RSP!",4,,RSPSTORE
    pop     %rbp
    NEXT
/*
defcode "RDROP",5,,RDROP
    add     $wordsize, %rbp
    NEXT
*/




defcode "KEY",3,,KEY
    call _KEY
    push    %rax            #       # push-им возвращенный символ на стек
    NEXT                    #
_KEY:                       # <--+
    mov     currkey-v0(%r15), %rbx  # Берем содержимое currkey (это относительный указатель) в %rbx
    cmp     bufftop-v0(%r15), %rbx  # (bufftop >= currkey)? - в буфере есть символы?
    jge     1f              #-+  |  # ?-Нет, переходим вперед
    xor     %rax, %rax      # |  |  # ?-Да,  (1) переносим символ, на который
    mov     (%rbx), %al     # |  |  #        указывает currkey в %rax,
    inc     %rbx            # |  |  #        (2) инкрементируем копию currkey
    mov     %rbx, currkey-v0(%r15)  #        (3) записываем ее в переменную currkey,
    ret                     # |  |  #        и выходим (в %rax лежит символ)
    # ---------------- RET    |  |
1:  #                     <---+  |  # Буфер ввода пуст, сделаем read из stdin
    push    %rsi            #    |  # Сохраняем %rsi
    push    %rdi            #    |  # и %rdi
    mov     $stdin, %rdi    #    |  #     param1: Дескриптор stdin в %rdi
    lea     input_buffer-v0(%r15), %rsi # param2: Кладем адрес буфера ввода в %rsi
    mov     %rsi, currkey-v0(%r15)  #  Сохраняем его (адрес буфера ввода) в currkey
    mov     $INPUT_BUFFER_SIZE, %rdx#     param3: Максимальная длина ввода в %rdx
    mov     $sys_read, %rax #    |  #  SYSCALL read в %rax
    syscall                 #    |  #  SYSCALL
    # Проверяем возвращенное     |  # должно быть количество символов + '\n'
    test    %rax, %rax      #    |  # (%rax <= 0)?
    jbe     2f              #-+  |  # ?-Да, это ошибка или EOF, переходим вперед
    add     %rax, %rsi      # |  |  # ?-Нет, (1) добавляем в %rsi кол-во прочитанных байт
    mov     %rsi, bufftop-v0(%r15)  #        (2) записываем %rsi в bufftop
    pop     %rdi            # |  |  # Теперь можно восстановить %rdi
    pop     %rsi            # |  |  # и %rsi
    jmp     _KEY            # |  |  № а потом повторить
    # ------------------------|--+
2:  #                     <---+     # Ошибка или конец потока ввода - выходим
    pop     %rdi            #       # Сначала надо восстановить %rdi & %rsi
    pop     %rsi            #
    mov     $sys_exit, %rax         # param1: SYSCALL #1 (exit)
    xor     %rdi, %rdi              # param2: код возврата
    syscall                         # SYSCALL
    # --------------- EXIT
    .align wordsize
currkey:
    # Хранит смещение на текущее положение в буфере ввода (следующий символ будет прочитан по нему)
    .quad input_buffer-v0
bufftop:
    # Хранит вершину буфера ввода (последние валидные данные + 1)
    .quad input_buffer-v0


defcode "WORD",4,,WORD
    call    _WORD
    push    %rdi            # push base address
    push    %rcx            # push length
    NEXT
_WORD:
    # Ищем первый непробельный символ, пропуская комменты, начинающиеся с обратного слэша
1:                      # <---+
    call    _KEY            # |     # Получаем следующую букву, возвращаемую в %rax
    cmpb    $'\\', %al      # |     # (Это начало комментария)?
    je      3f              #-|---+ # ?-Да, переходим вперед
    cmpb    $' ', %al       # |   | # ?-Нет. (Это пробел, возрат каретки, перевод строки)?
    jbe     1b              #-+   | # ?-Да, переходим назад
    #                             |
    # Ищем конец слова, сохраняя символы по мере продвижения
    lea     word_buffer-v0(%r15), %rdi # Указатель на возвращаемый буфер (absolute addr)
2:                      # <---+   |
    stosb                   # |   | # Добавляем символ в возвращаемый буфер
    call    _KEY            # |   | # Вызываем KEY символ будет возвращен в %al
    cmpb    $' ', %al       # |   | # (Это пробел, возрат каретки, перевод строки)?
    ja      2b              #-+   | # Если нет, повторим
    #                       #     |
    # Вернем слово (указатель на статический буфер черех %rcx) и его длину (через %rdi)
    mov     %rdi, %rcx      #     | # Указатель на конец возвращаемого буфера в  %rcx
    lea     word_buffer-v0(%r15), %rdi # Указатель на начало возвращаемого буфера
    sub     %rdi, %rcx          # | # return %rcx: длина слова
    ret                     #     | # return %rdi: адрес буфера
    # ----------------- RET       |
    #                             |
3:  # Это комментарий,  # <---+ <-+ пропускаем его до конца строки
    call    _KEY            # |
    cmpb    $'\n', %al      # |     # KEY вернул конец строки?
    jne     3b              #-+     # Нет, повторим
    jmp     1b              #
    # ---------------- to 1
    # Статический буфер, в котором возвращается WORD.
    # Последующие вызовы перезаписывают этот буфер.
    # Максимальная длина слова - 32 символа.
word_buffer:
    .space 32


defcode "LIT",3,,LIT
    # %rsi указывает на следующую команду, но в этом случае это указатель на следующий
    # литерал, представляющий собой 8-байтовое значение. Получение этого литерала в %rax
    # и инкремент %rsi на x86 -  это удобная однобайтовая инструкция! (см. NEXT macro)
    lods (%rsi),%rax                # %rax = (%rsi) , %rsi++
    # push literal в стек
    push %rax
    NEXT


defcode "NUMBER",6,,NUMBER
    pop     %rcx            # length of string
    pop     %rdi            # start address of string
    call    _NUMBER
    push    %rax            # parsed number
    push    %rcx            # number of unparsed characters (0 = no error)
    NEXT
_NUMBER:
    xor     %rax, %rax
    xor     %rbx, %rbx
    # Попытка распарсить пустую строку это ошибка но мы возвращаем 0
    test    %rcx, %rcx
    jz  5f                  #-> RET #
    # Строка не пуста, будем разбирать
    mov     var_BASE-v0(%r15), %rdx # Получаем BASE в %dl
    # Проверим, может быть первый символ '-'?
    movb    (%rdi), %bl     #       # %bl = первый символ строки
    inc     %rdi            #       #
    push    %rax            #       # push 0 в стек
    cmpb    $'-', %bl       #       # (Отрицательное число)?
    jnz 2f                  #-+     # ?-Нет, переходим к конвертации (2)
    pop     %rax            # |     # ?-Да, заберем обратно 0 из стека,
    push    %rbx            # |     #       push не ноль в стек, как индикатор отрицательного
    dec     %rcx            # |     #       уменьшим счетчик оставшихся символов
    jnz 1f                  #-----+ #       (Строка закончилась)? ?-Нет: переход на (1)
    pop     %rbx            # |   | #       ?-Да - это ошибка, строка "-". Забираем из стека
    mov     $1, %rcx        # |   | #            помещаем в возвращаемую нераспарсенную длину
    ret                     # |   | #            единицу и выходим.
    # --------------------- # |   | # -------------------------------------------------------
    # Цикл чтения чисел     # |   | #
1:  #                    <========+ #
    imul    %rdx, %rax      # |   | # %rax *= BASE
    movb    (%rdi), %bl     # |   | # %bl = следующий символ в строке
    inc     %rdi            # |   | # Увеличиваем указатель
2:  #                    <----+   | #
    # Преобразуем 0-9, A-Z в числа 0-35.
    subb    $'0', %bl       #     | # (< '0')?
    jb  4f                  #---+ | # ?-Да, херня какая-то, а не цифра, ошибка, идем на (4)
    cmp     $10, %bl        #   | | # ?-Нет, (<= '9')?
    jb  3f                  #-+ | | #        ?-Да, идем на (3), это число между 0 и 9
    subb    $17, %bl        # | | | #        ?-Нет, (< 'A')? потому что (17 = 'A'-'0')
    jb  4f                  #---+ | #               ?-Да, это ошибка, идем на (4)
    addb    $10, %bl        # | | | #               ?-Нет, добавляем к значению 10
3:  #                     <---+ | | #
    cmp     %dl, %bl        #   | | #                      (RESULT >= BASE)?
    jge 4f                  #---+ | #                      ?-Да, перебор, идем на (4)
    add     %rbx, %rax      #   | | #                      ?-Нет, все в порядке. Добавляем
    dec     %rcx            #   | | #                        RESULT к %rax и LOOP-им дальше.
    jnz 1b                  #---|-+ #
4:  #                     <-----+   #
    # Тут мы оказываемся если цикл закончился - тогда у нас %rcx=0
    # В ином случае %rcx содержит количество нераспарсенных символов
    # Если у нас отрицательный результат, то первый символ '-' (сохранен в стеке)
    pop     %rbx            #       #
    test    %rbx, %rbx      #       # (Отрицательное число)?
    jz  5f                  #-+     # ?-Нет, возвращаем как есть (5)
    neg     %rax            # |     # ?-Да, инвертируем
5:  #                     <---+
    ret



defcode "FIND",4,,FIND
    pop     %rcx            # %rcx = длина строки
    pop     %rdi            # %rdi = адрес строки (relative?absolute)
    call    _FIND
    push    %rax            # %rax = адрес слова (или ноль) (relative)
    NEXT
_FIND:
    push    %rsi            # Сохраним %rsi - так мы сможем использовать этот
    # регистр для сравнения строк командой CMPSB
    # Здесь мы начинаем искать в словаре это слово от конца к началу словаря
    mov  var_LATEST-v0(%r15), %rdx # теперь %rdx указывает (relative) на последнее слово в словаре
1:  #                   <------------+
    test    %rdx, %rdx      # (в %rdx находится NULL-указатель, т.е. словарь кончился)?
    je  4f                  #-----+  |  # ?-Да, переходим вперед к (4)
    #                             |  |
    # Сравним ожидаемую длину и длину слова
    # Внимание, если F_HIDDEN установлен для этого слова, то совпадения не будет.
    xor     %rax, %rax      #     |  |  # Очищаем %rax
    movb    8(%rdx, %r15), %al #  |  |  # %al = flags+length
    andb    $(F_HIDDEN|F_LENMASK), %al  # %al = теперь длина имени (маскируем флаги)
    cmpb    %cl, %al        #     |  |  # (Длины одинаковые?)
    jne 2f                  #--+  |  |  # ?-Нет, переходим вперед к (2)
    #                          |  |  |
    # Переходим к детальному сравнению
    push    %rcx            #  |  |  |  # Сохраним длину, потому что repe cmpsb уменьшает %rcx
    push    %rdi            #  |  |  |  # Сохраним адрес, потому что repe cmpsb двигает %rdi
    lea     9(%rdx, %r15), %rsi # |  |  # Загружаем в %rsi адрес начала имени слова
    repe    cmpsb           #  |  |  |  # Сравниваем
    pop     %rdi            #  |  |  |  # Восстанавливаем адрес (absolute)
    pop     %rcx            #  |  |  |  # Восстановим длину
    jne 2f                  #--+  |  |  # ?-Если не равны - переходим вперед к (2)
    #                          |  |  |
    # Строки равны - возвратим указатель на заголовок в %rax
    pop     %rsi            #  |  |  |  # Восстановим %rsi
    mov     %rdx, %rax      #  |  |  |  # %rdx все еще содержит rel-указатель, который возвращаем
    ret                     #  |  |  |  # Возврат
    # ----------------- RET    |  |  |
2:  #                     <----+  |  |
    mov     (%rdx, %r15), %rdx #  |  |  # Переходим по относительному указателю к следующему слову
    jmp 1b                  #     |  |  # И зацикливаемся
    # ----------------------------|--+
4:  #                     <-------+
    # Слово не найдено
    pop     %rsi            # Восстановим сохраненный %rsi
    xor     %rax, %rax      # Возвратим ноль в %rax
    ret                     # Возврат


defcode ">CFA",4,,TCFA
    pop     %rdi
    call    _TCFA
    push    %rdi
    NEXT
_TCFA:
    xor     %rax, %rax
    add     $wordsize, %rdi # Пропускаем LINK - указатель на предыдущее слово
    movb    (%rdi, %r15), %al  # Загружаем flags+len в %al
    inc     %rdi            # Пропускаем flags+len байт
    andb    $F_LENMASK, %al # Маскируем, чтобы получить длину имени, без флагов
    add     %rax, %rdi      # Пропускаем имя
    add     $(wordsize-1), %rdi        # Учитываем выравнивание
    and     $~(wordsize-1), %rdi
    ret


defcode ",",1,,COMMA
    pop     %rax      # Взять со стека данных в %rax то значение, которое будем вкомпиливать
    call    _COMMA
    NEXT
_COMMA:
    mov     var_HERE-v0(%r15), %rdi  # получить указатель HERE в %rdi
    stosq                     # Сохраниь по нему значение из %rax
    mov     %rdi, var_HERE-v0(%r15)  # Обновить HERE (используя инкремент, сделанный STOSQ)
    ret


defcode "INTERPRET",9,,INTERPRET
    call    _WORD           # Возвращает %rcx = длину, %rdi = указатель на слово.
    # Есть ли слово в словаре?
    xor     %rax, %rax
    mov     %rax, interpret_is_lit-v0(%r15) # Это не литерал (или пока не литерал)
    call    _FIND           #           # Возвращает в %rax rel-указатель на заголовок или 0
    test    %rax, %rax      #           # (Заголовок)?
    jz  1f                  #--------+  # ?-Нет. Переход вперед к (1)
    # Это словарное слово   #        |  # ?-Да. Найдено совпадающее слово. Продолжаем.
    # Это IMMEDIATE-слово?  #        |  #
    mov     %rax, %rdi      #        |  # %rdi = rel-указатель на слово
    movb    wordsize(%rdi, %r15), %al#  # %al = flags+length.
    push    %rax            #        |  # Сохраним его (flags+length) ненадолго
    call    _TCFA           #        |  # Преобразуем entry (в %rdi) в rel-указатель на codeword
    pop     %rax            #        |  # Восстановим flags+length
    andb    $F_IMMED, %al   #        |  # (Установлен флаг F_IMMED)?
    mov     %rdi, %rax      #        |  # %rdi->%rax
    jnz     4f              #--------|-+# ?-Да, переходим сразу к выполнению (4)
    jmp 2f                  #--+     | |# ?-Нет, переходим к проверке режима работы (2)
    # --------------------- #  |     | |# -------------------------------------------------
1:  #                   <------|-----+ |
    # Нет в словаре, будем считать, что это литерал
    incq interpret_is_lit-v0(%r15) #   |# Установим флаг
    call    _NUMBER         #  |       |# Возвращает число в %rax, %rcx > 0 если ошибка
    test    %rcx, %rcx      #  |       |# (Удалось распарсить число)?
    jnz 6f                  #--|-----+ |# ?-Нет, переходим к (6)
    mov     %rax, %rbx      #  |     | |# ?-Да, Перемещаем число в %rbx,
    mov     LIT-v0(%r15), %rax #     | |#     Устанавливаем слово LIT в %rax для jmp в 4f
2:  #                   <------+     | |#
    # Проверим в каком мы режиме     | |#
    mov     var_STATE-v0(%r15), %rdx#| |#
    test    %rdx, %rdx      #        | |#     (Мы компилируемся или выполняемся)?
    jz  4f                  #-----+  | |#     ?-Выполняемся. Переходим к (4)
    call    _COMMA          #     |  | |#     ?-Компилируемся. Добавляем словарное определение
    mov interpret_is_lit-v0(%r15), %rcx#
    test    %rcx, %rcx      #     |  | |#       (Это был литерал)?
    jz      3f              #--+  |  | |#       ?-Нет, переходим к NEXT
    mov     %rbx, %rax      #  |  |  | |#       ?-Да, поэтому за LIT следует число,
    call    _COMMA          #  |  |  | |#            вызываем _COMMA, чтобы скомпилить его
3:  #                   <------+  |  | |#
    NEXT                    #     |  | |# NEXT
    # --------------------- #     |  | |# -------------------------------------------------
4:  #                   <---------+<-|-+
    # Выполняемся                    |
    mov interpret_is_lit-v0(%r15), %rcx#
    test    %rcx, %rcx      #        |  # (Это литерал)?
    jnz 5f                  #--+     |  # ?-Да, переходим к (5)
    # Не литерал, выполним прямо сейчас. Мы не осуществляем возврата, но
    # codeword в конечном итоге вызовет NEXT, который повторно вернет цикл в QUIT
    lea  (%r15, %rax), %rax #  |     |  # let's get an absolute address
    mov  (%rax), %r13       #  |     |  # let's get relative codeword
    lea  (%r15, %r13), %r13 #  |     |  # let's get the absolute address of the codeword
    # jmp to indirect absolute addr, %rax contains absolute pointer to the codeword callee
    jmp  *%r13              #  |     |
    # --------------------- #  |     |  # -------------------------------------------------
5:  #                    <-----+     |
    # Выполняем литерал, что означает, что мы push-им его в стек и делаем NEXT
    push    %rbx            #        |
    NEXT                    #        |
6:  #                    <-----------+
    # Мы здесь, если не получилось распарсить число в текущей базе или этого
    # слова нет в словаре. Печатаем сообщение об ошибке и 40 символов контекста.
    push    %rsi
    push    %rdi
    push    %rdx
    mov     $stderr, %rdi               # param1: stderr
    lea     errmsg-v0(%r15), %rsi       # param2: Выводимая строка
    mov     $errmsgend-errmsg, %rdx     # param3: Длина выводимой строки
    mov     $sys_write, %rax            # SYSCALL write
    syscall                             # SYSCALL
    pop     %rdx
    pop     %rdi
    pop     %rsi
    # Ошибка произошла перед currkey
    mov     currkey-v0(%r15), %rcx      # currkey-указатель в %rcx
    mov     %rcx, %rdx      #           # и %rdx
    lea     input_buffer-v0(%r15), %r12 # адрес начала input-буфера в %r12
    sub     %r12, %rdx                  # %rdx = (currkey - buffer) (длина буфера перед currkey)
    cmp     $40, %rdx       #           # (if > 40)?
    jle 7f                  #--+        # ?-Нет, печатаем все
    mov     $40, %rdx       #  |        # ?-Да, печатать только 40 символов
7:  #                    <-----+
    sub     %rdx, %rcx      #           # %rcx = start of area to print, %edx = length
    push    %rsi
    push    %rdi
    push    %rdx
    mov     $stderr, %rdi               # param1: stderr
    mov     %rcx, %rsi                  # param2: Выводимая строка
    mov     %rdx, %rdx                  # param3: Длина
    mov     $sys_write, %eax            # SYSCALL write
    syscall                             # SYSCALL
    pop     %rdx
    pop     %rdi
    pop     %rsi
    # Выведем перевод строки
    push    %rsi
    push    %rdi
    mov     $stderr, %rdi               # param1: stderr
    lea     errmsgnl-v0(%r15), %rsi     # param2: newline
    mov     $1, %edx        #           # param3: Длина
    mov     $sys_write, %eax            # SYSCALL write
    syscall                 #           # SYSCALL
    pop     %rdi
    pop     %rsi
    NEXT                    #           # NEXT
    # ---------------------
errmsg:
    .ascii "PARSE ERROR: "
errmsgend:
errmsgnl:
    .ascii "\n"
    .align wordsize
interpret_is_lit:
    .quad 0                  # Флаг литерала

defcode "BRANCH",6,,BRANCH
    add     (%rsi),%rsi     # добавить offset к instruction pointer
    NEXT
/*
defcode "0BRANCH",7,,ZBRANCH
    pop     %rax
    test    %rax, %rax      # Вершина стека равна нулю?
    jz      code_BRANCH     # Если да, вернуться назад к функции BRANCH выше
    lodsq                   # иначе пропустить смещение
    NEXT
*/
# QUIT не должна возвращаться (те есть вызывать EXIT).
defword "QUIT",4,,QUIT
    # Положить константу RZ (начальное значение стека возвратов) на стек параметров.
    .quad RZ-v0
    # Установить значение, лежащее на стеке параметров, как новое значение вершины стека возвратов
    .quad RSPSTORE-v0       # Это очищает стек возвратов
    # Запустить интерпретатор команд                  <---------+
    .quad INTERPRET-v0      # Интерпретировать следующее слово  |
    # И навсегда зациклиться                                    |
    .quad BRANCH-v0,-16     # ----------------------------------+

/*



defcode "SYSCALL3",8,,SYSCALL3
    mov %rsi,%r10 #save %rsi
    mov %rdi,%r9 #save %rdi
    pop %rax        # System call number (see <asm/unistd.h>)
    pop %rdi        # First parameter.
    pop %rsi        # Second parameter
    pop %rdx        # Third parameter
    syscall
    push %rax       # Result (negative for -errno)
    mov %r10,%rsi
    mov %r9,%rdi
    NEXT

defcode "SYSCALL2",8,,SYSCALL2
    mov %rsi,%r10 #save %rsi
    mov %rdi,%r9 #save %rdi
    pop %rax        # System call number (see <asm/unistd.h>)
    pop %rdi        # First parameter.
    pop %rsi        # Second parameter
    syscall
    push %rax       # Result (negative for -errno)
    mov %r10,%rsi
    mov %r9,%rdi
    NEXT

defcode "SYSCALL1",8,,SYSCALL1
    mov %rsi,%r10 #save %rsi
    mov %rdi,%r9 #save %rdi
    pop %rax        # System call number (see <asm/unistd.h>)
    pop %rdi        # First parameter.
    syscall
    push %rax       # Result (negative for -errno)
    mov %r10,%rsi
    mov %r9,%rdi
    NEXT
*/

defcode "SYSCALL0",8,,SYSCALL0
    pop %rax        # System call number (see <asm/unistd.h>)
    syscall
    push %rax       # Result (negative for -errno)
    NEXT

## Buf

    .set RETURN_STACK_SIZE,8192
    .align 4096
return_stack:
    .space RETURN_STACK_SIZE
return_stack_top:           # Initial top of return stack.

    .set INPUT_BUFFER_SIZE,4096
    .align 4096
input_buffer:
    .space INPUT_BUFFER_SIZE

    # Data buffer <- HERE
    .set INITIAL_DATA_SEGMENT_SIZE,65536
    .align 4096
data_buffer:
    .space INITIAL_DATA_SEGMENT_SIZE
