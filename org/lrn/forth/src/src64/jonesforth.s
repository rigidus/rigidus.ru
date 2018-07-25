.set F_IMMED,0x80
.set F_HIDDEN,0x20
.set F_LENMASK,0x1f  # length mask

.macro NEXT
    lodsq
    jmp *(%rax)
.endm

.macro PUSHRSP reg
    lea     -8(%rbp),%rbp   # декремент %rbp на 8
    mov     \reg,(%rbp)     # push reg в стек возвратов
.endm

.macro POPRSP reg
    mov (%rbp),\reg         # pop вершину стека возвратов в reg
    lea 8(%rbp),%rbp        # инкремент %rbp на 8
.endm

    .set link,0             # Инициализировать начальное значение
                            # переменной времени компиляции link
.macro defword name, namelen, flags=0, label
    .section .rodata
    .align 8
    .globl name_\label
name_\label :
    .quad link              # link
    .set link,name_\label
    .byte \flags+\namelen   # flags + байт длины
    .ascii "\name"          # имя
    .align 8                # выравнивание на 8-байтовую границу
    .globl \label
\label :
    .quad DOCOL             # codeword - указатель на функцию-интепретатор
    # дальше будут идти указатели на слова
.endm

.macro defcode name, namelen, flags=0, label
    .section .rodata
    .align 8
    .globl name_\label
name_\label :
    .quad   link               # link
    .set    link,name_\label
    .byte   \flags+\namelen    # flags + байт длины
    .ascii  "\name"            # имя
    .align  8                  # выравнивание на 4-х байтовую границу
    .globl  \label
\label :
    .quad   code_\label        # codeword
    .text
    //.align 8
    .globl  code_\label
code_\label :
    # далее следует ассемблерный код
.endm

.macro defvar name, namelen, flags=0, label, initial=0
    defcode \name,\namelen,\flags,\label
    push    $var_\name
    NEXT
    .data
    .align 8
    var_\name :
    .quad \initial
.endm

.macro defconst name, namelen, flags=0, label, value
    defcode \name,\namelen,\flags,\label
    push $\value
    NEXT
.endm

defvar "STATE",5,,STATE
defvar "HERE",4,,HERE
defvar "LATEST",6,,LATEST,name_SYSCALL0  # SYSCALL0 должен быть последним встроенным словом
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

.set stdin,0
.set stdout,1
.set stderr,2

defconst "SYS_EXIT",8,,SYS_EXIT,sys_exit
defconst "SYS_OPEN",8,,SYS_OPEN,sys_open
defconst "SYS_CLOSE",9,,SYS_CLOSE,sys_close
defconst "SYS_READ",8,,SYS_READ,sys_read
defconst "SYS_WRITE",9,,SYS_WRITE,sys_write
defconst "SYS_CREAT",9,,SYS_CREAT,sys_creat

defconst "O_RDONLY",8,,__O_RDONLY,0
defconst "O_WRONLY",8,,__O_WRONLY,1
defconst "O_RDWR",6,,__O_RDWR,2
defconst "O_CREAT",7,,__O_CREAT,0100
defconst "O_EXCL",6,,__O_EXCL,0200
defconst "O_TRUNC",7,,__O_TRUNC,01000
defconst "O_APPEND",8,,__O_APPEND,02000
defconst "O_NONBLOCK",10,,__O_NONBLOCK,04000

    .text
    .align 8
DOCOL:
    PUSHRSP %rsi            # Сохранить %rsi в стеке возвратов
    leaq    8(%rax), %rsi   # %rsi теперь указывает на param-field
    /*
    # Или другими словами:
    # add   $8, %rax
    # mov   %rax, %rsi
    */
    NEXT                    # Делаем NEXT

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

defcode "RSP!",4,,RSPSTORE
    pop     %rbp
    NEXT

defcode "RDROP",5,,RDROP
    add     $8, %rbp
    NEXT

defcode "DROP",4,,DROP
    pop     %rax            # сбросить верхний элемент стека
    NEXT

defcode "SWAP",4,,SWAP
    pop     %rax            # поменять местами два верхних элемента на стеке
    pop     %rbx
    push    %rax
    push    %rbx
    NEXT

defcode "DUP",3,,DUP
    mov     (%rsp), %rax    # дублировать верхний элемент стека
    push    %rax
    NEXT

defcode "OVER",4,,OVER
    mov     8(%rsp), %rax   # взять второй от верха элемент стека
    push    %rax            # и положить его копию сверху
    NEXT

defcode "ROT",3,,ROT
    pop     %rax
    pop     %rbx
    pop     %rcx
    push    %rbx
    push    %rax
    push    %rcx
    NEXT

defcode "-ROT",4,,NROT
    pop     %rax
    pop     %rbx
    pop     %rcx
    push    %rax
    push    %rcx
    push    %rbx
    NEXT

defcode "2DROP",5,,TWODROP
    pop     %rax            # сбросить два верхних элемента со стека
    pop     %rax
    NEXT

defcode "2DUP",4,,TWODUP
    mov     (%rsp), %rax    # дублировать два верхних элемента на стеке
    mov     8(%rsp), %rbx
    push    %rbx
    push    %rax
    NEXT

defcode "2SWAP",5,,TWOSWAP
    pop     %rax            # поменять местами две пары элементов на стеке
    pop     %rbx
    pop     %rcx
    pop     %rdx
    push    %rbx
    push    %rax
    push    %rdx
    push    %rcx
    NEXT

defcode "?DUP",4,,QDUP
    mov     (%rsp), %rax    # дублировать верхний элемент стека если он не нулевой
    test    %rax, %rax
    jz      1f
    push    %rax
1:
    NEXT

defcode "1+",2,,INCR
    incq    (%rsp)          # увеличить верхний элемент стека на единицу
    NEXT

defcode "1-",2,,DECR
    decq    (%rsp)          # уменьшить верхний элемент стека на единицу
    NEXT

defcode "4+",2,,INCR4
    addq    $4, (%rsp)      # увеличить верхний элемент стека на 4
    NEXT

defcode "4-",2,,DECR4
    subq    $4, (%rsp)      # уменьшить верхний элемент стека на 4
    NEXT

defcode "8+",2,,INCR8
    addq    $8, (%rsp)      # увеличить верхний элемент стека на 8
    NEXT

defcode "8-",2,,DECR8
    subq    $8, (%rsp)      # уменьшить верхний элемент стека на 8
    NEXT

defcode "+",1,,ADD
    pop     %rax            # взять верхний элемент со стека
    add     %rax, (%rsp)    # прибавиь его значение к элементу, который стал верхним
    NEXT

defcode "-",1,,SUB
    pop     %rax            # взять верхний элемент со стека
    sub     %rax, (%rsp)    # вычесть его значение из элемента, который стал верхним верхним
    NEXT

defcode "*",1,,MUL
    pop     %rax            # взять со стека верхний элемент
    pop     %rbx            # взять со стека следующий верхний элемент
    imul    %rbx, %rax      # умножить их друг на друга
    push    %rax            # игнорируем переполнение
    NEXT

defcode "/MOD",4,,DIVMOD
    xor %rdx, %rdx
    pop     %rbx
    pop     %rax
    idiv    %rbx
    push    %rdx            # push остаток
    push    %rax            # push частное
    NEXT

defcode "U/MOD",5,,UDIVMOD
    xor %rdx, %rdx
    pop %rbx
    pop %rax
    div  %rbx
    push %rdx               # push остаток
    push %rax               # push частное
    NEXT

defcode "=",1,,EQU
    pop     %rax            # два верхних элемента стека равны?
    pop     %rbx
    cmp     %rbx, %rax
    sete    %al
    movzb   %al, %rax
    push    %rax
    NEXT

defcode "<>",2,,NEQU
    pop     %rax            # два верхних элемента стека не равны?
    pop     %rbx
    cmp     %rbx, %rax
    setne   %al
    movzb   %al, %rax
    push    %rax
    NEXT

defcode "<",1,,LT
    pop     %rax
    pop     %rbx
    cmp     %rax, %rbx
    setl    %al
    movzb   %al, %rax
    push    %rax
    NEXT

defcode ">",1,,GT
    pop     %rax
    pop     %rbx
    cmp     %rax, %rbx
    setg    %al
    movzb   %al, %rax
    push    %rax
    NEXT

defcode "<=",2,,LE
    pop     %rax
    pop     %rbx
    cmp     %rax, %rbx
    setle   %al
    movzb   %al, %rax
    push    %rax
    NEXT

defcode ">=",2,,GE
    pop     %rax
    pop     %rbx
    cmp     %rax, %rbx
    setge   %al
    movzb   %al, %rax
    push    %rax
    NEXT

defcode "0=",2,,ZEQU
    pop     %rax            # верхний элемент стека равен нулю?
    test    %rax, %rax
    setz    %al
    movzb   %al, %rax
    push    %rax
    NEXT

defcode "0<>",3,,ZNEQU
    pop     %rax            # верхний элемент стека не равен нулю?
    test    %rax, %rax
    setnz   %al
    movzb   %al, %rax
    push    %rax
    NEXT

defcode "0<",2,,ZLT
    pop     %rax            # comparisons with 0
    test    %rax, %rax
    setl    %al
    movzb   %al, %rax
    push    %rax
    NEXT

defcode "0>",2,,ZGT
    pop     %rax
    test    %rax, %rax
    setg    %al
    movzb   %al, %rax
    push    %rax
    NEXT

defcode "0<=",3,,ZLE
    pop     %rax
    test    %rax, %rax
    setle   %al
    movzb   %al, %rax
    push    %rax
    NEXT

defcode "0>=",3,,ZGE
    pop     %rax
    test    %rax, %rax
    setge   %al
    movzb   %al, %rax
    push    %rax
    NEXT

defcode "AND",3,,AND
    pop     %rax            # битовый AND
    and     %rax, (%rsp)
    NEXT

defcode "OR",2,,OR
    pop     %rax            # битовый OR
    or      %rax, (%rsp)
    NEXT

defcode "XOR",3,,XOR
    pop     %rax            # битовый XOR
    xor     %rax, (%rsp)
    NEXT

defcode "INVERT",6,,INVERT
    notq    (%rsp)          # это битовая функция "NOT" (см. NEGATE and NOT)
    NEXT

defcode "EXIT",4,,EXIT
    POPRSP  %rsi            # Восстановить указатель из стека возвратов в %rsi
    NEXT                    # Сделать NEXT

defcode "!",1,,STORE
    pop     %rbx            # забираем со стека адрес, куда будем сохранять
    pop     %rax            # забираем со стека данные, которые будем сохранять
    mov     %rax, (%rbx)    # сохраняем данные по адресу
    NEXT

defcode "@",1,,FETCH
    pop     %rbx            # забираем со стека адрес переменной, значение которой надо вернуть
    mov     (%rbx), %rax    # выясняем значение по этому адресу
    push    %rax            # push-им значение в стек
    NEXT

defcode "+!",2,,ADDSTORE
    pop     %rbx            # забираем со стека адрес переменной, которую будем увеличивать
    pop     %rax            # забираем значение на которое будем увеличивать
    add     %rax, (%rbx)    # добавляем значение к переменной по этому адресу
    NEXT

defcode "-!",2,,SUBSTORE
    pop     %rbx            # забираем со стека адрес переменной, которую будем уменьшать
    pop     %rax            # забираем значение на которое будем уменьшать
    sub     %rax, (%rbx)    # вычитаем значение из переменной по этому адресу
    NEXT

defcode "C!",2,,STOREBYTE
    pop     %rbx            # забираем со стека адрес, куда будем сохранять
    pop     %rax            # забираем со стека данные, которые будем сохранять
    movb    %al, (%rbx)     # сохраняем данные по адресу
    NEXT

defcode "C@",2,,FETCHBYTE
    pop     %rbx            # забираем со стека адрес переменной, значение которой надо вернуть
    xor     %rax, %rax      # очищаем регистр %rax
    movb    (%rbx), %al     # выясняем значение по этому адресу
    push    %rax            # push-им значение в стек
    NEXT

# C@C! - это полезный примитив для копирования байт
defcode "C@C!",4,,CCOPY
    mov     8(%rsp), %rbx   # адрес источника
    movb    (%rbx), %al     # получаем байт из источника
    pop     %rdi            # адрес приемника
    stosb                   # копируем байт в приемник
    push    %rdi            # увеличиваем адрес приемника
    incq    8(%rsp)         # увеличиваем адрес источника
    NEXT

# CMOVE - операция копирования блока байтов
defcode "CMOVE",5,,CMOVE
    mov     %rsi, %rdx      # сохраним %rsi
    pop     %rcx            # length
    pop     %rdi            # адрес приемника
    pop     %rsi            # адрес источника
    rep     movsb           # копируем источник в приемник length раз
    mov     %rdx, %rsi      # восстанавливаем %rsi
    NEXT

defcode "DSP@",4,,DSPFETCH
    mov     %rsp, %rax
    push    %rax
    NEXT

defcode "DSP!",4,,DSPSTORE
    pop     %rsp
    NEXT

    defcode "KEY",3,,KEY
    call _KEY
    push    %rax            #       # push-им возвращенный символ на стек
    NEXT                    #
_KEY:                       # <--+
    mov     (currkey), %rbx #    |  # Берем указатель currkey в %rbx
    cmp     (bufftop), %rbx #    |  # (bufftop >= currkey)? - в буфере есть символы?
    jge     1f              #-+  |  # ?-Нет, переходим вперед
    xor     %rax, %rax      # |  |  # ?-Да,  (1) переносим символ, на который
    mov     (%rbx), %al     # |  |  #        указывает bufftop в %rax,
    inc     %rbx            # |  |  #        (2) инкрементируем копию bufftop
    mov     %rbx, (currkey) # |  |  #        (3) записываем ее в переменную currkey,
    ret                     # |  |  #        и выходим (в %rax лежит символ)
    # ---------------- RET    |  |
1:  #                     <---+  |  # Буфер ввода пуст, сделаем read из stdin
    push    %rsi            #    |  #
    push    %rdi            #    |  #
    mov     $sys_read, %rax #    |  #  param0: SYSCALL #3 (read) в %rax
    mov     $stdin, %rdi    #    |  #  param1: Дескриптор #0 (stdin) в %rdi
    mov     $input_buffer, %rsi #|  #  param2: Кладем адрес буфера ввода в %rsi ?
    mov     %rsi, currkey   #    |  #  Сохраняем его (адрес буфера) ввода в currkey
    mov     $INPUT_BUFFER_SIZE, %rdx # param3: Максимальная длина ввода в %rdx
    syscall                 #    |  #  SYSCALL
    pop     %rdi            #    |
    pop     %rsi            #    |
    # Проверяем возвращенное     |  # должно быть количество символов + '\n'
    test    %rax, %rax      #    |  # (%rax <= 0)?
    jbe     2f              #-+  |  # ?-Да, это ошибка, переходим вперед
    add     %rax, %rcx      # |  |  # ?-Нет, (1) добавляем в %rcx кол-во прочитанных байт
    mov     %rcx, bufftop   # |  |  #        (2) записываем %rcx в bufftop
    jmp     _KEY            # |  |
    # ------------------------|--+
2:  #                     <---+     # Ошибка или конец потока ввода - выходим
    mov     $sys_exit, %rax         # param1: SYSCALL #1 (exit)
    xor     %rdi, %rdi              # param2: код возврата
    syscall                         # SYSCALL
    # --------------- EXIT
    .data
    .align 8
currkey:
    # Хранит смещение на текущее положение в буфере ввода (следующий символ будет прочитан по нему)
    .quad input_buffer
bufftop:
    # Хранит вершину буфера ввода (последние валидные данные + 1)
    .quad input_buffer

defcode "EMIT",4,,EMIT
    pop     %rax
    call    _EMIT
    NEXT
_EMIT:
    push    %rsi            #    |  #
    push    %rdi            #    |  #
    mov     $sys_write, %rax        # SYSCALL #4 (write)
    mov     $stdout, %rdi           # param1: stdout в $rdi
    mov     %al, emit_scratch       # берем байт и заносим его в emit_scratch
    mov     $emit_scratch, %rsi     # param2: адрес выводимого значения в %rsi
    mov     $1, %rdx                # param3: длина
    syscall
    pop     %rdi            #    |
    pop     %rsi            #    |
    ret

    .data           # NB: проще записать в .data section
emit_scratch:
    .space 1        # Место для байта, который выводит EMIT

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
    mov     $word_buffer, %rdi  # | # Указатель на возвращаемый буфер
2:                      # <---+   |
    stosb                   # |   | # Добавляем символ в возвращаемый буфер
    call    _KEY            # |   | # Вызываем KEY символ будет возвращен в %al
    cmpb    $' ', %al       # |   | # (Это пробел, возрат каретки, перевод строки)?
    ja      2b              #-+   | # Если нет, повторим
    #                       #     |
    # Вернем слово (указатель на статический буфер черех %rcx) и его длину (через %rdi)
    sub     $word_buffer, %rdi  # |
    mov     %rdi, %rcx      #     | # return: длина слова
    mov     $word_buffer, %rdi  # | # return: адрес буфера
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
    pop     %rcx            # %rcx = длина строки
    pop     %rdi            # %rdi = адрес строки
    call    _FIND
    push    %rax            # %rax = адрес слова (или ноль)
    NEXT
_FIND:
    push    %rsi            # Сохраним %rsi - так мы сможем использовать этот
                            # регистр для сравнения строк командой CMPSB
    # Здесь мы начинаем искать в словаре это слово от конца к началу словаря
    mov     (var_LATEST), %rdx          # %rdx теперь указывает на последнее слово в словаре
1:  #                   <------------+
    test    %rdx, %rdx      # (в %rdx находится NULL-указатель, т.е. словарь кончился)?
    je  4f                  #-----+  |  # ?-Да, переходим вперед к (4)
    #                             |  |
    # Сравним ожидаемую длину и длину слова
    # Внимание, если F_HIDDEN установлен для этого слова, то совпадения не будет.
    xor     %rax, %rax      #     |  |  # Очищаем %rax
    movb    8(%rdx), %al    #     |  |  # %al = flags+length
    andb    $(F_HIDDEN|F_LENMASK), %al  # %al = теперь длина имени (маскируем флаги)
    cmpb    %cl, %al        #     |  |  # (Длины одинаковые?)
    jne 2f                  #--+  |  |  # ?-Нет, переходим вперед к (2)
    #                          |  |  |
    # Переходим к детальному сравнению
    push    %rcx            #  |  |  |  # Сохраним длину, потому что repe cmpsb уменьшает %rcx
    push    %rdi            #  |  |  |  # Сохраним адрес, потому что repe cmpsb двигает %rdi
    lea     9(%rdx), %rsi   #  |  |  |  # Загружаем в %rsi адрес начала имени слова
    repe    cmpsb           #  |  |  |  # Сравниваем
    pop     %rdi            #  |  |  |  # Восстанавливаем адрес
    pop     %rcx            #  |  |  |  # Восстановим длину
    jne 2f                  #--+  |  |  # ?-Если не равны - переходим вперед к (2)
    #                          |  |  |
    # Строки равны - возвратим указатель на заголовок в %rax
    pop     %rsi            #  |  |  |  # Восстановим %rsi
    mov     %rdx, %rax      #  |  |  |  # %rdx все еще содержит указатель, который возвращаем
    ret                     #  |  |  |  # Возврат
    # ----------------- RET    |  |  |
2:  #                     <----+  |  |
    mov     (%rdx), %rdx    #     |  |  # Переходим по указателю к следующему слову
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
    add     $8, %rdi        # Пропускаем LINK - указатель на предыдущее слово
    movb    (%rdi), %al     # Загружаем flags+len в %al
    inc     %rdi            # Пропускаем flags+len байт
    andb    $F_LENMASK, %al # Маскируем, чтобы получить длину имени, без флагов
    add     %rax, %rdi      # Пропускаем имя
    add     $7, %rdi        # Учитываем выравнивание
    and     $~7, %rdi
    ret

defword ">DFA",4,,TDFA
    .quad TCFA       # >CFA     (получаем code field address)
    .quad INCR8      # 8+       (добавляем 8, чтобы получить адрес первого слова в опредении)
    .quad EXIT       # EXIT     (возвращаемся)

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
    mov     (var_BASE), %rdx#       # Получаем BASE в %dl
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

defcode "LIT",3,,LIT
    # %rsi указывает на следующую команду, но в этом случае это указатель на следующий
    # литерал, представляющий собой 8-байтовое значение. Получение этого литерала в %rax
    # и инкремент %rsi на x86 -  это удобная однобайтовая инструкция! (см. NEXT macro)
    lodsq
    # push literal в стек
    push %rax
    NEXT
defcode "LITSTRING",9,,LITSTRING
    lodsq                   # Получить длину строки
    push    %rsi            # push адрес начала строки
    push    %rax            # push длину
    add     %rax,%rsi       # пропустить строку
    add     $7,%esi         # но округлить до следующей 4 байтовой границы
    and     $~7,%esi
    NEXT

defcode "TELL",4,,TELL
    pop     %rdx                # param3: длина строки
    pop     %rcx                # param2: адрес строки временно помещаем в %rcx
    push    %rsi                # save %rsi
    push    %rdi                # save %rdi
    mov     $stdout, %rdi       # param1: stdout
    mov     %rcx, %rsi          # param2: адрес строки перемещаем в %rsi
    mov     $sys_write, %rax    # SYSCALL #4 (write)
    syscall
    pop     %rdi                # restore %rdi
    pop     %rsi                # restore %rsi
    NEXT

defcode "CREATE",6,,CREATE

    # Получаем length и address имени из стека данных
    pop     %rcx            # %rcx = length
    pop     %rbx            # %rbx = address

    # Формируем указатель LINK
    mov     (var_HERE), %rdi# %rdi теперь указывает на заголовок
    mov     (var_LATEST), %rax # Получаем указатель на последнее слово -
                            # - это LINK создаваемого слова
    stosq                   # и сохраняем его в формируемое слово

    # Формируем Байт длины и имя слова
    mov     %cl,%al         # Получаем длину
    stosb                   # Сохраняем length/flags байт.
    push    %rsi            # Ненадолго сохраним %rsi
    mov     %rbx, %rsi      # в %rsi теперь адрес начала имени
    rep     movsb           # Копируем имя слова
    pop     %rsi            # Восстановим %rsi
    add     $7, %rdi        # Вычислим выравнивание
    and     $~7, %rdi

    # Обновим LATEST и HERE.
    mov     (var_HERE), %rax
    mov     %rax, (var_LATEST)
    mov     %rdi, (var_HERE)
    NEXT

defcode ",",1,,COMMA
    pop     %rax      # Взять со стека данных в %rax то значение, которое будем вкомпиливать
    call    _COMMA
    NEXT
_COMMA:
    mov     (var_HERE), %rdi  # получить указатель HERE в %rdi
    stosq                     # Сохраниь по нему значение из %rax
    mov     %rdi, (var_HERE)  # Обновить HERE (используя инкремент, сделанный STOSQ)
    ret

defcode "[",1,F_IMMED,LBRAC
    xor     %rax, %rax
    mov     %rax, (var_STATE)   # Установить STATE в 0
    NEXT

defcode "]",1,,RBRAC
    movq    $1, (var_STATE)     # Установить STATE в 1
    NEXT

defword ":",1,,COLON
    .quad WORD               # Получаем имя нового слова
    .quad CREATE             # CREATE заголовок записи словаря
    .quad LIT, DOCOL, COMMA  # Добавляем DOCOL (как codeword).
    .quad LATEST, FETCH, HIDDEN # Делаем слово скрытым (см. ниже определение HIDDEN).
    .quad RBRAC              # Переходим в режим компиляции
    .quad EXIT               # Возврат из функции

defword ";",1,F_IMMED,SEMICOLON
    .quad LIT, EXIT, COMMA   # Добавляем EXIT (так слово делает RETURN).
    .quad LATEST, FETCH, HIDDEN # Переключаем HIDDEN flag  (см. ниже для определения).
    .quad LBRAC              # Возвращаемся в IMMEDIATE режим.
    .quad EXIT               # Возврат из функции

defcode "IMMEDIATE",9,F_IMMED,IMMEDIATE
    mov     (var_LATEST), %rdi  # LATEST слово в %rdi.
    add     $8, %rdi            # Теперь %rdi указывает на байт name/flags
    xorb    $F_IMMED, (%rdi)    # Переключить the F_IMMED бит.
    NEXT

defcode "HIDDEN",6,,HIDDEN
    pop     %rdi                # Указатель на слово в %rdi
    add     $8, %rdi            # Теперь указывает на байт length/flags.
    xor     $F_HIDDEN, (%rdi)   # Переключаем HIDDEN бит.
    NEXT

defword "HIDE",4,,HIDE
    .quad    WORD                # Получаем слово (ищущее за HIDE).
    .quad    FIND                # Ищем его в словаре
    .quad    HIDDEN              # Устанавливаем F_HIDDEN флаг.
    .quad    EXIT                # Выходим

defcode "'",1,,TICK
    lodsq                   # Получить адрес следующего слова и пропустить его
    push     %rax           # Push его в стек
    NEXT

defcode "INTERPRET",9,,INTERPRET
  #  ret

    call    _WORD           # Возвращает %rcx = длину, %rdi = указатель на слово.
    # Есть ли слово в словаре?
    xor     %rax, %rax
    mov     %rax, (interpret_is_lit)    # Это не литерал (или пока не литерал)
    call    _FIND           #           # Возвращает в %eax указатель на заголовок или 0
    test    %rax, %rax      #           # (Совпадение)?
    jz  1f                  #--------+  # ?-Не думаю! Переход вперед к (1)
    # Это словарное слово   #        |  # ?-Да. Найдено совпадающее слово. Продолжаем.
    # Это IMMEDIATE-слово?  #        |  #
    mov     %rax, %rdi      #        |  # %edi = указатель на слово
    movb    8(%rdi), %al    #        |  # %al = flags+length.
    push    %rax            #        |  # Сохраним его (flags+length) ненадолго
    call    _TCFA           #        |  # Преобразуем entry (в %rdi) в указатель на codeword
    pop     %rax            #        |  # Восстановим flags+length
    andb    $F_IMMED, %al   #        |  # (Установлен флаг F_IMMED)?
    mov     %rdi, %rax      #        |  # %rdi->%rax
    jnz     4f              #--------|-+# ?-Да, переходим сразу к выполнению (4)
    jmp 2f                  #--+     | |# ?-Нет, переходим к проверке режима работы (2)
    # --------------------- #  |     | |# -------------------------------------------------
1:  #                   <------|-----+ |
    # Нет в словаре, будем считать, что это литерал
    incq    (interpret_is_lit)#|       |# Установим флаг
    call    _NUMBER         #  |       |# Возвращает число в %rax, %rcx > 0 если ошибка
    test    %rcx, %rcx      #  |       |# (Удалось распарсить число)?
    jnz 6f                  #--|-----+ |# ?-Нет, переходим к (6)
    mov     %rax, %rbx      #  |     | |# ?-Да, Перемещаем число в %ebx,
    mov     $LIT, %rax      #  |     | |#     Устанавливаем слово LIT в %eax <ЗАЧЕМ????>
2:  #                   <------+     | |#
    # Проверим в каком мы режиме     | |#
    mov     (var_STATE), %rdx#       | |#
    test    %rdx, %rdx      #        | |#     (Мы компилируемся или выполняемся)?
    jz  4f                  #-----+  | |#     ?-Выполняемся. Переходим к (4)
    call    _COMMA          #     |  | |#     ?-Компилируемся. Добавляем словарное определение
    mov     (interpret_is_lit), %rcx#| |#
    test    %rcx, %rcx      #     |  | |#       (Это был литерал)?
    jz      3f              #--+  |  | |#       ?-Нет, переходим к NEXT
    mov     %rbx, %rax      #  |  |  | |#       ?-Да, поэтому за LIT следует число,
    call    _COMMA          #  |  |  | |#            вызываем _COMMA, чтобы скомпилить его
3:  #                   <------+  |  | |#
    NEXT                    #     |  | |# NEXT
    # ---------------------       |  | |# -------------------------------------------------
4:  #                   <---------+<-|-+
    # Выполняемся                    |
    mov     (interpret_is_lit), %rcx#|
    test    %rcx, %rcx      #        |  # (Это литерал)?
    jnz 5f                  #--+     |  # ?-Да, переходим к (5)
    # Не литерал, выполним прямо сейчас. Мы не осуществляем возврата, но
    # codeword в конечном итоге вызовет NEXT, который повторно вернет цикл в QUIT
    jmp     *(%rax)         #  |     |
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
    mov     $sys_write, %rax#           # SYSCALL #4 (write)
    mov     $stderr, %rdi   #           # param1: stderr
    mov     $errmsg, %rsi   #           # param2: Выводимая строка
    mov     $errmsgend-errmsg, %rdx     # param3: Длина выводимой строки
    syscall                 #           # SYSCALL
    pop     %rdx
    pop     %rdi
    pop     %rsi
    # Ошибка произошла перед currkey
    mov     (currkey), %rcx #
    mov     %rcx, %rdx      #
    sub     $input_buffer, %rdx         # %rdx = (currkey - buffer) (длина буфера перед currkey)
    cmp     $40, %rdx       #           # (if > 40)?
    jle 7f                  #--+        # ?-Нет, печатаем все
    mov     $40, %rdx       #  |        # ?-Да, печатать только 40 символов
7:  #                    <-----+
    sub     %rdx, %rcx      #           # %ecx = start of area to print, %edx = length
    push    %rsi
    push    %rdi
    push    %rdx
    mov     $sys_write, %eax            # SYSCALL #4 (write)
    mov     $stderr, %rdi               # param1: stderr
    mov     %rcx, %rsi                  # param2: Выводимая строка
    mov     %rdx, %rdx                  # param3: Длина
    syscall                 #           # SYSCALL
    pop     %rdx
    pop     %rdi
    pop     %rsi
    # Выведем перевод строки
    push    %rsi
    push    %rdi
    mov     $sys_write, %eax            # SYSCALL #4 (write)
    mov     $stderr, %rdi               # param1: stderr
    mov     $errmsgnl, %rsi #           # param2: newline
    mov     $1, %edx        #           # param3: Длина
    syscall                 #           # SYSCALL
    pop     %rdi
    pop     %rsi
    NEXT                    #           # NEXT
    # ---------------------
    .section .rodata
errmsg:
    .ascii "PARSE ERROR: "
errmsgend:
errmsgnl:
    .ascii "\n"

    .data                   # NB: проще записать в .data section
    .align 8
interpret_is_lit:
    .quad 0                  # Флаг литерала

defcode "BRANCH",6,,BRANCH
    add     (%rsi),%rsi     # добавить offset к instruction pointer
    NEXT

defcode "0BRANCH",7,,ZBRANCH
    pop     %rax
    test    %rax, %rax      # Вершина стека равна нулю?
    jz      code_BRANCH     # Если да, вернуться назад к функции BRANCH выше
    lodsq                   # иначе пропустить смещение
    NEXT

# QUIT не должна возвращаться (те есть вызывать EXIT).
defword "QUIT",4,,QUIT
    # Положить константу RZ (начальное значение стека возвратов) на стек параметров.
    .quad RZ
    # Установить значение, лежащее на стеке параметров, как новое значение вершины стека возвратов
    .quad RSPSTORE       # Это очищает стек возвратов
    # Запустить интерпретатор команд                  <------+
    .quad INTERPRET      # Интерпретировать следующее слово  |
    # И навсегда зациклиться                                 |
    .quad BRANCH,-16     # -----------------------------------

defcode "CHAR",4,,CHAR
    call    _WORD           # Возвращает %ecx = length, %edi = указатель на слово.
    xor     %rax, %rax
    movb    (%rdi), %al     # Получаем первый символ слова
    push    %rax            # Кладем его в стек
    NEXT

defcode "EXECUTE",7,,EXECUTE
    pop     %rax            # Получить токен выполнения в %eax
    jmp     *(%rax)         # и выполнить jump на него.

DODOES:
    PUSHRSP %rsi            # (с) Сохраняем ESI на стеке возвратов

    pop     %rsi            # (b,d) CALL-RETADDR -> ESI

    lea     4(%rax), %rax   # (a) вычислить param-field DEUX
    push    %rax            # (a) push его на стек данных

    NEXT                    # (e) вызвать интерпретатор

defconst "DODOES_ADDR",11,,DODOES_ADDR,DODOES

/*
;; defcode "SYSCALL3",8,,SYSCALL3
;;     pop     %eax            # Номер системного вызова (см. <asm/unistd.h>)
;;     pop     %ebx            # Первый параметр.
;;     pop     %ecx            # Второй параметр
;;     pop     %edx            # Третий параметр
;;     int     $0x80
;;     push    %eax            # Результат
;;     NEXT
*/
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
/*
;; defcode "SYSCALL2",8,,SYSCALL2
;;     pop     %eax            # Номер системного вызова (см. <asm/unistd.h>)
;;     pop     %ebx            # Первый параметр.
;;     pop     %ecx            # Второй параметр
;;     int     $0x80
;;     push    %eax            # Результат
;;     NEXT
*/
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
/*
;; defcode "SYSCALL1",8,,SYSCALL1
;;     pop     %eax            # Номер системного вызова (см. <asm/unistd.h>)
;;     pop     %ebx            # Первый параметр.
;;     int     $0x80
;;     push    %eax            # Результат
;;     NEXT
*/
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
/*
;; defcode "SYSCALL0",8,,SYSCALL0
;;     pop     %eax            # Номер системного вызова (см. <asm/unistd.h>)
;;     int     $0x80
;;     push    %eax            # Результат
;;     NEXT
*/
  defcode "SYSCALL0",8,,SYSCALL0
  pop %rax        # System call number (see <asm/unistd.h>)
  syscall
  push %rax       # Result (negative for -errno)
  NEXT

    /* Assembler entry point. */

    .text
    .globl  forth_asm_start
    .type   forth_asm_start, @function
forth_asm_start:
    # Сбрасываем флаг направления
    cld
    # Записываем вершину стека параметров %rsp в переменную S0
    mov     %rsp, (var_S0)
    # Устанавливаем стек возвратов %rbp
    mov     $return_stack_top, %rbp
    # Устанавливаем указатель HERE на начало области данных.
    mov     $data_buffer, %rax
    mov     %rax, (var_HERE)
    # Инициализируем IP
    mov     $cold_start, %rsi
    # Запускаем интерпретатор
    NEXT

    .section .rodata
cold_start:                             # High-level code without a codeword.
    .quad QUIT

    .bss

    # Стек возвратов Forth
    .set RETURN_STACK_SIZE,8192
    .align 4096
return_stack:
    .space RETURN_STACK_SIZE
return_stack_top:           # Initial top of return stack.

    # Буфер ввода
    .set INPUT_BUFFER_SIZE,4096
    .align 4096
input_buffer:
    .space INPUT_BUFFER_SIZE

    # Буфер данных - cюда указывает HERE
    .set INITIAL_DATA_SEGMENT_SIZE,65536
    .align 4096
data_buffer:
    .space INITIAL_DATA_SEGMENT_SIZE
