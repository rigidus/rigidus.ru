.set F_IMMED,0x80
.set F_HIDDEN,0x20
.set F_LENMASK,0x1f  # length mask

.macro NEXT
    lodsl
    jmp *(%eax)
.endm

.macro PUSHRSP reg
    lea     -4(%ebp), %ebp  # декремент %ebp на 4
    movl    \reg, (%ebp)    # push reg в стек возвратов
.endm

.macro POPRSP reg
    mov     (%ebp), \reg    # pop вершину стека возвратов в reg
    lea     4(%ebp), %ebp   # инкремент %ebp на 4
.endm

    .set link,0             # Инициализировать начальное значение
                            # переменной времени компиляции link
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
    # дальше будут идти указатели на слова
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

.set stdin,2
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
    .align 4
DOCOL:
    PUSHRSP %esi            # Сохранить %esi в стеке возвратов
    leal    4(%eax), %esi   # %esi теперь указывает на param-field
    NEXT                    # Делаем NEXT

defcode ">R",2,,TOR
    popl    %eax            # pop со стека данных в %eax
    PUSHRSP %eax            # push %eax на стек возвратов
    NEXT

defcode "R>",2,,FROMR
    POPRSP  %eax            # pop со стека возвратов в %eax
    pushl   %eax            # push %eax на стек параметров
    NEXT

defcode "RSP@",4,,RSPFETCH
    pushl    %ebp
    NEXT

defcode "RSP!",4,,RSPSTORE
    popl    %ebp
    NEXT

defcode "RDROP",5,,RDROP
    addl    $4, %ebp
    NEXT

defcode "DROP",4,,DROP
    popl    %eax            # сбросить верхний элемент стека
    NEXT

defcode "SWAP",4,,SWAP
    popl    %eax            # поменять местами два верхних элемента на стеке
    popl    %ebx
    pushl   %eax
    pushl   %ebx
    NEXT

defcode "DUP",3,,DUP
    mov     (%esp), %eax    # дублировать верхний элемент стека
    pushl   %eax
    NEXT

defcode "OVER",4,,OVER
    mov     4(%esp), %eax   # взять второй от верха элемент стека
    pushl   %eax            # и положить его копию сверху
    NEXT

defcode "ROT",3,,ROT
    popl    %eax
    popl    %ebx
    popl    %ecx
    pushl   %ebx
    pushl   %eax
    pushl   %ecx
    NEXT

defcode "-ROT",4,,NROT
    popl    %eax
    popl    %ebx
    popl    %ecx
    pushl   %eax
    pushl   %ecx
    pushl   %ebx
    NEXT

defcode "2DROP",5,,TWODROP
    popl    %eax            # сбросить два верхних элемента со стека
    popl    %eax
    NEXT

defcode "2DUP",4,,TWODUP
    movl    (%esp), %eax    # дублировать два верхних элемента на стеке
    movl    4(%esp), %ebx
    pushl   %ebx
    pushl   %eax
    NEXT

defcode "2SWAP",5,,TWOSWAP
    popl    %eax            # поменять местами две пары элементов на стеке
    popl    %ebx
    popl    %ecx
    popl    %edx
    pushl   %ebx
    pushl   %eax
    pushl   %edx
    pushl   %ecx
    NEXT

defcode "?DUP",4,,QDUP
    movl    (%esp), %eax    # дублировать верхний элемент стека если он не нулевой
    test    %eax, %eax
    jz      1f
    pushl   %eax
1:
    NEXT

defcode "1+",2,,INCR
    incl    (%esp)          # увеличить верхний элемент стека на единицу
    NEXT

defcode "1-",2,,DECR
    decl    (%esp)          # уменьшить верхний элемент стека на единицу
    NEXT

defcode "4+",2,,INCR4
    addl    $4, (%esp)      # увеличить верхний элемент стека на 4
    NEXT

defcode "4-",2,,DECR4
    subl    $4, (%esp)      # уменьшить верхний элемент стека на 4
    NEXT

defcode "+",1,,ADD
    popl    %eax            # взять верхний элемент со стека
    addl    %eax, (%esp)    # прибавиь его значение к элементу, который стал верхним
    NEXT

defcode "-",1,,SUB
    popl    %eax            # взять верхний элемент со стека
    subl    %eax, (%esp)    # вычесть его значение из элемента, который стал верхним верхним
    NEXT

defcode "*",1,,MUL
    popl    %eax            # взять со стека верхний элемент
    popl    %ebx            # взять со стека следующий верхний элемент
    imull   %ebx, %eax      # умножить их друг на друга
    pushl   %eax            # игнорируем переполнение
    NEXT

defcode "/MOD",4,,DIVMOD
    pop     %ebx
    pop     %eax
    cdq
    idivl   %ebx
    pushl   %edx            # push остаток
    pushl   %eax            # push частное
    NEXT

defcode "U/MOD",5,,UDIVMOD
    xor %edx, %edx
    pop %ebx
    pop %eax
    divl %ebx
    push %edx               # push остаток
    push %eax               # push частное
    NEXT

defcode "=",1,,EQU
    popl    %eax            # два верхних элемента стека равны?
    popl    %ebx
    cmpl    %ebx, %eax
    sete    %al
    movzbl  %al, %eax
    pushl   %eax
    NEXT

defcode "<>",2,,NEQU
    popl    %eax            # два верхних элемента стека не равны?
    popl    %ebx
    cmpl    %ebx, %eax
    setne   %al
    movzbl  %al, %eax
    pushl   %eax
    NEXT

defcode "<",1,,LT
    popl    %eax
    popl    %ebx
    cmpl    %eax, %ebx
    setl    %al
    movzbl  %al, %eax
    pushl   %eax
    NEXT

defcode ">",1,,GT
    popl    %eax
    popl    %ebx
    cmpl    %eax, %ebx
    setg    %al
    movzbl  %al, %eax
    pushl   %eax
    NEXT

defcode "<=",2,,LE
    popl    %eax
    popl    %ebx
    cmpl    %eax, %ebx
    setle   %al
    movzbl  %al, %eax
    pushl   %eax
    NEXT

defcode ">=",2,,GE
    popl    %eax
    popl    %ebx
    cmpl    %eax, %ebx
    setge   %al
    movzbl  %al, %eax
    pushl   %eax
    NEXT

defcode "0=",2,,ZEQU
    popl    %eax            # верхний элемент стека равен нулю?
    test    %eax, %eax
    setz    %al
    movzbl  %al, %eax
    pushl   %eax
    NEXT

defcode "0<>",3,,ZNEQU
    popl    %eax            # верхний элемент стека не равен нулю?
    testl   %eax, %eax
    setnz   %al
    movzbl  %al, %eax
    pushl   %eax
    NEXT

defcode "0<",2,,ZLT
    popl    %eax            # comparisons with 0
    test    %eax, %eax
    setl    %al
    movzbl  %al, %eax
    pushl   %eax
    NEXT

defcode "0>",2,,ZGT
    popl    %eax
    testl   %eax, %eax
    setg    %al
    movzbl  %al, %eax
    pushl   %eax
    NEXT

defcode "0<=",3,,ZLE
    popl    %eax
    testl   %eax, %eax
    setle   %al
    movzbl  %al, %eax
    pushl   %eax
    NEXT

defcode "0>=",3,,ZGE
    popl    %eax
    test    %eax, %eax
    setge   %al
    movzbl  %al, %eax
    pushl   %eax
    NEXT

defcode "AND",3,,AND
    popl    %eax            # битовый AND
    andl    %eax, (%esp)
    NEXT

defcode "OR",2,,OR
    popl    %eax            # битовый OR
    orl     %eax, (%esp)
    NEXT

defcode "XOR",3,,XOR
    popl    %eax            # битовый XOR
    xorl    %eax, (%esp)
    NEXT

defcode "INVERT",6,,INVERT
    notl    (%esp)          # это битовая функция "NOT" (см. NEGATE and NOT)
    NEXT

defcode "EXIT",4,,EXIT
    POPRSP  %esi            # Восстановить указатель из стека возвратов в %esi
    NEXT                    # Сделать NEXT

defcode "!",1,,STORE
    popl    %ebx            # забираем со стека адрес, куда будем сохранять
    popl    %eax            # забираем со стека данные, которые будем сохранять
    movl    %eax, (%ebx)    # сохраняем данные по адресу
    NEXT

defcode "@",1,,FETCH
    popl    %ebx            # забираем со стека адрес переменной, значение которой надо вернуть
    movl    (%ebx), %eax    # выясняем значение по этому адресу
    pushl   %eax            # push-им значение в стек
    NEXT

defcode "+!",2,,ADDSTORE
    popl    %ebx            # забираем со стека адрес переменной, которую будем увеличивать
    popl    %eax            # забираем значение на которое будем увеличивать
    addl    %eax, (%ebx)    # добавляем значение к переменной по этому адресу
    NEXT

defcode "-!",2,,SUBSTORE
    popl    %ebx            # забираем со стека адрес переменной, которую будем уменьшать
    popl    %eax            # забираем значение на которое будем уменьшать
    subl    %eax, (%ebx)    # вычитаем значение из переменной по этому адресу
    NEXT

defcode "C!",2,,STOREBYTE
    popl    %ebx            # забираем со стека адрес, куда будем сохранять
    popl    %eax            # забираем со стека данные, которые будем сохранять
    movb    %al, (%ebx)     # сохраняем данные по адресу
    NEXT

defcode "C@",2,,FETCHBYTE
    popl    %ebx            # забираем со стека адрес переменной, значение которой надо вернуть
    xorl    %eax, %eax      # очищаем регистр %eax
    movb    (%ebx), %al     # выясняем значение по этому адресу
    push    %eax            # push-им значение в стек
    NEXT

# C@C! - это полезный примитив для копирования байт
defcode "C@C!",4,,CCOPY
    movl    4(%esp), %ebx   # адрес источника
    movb    (%ebx), %al     # получаем байт из источника
    popl    %edi            # адрес приемника
    stosb                   # копируем байт в приемник
    push    %edi            # увеличиваем адрес приемника
    incl    4(%esp)         # увеличиваем адрес источника
    NEXT

# CMOVE - операция копирования блока байтов
defcode "CMOVE",5,,CMOVE
    movl    %esi, %edx      # сохраним %esi
    popl    %ecx            # length
    popl    %edi            # адрес приемника
    popl    %esi            # адрес источника
    rep     movsb           # копируем источник в приемник length раз
    movl    %edx, %esi      # восстанавливаем %esi
    NEXT

defcode "DSP@",4,,DSPFETCH
    mov     %esp, %eax
    push    %eax
    NEXT

defcode "DSP!",4,,DSPSTORE
    popl    %esp
    NEXT

    defcode "KEY",3,,KEY
    call _KEY
    push    %eax            #       # push-им возвращенный символ на стек
    NEXT                    #
_KEY:                       # <--+
    mov     (currkey), %ebx #    |  # Берем указатель currkey в %ebx
    cmp     (bufftop), %ebx #    |  # (bufftop >= currkey)? - в буфере есть символы?
    jge     1f              #-+  |  # ?-Нет, переходим вперед
    xor     %eax, %eax      # |  |  # ?-Да,  (1) переносим символ, на который
    mov     (%ebx), %al     # |  |  #        указывает bufftop в %eax,
    inc     %ebx            # |  |  #        (2) инкрементируем копию bufftop
    mov     %ebx, (currkey) # |  |  #        (3) записываем ее в переменную currkey,
    ret                     # |  |  #        и выходим (в %eax лежит символ)
    # ---------------- RET    |  |
1:  #                     <---+  |  # Буфер ввода пуст, сделаем read из stdin
    mov     $sys_read, %eax #    |  # param1: SYSCALL #3 (read)
    mov     $stdin, %ebx    #    |  # param2: Дескриптор #2 (stdin)
    mov     $input_buffer, %ecx #|  # param3: Кладем адрес буфера ввода в %ecx
    mov     %ecx, currkey   #    |  # Сохраняем адрес буфера ввода в currkey
    mov     $INPUT_BUFFER_SIZE, %edx # Максимальная длина ввода
    int     $0x80           #    |  # SYSCALL
    # Проверяем возвращенное     |  # должно быть количество символов + '\n'
    test    %eax, %eax      #    |  # (%eax <= 0)?
    jbe     2f              #-+  |  # ?-Да, это ошибка, переходим вперед
    addl    %eax, %ecx      # |  |  # ?-Нет, (1) добавляем в %ecx кол-во прочитанных байт
    mov     %ecx, bufftop   # |  |  #        (2) записываем %ecx в bufftop
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

defcode "EMIT",4,,EMIT
    popl    %eax
    call    _EMIT
    NEXT
_EMIT:
    movl    $1, %ebx            # param1: stdout
    mov     %al, emit_scratch   # берем байт и заносим его в emit_scratch
    mov     $emit_scratch, %ecx # param2: адрес выводимого значения
    mov     $1, %edx            # param3: длина
    mov     $sys_write, %eax    # SYSCALL #4 (write)
    int     $0x80
    ret

    .data           # NB: проще записать в .data section
emit_scratch:
    .space 1        # Место для байта, который выводит EMIT

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
    mov     $word_buffer, %edi  # | # Указатель на возвращаемый буфер
2:                      # <---+   |
    stosb                   # |   | # Добавляем символ в возвращаемый буфер
    call    _KEY            # |   | # Вызываем KEY символ будет возвращен в %al
    cmpb    $' ', %al       # |   | # (Это пробел, возрат каретки, перевод строки)?
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
    push    %esi            # Сохраним %esi - так мы сможем его использовать
                            # для сравнения строк командой cmpsb
    # Здесь мы начинаем искать в словаре это слово от конца к началу словаря
    mov     (var_LATEST), %edx # %edx = LATEST указыввет на name header
                             # последнего слова в словаре
1:  #                   <------------+
    test    %edx, %edx      # (NULL указатель, т.е. словарь кончился)?
    je  4f                  #----+   |  # ?-Да, переходим вперед к (4)
    #                             |  |
    # Сравним ожидаемую длину и длину слова
    # Внимание, если F_HIDDEN установлен для этого слова, то она не совпадет.
    xor     %eax, %eax      #     |  |  #
    movb    4(%edx), %al    #     |  |  # %al = flags+length
    andb    $(F_HIDDEN|F_LENMASK), %al  # %al = длина имени
    cmpb    %cl, %al        #     |  |  # (Длины одинаковые?)
    jne 2f                  #--+  |  |  # ?-Нет, переходим вперед к (2)
    #                          |  |  |
    # Переходим к детальному сравнению
    push    %ecx            #  |  |  |  # Сохраним длину
    push    %edi            #  |  |  |  # Сохраним адрес, потому что repe cmpsb двигает %edi
    lea     5(%edx), %esi   #  |  |  |  # Загружаем в %esi адрес начала слова
    repe    cmpsb           #  |  |  |  # Сравниваем
    pop     %edi            #  |  |  |  # Восстанавливаем адрес
    pop     %ecx            #  |  |  |  # Восстановим длину
    jne 2f                  #--+  |  |  # ?-Если не равны - переходим вперед к (2)
    #                          |  |  |
    # Строки равны - возвратим указатель на заголовок в %eax
    pop     %esi            #  |  |  |  # Восстановим %esi
    mov     %edx, %eax      #  |  |  |  # %edx все еще содержит возвращаемый указатель
    ret                     #  |  |  |  # Возврат
    # ----------------- RET    |  |  |
2:  #                     <----+  |  |
    mov     (%edx), %edx    #     |  |  # Переходим по указателю к следующему слову
    jmp 1b                  #     |  |  # И зацикливаемся
    # ----------------------------|--+
4:  #                     <-------+
    # Слово не найдено
    pop     %esi
    xor     %eax, %eax      # Возвратим ноль в #eax
    ret                     # Возврат

    defcode ">CFA",4,,TCFA
    pop     %edi
    call    _TCFA
    push    %edi
    NEXT
_TCFA:
    xor     %eax, %eax
    add     $4, %edi        # Пропускаем LINK - указатель на предыдущее слово
    movb    (%edi), %al     # Загружаем flags+len в %al
    inc     %edi            # Пропускаем flags+len байт
    andb    $F_LENMASK, %al # Маскируем, чтобы получить длину имени, без флагов
    add     %eax, %edi      # Пропускаем имя
    addl    $3, %edi        # Учитываем выравнивание
    andl    $~3, %edi
    ret

defword ">DFA",4,,TDFA
    .int TCFA       # >CFA     (получаем code field address)
    .int INCR4      # 4+       (добавляем 4, чтобы получить адрес первого слова в опредении)
    .int EXIT       # EXIT     (возвращаемся)

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
    # Попытка распарсить пустую строку это ошибка но мы возвращаем 0
    test    %ecx, %ecx
    jz  5f                  #-> RET #
    # Строка не пуста, будем разбирать
    movl    var_BASE, %edx  #       # Получаем BASE в %dl
    # Проверим, может быть первый символ '-'?
    movb    (%edi), %bl     #       # %bl = первый символ строки
    inc     %edi            #       #
    push    %eax            #       # push 0 в стек
    cmpb    $'-', %bl       #       # (Отрицательное число)?
    jnz 2f                  #-+     # ?-Нет, переходим к конвертации (2)
    pop     %eax            # |     # ?-Да, заберем обратно 0 из стека,
    push    %ebx            # |     #       push не ноль в стек, как индикатор отрицательного
    dec     %ecx            # |     #       уменьшим счетчик оставшихся символов
    jnz 1f                  #-----+ #       (Строка закончилась)? ?-Нет: переход на (1)
    pop     %ebx            # |   | #       ?-Да - это ошибка, строка "-". Забираем из стека
    movl    $1, %ecx        # |   | #            помещаем в возвращаемую нераспарсенную длину
    ret                     # |   | #            единицу и выходим.
    # --------------------- # |   | # -------------------------------------------------------
    # Цикл чтения чисел     # |   | #
1:  #                    <========+ #
    imull   %edx, %eax      # |   | # %eax *= BASE
    movb    (%edi), %bl     # |   | # %bl = следующий символ в строке
    inc     %edi            # |   | # Увеличиваем указатель
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
    add     %ebx, %eax      #   | | #                      ?-Нет, все в порядке. Добавляем
    dec     %ecx            #   | | #                        RESULT к %eax и LOOP-им дальше.
    jnz 1b                  #---|-+ #
4:  #                     <-----+   #
    # Тут мы оказываемся если цикл закончился - тогда у нас %ecx=0
    # В ином случае %ecx содержит количество нераспарсенных символов
    # Если у нас отрицательный результат, то первый символ '-' (сохранен в стеке)
    pop     %ebx            #       #
    test    %ebx, %ebx      #       # (Отрицательное число)?
    jz  5f                  #-+     # ?-Нет, возвращаем как есть (5)
    neg     %eax            # |     # ?-Да, инвертируем
5:  #                     <---+
    ret

defcode "LIT",3,,LIT
    # %esi указывает на следующую команду, но в этом случае это указатель на следующий
    # литерал, представляющий собой 4 байтовое значение. Получение этого литерала в %eax
    # и инкремент %esi на x86 -  это удобная однобайтовая инструкция! (см. NEXT macro)
    lodsl
    # push literal в стек
    push %eax
    NEXT
defcode "LITSTRING",9,,LITSTRING
    lodsl                   # Получить длину строки
    push    %esi            # push адрес начала строки
    push    %eax            # push длину
    addl    %eax,%esi       # пропустить строку
    addl    $3,%esi         # но округлить до следующей 4 байтовой границы
    andl    $~3,%esi
    NEXT

defcode "TELL",4,,TELL
    pop     %edx                # param3: длина строки
    pop     %ecx                # param2: адрес строки
    mov     $1, %ebx            # param1: stdout
    mov     $sys_write, %eax    # SYSCALL #4 (write)
    int     $0x80
    NEXT

defcode "CREATE",6,,CREATE

    # Получаем length и address имени
    pop     %ecx            # %ecx = length
    pop     %ebx            # %ebx = address

    # Формируем указатель LINK
    movl    var_HERE, %edi  # %edi теперь адрес заголовка
    movl    var_LATEST, %eax    # Получаем значение указателя LINK
    stosl                   # и сохраняем его в заголовок

    # Байт длины и с само слово
    mov     %cl,%al         # Получаем длину
    stosb                   # Сохраняем length/flags байт.
    push    %esi            # Ненадолго сохраним %esi
    mov     %ebx, %esi      # %esi теперь указывает на имя слова
    rep     movsb           # Копируем имя слова
    pop     %esi            # Восстановим %esi
    addl    $3, %edi        # Вычислим выравнивание
    andl    $~3, %edi

    # Обновим LATEST и HERE.
    movl    var_HERE, %eax
    movl    %eax, var_LATEST
    movl    %edi, var_HERE
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

defcode "[",1,F_IMMED,LBRAC
    xor     %eax, %eax
    movl    %eax, var_STATE     # Установить STATE в 0
    NEXT

defcode "]",1,,RBRAC
    movl    $1, var_STATE       # Установить STATE в 1
    NEXT

defword ":",1,,COLON
    .int WORD               # Получаем имя нового слова
    .int CREATE             # CREATE заголовок записи словаря
    .int LIT, DOCOL, COMMA  # Добавляем DOCOL (как codeword).
    .int LATEST, FETCH, HIDDEN # Делаем слово скрытым (см. ниже определение HIDDEN).
    .int RBRAC              # Переходим в режим компиляции
    .int EXIT               # Возврат из функции

defword ";",1,F_IMMED,SEMICOLON
    .int LIT, EXIT, COMMA   # Добавляем EXIT (так слово делает RETURN).
    .int LATEST, FETCH, HIDDEN # Переключаем HIDDEN flag  (см. ниже для определения).
    .int LBRAC              # Возвращаемся в IMMEDIATE режим.
    .int EXIT               # Возврат из функции

defcode "IMMEDIATE",9,F_IMMED,IMMEDIATE
    movl    var_LATEST, %edi    # LATEST слово в %edi.
    addl    $4, %edi            # Теперь %edi указывает на байт name/flags
    xorb    $F_IMMED, (%edi)    # Переключить the F_IMMED бит.
    NEXT

defcode "HIDDEN",6,,HIDDEN
    pop     %edi                # Указатель на слово в %edi
    addl    $4, %edi            # Теперь указывает на байт length/flags.
    xorb    $F_HIDDEN, (%edi)   # Переключаем HIDDEN бит.
    NEXT

defword "HIDE",4,,HIDE
    .int    WORD                # Получаем слово (ищущее за HIDE).
    .int    FIND                # Ищем его в словаре
    .int    HIDDEN              # Устанавливаем F_HIDDEN флаг.
    .int    EXIT                # Выходим

defcode "'",1,,TICK
    lodsl                   # Получить адрес следующего слова и пропустить его
    pushl    %eax           # Push его в стек
    NEXT

defcode "INTERPRET",9,,INTERPRET
    call    _WORD           # Возвращает %ecx = длину, %edi = указатель на слово.
    # Есть ли слово в словаре?
    xor     %eax, %eax
    movl    %eax, (interpret_is_lit)    # Это не литерал (или пока не литерал)
    call    _FIND           #           # Возвращает в %eax указатель на заголовок или 0
    test    %eax, %eax      #           # (Совпадение)?
    jz  1f                  #--------+  # ?-Не думаю! Переход вперед к (1)
    # Это словарное слово   #        |  # ?-Да. Найдено совпадающее слово. Продолжаем.
    # Это IMMEDIATE-слово?  #        |  #
    mov     %eax, %edi      #        |  # %edi = указатель на слово
    movb    4(%edi), %al    #        |  # %al = flags+length.
    push    %eax            #        |  # Сохраним его (flags+length) ненадолго
    call    _TCFA           #        |  # Преобразуем entry (в %edi) в указатель на codeword
    pop     %eax            #        |  # Восстановим flags+length
    andb    $F_IMMED, %al   #        |  # (Установлен флаг F_IMMED)?
    mov     %edi, %eax      #        |  # %edi->%eax
    jnz     4f              #--------|-+# ?-Да, переходим сразу к выполнению (4)
    jmp 2f                  #--+     | |# ?-Нет, переходим к проверке режима работы (2)
    # --------------------- #  |     | |# -------------------------------------------------
1:  #                   <------|-----+ |
    # Нет в словаре, будем считать, что это литерал
    incl    (interpret_is_lit)#|       |# Установим флаг
    call    _NUMBER         #  |       |# Возвращает число в in %eax, %ecx > 0 если ошибка
    test    %ecx, %ecx      #  |       |# (Удалось распарсить число)?
    jnz 6f                  #--|-----+ |# ?-Нет, переходим к (6)
    mov     %eax, %ebx      #  |     | |# ?-Да, Перемещаем число в %ebx,
    mov     $LIT, %eax      #  |     | |#     Устанавливаем слово LIT в %eax <ЗАЧЕМ????>
2:  #                   <------+     | |#
    # Проверим в каком мы режиме     | |#
    movl    var_STATE, %edx #        | |#
    test    %edx, %edx      #        | |#     (Мы компилируемся или выполняемся)?
    jz  4f                  #-----+  | |#     ?-Выполняемся. Переходим к (4)
    call    _COMMA          #     |  | |#     ?-Компилируемся. Добавляем словарное определение
    mov     (interpret_is_lit), %ecx#| |#
    test    %ecx, %ecx      #     |  | |#       (Это был литерал)?
    jz      3f              #--+  |  | |#       ?-Нет, переходим к NEXT
    mov     %ebx, %eax      #  |  |  | |#       ?-Да, поэтому за LIT следует число,
    call    _COMMA          #  |  |  | |#            вызываем _COMMA, чтобы скомпилить его
3:  #                   <------+  |  | |#
    NEXT                    #     |  | |# NEXT
    # ---------------------       |  | |# -------------------------------------------------
4:  #                   <---------+<-|-+
    # Выполняемся                    |
    mov     (interpret_is_lit), %ecx#|
    test    %ecx, %ecx      #        |  # (Это литерал)?
    jnz 5f                  #--+     |  # ?-Да, переходим к (5)
    # Не литерал, выполним прямо сейчас. Мы не осуществляем возврата, но
    # codeword в конечном итоге вызовет NEXT, который повторно вернет цикл в QUIT
    jmp     *(%eax)         #  |     |
    # --------------------- #  |     |  # -------------------------------------------------
5:  #                    <-----+     |
    # Выполняем литерал, что означает, что мы push-им его в стек и делаем NEXT
    push    %ebx            #        |
    NEXT                    #        |
6:  #                    <-----------+
    # Мы здесь, если не получилось распарсить число в текущей базе или этого
    # слова нет в словаре. Печатаем сообщение об ошибке и 40 символов контекста.
    mov     $sys_write, %eax#           # SYSCALL #4 (write)
    mov     $stderr, %ebx   #           # param1: stderr
    mov     $errmsg, %ecx   #           # param2: Выводимая строка
    mov     $errmsgend-errmsg, %edx     # param3: Длина выводимой строки
    int     $0x80           #           # SYSCALL
    # Ошибка произошла перед currkey
    mov     (currkey), %ecx #
    mov     %ecx, %edx      #
    sub     $input_buffer, %edx         # %edx = (currkey - buffer) (длина буфера перед currkey)
    cmp     $40, %edx       #           # (if > 40)?
    jle 7f                  #--+        # ?-Нет, печатаем все
    mov     $40, %edx       #  |        # ?-Да, печатать только 40 символов
7:  #                    <-----+
    sub     %edx, %ecx      #           # %ecx = start of area to print, %edx = length
    mov     $sys_write, %eax            # SYSCALL #4 (write)
    int     $0x80           #           # SYSCALL
    # Выведем перевод строки
    mov     $sys_write, %eax            # SYSCALL #4 (write)
    mov     $errmsgnl, %ecx #           # newline
    mov     $1, %edx        #           # Длина
    int     $0x80           #           # SYSCALL
    NEXT                    #           # NEXT
    # ---------------------
    .section .rodata
errmsg:
    .ascii "PARSE ERROR: "
errmsgend:
errmsgnl:
    .ascii "\n"

    .data                   # NB: проще записать в .data section
    .align 4
interpret_is_lit:
    .int 0                  # Флаг литерала

defcode "BRANCH",6,,BRANCH
    add     (%esi),%esi     # добавить offset к instruction pointer
    NEXT

defcode "0BRANCH",7,,ZBRANCH
    pop     %eax
    test    %eax, %eax      # Вершина стека равна нулю?
    jz      code_BRANCH     # Если да, вернуться назад к функции BRANCH выше
    lodsl                   # иначе пропустить смещение
    NEXT

# QUIT не должна возвращаться (те есть вызывать EXIT).
defword "QUIT",4,,QUIT
    # Положить константу RZ (начальное значение стека возвратов) на стек параметров.
    .int RZ
    # Установить значение, лежащее на стеке параметров, как новое значение вершины стека возвратов
    .int RSPSTORE       # Это очищает стек возвратов
    # Запустить интерпретатор команд                  <-----+
    .int INTERPRET      # Интерпретировать следующее слово  |
    # И навсегда зациклиться                                |
    .int BRANCH,-8      # -----------------------------------

defcode "CHAR",4,,CHAR
    call    _WORD           # Возвращает %ecx = length, %edi = указатель на слово.
    xor     %eax, %eax
    movb    (%edi), %al     # Получаем первый символ слова
    push    %eax            # Кладем его в стек
    NEXT

defcode "EXECUTE",7,,EXECUTE
    pop     %eax            # Получить токен выполнения в %eax
    jmp     *(%eax)         # и выполнить jump на него.

DODOES:
    PUSHRSP %esi            # (с) Сохраняем ESI на стеке возвратов

    pop     %esi            # (b,d) CALL-RETADDR -> ESI

    lea     4(%eax), %eax   # (a) вычислить param-field DEUX
    pushl   %eax            # (a) push его на стек данных

    NEXT                    # (e) вызвать интерпретатор

defconst "DODOES_ADDR",11,,DODOES_ADDR,DODOES

defcode "SYSCALL3",8,,SYSCALL3
    pop     %eax            # Номер системного вызова (см. <asm/unistd.h>)
    pop     %ebx            # Первый параметр.
    pop     %ecx            # Второй параметр
    pop     %edx            # Третий параметр
    int     $0x80
    push    %eax            # Результат
    NEXT

defcode "SYSCALL2",8,,SYSCALL2
    pop     %eax            # Номер системного вызова (см. <asm/unistd.h>)
    pop     %ebx            # Первый параметр.
    pop     %ecx            # Второй параметр
    int     $0x80
    push    %eax            # Результат
    NEXT

defcode "SYSCALL1",8,,SYSCALL1
    pop     %eax            # Номер системного вызова (см. <asm/unistd.h>)
    pop     %ebx            # Первый параметр.
    int     $0x80
    push    %eax            # Результат
    NEXT

defcode "SYSCALL0",8,,SYSCALL0
    pop     %eax            # Номер системного вызова (см. <asm/unistd.h>)
    int     $0x80
    push    %eax            # Результат
    NEXT

    /* Assembler entry point. */

    .text
    .globl  forth_asm_start
    .type   forth_asm_start, @function
forth_asm_start:
    # Сбрасываем флаг направления
    cld
    # Записываем вершину стека параметров %esp в переменную S0
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
