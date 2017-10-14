
    // definitions
    .set MAX_X, 24
    .set MAX_Y, 14
    .set LEFT,  1
    .set UP,    2
    .set DOWN,  3
    .set RIGHT, 4
    .set snake.first,   snake
    .set snake.last,    1+snake
    .set snake.len,     2+snake
    .set snake.elems,   3+snake
    .set head.x,        head
    .set head.y,        1+head
    .set max.x,         24
    .set max.y,         14

    // macro for load sprite
    .macro load_sprite pathname, variable
    leaq    \pathname(%rip), %rdi
    call    load_sprite
    movq    %rax, \variable(%rip)
    .endm

    // macro for save registers
    .macro push_regs regs:vararg
    .irp    reg, \regs
    pushq   \reg
    .endr
    .endm

    // macro for restore registers
    .macro pop_regs regs:vararg
    .irp    reg, \regs
    popq    \reg
    .endr
    .endm


    // TEXT SECTION
    .section .text
apple_sprte_file:
    .string "assets/apple.bmp"
shead_sprte_file:
    .string "assets/head.bmp"
snake_sprte_file:
    .string "assets/snake.bmp"
field_sprte_file:
    .string "assets/field.bmp"

    .globl  asmo_init
    .type   asmo_init, @function
asmo_init:
    pushq   %rbp
    movq    %rsp, %rbp

    // load sprites
    load_sprite apple_sprte_file, fruit_texture
    load_sprite shead_sprte_file, shead_texture
    load_sprite snake_sprte_file, snake_texture
    load_sprite field_sprte_file, field_texture

    // clear field
    movq    $MAX_Y+1, %rsi
loop_y:
    decq    %rsi
    movq    $MAX_X+1, %rdi
loop_x:
    decq    %rdi
    push_regs %rdi, %rsi
    movq    field_texture(%rip), %rdx
    call    show_sprite
    pop_regs %rsi, %rdi
    test    %rdi, %rdi
    jne     loop_x
    test    %rsi, %rsi
    jne     loop_y

    // set apple position
    movw    $0x0505, fruit(%rip)

    // set direction
    movb    $RIGHT, dir(%rip)

    // let him eat
    movw    fruit(%rip), %ax
    movw    %ax, head(%rip)

    // queue init
    movl    $0, snake.first(%rip)
    movl    $0, snake.last(%rip)
    movl    $0, snake.len(%rip)

    call    enqueue

    // leave
    movq    %rbp, %rsp
    popq    %rbp
    ret

    .globl  enqueue
    .type   enqueue, @function
enqueue:
    pushq   %rbp
    movq    %rsp, %rbp
    // snake.elems[snake.last] = head;
    xor     %rcx, %rcx                  # clear C
    movb    snake.last(%rip), %cl       # C = snake.last
    leaq    snake.elems(%rip), %rax     # А = база snake.elems
    movw    head(%rip), %dx             # D = [head] (читаем два байта)
    movw    %dx, (%rax, %rcx, 2)        # [A + C*2] = D (пишем два байта)
    // snake.last = (snake.last + 1) % 255;
    incb    snake.last(%rip)
    // snake.len++;
    incb    snake.len(%rip)
    // mat[head.x][head.y] = 1
    movzbq  head.x(%rip), %rax          # RAX = head.x
    movw    $max.y+1, %dx               # DX  = max.y + 1
    mul     %dx                         # RAX = (max.y + 1) * head.x
    movzbq  head.y(%rip), %rcx          # RCX = head.y
    add     %rcx, %rax                  # RAX = (max.y + 1) * head.x) + head.y
    leaq    mat(%rip), %rdx             # RDX = mat
    movb    $1, (%rax, %rdx)
    // leave
    popq    %rbp
    ret
