
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
    .set tail.x,        tail
    .set tail.y,        1+tail
    .set max.x,         24
    .set max.y,         14
    .set body.x,        body
    .set body.y,        1+body
    .set fruit.x,       fruit
    .set fruit.y,       1+fruit
    .set scan.up,       82
    .set scan.down,     81
    .set scan.left,     80
    .set scan.right,    79
    .set scan.escape,   41

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

    // macro for call show_sprite
    .macro call_show_sprite x y texture
    movq	\texture, %rdx
	movzbl	\y, %esi
	movzbl	\x, %edi
	call	show_sprite
    .endm



    // READ ONLY DATA SECTION
    .section	.rodata

apple_sprte_file:
    .string "assets/apple.bmp"
shead_sprte_file:
    .string "assets/head.bmp"
snake_sprte_file:
    .string "assets/snake.bmp"
field_sprte_file:
    .string "assets/field.bmp"

    // TEXT SECTION
    .section .text

    .globl  asmo_init
    .type   asmo_init, @function
asmo_init:
    ret


    // in  : -
    // use : rax rbx rcx rdx
    .globl  enqueue
    .type   enqueue, @function
enqueue:
    ret

    // in  : -
    // use : rax rbx rcx rdx
    .globl  dequeue
    .type   dequeue, @function
dequeue:
    ret

    // in  : al=x cl=y bl=setval
    // use : rax  rcx  rdx
    .globl  set_fld
    .type   set_fld, @function
set_fld:
    ret


    // in  : al=x cl=y
    // use : rax  rcx  rdx
    // ret : rax
    .globl  get_mat_addr
    .type   get_mat_addr, @function
get_mat_addr:
    ret


    // in  : -
    // use : rax
    .globl	next_fruit
	.type	next_fruit, @function
next_fruit:
	ret


    // in  : -
    // use :
    // side-effects : gameover_flag
    // ret : -
    .globl	check_borders
	.type	check_borders, @function
check_borders:
    ret

    // in  : -
    // use :
    // side-effects : gameover_flag
    // ret : -
    .globl	check_matrix
	.type	check_matrix, @function
check_matrix:
    ret


    .globl	update2
    ret


	.globl	render
	.type	render, @function
render:
	ret


    .globl	input
	.type	input, @function
input:
	ret


    .globl	show_sprite
	.type	show_sprite, @function
show_sprite:
	ret
