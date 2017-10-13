
    // definitions
    .set MAX_X, 24
    .set MAX_Y, 14

    // macro for load sprite
    .macro load_sprite pathname, variable
   	leaq	\pathname(%rip), %rdi
	call	load_sprite
	movq	%rax, \variable(%rip)
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


    .section .text
apple:
	.string	"assets/apple.bmp"
shead:
	.string "assets/head.bmp"
snake:
	.string "assets/snake.bmp"
field:
	.string "assets/field.bmp"

	.globl asmo_init
	.type asmo_init, @function
asmo_init:
    pushq	%rbp
	movq	%rsp, %rbp
    // load sprites
    load_sprite apple, fruit_texture
    load_sprite shead, shead_texture
    load_sprite snake, snake_texture
    load_sprite field, field_texture
    // clear field
    movq    $MAX_Y+1, %rsi
loop_y:
    decq    %rsi
    movq    $MAX_X+1, %rdi
loop_x:
    decq    %rdi
    push_regs %rdi, %rsi
	movq	field_texture(%rip), %rdx
	call	show_sprite
    pop_regs %rsi, %rdi
    test    %rdi, %rdi
    jne     loop_x
    test    %rsi, %rsi
    jne     loop_y
    // leave
    movq    %rbp, %rsp
    popq    %rbp
    ret
