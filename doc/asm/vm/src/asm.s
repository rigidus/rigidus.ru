
    // definitions
    .set MAX_X, 24
    .set MAX_Y, 14
    .set LEFT,  1
    .set UP,    2
    .set DOWN,  3
    .set RIGHT, 4

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


    // TEXT SECTION
    .section .text
apple_sprte_file:
	.string	"assets/apple.bmp"
shead_sprte_file:
	.string "assets/head.bmp"
snake_sprte_file:
	.string "assets/snake.bmp"
field_sprte_file:
	.string "assets/field.bmp"

	.globl asmo_init
	.type asmo_init, @function
asmo_init:
    pushq	%rbp
	movq	%rsp, %rbp

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
	movq	field_texture(%rip), %rdx
	call	show_sprite
    pop_regs %rsi, %rdi
    test    %rdi, %rdi
    jne     loop_x
    test    %rsi, %rsi
    jne     loop_y

    // set apple position
	movw	$0x0505, fruit(%rip)

    // set direction
    movb    $RIGHT, dir(%rip)

    // let him eat
    movw    fruit(%rip), %ax
    movw    %ax, head(%rip)

	movl	$0, snake(%rip)     ; snake.first = 0
	movl	$0, 4+snake(%rip)   ; snake.last = 0
	movl	$0, 8+snake(%rip)   ; snake.len = 0


    // leave
    movq    %rbp, %rsp
    popq    %rbp
    ret
