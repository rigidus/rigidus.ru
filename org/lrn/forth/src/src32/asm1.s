
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
    # leaq    \pathname(%rip), %rdi
    # call    load_sprite
    # movq    %rax, \variable(%rip)
    .endm

    // macro for save registers
    .macro push_regs regs:vararg
    # .irp    reg, \regs
    # pushq   \reg
    # .endr
    .endm

    // macro for restore registers
    .macro pop_regs regs:vararg
    # .irp    reg, \regs
    # popq    \reg
    # .endr
    .endm

    // macro for call show_sprite
    .macro call_show_sprite x y texture
    # movq	\texture, %rdx
	# movzbl	\y, %esi
	# movzbl	\x, %edi
	# call	show_sprite
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
    // load sprites
    load_sprite apple_sprte_file, fruit_texture
    load_sprite shead_sprte_file, shead_texture
    load_sprite snake_sprte_file, snake_texture
    load_sprite field_sprte_file, field_texture
    // clear field
#     movq    $max.y+1, %rsi
# loop_y:
#     decq    %rsi
#     movq    $max.x+1, %rdi
# loop_x:
#     decq    %rdi
#     push_regs %rdi, %rsi
#     movq    field_texture(%rip), %rdx
#     call    show_sprite
#     pop_regs %rsi, %rdi
#     test    %rdi, %rdi
#     jne     loop_x
#     test    %rsi, %rsi
#     jne     loop_y
#     // set apple position
#     movw    $0x0505, fruit(%rip)
#     // set direction
#     movb    $RIGHT, dir(%rip)
#     // let him eat
#     movw    fruit(%rip), %ax
#     movw    %ax, head(%rip)
#     // queue init
#     movl    $0, snake.first(%rip)
#     movl    $0, snake.last(%rip)
#     movl    $0, snake.len(%rip)
#     // add elt to queue
#     call    enqueue
#     call    next_fruit
#     // let him eat
# 	movb	$1, eaten(%rip)
# 	movb	$0, old_dir(%rip)
#     // leave
    ret


#     // in  : -
#     // use : rax rbx rcx rdx
#     .globl  enqueue
#     .type   enqueue, @function
# enqueue:
#     // snake.elems[snake.last] = head;
#     xor     %rcx, %rcx                  # clear C
#     movb    snake.last(%rip), %cl       # C = snake.last
#     leaq    snake.elems(%rip), %rax     # А = база snake.elems
#     movw    head(%rip), %dx             # D = [head] (читаем два байта)
#     movw    %dx, (%rax, %rcx, 2)        # [A + C*2] = D (пишем два байта)
#     // snake.last = (snake.last + 1) % 255;
#     incb    snake.last(%rip)
#     // snake.len++;
#     incb    snake.len(%rip)
#     // mat[head.x][head.y] = 1
#     movb    head.x(%rip), %al
#     movb    head.y(%rip), %cl
#     movb    $1, %bl
#     jmp     set_fld


#     // in  : -
#     // use : rax rbx rcx rdx
#     .globl  dequeue
#     .type   dequeue, @function
# dequeue:
#     // tail = snake.elems[snake.first];
#     xor     %rcx, %rcx                  # clear C
#     movb    snake.first(%rip), %cl      # C = snake.first
#     leaq    snake.elems(%rip), %rax     # А = база snake.elems
#     movw    (%rax, %rcx, 2), %dx        # D = [A + C*2] (читаем два байта)
#     movw    %dx, tail(%rip)             # [tail] = D (пишем два байта)
#     // snake.first = (snake.first + 1) % 255;
#     incb    snake.first(%rip)
#     // snake.len--;
#     decb    snake.len(%rip)
#     // mat[tail.x][tail.y] = 0;
#     movb    tail.x(%rip), %al
#     movb    tail.y(%rip), %cl
#     movb    $0, %bl
#     jmp     set_fld


#     // in  : al=x cl=y bl=setval
#     // use : rax  rcx  rdx
#     .globl  set_fld
#     .type   set_fld, @function
# set_fld:
#     // mat[x][y] = 0;
#     call    get_mat_addr
#     movb    %bl, (%rax)                # [RAX+RDX] = bl
#     ret


#     // in  : al=x cl=y
#     // use : rax  rcx  rdx
#     // ret : rax
#     .globl  get_mat_addr
#     .type   get_mat_addr, @function
# get_mat_addr:
#     movzbq  %al, %rax                   # RAX = x
#     movw    $max.y+1, %dx               # DX  = max.y + 1
#     mul     %dx                         # RAX = (max.y + 1) * x
#     movzbq  %cl, %rcx                   # RCX = y
#     add     %rcx, %rax                  # RAX = (max.y + 1) * x) + y
#     leaq    mat(%rip), %rdx             # RDX = mat
#     add     %rdx, %rax                  # RAX = mat + (max.y + 1) * x) + y
#     ret


#     // in  : -
#     // use : rax
#     .globl	next_fruit
# 	.type	next_fruit, @function
# next_fruit:
# rpt_rnd_x:
# 	call	rand@PLT
#     cmpb	$max.x, %al
# 	ja	    rpt_rnd_x
#     movb	%al, fruit(%rip)
# rpt_rnd_y:
# 	call	rand@PLT
#     cmpb	$max.y, %al
# 	ja	    rpt_rnd_y
#     movb	%al, 1+fruit(%rip)
# 	ret


#     // in  : -
#     // use :
#     // side-effects : gameover_flag
#     // ret : -
#     .globl	check_borders
# 	.type	check_borders, @function
# check_borders:
#     // body = head;
# 	movzwl	head(%rip), %eax
# 	movw	%ax, body(%rip)
#     // switch by dir
# 	movzbl	dir(%rip), %eax
#     cmpl	$UP, %eax
# 	je	check_borders_up
# 	cmpl	$DOWN, %eax
# 	je	check_borders_down
#     cmpl	$LEFT, %eax
# 	je	check_borders_left
#     cmpl	$RIGHT, %eax
# 	je	check_borders_right
# check_borders_fail:
#     movb    $1, gameover_flag(%rip)
# check_borders_leave:
# 	ret
# check_borders_up:
#     decb    head.y(%rip)
#     js  check_borders_fail
# 	jmp	check_borders_leave
# check_borders_down:
#     incb    head.y(%rip)
#     cmpb    $max.y, head.y(%rip)
#     ja check_borders_fail
# 	jmp	check_borders_leave
# check_borders_left:
#     decb    head.x(%rip)
#     js  check_borders_fail
# 	jmp	check_borders_leave
# check_borders_right:
#     incb    head.x(%rip)
#     cmpb    $max.x, head.x(%rip)
#     ja  check_borders_fail
#     jmp	check_borders_leave


#     // in  : -
#     // use :
#     // side-effects : gameover_flag
#     // ret : -
#     .globl	check_matrix
# 	.type	check_matrix, @function
# check_matrix:
#     movb    head.x(%rip), %al
#     movb    head.y(%rip), %cl
#     call    get_mat_addr
#     testb   $1, (%rax)
#     je      check_matrix_leave
#     movb    $1, gameover_flag(%rip)
# check_matrix_leave:
#     ret



    .globl	forth_asm_start
	.type	forth_asm_start, @function
forth_asm_start:
    ret



    .globl	update2
	.type	update2, @function
update2:
# 	call  check_borders
# 	call  check_matrix
#     // head.x == fruit
# 	movzbl	head(%rip), %edx
# 	movzbl	fruit(%rip), %eax
# 	cmpb	%al, %dl
# 	jne upd_else
#     // head.y == fruit.y
# 	movzbl	1+head(%rip), %edx
# 	movzbl	1+fruit(%rip), %eax
# 	cmpb	%al, %dl
# 	jne	upd_else
#     // && then
# upd_then:
# 	call next_fruit
# 	movb	$1, eaten(%rip)
# 	jmp	 upd_que
# upd_else:
# 	call dequeue
# 	movb	$0, eaten(%rip)
# upd_que:
# 	call enqueue
    ret


	.globl	render
	.type	render, @function
render:
# 	//  if (snake.len > 1) show snake_texture
# 	cmpb	$1, snake.len(%rip)
# 	jbe	over_show_sprite_body
# 	call_show_sprite body.x(%rip), body.y(%rip), snake_texture(%rip)
# over_show_sprite_body:
#     // if (eaten) show fruit_texture
# 	testb	$1, eaten(%rip)
# 	je	show_field
# 	call_show_sprite fruit.x(%rip), fruit.y(%rip), fruit_texture(%rip)
# 	jmp	show_head
# show_field:
# 	call_show_sprite tail.x(%rip), tail.y(%rip), field_texture(%rip)
# show_head:
#     call_show_sprite head.x(%rip), head.y(%rip), shead_texture(%rip)
# render_finish:
# 	movq	renderer(%rip), %rdi
# 	call	SDL_RenderPresent@PLT
# render_leave:
	ret


     .globl	input
 	.type	input, @function
 input:
# 	// RBX = SDL_GetKeyboardState(NULL);
# 	xorl	%edi, %edi
# 	call	SDL_GetKeyboardState@PLT
# 	movq	%rax, %rbx # RBX - pointer of state
# 	// SDL_PumpEvents();
# 	call	SDL_PumpEvents@PLT
#     movq    $scan.up, %rax
# 	movb	(%rax, %rbx), %al
# 	testb	%al, %al
# 	je	input_dir_not_up
# 	movb	$UP, dir(%rip)
# 	jmp	input_fin_scan
# input_dir_not_up:
# 	movq	$scan.down, %rax
# 	movb	(%rax, %rbx), %al
# 	testb	%al, %al
# 	je	input_dir_not_down
# 	movb	$DOWN, dir(%rip)
# 	jmp	input_fin_scan
# input_dir_not_down:
# 	movq	$scan.left, %rax
# 	movb	(%rax, %rbx), %al
# 	testb	%al, %al
# 	je	input_dir_not_left
# 	movb	$LEFT, dir(%rip)
# 	jmp	input_fin_scan
# input_dir_not_left:
# 	movq	$scan.right, %rax
# 	movb	(%rax, %rbx), %al
# 	testb	%al, %al
# 	je	input_dir_not_right
# 	movb	$RIGHT, dir(%rip)
# 	jmp	input_fin_scan
# input_dir_not_right:
# 	movq	$scan.escape, %rax
# 	movb	(%rax, %rbx), %al
# 	testb	%al, %al
# 	je	input_fin_scan
# 	movl	$0, %edi
# 	call	exit@PLT
# input_fin_scan:
# 	movb	snake.len(%rip), %al
# 	cmpb	$1, %al
# 	je	input_len_is_one
#     // else
# 	movzbl	dir(%rip), %edx
# 	movzbl	old_dir(%rip), %eax
# 	addl	%edx, %eax
# 	cmpl	$5, %eax
# 	je	input_opposite_direction
# input_len_is_one:
#     // old_dir = dir (ignore)
# 	movzbl	dir(%rip), %eax
# 	movb	%al, old_dir(%rip)
# 	jmp	input_leave
# input_opposite_direction:
# 	// dir = old_dir (new_dir)
# 	movzbl	old_dir(%rip), %eax
# 	movb	%al, dir(%rip)
# input_leave:
 	ret


#     .globl	show_sprite
# 	.type	show_sprite, @function
# show_sprite:
# 	pushq	%rbp
# 	movq	%rsp, %rbp
# 	subq	$16, %rsp
# 	movl	%edi, -4(%rbp)
# 	movl	%esi, -8(%rbp)      # RSI -> RDX
# 	movq	%rdx, -16(%rbp)     #
# 	// rect.h = TILE_SIZE;
# 	movl	$32, 12+rect(%rip)
#     // rect.w = TILE_SIZE;
# 	movl	$32, 8+rect(%rip)
# 	// rect.x = x * TILE_SIZE;
# 	movl	-4(%rbp), %eax
# 	sall	$5, %eax
# 	movl	%eax, rect(%rip)
# 	// rect.y = y * TILE_SIZE;
# 	movl	-8(%rbp), %eax       #
# 	sall	$5, %eax
# 	movl	%eax, 4+rect(%rip)
# 	// SDL_RenderCopy(renderer, texture, NULL, &rect);
#     //                  RDI       RSI    EDX    RCX
# 	movq	renderer(%rip), %rdi
# 	movq	-16(%rbp), %rsi      # RDX -> RSI
# 	leaq	rect(%rip), %rcx
# 	xor 	%edx, %edx
# 	call	SDL_RenderCopy@PLT
# 	leave
# 	ret
