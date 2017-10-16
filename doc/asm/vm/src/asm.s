
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
level_one_string:
	.string	"Level 1"

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
    movq    $max.y+1, %rsi
loop_y:
    decq    %rsi
    movq    $max.x+1, %rdi
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
    // add elt to queue
    call    enqueue
    call    next_fruit
    // let him eat
	movb	$1, eaten(%rip)
	movb	$0, old_dir(%rip)
	leaq	level_one_string(%rip), %rdi
	call	puts@PLT
    // leave
    ret


    // in  : -
    // use : rax rbx rcx rdx
    .globl  enqueue
    .type   enqueue, @function
enqueue:
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
    movb    head.x(%rip), %al
    movb    head.y(%rip), %cl
    movb    $1, %bl
    jmp     set_fld


    // in  : -
    // use : rax rbx rcx rdx
    .globl  dequeue
    .type   dequeue, @function
dequeue:
    // tail = snake.elems[snake.first];
    xor     %rcx, %rcx                  # clear C
    movb    snake.first(%rip), %cl      # C = snake.first
    leaq    snake.elems(%rip), %rax     # А = база snake.elems
    movw    (%rax, %rcx, 2), %dx        # D = [A + C*2] (читаем два байта)
    movw    %dx, tail(%rip)             # [tail] = D (пишем два байта)
    // snake.first = (snake.first + 1) % 255;
    incb    snake.first(%rip)
    // snake.len--;
    decb    snake.len(%rip)
    // mat[tail.x][tail.y] = 0;
    movb    tail.x(%rip), %al
    movb    tail.y(%rip), %cl
    movb    $0, %bl
    jmp     set_fld


    // in  : al=x cl=y bl=setval
    // use : rax  rcx  rdx
    .globl  set_fld
    .type   set_fld, @function
set_fld:
    // mat[x][y] = 0;
    call    get_mat_addr
    movb    %bl, (%rax)                # [RAX+RDX] = bl
    ret


    // in  : al=x cl=y
    // use : rax  rcx  rdx
    // ret : rax
    .globl  get_mat_addr
    .type   get_mat_addr, @function
get_mat_addr:
    movzbq  %al, %rax                   # RAX = x
    movw    $max.y+1, %dx               # DX  = max.y + 1
    mul     %dx                         # RAX = (max.y + 1) * x
    movzbq  %cl, %rcx                   # RCX = y
    add     %rcx, %rax                  # RAX = (max.y + 1) * x) + y
    leaq    mat(%rip), %rdx             # RDX = mat
    add     %rdx, %rax                  # RAX = mat + (max.y + 1) * x) + y
    ret


    // in  : -
    // use : rax
    .globl	next_fruit
	.type	next_fruit, @function
next_fruit:
rpt_rnd_x:
	call	rand@PLT
    cmpb	$max.x, %al
	ja	    rpt_rnd_x
    movb	%al, fruit(%rip)
rpt_rnd_y:
	call	rand@PLT
    cmpb	$max.y, %al
	ja	    rpt_rnd_y
    movb	%al, 1+fruit(%rip)
	ret


    // in  : -
    // use :
    // side-effects : gameover_flag
    // ret : -
    .globl	check_borders
	.type	check_borders, @function
check_borders:
    // body = head;
	movzwl	head(%rip), %eax
	movw	%ax, body(%rip)
    // switch by dir
	movzbl	dir(%rip), %eax
    cmpl	$UP, %eax
	je	check_borders_up
	cmpl	$DOWN, %eax
	je	check_borders_down
    cmpl	$LEFT, %eax
	je	check_borders_left
    cmpl	$RIGHT, %eax
	je	check_borders_right
check_borders_fail:
    movb    $1, gameover_flag(%rip)
check_borders_leave:
	ret
check_borders_up:
    decb    head.y(%rip)
    js  check_borders_fail
	jmp	check_borders_leave
check_borders_down:
    incb    head.y(%rip)
    cmpb    $max.y, head.y(%rip)
    ja check_borders_fail
	jmp	check_borders_leave
check_borders_left:
    decb    head.x(%rip)
    js  check_borders_fail
	jmp	check_borders_leave
check_borders_right:
    incb    head.x(%rip)
    cmpb    $max.x, head.x(%rip)
    ja  check_borders_fail
    jmp	check_borders_leave


    // in  : -
    // use :
    // side-effects : gameover_flag
    // ret : -
    .globl	check_matrix
	.type	check_matrix, @function
check_matrix:
    movb    head.x(%rip), %al
    movb    head.y(%rip), %cl
    call    get_mat_addr
    testb   $1, (%rax)
    je      check_matrix_leave
    movb    $1, gameover_flag(%rip)
check_matrix_leave:
    ret


    .globl	update2
	.type	update2, @function
update2:
	call  check_borders
	call  check_matrix
    // head.x == fruit
	movzbl	head(%rip), %edx
	movzbl	fruit(%rip), %eax
	cmpb	%al, %dl
	jne upd_else
    // head.y == fruit.y
	movzbl	1+head(%rip), %edx
	movzbl	1+fruit(%rip), %eax
	cmpb	%al, %dl
	jne	upd_else
    // && then
upd_then:
	call next_fruit
	movb	$1, eaten(%rip)
	jmp	 upd_que
upd_else:
	call dequeue
	movb	$0, eaten(%rip)
upd_que:
	call enqueue
    ret


	.globl	render
	.type	render, @function
render:
	//  if (snake.len > 1) show snake_texture
	cmpb	$1, snake.len(%rip)
	jbe	over_show_sprite_body
	movq	snake_texture(%rip), %rdx
	movzbl	body.y(%rip), %esi
	movzbl	body.x(%rip), %edi
	call	show_sprite
over_show_sprite_body:
    // if (eaten) show fruit_texture
	testb	$1, eaten(%rip)
	je	show_fruit
	movq	fruit_texture(%rip), %rdx
	movzbl	fruit.y(%rip), %esi
	movzbl	fruit.x(%rip), %edi
	call	show_sprite
	jmp	show_head
show_fruit:
	movq	field_texture(%rip), %rdx
	movzbl	tail.y(%rip), %esi
	movzbl	tail.x(%rip), %edi
	call	show_sprite
show_head:
	.loc 1 113 0
	movq	shead_texture(%rip), %rdx
	movzbl	head.y(%rip), %esi
	movzbl	head.x(%rip), %edi
	call	show_sprite
render_finish:
	movq	renderer(%rip), %rdi
	call	SDL_RenderPresent@PLT
render_leave:
	ret
