	.file	"bo.c"
	.section	.rodata
.LC0:
	.string	"Enter password: "
.LC1:
	.string	"secret"
	.text
	.globl	main
	.type	main, @function
main:
.LFB0:
	.cfi_startproc
	leal	4(%esp), %ecx
	.cfi_def_cfa 1, 0
	andl	$-16, %esp
	pushl	-4(%ecx)
	pushl	%ebp
	.cfi_escape 0x10,0x5,0x2,0x75,0
	movl	%esp, %ebp
	pushl	%ebx
	pushl	%ecx
	.cfi_escape 0xf,0x3,0x75,0x78,0x6
	.cfi_escape 0x10,0x3,0x2,0x75,0x7c
	subl	$80, %esp
	call	__x86.get_pc_thunk.bx
	addl	$_GLOBAL_OFFSET_TABLE_, %ebx
	movl	%ecx, %eax
	movl	4(%eax), %eax
	movl	%eax, -76(%ebp)
	movl	%gs:20, %eax
	movl	%eax, -12(%ebp)
	xorl	%eax, %eax
	movl	$0, -68(%ebp)

    subl	$12, %esp               # "sp -= 12"
	leal	.LC0@GOTOFF(%ebx), %eax # "Enter password: "
	pushl	%eax                    # "sp -= 4"
	call	puts@PLT                # "puts"
	addl	$16, %esp               # "sp += 16  :: clear stack"

	subl	$12, %esp               # "sp -= 12"
	leal	-62(%ebp), %eax         # "%ebp buffer"
	pushl	%eax                    # "sp -= 4"
	call	gets@PLT                # "gets"
	addl	$16, %esp               # "sp += 16  :: clear stack"

	subl	$8, %esp                # "sp -= 8"
	leal	.LC1@GOTOFF(%ebx), %eax # "secret"
	pushl	%eax                    # "sp -= 4"
	leal	-62(%ebp), %eax         # "-62(%ebp) :: buffer"
	pushl	%eax                    # "sp -= 4"
	call	strcmp@PLT              # "strcmp"
	addl	$16, %esp               # "sp += 16  :: clear stack"

	testl	%eax, %eax              # "test"
	je	.L2                         # "jmp if not equal"
	movl	$1, -68(%ebp)           # "set 1 to -68(%ebp)"
.L2:
	subl	$12, %esp
	pushl	-68(%ebp)
	call	do_smth
	addl	$16, %esp
	movl	$0, %eax
	movl	-12(%ebp), %edx
	xorl	%gs:20, %edx
	je	.L4
	call	__stack_chk_fail_local
.L4:
	leal	-8(%ebp), %esp
	popl	%ecx
	.cfi_restore 1
	.cfi_def_cfa 1, 0
	popl	%ebx
	.cfi_restore 3
	popl	%ebp
	.cfi_restore 5
	leal	-4(%ecx), %esp
	.cfi_def_cfa 4, 4
	ret
	.cfi_endproc
.LFE0:
	.size	main, .-main
	.section	.rodata
.LC2:
	.string	"hello, registred user"
.LC3:
	.string	"go away, hacker"
	.text
	.globl	do_smth
	.type	do_smth, @function
do_smth:
.LFB1:
	.cfi_startproc
	pushl	%ebp
	.cfi_def_cfa_offset 8
	.cfi_offset 5, -8
	movl	%esp, %ebp
	.cfi_def_cfa_register 5
	pushl	%ebx
	subl	$4, %esp
	.cfi_offset 3, -12
	call	__x86.get_pc_thunk.ax
	addl	$_GLOBAL_OFFSET_TABLE_, %eax
	cmpl	$0, 8(%ebp)
	jne	.L6
	subl	$12, %esp
	leal	.LC2@GOTOFF(%eax), %edx
	pushl	%edx
	movl	%eax, %ebx
	call	puts@PLT
	addl	$16, %esp
	jmp	.L8
.L6:
	subl	$12, %esp
	leal	.LC3@GOTOFF(%eax), %edx
	pushl	%edx
	movl	%eax, %ebx
	call	puts@PLT
	addl	$16, %esp
.L8:
	nop
	movl	-4(%ebp), %ebx
	leave
	.cfi_restore 5
	.cfi_restore 3
	.cfi_def_cfa 4, 4
	ret
	.cfi_endproc
.LFE1:
	.size	do_smth, .-do_smth
	.section	.text.__x86.get_pc_thunk.ax,"axG",@progbits,__x86.get_pc_thunk.ax,comdat
	.globl	__x86.get_pc_thunk.ax
	.hidden	__x86.get_pc_thunk.ax
	.type	__x86.get_pc_thunk.ax, @function
__x86.get_pc_thunk.ax:
.LFB2:
	.cfi_startproc
	movl	(%esp), %eax
	ret
	.cfi_endproc
.LFE2:
	.section	.text.__x86.get_pc_thunk.bx,"axG",@progbits,__x86.get_pc_thunk.bx,comdat
	.globl	__x86.get_pc_thunk.bx
	.hidden	__x86.get_pc_thunk.bx
	.type	__x86.get_pc_thunk.bx, @function
__x86.get_pc_thunk.bx:
.LFB3:
	.cfi_startproc
	movl	(%esp), %ebx
	ret
	.cfi_endproc
.LFE3:
	.hidden	__stack_chk_fail_local
	.ident	"GCC: (Ubuntu 6.2.0-5ubuntu12) 6.2.0 20161005"
	.section	.note.GNU-stack,"",@progbits
