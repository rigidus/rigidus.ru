	.file	"argv.c"
	.text
	.globl	main
	.type	main, @function
main:
.LFB23:

#    mov     1, %eax
#    mov     2, %ebx
#    lea     (%eax, %ebx), %edx
#    nop
#    nop
#    nop
#    nop
    leal	4(%esp), %ecx
	andl	$-16, %esp
	pushl	-4(%ecx)
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%edi
	pushl	%esi
	pushl	%ebx
	pushl	%ecx
	subl	$8, %esp
	call	__x86.get_pc_thunk.bx
	addl	$_GLOBAL_OFFSET_TABLE_, %ebx
	movl	(%ecx), %esi
	movl	4(%ecx), %edi
	subl	$1, %esi
	testl	%esi, %esi
	jle	.L1
.L4:
	subl	$12, %esp
	pushl	(%edi,%esi,4)
	call	puts@PLT
	addl	$16, %esp
	subl	$1, %esi
	jne	.L4
.L1:
	leal	-16(%ebp), %esp
	popl	%ecx
	popl	%ebx
	popl	%esi
	popl	%edi
	popl	%ebp
	leal	-4(%ecx), %esp
	ret
.LFE23:
	.size	main, .-main
	.section	.text.__x86.get_pc_thunk.bx,"axG",@progbits,__x86.get_pc_thunk.bx,comdat
	.globl	__x86.get_pc_thunk.bx
	.hidden	__x86.get_pc_thunk.bx
	.type	__x86.get_pc_thunk.bx, @function
__x86.get_pc_thunk.bx:
.LFB24:
	movl	(%esp), %ebx
	ret
.LFE24:
	.ident	"GCC: (Ubuntu 6.2.0-5ubuntu12) 6.2.0 20161005"
	.section	.note.GNU-stack,"",@progbits
