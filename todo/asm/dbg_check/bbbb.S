	.file	"bbbb.c"
	.section	.rodata
frmt_str:
	.string	"%x\n"
dbg_str:
	.string	"debugger!"

	.text
	.globl	main
	.type	main, @function
main:
.LFB0:
	.cfi_startproc
	leal	4(%esp), %ecx
	andl	$-16, %esp
	pushl	-4(%ecx)
	pushl	%ebp
	movl	%esp, %ebp

	pushl	%ecx
	subl	$4, %esp
	call	lab
	subl	$8, %esp

	pushl	%eax
	pushl	$frmt_str
	call	printf
	addl	$16, %esp

	nop

	movl	-4(%ebp), %ecx

	leave
	leal	-4(%ecx), %esp
	ret
	.cfi_endproc
end_main:	
	.size	main, .-main
	.globl	lab
	.type	lab, @function
lab:
	.cfi_startproc
	pushl	%ebp
	movl	%esp, %ebp
prepare:	
 	xor	%edx, %edx
 	movl    $main, %esi
 	movl    $end_main, %edi
 	jmp	loop_check
loop:
 	movzbl  (%esi), %eax
 	cmpb    $0xCC, %al
 	jne     over_dbg
dbg:	
 	subl	$12, %esp
 	pushl	$dbg_str
 	call	puts
 	addl	$16, %esp
over_dbg:		
 	add	%eax, %edx
 	inc	%esi
loop_check:
 	cmp 	%esi, %edi
 	jne	loop
leave:	
 	movl	%edx, %eax
	leave
	ret
	.cfi_endproc
.LFE1:
	.size	lab, .-lab
	.ident	"GCC: (Ubuntu 5.4.0-6ubuntu1~16.04.5) 5.4.0 20160609"
	.section	.note.GNU-stack,"",@progbits
