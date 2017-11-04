	.text
	.globl	_start
	.type	_start, @function
_start:
    ret

a:
    .ascii "_."
    .byte 0
b:
    .ascii "_..."
c:
    .ascii "_._."
    .ascii "AAAA"
table:
table_a:
    .ascii "a"
    .int a
table_b:
    .ascii "b"
    .int b
table_c:
    .ascii "c"
    .int c
