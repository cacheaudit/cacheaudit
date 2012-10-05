	.text
.globl _main
_main:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$24, %esp
	movl	$-2, -12(%ebp)
	movl	-12(%ebp), %eax
	leave
	ret
	.subsections_via_symbols
