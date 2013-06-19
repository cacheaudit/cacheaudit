	.text
.globl _main
_main:
	pushl	%ebp
	movl	%esp, %ebp
	pushl	$0
	pushl   $1
	popl    %eax
	popl	%ebx
	leave
	ret
	.subsections_via_symbols
