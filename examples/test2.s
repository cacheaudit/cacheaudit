	.text
.globl _main
_main:
	pushl %ebp
	movl %esp, %ebp
	movl %eax, %ebx
	movl %ebx, %ecx
	movl %ecx, %edx
	xorl %eax, %ebx
	xorl %eax, %ecx
	xorl %eax, %edx
	xorl %eax, %eax
	leave
	ret
.subsections_via_symbols
