	.text
.globl _main
_main:
	pushl %ebp
	movl %esp,%ebp
	subl $32, %esp
	movl $2,-4(%ebp)
	movl $4,-8(%ebp)
	movl $6,-12(%ebp)
	cmpl $10, %eax
	jbe L1
	movl (%ebp),%eax
	jmp L2
L1:	
	movl -32(%ebp),%eax
L2:	
	leave
	ret
.subsections_via_symbols
