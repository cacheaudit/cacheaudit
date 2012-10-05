	.file	"looptest3.c"
	.text
	.globl	main
	.type	main, @function
main:
.LFB0:
	.cfi_startproc
	pushl	%ebp
	.cfi_def_cfa_offset 8
	.cfi_offset 5, -8
	movl	%esp, %ebp
	.cfi_def_cfa_register 5
	subl	$96, %esp
	movl	$0, -4(%ebp)
	movl	$0, -8(%ebp)
	jmp	.L2
.L3:
	addl	$1, -4(%ebp)
	addl	$1, -8(%ebp)
.L2:
	cmpl	$4, -8(%ebp)
	jbe	.L3
	movl	-4(%ebp), %eax
	movl	-88(%ebp,%eax,4), %eax
	movl	%eax, -4(%ebp)
	movl	$0, %eax
	leave
	.cfi_restore 5
	.cfi_def_cfa 4, 4
	ret
	.cfi_endproc
.LFE0:
	.size	main, .-main
	.ident	"GCC: (Ubuntu/Linaro 4.6.3-1ubuntu5) 4.6.3"
	.section	.note.GNU-stack,"",@progbits
