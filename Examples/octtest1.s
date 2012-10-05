	.text
.globl main
main:
	#pushl %ebp
	#movl %esp, %ebp
	movl -32(%ebp), %eax	# move c into set 1
	movl -64(%ebp), %eax	# move d into set 2
	movl -96(%ebp), %eax	# move a into set 1
	movl -128(%ebp), %eax	# move b into set 2
	cmpl $1, %eax           # make some case distinction
	jbe L1                  # in the first case access c
	movl -32(%ebp),%eax     # in the second case access d
	jmp L2
L1:	
	movl -64(%ebp),%eax
L2:	
	#leave
	ret			# now all blocks should have both possible ages 0 and 1
				# -> 4 possibilities from which 2 are invalid
				# octagons will achieve 100% precision here
