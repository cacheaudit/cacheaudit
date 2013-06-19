	.text			# Symmetric case of octtest3b, should lead to the same final state
.globl main
main:
	#pushl %ebp
	#movl %esp, %ebp
	movl  -32(%ebp), %eax	# move a into set 1
	movl  -64(%ebp), %eax	# move b into set 2
	movl  -96(%ebp), %eax	# move c into set 1
	cmpl $1, %eax           # make some case distinction
	jbe L1                  # in the first case L1
	movl -160(%ebp), %eax   # second case: move e into set 1
	movl -192(%ebp), %eax	# move f into set 2
	movl -256(%ebp), %eax	# move h into set 2
	movl -320(%ebp), %eax	# move j into set 2
	jmp L2
L1:	
	movl -128(%ebp),%eax	# move d into set 2
	movl  -32(%ebp),%eax	# access a
	movl -160(%ebp),%eax	# move d into set 2
L2:	
	#leave
	ret			# should test reordering of octagons
				# different variables are added
				# a different number of variables are added
				# the same variable is added at different positions
