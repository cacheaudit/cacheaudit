	.text
.globl main
main:
	#pushl %ebp
	#movl %esp, %ebp
	movl -128(%ebp), %eax	# load sth in eax
	cmpl $1, %eax           # make some case distinction
	jbe L1                  # in the first case access a
	movl -32(%ebp),%eax     # in the second case access b
	jmp L2
L1:	
	movl -64(%ebp),%eax
L2:	
	#leave
	ret			# after the join, either a or be must(!) be in the cache
				# we cannot deduce that (in general) from the SimpleValueAD
				# using octagons we can by having a + b <= 5 (cache size 4)
