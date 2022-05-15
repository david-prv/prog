.text
	.globl move_left
	
#
#	$a0 buffer address
#	$a1 buffer length
#
#	|----|----|----|----|		|----|----|----|----|	
#	|  0 |  2 |  0 |  4 |	=> 	|  2 |  4 |  0 |  0 |
#	|----|----|----|----|		|----|----|----|----|	
#
	
# function to move all tiles as
# far left as possible
move_left:
	# Prologue
	addiu	$sp $sp -4 # we want to save return-addr
	sw	$ra ($sp)
	
	# call move one
	jal	move_one
	
	# Epilogue
	lw	$ra ($sp)
	addiu	$sp $sp 4
	
	# check if v0 = zero
	beqz 	$v0 exit
	
	# moves were possible
	j	move_left
	
	exit:
		jr	$ra
