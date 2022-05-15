.text
	.globl complete_move
	

#
#	$a0 buffer address
#	$a1 buffer length
#
#	|----|----|----|----|		|----|----|----|----|
#	|  2 |  2 |  0 |  4 |  => 	|  4 |  4 |  0 |  0 |
#	|----|----|----|----|		|----|----|----|----|
#
#   BONUS: Return the number of merges in $v0 and the
#          total base score of the merges in $v1. 


complete_move:
	# Prologue
	addiu	$sp $sp -4 # we want to save return-addr
	sw	$ra ($sp)
	
		
		# Prologue
		addiu	$sp $sp -8 # we want to save return-addr
		sw	$v0 4($sp)
		sw	$v1 ($sp)
	
		# And call move_left
		jal	move_left
		
		# Epilogue
		lw	$v0 4($sp)
		lw	$v1 ($sp)
		addiu	$sp $sp 8
		

	# ... move_left returns here
	
	# Call merge
	jal	merge
	
	# ... merge returns here
	
		# Prologue
		addiu	$sp $sp -8 # we want to save return-addr
		sw	$v0 4($sp)
		sw	$v1 ($sp)
	
		# And call move_left once again
		jal	move_left
		
		# Epilogue
		lw	$v0 4($sp)
		lw	$v1 ($sp)
		addiu	$sp $sp 8
	
	# Epilogue
	lw	$ra ($sp)
	addiu	$sp $sp 4
	
exit:
	jr	$ra
