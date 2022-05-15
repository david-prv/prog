.text
	.globl merge

#
#	$a0 buffer address
#	$a1 buffer length
#
#	|----|----|----|----|		|----|----|----|----|
#	|  2 |  2 |  0 |  4 |  => 	|  4 |  0 |  0 |  4 |
#	|----|----|----|----|		|----|----|----|----|
#
#   BONUS: Return the number of merges in $v0 and the
#          total base score of the merges in $v1. 

# subroutine that merges two neighbor tiles with the same values
# and replaces the leftmost with a tile with the sum inside and removes the rightmost tile
merge:
	li 	$v0 0 # standard: number of merges = 0
	li	$v1 0 # standard: total base score
	move	$t0 $a0 # base address of address-array
	move	$t1 $a1 # counter variable
	
	loop:
		# loop logics
		addiu	$t1 $t1 -1 # decrement counter by 1
		beq	$t1 $zero exit # exit if all iterations are done
		
		# load first value
		lw	$t2 ($t0) # load address at current index
		lhu	$t3 ($t2) # load value at retrieved address
		
		# load second value
		lw	$t4 4($t0) # load next address in array
		lhu	$t5 ($t4) # load value of retrieved (next) address
		
		beq 	$t3 $t5 tiles_equal # if tiles are equal jump to tiles_equal
		continue:
		
		# change base address
		addiu	$t0 $t0 4 # increment base address by 4
		
		j	loop
		
# if visited tiles are equal:	 	
tiles_equal:
	# if first tile is zero: I don't want to merge!
	beqz	$t3 continue

	# calculate sum
	li	$t6 0
	addu	$t6 $t3 $t5
	
	sh	$t6 ($t2) # save sum at left-most tile
	sh	$zero ($t4) # empty right-most tile
	
	# sum up base score
	addu	$v1 $v1 $t6
	
	# increment number of merges by 1
	addiu	$v0 $v0 1
	
	# continue loop
	j 	continue

exit:
	jr	$ra
