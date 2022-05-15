.text 
	.globl move_one
	
#
#	$a0 buffer address
#	$a1 buffer length
#
#	|----|----|----|----|----|		|----|----|----|----|----|	
#	|  2 |  0 |  2 |  0 |  4 |	=> 	|  2 |  2 |  0 |  4 |  0 |
#	|----|----|----|----|----|		|----|----|----|----|----|
#
#	$v0 1 if something changed else 0

# subroutine to swap every tile exactly once, 
# if and only if possible!
move_one:
	li	$t6 0 # how many have changed
	li 	$v0 0 # standard: nothing changed
	move	$t0 $a0 # base address of address-array
	move	$t1 $a1 # counter variable
	
	loop:
		# loop logics
		addiu	$t1 $t1 -1 # decrement counter by 1
		beq	$t1 $zero moves_done # exit if all iterations are done
		
		# load first value
		lw	$t2 ($t0) # load address at current index
		lhu	$t3 ($t2) # load value at retrieved address
		
		# load second value
		lw	$t4 4($t0) # load next address in array
		lhu	$t5 ($t4) # load value of retrieved (next) address
		
		# change base address
		addiu	$t0 $t0 4 # increment base address by 4
		
		# loop if it is not possible
		bgtz  	$t3 loop # if first tile not zero: next iteration
		beqz	$t5 loop # if second tile is zero: next iteration
		
		# swapping
		addiu	$t6 $t6 1 # increment "changed values" counter	
		


# swaps values at addresses
# $t2 and $t4
swap:
	# temporary storage of value in $t2
	li	$t7 0
	addu	$t7 $t7 $t3
	# store value of $t4 at $t2
	sh	$t5 ($t2)
	# restore $t2 value and store it at $t4
	sh	$t7 ($t4)
	
	# jump back to loop
	j	loop
	

# all moves are done	
moves_done:
	beq	$t6 $zero exit # nothing changed
	li	$v0 1 # return 1
exit:
	jr 	$ra
