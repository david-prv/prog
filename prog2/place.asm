.text
	.globl place
	
# 	$a0 board address
# 	$a1 board length
#	$a2 field number to place into
#	$a3 number to place
#
#	$v0 == 0 if place succesfull else 1
#	Prevent tile place if position is not zero

place:
	bge 	$a2 $a1 exit # check if index is in boundaries
	li 	$v0 0 # standard: false
	move 	$t0 $a2 # load field number into temp
	mul 	$t0 $t0 2 # calculate offset (x * 2)
	move	$t2 $a0 # move board address
	addu	$t2 $t2 $t0 # add offset to address
	lhu	$t1 ($t2) # load number of field with calculated address
	beqz 	$t1 place_tile # place if tile is not occupied
	
exit:
	addiu 	$v0 $v0 1
	jr 	$ra

place_tile:
	sh	$a3 ($t2)
	jr	$ra
