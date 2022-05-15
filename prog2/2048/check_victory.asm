.text
	.globl check_victory


#
#	$a0 board address
#	$a1 board length
#
#	$v0 == 1 iff 2048 found
#

# Checks for victory
check_victory:
	li 	$v0 0 # standard: false
	move	$t0 $a0
	move 	$t1 $a1

# Loop that goes through all playing fields
# and checks for the number 2048
loop:
	# $t1 = counter variable
	# $t0 = current address of current field
	# reg $t2 shows current number
	lhu 	$t2 ($t0)
	beq	$t2 2048 won
	beq	$t1 1 exit
	addiu	$t1 $t1 -1 # decrement counter variable
	addiu	$t0 $t0 2 # increment current address
	j	loop
	
exit:
	jr 	$ra

won:
	addiu 	$v0 $v0 1
	j	exit
