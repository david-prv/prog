.text
	.globl move_check
#
#	$a0 buffer address
#	$a1 buffer length
#
#   	$v0 = 1 if left move possible and would change something
#            else 0
#

# global function to check if a left-move
# is possible
move_check:
	li 	$v0 0 # standard: game is lost
	# $t0 contains current address
	# $t1 is counter variable
	move	$t0 $a0 
	move	$t1 $a1
	addiu	$t1 $t1	-1 # 3 iterations are possible
	
	loop:
		beq	$t1 $zero exit # exit if all iterations are done
		lw	$t2 ($t0) # load address at current index
		lhu	$t3 ($t2) # load value at retrieved address
		
		lw	$t4 4($t0) # load next address in array
		lhu	$t5 ($t4) # load value of retrieved (next) address
		
		beqz 	$t3 move_possible # if first tile is zero: left-move possible
		
		beq	$t3 $t5 move_possible # if both tiles are equal: left-move possible
		
		addiu	$t0 $t0 4 # increment base address by 4
		addiu	$t1 $t1 -1 # decrement counter by 1
		
		j	loop # start next interation
		
exit:
	jr 	$ra # jump to return-address and return 0

# if second tile is zero
snd_is_zero:
	addiu	$t0 $t0 4 # increment base address by 4
	addiu	$t1 $t1 -1 # decrement counter by 1
	
	j 	loop # next iteration

# first tile was zero,
# check now if move is really possible
move_possible:
	beqz	$t5 snd_is_zero
	addiu 	$v0 $v0 1 # return 1
	
	jr	$ra # leave sub-routine
