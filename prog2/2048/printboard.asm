.data
	.globl printboard
	line: .asciiz "-----------------------------\n"
	wall: .asciiz "|"
	linebreak: .asciiz "\n"
	emptyline: .asciiz "|      |      |      |      |\n"
	pad4: .asciiz "    "
	pad3: .asciiz "   "
	pad2: .asciiz "  "
	pad1: .asciiz " "

#
# a0 Address of the first field of the board
#
#	-----------------------------
#	|      |      |      |      |
#	| 2048 |  128 |    8 | 1024 |
#	|      |      |      |      |
#	-----------------------------
#	|      |      |      |      |
#	| 1024 |   64 |    4 |    8 |
#	|      |      |      |      |
#	-----------------------------
#	|      |      |      |      |
#	|  512 |   32 |  512 |  128 |
#	|      |      |      |      |
#	-----------------------------
#	|      |      |      |      |
#	|  256 |   16 | 2048 | 1024 |
#	|      |      |      |      |
#	-----------------------------
#
.text

printboard:
	move	$t0 $a0 # copy base address
	li	$t1 0 # row counter
	li	$t2 0 # column counter
	li	$t3 0 # loop counter
	
	j	print_top # print first lines
	
	# main game loop
	loop:
		lhu 	$t4 ($t0) # load current value
		
		# increment column
		addiu	$t2 $t2 1
		
		# decide if a row ends here
		blt	$t2 5 print_number
		
			# row ends here
			move	$t2 $zero # reset column counter
			addiu	$t2 $t2 1 # increase by one
			addiu	$t1 $t1 1 # increase row counter
			j	make_row_break # creates line break after row
		
		print_number:
		# decide which padding is used
		blt	$t4 10 print_single_digit
		blt	$t4 100 print_two_digits
		blt	$t4 1000 print_three_digits
		blt	$t4 10000 print_four_digits
		
		continue:
		# assuming that the column cell is correctly padded now
		# print the actual number
		move	$a0 $t4
		li	$v0 1
		syscall
		
		# print one space
		la	$a0 pad1
		li	$v0 4
		syscall
		
		# print wall
		la	$a0 wall
		li	$v0 4
		syscall
		
		addiu	$t0 $t0 2
		
		j 	loop
	

# Create line break afterwards
make_row_break:
	# check if row is still in boundaries
	# if it is the 4th row: print bottom and exit
	beq	$t1 4 print_bottom
	
	# print linebreak
	la	$a0 linebreak
	li	$v0 4
	syscall
	
	j	print_separation_row

# Print a single digit
print_single_digit:
	# print padding 4
	la	$a0 pad4
	li	$v0 4
	syscall
	
	j	continue

# Print two digits
print_two_digits:
	# print padding 3
	la	$a0 pad3
	li	$v0 4
	syscall
	
	j	continue

# Print three digits
print_three_digits:
	# print padding 2
	la	$a0 pad2
	li	$v0 4
	syscall
	
	j	continue

# Print four digits
print_four_digits:	
	# print padding 1
	la	$a0 pad1
	li	$v0 4
	syscall
	
	j	continue	
	
# Print everything needed for a new row
print_new_row:	
	# print first wall
	la	$a0 wall
	li	$v0 4
	syscall
	
	j	loop

# print top of playing field
print_top:
	# print line
	la	$a0 line
	li	$v0 4
	syscall
	
	# print empty line
	la	$a0 emptyline
	li	$v0 4
	syscall
	
	j	print_new_row # after top there starts a new row
	
# print bottom of playing field
print_bottom:
	# print new line
	la	$a0 linebreak
	li	$v0 4
	syscall
	
	# print empty line
	la	$a0 emptyline
	li	$v0 4
	syscall
	
	# print line
	la	$a0 line
	li	$v0 4
	syscall
	
	j	exit

# print separating lines between rows:
print_separation_row:
	# print empty line
	la	$a0 emptyline
	li	$v0 4
	syscall
	
	# print line
	la	$a0 line
	li	$v0 4
	syscall
	
	# print empty line
	la	$a0 emptyline
	li	$v0 4
	syscall
	
	# print first wall
	la	$a0 wall
	li	$v0 4
	syscall
	
	j	print_number

# exit program
exit:
	jr	$ra
