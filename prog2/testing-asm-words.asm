.data
number: .word -2147

.text
  lw $a0 number
  bgez $a0 exit
  subu $a0 $zero $a0
exit:
  li $v0 1
  syscall
  li $v0 10
  syscall
