    .globl identityTail
identityTail:
    pushl %ebp
    movl %esp, %ebp
    sub $4, %esp
identityTail_init:
  # int32 %0 = identityTailH(%-1,0)
    xor %eax, %eax
    pushl %eax
    movl 8(%ebp), %eax
    pushl %eax
    call identityTailH
    add $8, %esp
    movl %eax, -4(%ebp)
  # RET %0
    movl -4(%ebp), %eax
    leave
    ret
    .globl identityTailH
identityTailH:
    pushl %ebp
    movl %esp, %ebp
    sub $8, %esp
identityTailH_init:
  # BR (EQ %-1 0) identityTailH_if_body_then_1 identityTailH_if_body_else_2
    movl 8(%ebp), %eax
    xor %ecx, %ecx
    cmp %ecx, %eax
    jne identityTailH_if_body_else_2
identityTailH_if_body_then_1:
  # RET %-2
    movl 12(%ebp), %eax
    leave
    ret
identityTailH_if_body_else_2:
  # int32 %3 = SUB %-1 1
    movl 8(%ebp), %eax
    movl $1, %ecx
    sub %ecx, %eax
    movl %eax, -4(%ebp)
  # int32 %4 = ADD %-2 1
    movl 12(%ebp), %eax
    movl $1, %ecx
    add %ecx, %eax
    movl %eax, -8(%ebp)
  # tail identityTailH(%3,%4)
    movl -8(%ebp), %eax
    movl %eax, 12(%ebp)
    movl -4(%ebp), %eax
    movl %eax, 8(%ebp)
    jmp identityTailH_init
identityTailH_if_cont_0:
  # *unreachable*
    call __unreachable
    .globl identityNotail
identityNotail:
    pushl %ebp
    movl %esp, %ebp
    sub $12, %esp
identityNotail_init:
  # BR (EQ %-1 0) identityNotail_if_body_1 identityNotail_if_cont_0
    movl 8(%ebp), %eax
    xor %ecx, %ecx
    cmp %ecx, %eax
    jne identityNotail_if_cont_0
identityNotail_if_body_1:
  # RET 0
    xor %eax, %eax
    leave
    ret
identityNotail_if_cont_0:
  # int32 %2 = SUB %-1 1
    movl 8(%ebp), %eax
    movl $1, %ecx
    sub %ecx, %eax
    movl %eax, -4(%ebp)
  # int32 %3 = identityNotail(%2)
    movl -4(%ebp), %eax
    pushl %eax
    call identityNotail
    add $4, %esp
    movl %eax, -8(%ebp)
  # int32 %4 = ADD %3 1
    movl -8(%ebp), %eax
    movl $1, %ecx
    add %ecx, %eax
    movl %eax, -12(%ebp)
  # RET %4
    movl -12(%ebp), %eax
    leave
    ret
    .globl main
main:
    pushl %ebp
    movl %esp, %ebp
    sub $20, %esp
main_init:
  # int32 %0 = 0
    movl $0, -4(%ebp)
  # int32 %0 = 1000000000
    movl $1000000000, -4(%ebp)
  # int1 %1 = 0
    movl $0, -8(%ebp)
  # int1 %1 = 1
    movl $1, -8(%ebp)
  # int32 %2 = 0
    movl $0, -12(%ebp)
  # JMP main_block_3
main_block_3:
  # int32 %5 = identityTail(%0)
    movl -4(%ebp), %eax
    pushl %eax
    call identityTail
    add $4, %esp
    movl %eax, -16(%ebp)
  # int32 %2 = %5
    movl -16(%ebp), %edx
    movl %edx, -12(%ebp)
  # JMP main_block_4
main_block_4:
  # void %6 = printInt(%2)
    movl -12(%ebp), %eax
    pushl %eax
    call printInt
    add $4, %esp
    movl %eax, -20(%ebp)
  # RET 0
    xor %eax, %eax
    leave
    ret
