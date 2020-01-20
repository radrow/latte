    .globl main
main:
    pushl %ebp
    movl %esp, %ebp
    sub $48, %esp
main_init:
  # int32 %0 = 0
    movl $0, -4(%ebp)
  # int32 %0 = 2
    movl $2, -4(%ebp)
  # int32 %1 = 0
    movl $0, -8(%ebp)
  # int32 %1 = 4
    movl $4, -8(%ebp)
  # int32 %2 = 0
    movl $0, -12(%ebp)
  # int32 %2 = 2137
    movl $2137, -12(%ebp)
  # int32 %3 = DIV %0 2
    movl -4(%ebp), %eax
    movl $2, %ecx
    cdq
    idiv %ecx
    movl %eax, -16(%ebp)
  # int32 %4 = ADD %0 %3
    movl -4(%ebp), %eax
    movl -16(%ebp), %ecx
    add %ecx, %eax
    movl %eax, -20(%ebp)
  # int32 %5 = SUB %4 1
    movl -20(%ebp), %eax
    movl $1, %ecx
    sub %ecx, %eax
    movl %eax, -24(%ebp)
  # int32 %6 = SUB %5 2
    movl -24(%ebp), %eax
    movl $2, %ecx
    sub %ecx, %eax
    movl %eax, -28(%ebp)
  # int32 %7 = MUL 6 1
    movl $6, %eax
    movl $1, %ecx
    cdq
    imul %ecx
    movl %eax, -32(%ebp)
  # int32 %8 = DIV %7 3
    movl -32(%ebp), %eax
    movl $3, %ecx
    cdq
    idiv %ecx
    movl %eax, -36(%ebp)
  # int32 %9 = MOD %8 10
    movl -36(%ebp), %eax
    movl $10, %ecx
    cdq
    idiv %ecx
    movl %edx, %eax
    movl %eax, -40(%ebp)
  # int32 %10 = ADD %6 %9
    movl -28(%ebp), %eax
    movl -40(%ebp), %ecx
    add %ecx, %eax
    movl %eax, -44(%ebp)
  # void %11 = printInt(%10)
    movl -44(%ebp), %eax
    pushl %eax
    call printInt
    add $4, %esp
    movl %eax, -48(%ebp)
  # RET 0
    xor %eax, %eax
    leave
    ret
