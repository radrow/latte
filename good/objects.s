    .globl __cstr_1_unnamed_
__cstr_1_unnamed_:
    pushl %ebp
    movl %esp, %ebp
    sub $0, %esp
__cstr_1_unnamed__init:
  # RET %-1
    movl 8(%ebp), %eax
    leave
    ret
    .globl __cstr_1_smart
__cstr_1_smart:
    pushl %ebp
    movl %esp, %ebp
    sub $12, %esp
__cstr_1_smart_init:
  # BR (EQ %-2 0) __cstr_1_smart_if_body_then_1 __cstr_1_smart_if_body_else_2
    movl 12(%ebp), %eax
    xor %ecx, %ecx
    cmp %ecx, %eax
    jne __cstr_1_smart_if_body_else_2
__cstr_1_smart_if_body_then_1:
  # obj:2 %3 = new_obj
    pushl $4
    call malloc
    add $4, %esp
    movl $2, (%eax)
    movl %eax, -4(%ebp)
  # RET %3
    movl -4(%ebp), %eax
    leave
    ret
__cstr_1_smart_if_body_else_2:
  # BR (EQ %-2 1) __cstr_1_smart_if_body_then_5 __cstr_1_smart_if_body_else_6
    movl 12(%ebp), %eax
    movl $1, %ecx
    cmp %ecx, %eax
    jne __cstr_1_smart_if_body_else_6
__cstr_1_smart_if_body_then_5:
  # obj:3 %7 = new_obj
    pushl $4
    call malloc
    add $4, %esp
    movl $3, (%eax)
    movl %eax, -8(%ebp)
  # RET %7
    movl -8(%ebp), %eax
    leave
    ret
__cstr_1_smart_if_body_else_6:
  # BR (EQ %-2 2) __cstr_1_smart_if_body_then_9 __cstr_1_smart_if_body_else_10
    movl 12(%ebp), %eax
    movl $2, %ecx
    cmp %ecx, %eax
    jne __cstr_1_smart_if_body_else_10
__cstr_1_smart_if_body_then_9:
  # obj:4 %11 = new_obj
    pushl $4
    call malloc
    add $4, %esp
    movl $4, (%eax)
    movl %eax, -12(%ebp)
  # RET %11
    movl -12(%ebp), %eax
    leave
    ret
__cstr_1_smart_if_body_else_10:
  # RET %-1
    movl 8(%ebp), %eax
    leave
    ret
__cstr_1_smart_if_cont_8:
  # JMP __cstr_1_smart_if_cont_4
__cstr_1_smart_if_cont_4:
  # JMP __cstr_1_smart_if_cont_0
__cstr_1_smart_if_cont_0:
  # RET %-1
    movl 8(%ebp), %eax
    leave
    ret
    .globl __mth_1_f
__mth_1_f:
    sub $4, %esp
__mth_1_f_init:
  # void %0 = printInt(%-2)
    movl 12(%ebp), %eax
    pushl %eax
    call printInt
    add $4, %esp
    movl %eax, -4(%ebp)
  # RET 21
    movl $21, %eax
    leave
    ret
    .globl __mth_2_f
__mth_2_f:
    sub $4, %esp
__mth_2_f_init:
  # void %0 = printInt(%-2)
    movl 12(%ebp), %eax
    pushl %eax
    call printInt
    add $4, %esp
    movl %eax, -4(%ebp)
  # RET 42
    movl $42, %eax
    leave
    ret
    .globl __mth_3_f
__mth_3_f:
    sub $16, %esp
__mth_3_f_init:
  # void %0 = printInt(%-2)
    movl 12(%ebp), %eax
    pushl %eax
    call printInt
    add $4, %esp
    movl %eax, -4(%ebp)
  # classID %1 = 1
    movl $1, -8(%ebp)
  # int32 %2 = %1:f(%0,%-2)
    movl 12(%ebp), %eax
    pushl %eax
    movl -4(%ebp), %eax
    pushl %eax
    movl -8(%ebp), %eax
    pushl %eax
    call __f_dispatch
    add $8, %esp
    movl %eax, -12(%ebp)
  # int32 %3 = ADD %2 10000
    movl -12(%ebp), %eax
    movl $10000, %ecx
    add %ecx, %eax
    movl %eax, -16(%ebp)
  # RET %3
    movl -16(%ebp), %eax
    leave
    ret
    .globl main
main:
    pushl %ebp
    movl %esp, %ebp
    sub $124, %esp
main_init:
  # obj:1 %0 = new_obj
    pushl $4
    call malloc
    add $4, %esp
    movl $1, (%eax)
    movl %eax, -4(%ebp)
  # obj:1 %1 = __cstr_1_unnamed_(%0)
    movl -4(%ebp), %eax
    pushl %eax
    call __cstr_1_unnamed_
    add $4, %esp
    movl %eax, -8(%ebp)
  # int32 %2 = NEG 1
    movl $1, %eax
    neg %eax
    movl %eax, -12(%ebp)
  # classID %3 = classID %1
    movl -8(%ebp), %eax
    movl (%eax), %eax
    movl %eax, -16(%ebp)
  # int32 %4 = %3:f(%1,%2)
    movl -12(%ebp), %eax
    pushl %eax
    movl -8(%ebp), %eax
    pushl %eax
    movl -16(%ebp), %eax
    pushl %eax
    call __f_dispatch
    add $8, %esp
    movl %eax, -20(%ebp)
  # void %5 = printInt(%4)
    movl -20(%ebp), %eax
    pushl %eax
    call printInt
    add $4, %esp
    movl %eax, -24(%ebp)
  # obj:1 %6 = new_obj
    pushl $4
    call malloc
    add $4, %esp
    movl $1, (%eax)
    movl %eax, -28(%ebp)
  # obj:1 %7 = __cstr_1_smart(%6,0)
    xor %eax, %eax
    pushl %eax
    movl -28(%ebp), %eax
    pushl %eax
    call __cstr_1_smart
    add $8, %esp
    movl %eax, -32(%ebp)
  # classID %8 = classID %7
    movl -32(%ebp), %eax
    movl (%eax), %eax
    movl %eax, -36(%ebp)
  # int32 %9 = %8:f(%7,0)
    xor %eax, %eax
    pushl %eax
    movl -32(%ebp), %eax
    pushl %eax
    movl -36(%ebp), %eax
    pushl %eax
    call __f_dispatch
    add $8, %esp
    movl %eax, -40(%ebp)
  # void %10 = printInt(%9)
    movl -40(%ebp), %eax
    pushl %eax
    call printInt
    add $4, %esp
    movl %eax, -44(%ebp)
  # obj:1 %11 = new_obj
    pushl $4
    call malloc
    add $4, %esp
    movl $1, (%eax)
    movl %eax, -48(%ebp)
  # obj:1 %12 = __cstr_1_smart(%11,1)
    movl $1, %eax
    pushl %eax
    movl -48(%ebp), %eax
    pushl %eax
    call __cstr_1_smart
    add $8, %esp
    movl %eax, -52(%ebp)
  # classID %13 = classID %12
    movl -52(%ebp), %eax
    movl (%eax), %eax
    movl %eax, -56(%ebp)
  # int32 %14 = %13:f(%12,1)
    movl $1, %eax
    pushl %eax
    movl -52(%ebp), %eax
    pushl %eax
    movl -56(%ebp), %eax
    pushl %eax
    call __f_dispatch
    add $8, %esp
    movl %eax, -60(%ebp)
  # void %15 = printInt(%14)
    movl -60(%ebp), %eax
    pushl %eax
    call printInt
    add $4, %esp
    movl %eax, -64(%ebp)
  # obj:1 %16 = new_obj
    pushl $4
    call malloc
    add $4, %esp
    movl $1, (%eax)
    movl %eax, -68(%ebp)
  # obj:1 %17 = __cstr_1_smart(%16,2)
    movl $2, %eax
    pushl %eax
    movl -68(%ebp), %eax
    pushl %eax
    call __cstr_1_smart
    add $8, %esp
    movl %eax, -72(%ebp)
  # classID %18 = classID %17
    movl -72(%ebp), %eax
    movl (%eax), %eax
    movl %eax, -76(%ebp)
  # int32 %19 = %18:f(%17,2)
    movl $2, %eax
    pushl %eax
    movl -72(%ebp), %eax
    pushl %eax
    movl -76(%ebp), %eax
    pushl %eax
    call __f_dispatch
    add $8, %esp
    movl %eax, -80(%ebp)
  # void %20 = printInt(%19)
    movl -80(%ebp), %eax
    pushl %eax
    call printInt
    add $4, %esp
    movl %eax, -84(%ebp)
  # obj:1 %21 = new_obj
    pushl $4
    call malloc
    add $4, %esp
    movl $1, (%eax)
    movl %eax, -88(%ebp)
  # obj:1 %22 = __cstr_1_smart(%21,3)
    movl $3, %eax
    pushl %eax
    movl -88(%ebp), %eax
    pushl %eax
    call __cstr_1_smart
    add $8, %esp
    movl %eax, -92(%ebp)
  # classID %23 = classID %22
    movl -92(%ebp), %eax
    movl (%eax), %eax
    movl %eax, -96(%ebp)
  # int32 %24 = %23:f(%22,3)
    movl $3, %eax
    pushl %eax
    movl -92(%ebp), %eax
    pushl %eax
    movl -96(%ebp), %eax
    pushl %eax
    call __f_dispatch
    add $8, %esp
    movl %eax, -100(%ebp)
  # void %25 = printInt(%24)
    movl -100(%ebp), %eax
    pushl %eax
    call printInt
    add $4, %esp
    movl %eax, -104(%ebp)
  # obj:1 %26 = new_obj
    pushl $4
    call malloc
    add $4, %esp
    movl $1, (%eax)
    movl %eax, -108(%ebp)
  # obj:1 %27 = __cstr_1_smart(%26,4)
    movl $4, %eax
    pushl %eax
    movl -108(%ebp), %eax
    pushl %eax
    call __cstr_1_smart
    add $8, %esp
    movl %eax, -112(%ebp)
  # classID %28 = classID %27
    movl -112(%ebp), %eax
    movl (%eax), %eax
    movl %eax, -116(%ebp)
  # int32 %29 = %28:f(%27,4)
    movl $4, %eax
    pushl %eax
    movl -112(%ebp), %eax
    pushl %eax
    movl -116(%ebp), %eax
    pushl %eax
    call __f_dispatch
    add $8, %esp
    movl %eax, -120(%ebp)
  # void %30 = printInt(%29)
    movl -120(%ebp), %eax
    pushl %eax
    call printInt
    add $4, %esp
    movl %eax, -124(%ebp)
  # RET 0
    xor %eax, %eax
    leave
    ret
__f_vtable:
    .long __virtual_method_fail
    .long __mth_1_f
    .long __mth_2_f
    .long __mth_3_f
    .long __mth_1_f
__f_dispatch:
    popl %edx
    popl %ecx
    pushl %edx
    pushl %ebp
    movl %esp, %ebp
    lea __f_vtable, %edx
    movl 0(%edx,%ecx,4), %edx
    jmp *%edx
