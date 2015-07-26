movl $42, 12(%esp)
je foo
movl $8, 4(%esp)
jmp end
foo:
movl $12, 8(%esp)
end:

mov 8(%esp), %eax
mov 4(%esp), %ebx
mov 12(%esp), %ecx
