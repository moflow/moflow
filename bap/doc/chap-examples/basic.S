add %eax, %ebx
shl %cl, %ebx
jc target

elsewhere:
        inc %eax

target:
	inc %ebx

