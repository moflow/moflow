mov $0, %ecx

loop:

inc %ecx
cmp $20, %ecx
jb loop

done:
hlt
