mov $30, %ecx

loop:

dec %ecx
cmp $20, %ecx
jae loop

done:
hlt
