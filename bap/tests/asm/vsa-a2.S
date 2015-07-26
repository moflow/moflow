mov $30, %ecx

loop:

dec %ecx
cmp $20, %ecx
ja loop

done:
hlt
