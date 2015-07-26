ASFLAGS = -m32

VSA_OBJ = $(patsubst %.S,%.o,$(wildcard *.S))

all: nop $(VSA_OBJ)

nop: nop.S
	gcc -m32 -c -o nop nop.S
