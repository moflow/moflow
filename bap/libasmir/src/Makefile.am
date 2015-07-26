AUTOMAKE_OPTIONS = subdir-objects

RANLIB = @RANLIB@
lib_LIBRARIES = libasmir.a
AM_CFLAGS = -I../include -Itraces -fPIC -DGET_OPERANDS
AM_CXXFLAGS = $(AM_CFLAGS)


libasmir_a_SOURCES = \
	asm_program.c \
	symbols.c \
	ocaml_helpers.c \
	llvm.cpp

libasmir_a_LIBADD =  @BAP_DIR@/libtracewrap/libtrace/src/cpp/libtrace.a
