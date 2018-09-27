SOLVER_PPFLAGS=-D WITH_Z3_BINDINGS
SOLVER_CLIBS = z3stubs z3 rt
SOLVER_SOURCES = /home/ubuntu/moflow/bap/solvers/z3/ocaml/z3.ml
SOLVER_LIBDIRS = /home/ubuntu/moflow/bap/solvers/z3/lib
SOLVER_LDFLAGS = -fopenmp -Wl,-rpath=/home/ubuntu/moflow/bap/solvers/z3/lib
