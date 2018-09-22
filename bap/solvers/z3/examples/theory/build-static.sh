# Note 1: You must have GMP installed in your machine
# Note 2: Z3 was built using C++, so libz3.a has C++ dependencies. 
#         You can use gcc to link the program, but you need tell it 
#         to link the C++ libraries
g++ -fopenmp -static -I../../include -L../../lib test_user_theory.c -lz3 -lgmp -o test_user_theory
