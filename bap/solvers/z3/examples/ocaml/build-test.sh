#!/bin/bash

OCAMLLIB=$1
 
ocamlopt -dtypes -o test_mlapi -ccopt "-I../../ocaml -L../../bin -L../../lib -fopenmp" -I ../../ocaml/ -cclib -lz3 -cclib -lz3stubs ${OCAMLLIB}/libcamlidl.a z3.cmxa test_mlapi.ml
