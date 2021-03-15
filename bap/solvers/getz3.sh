#!/bin/bash

tar xfvz  z3-x64-3.2.tar.gz
cd ./z3/ocaml && ./build-lib.sh $(ocamlfind query unix)

