#!/bin/bash

# bap-0.8 build script for Ubuntu 14.04 x64

./autogen.sh && \
( cd solvers && ./getz3.sh ) && \
( cd ocamlgraph && ./configure && make -i && make -i install-findlib ) && \
( cd pintraces && ./getpin.sh ) && \
./configure --with-z3=`pwd`/solvers/z3 && make -i 
