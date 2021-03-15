#!/bin/bash

# bap-0.8 build script for Ubuntu 14.04 x64

sudo apt-get -y install ocaml-nox ocaml-native-compilers ocaml-findlib   camlidl binutils-dev automake libcamomile-ocaml-dev otags libpcre3-dev   camlp4-extra bison flex zlib1g-dev libgmp3-dev g++ libtool make gcc-multilib g++-multilib lib32z1-dev libiberty-dev 

sudo ln -s /usr/lib/x86_64-linux-gnu/libgmp.so.10 /usr/lib/x86_64-linux-gnu/libgmp.so.3

./autogen.sh && \
( cd solvers && ./getz3.sh ) && \
( cd ocamlgraph && ./configure && make -i -j && sudo make -i install-findlib ) && \
( cd pintraces && ./getpin.sh ) && \
./configure --with-z3=`pwd`/solvers/z3 && make -i -j
