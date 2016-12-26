#!/bin/bash

# bap-0.8 build script for Ubuntu 14.04 x64

apt-get update && apt-get install -y \
  git build-essential vim autotools-dev automake \
  camlidl binutils-dev automake libcamomile-ocaml-dev otags libpcre3-dev \
  camlp4-extra bison flex zlib1g-dev libgmp3-dev g++ libtool make \
  gcc-multilib g++-multilib lib32z1-dev curl unzip wget \
  ocaml-nox ocaml-native-compilers ocaml-findlib \
  libgmp10 libgmp-dev libiberty-dev nasm exuberant-ctags

# hack for z3
ln -s /usr/lib/x86_64-linux-gnu/libgmp.so.10 /usr/lib/x86_64-linux-gnu/libgmp.so.3

./autogen.sh && \
cd solvers && ./getz3.sh && cd .. && \
cd ocamlgraph && ./configure && make && make install-findlib && cd .. && \
cd pintraces && ./getpin.sh && cd .. && \
./configure --with-z3=`pwd`/solvers/z3 && make -i 
