FROM ubuntu:trusty
MAINTAINER Richard Johnson “rjohnson@moflow.org”

RUN apt-get update && apt-get install -y \
  git build-essential vim autotools-dev automake \
  camlidl binutils-dev automake libcamomile-ocaml-dev otags libpcre3-dev \
  camlp4-extra bison flex zlib1g-dev libgmp3-dev g++ libtool make \
  gcc-multilib g++-multilib lib32z1-dev curl unzip wget \
  ocaml-nox ocaml-native-compilers ocaml-findlib \
  libgmp10 libgmp-dev libiberty-dev nasm exuberant-ctags && \
  ln -s /usr/lib/x86_64-linux-gnu/libgmp.so.10 /usr/lib/x86_64-linux-gnu/libgmp.so.3

COPY . /moflow

WORKDIR /moflow
RUN ( cd bap && ./build_bap.sh ) && \
    ( cd bap/libtracewrap/libtrace/protobuf && make install ) && \
    mv /usr/local/lib/libproto* /usr/lib/x86_64-linux-gnu && \
    ( cd bap/libtracewrap/libtrace32/protobuf && make install ) && \
    ldconfig /usr/local/lib && \
    mkdir tracer && \
    cp bap/pintraces/obj-ia32/gentrace.so tracer/gentrace32.so && \
    cp bap/pintraces/obj-intel64/gentrace.so tracer/gentrace64.so && \
    mkdir utils && find bap/utils -perm /a+x -exec cp {} utils \; && \
    ( cd slicer && make ) && \
    ( cd egas && make ) && \
    ( cd bap && make clean )

CMD /bin/bash
