#!/bin/bash

set -ex

aclocal
autoconf
autoheader
automake --add-missing --copy
(cd ocamlgraph && autoconf)
(cd libtracewrap/libtrace && ./autogen.sh)

