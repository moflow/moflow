#!/bin/bash

set -ex

(cd protobuf && ./autogen.sh)
aclocal -I m4
autoconf
autoheader
automake --add-missing --copy
