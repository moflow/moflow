#!/bin/bash

CMD=${1:-make -j test}

# Thorough cleaning
(git checkout . && git clean -f -x -d) || exit 125

# Always git clean when exiting, so git bisect can use checkout.
trap "(git checkout . && git clean -f -x -d) || exit 125" EXIT

# If PINPATH is set, copy pin into the directory.
if [ "x$PINPATH" != "x" ]
then
cp -r $PINPATH . || exit 125
fi

# Build bap
./autogen.sh && ./configure --without-llvm && make -j

# A build error is not considered 'bad'
if [ $? -ne 0 ]
then
    exit 125
fi

echo "Executing $CMD"
eval $CMD
RESULT=$?

exit $RESULT