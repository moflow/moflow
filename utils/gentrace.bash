#!/bin/bash

BAPDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && cd .. && pwd )"
PINDIR="$BAPDIR/pin"
PIN="$PINDIR/pin"

TOOL32="$BAPDIR/pintraces/obj-ia32/gentrace.so"
TOOL64="$BAPDIR/pintraces/obj-intel64/gentrace.so"

if [ $# -eq 0 ]
then
    echo "Usage: $0 <arguments to PIN tool>"
    exit 1
fi


if [ ! -f $TOOL32 ]
then
    echo The BAP pintool does not seem to be built.
    echo See the BAP user manual.
    exit 1
fi

if [ ! -f $BAPDIR/pin/pin ]
then
    echo PIN does not appear to be installed in $PINDIR
    exit 1
fi

# Try to run 32-bit PIN

$PIN -t $TOOL32 "$@"
SUCCESS=$?

if [ $SUCCESS -ne 0 -a -f $TOOL64 ]
then
    # Try 64-bit
    $PIN -t $TOOL64 "$@"
    SUCCESS=$?
fi

exit $SUCCESS