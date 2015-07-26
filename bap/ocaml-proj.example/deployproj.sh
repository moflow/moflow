#!/bin/bash
# Deploy sample project specific directories to the directory
# containing BAP

set -e

BAPDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && cd ".." && pwd )"
TARGETDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && cd "../.." && pwd )"
SCRIPT="$( basename "${BASH_SOURCE[0]}" )"

function copy {
    cp -r $BAPDIR/$1.example $TARGETDIR/$1
}

copy ocaml-proj
copy utils-proj
copy tests-proj

# Remove script in ocaml-proj to avoid confusion!

rm $TARGETDIR/ocaml-proj/$SCRIPT

echo "Project directories deployed to $TARGETDIR"
