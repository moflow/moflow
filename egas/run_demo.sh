#!/bin/bash

PATH=../bap/pin:$PATH
( cd demo && gcc -o demo demo.c && rm -rf crashes samples )
./egas -app demo/demo -samples-dir demo/samples -crashes-dir demo/crashes -seed demo/input.txt -fmt "cdep %s" 
ls demo/crashes

