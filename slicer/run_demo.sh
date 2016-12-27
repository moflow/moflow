#!/bin/bash

echo "Tracing taint propagation in demo vuln binary with pintool.."
sleep 2
sudo ../bap/pin/pin -t ../tracer/gentrace32.so -taint_indices -taint_files input.txt -snapshot-file /tmp/demo.snapshot -o /tmp/demo.trace -- ./demo/demo tlv demo/input.txt

echo -e "\n\nLift trace and concretize BAP IL.."
sleep 2
../utils/iltrans -trace /tmp/demo.trace -trace-concrete-subst -trace-dsa -pp-ast /tmp/demo.trace.il

echo -e "\n\nDisplay instructions executed in main image.."
sleep 2
grep "addr 0x804" /tmp/demo.trace.il

echo -e "\n\nDisplay IL for executed instructions.."
sleep 2
grep -A17 "addr 0x804" /tmp/demo.trace.il 

echo -e "\n\nGetting slice for tainted memcpy size.."
sleep 2
SIZE_VAR=`tac /tmp/demo.trace.il | grep -B17 -m1 "esp+0x8" | grep dsa | head -1 | cut -d ':'  -f 1`
./slicer -il /tmp/demo.trace.il -var $SIZE_VAR
