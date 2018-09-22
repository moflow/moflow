slicer
------

Slicer isolates the instructions in a trace related to a specific dataflow reference.  

```
% ./slicer -help 
Usage: ./slicer <options>

  -il <trace.il>
  -var <var to slice by>
  -f slice forward
  -b slice backward (default)
  -o output file (default: /dev/stdout)
  -help  Display this list of options
  --help  Display this list of options
```

Trace has to be concretized with -trace-concrete-subst. This option turns every
memory deref into a variable, so instead of `mem[addr]` (variable name + index),
we have: `mem_addr`. To get unique variable names, you can use the "-trace-dsa"
option in iltrans. Unique names make it easier to slice by variables set in
the middle of the trace -- there's no need to cut traces manually. 


Example
-------
```
./prep-slice.sh trace.bpt trace.il
./slicer -il trace.il -var symb_1 -f
```

Only data dependencies are captured exactly. Asserts (conditional jumps) are  
included iff their expressions contain variables influenced by (in case of   
forward slices), or influencing (backward slices) the slicing source/sink

***

Beware of pretty printing serialized traces to .il without any kind of 
processing:
```
iltrans -serializedtrace trace.bpt -pp-ast nice.il
iltrans -il nice.il -trace-concrete -pp-ast concrete.il
```
will not give the same result as:
```
iltrans -serializedtrace trace.bpt -trace-concrete -pp-ast concrete.il
```
This happens because contents of memories are not serialized/deserialized correctly.
