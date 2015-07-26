PDFS=basiccfg.pdf ssacfg.pdf ssaconvcfg.pdf ssasimpcfg.pdf ddg.pdf cdg.pdf test.il
DOTS=$(addsuffix .dot, $(basename $(PDFS)))
TARGETS=$(PDFS) test.il

.INTERMEDIATE: $(DOTS)

.PHONY: all
all: $(TARGETS)

.PHONY: clean
clean:
	$(RM) $(TARGETS)

%.pdf: %.dot
	dot -Tpdf $< -o $@

basiccfg.dot: basic.il
	../../utils/iltrans -il basic.il -to-cfg -prune-cfg -coalesce-ast -pp-ast-asms $@

ssacfg.dot: basic.il
	../../utils/iltrans -il basic.il -to-ssa -pp-ssa $@

ssaconvcfg.dot: basic.il
	../../utils/iltrans -il basic.il -to-ssa -to-cfg -pp-ast-cfg $@

ssasimpcfg.dot: basic.il
	../../utils/iltrans -il basic.il -to-ssa -simp-ssa -to-cfg -pp-ast-cfg $@

ddg.dot: basic.il
	../../utils/iltrans -il basic.il -to-ssa -pp-ssa-ddg $@

cdg.dot: basic.il
	../../utils/iltrans -il basic.il -to-ssa -pp-ssa-cdg $@

test.il: test
	../../utils/toil -bin $< -o $@

basic.o: basic.S
	$(CC) -m32 -c $< -o $@

basic.il: basic.o
	../../utils/toil -bin $< -o $@