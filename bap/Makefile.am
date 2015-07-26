# Work around buggy Debian ocaml patch
# See http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=678577
export OCAML_COMPAT='c'

AUTOMAKE_OPTIONS = subdir-objects

if HAVE_PIN
PINTRACES=pintraces
endif


BAPDIRS=libasmir ocaml utils tests $(PINTRACES)
SUBDIRS=batteries zarith objsize-0.16 ounit pcre-ocaml ocamlgraph libtracewrap $(BAPDIRS)

test: all
	$(MAKE) -C tests test

install:
	@echo "BAP does not support the install target"
	@false

.PHONY: cscope

cscope:
	cscope -b `find . -name "*.[ch]" -or -name "*.hh" -or -name "*.cc" -or -name "*.cpp"`

.PHONY: bap-clean
bap-clean:
	for d in $(BAPDIRS); do $(MAKE) -C $$d clean; done

if HAVE_ETAGS
all-local: tags
endif
