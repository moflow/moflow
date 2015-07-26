FIGS = $(wildcard *.fig)

all: $(FIGS:.fig=.pdf) add-shl.pdf

%.pdf : %.dot
	dot -Tpdf -o $@ $<

%.pdf : %.fig
	fig2dev -L pdf $< $@

clean:
	rm -f $(FIGS:.fig=.pdf)