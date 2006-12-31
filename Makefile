# $Id$

mlisp = mlisp

measures.fasl: measures.cl
	$(mlisp) -batch -C measures.cl -kill

clean: FORCE
	rm -f *.fasl

FORCE:
