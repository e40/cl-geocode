# $Id$

SOURCE_FILES = Makefile measures.cl readme.txt

include ../makefile.include

all: measures.fasl

install: FORCE
	-mv -f /usr/local/lisp/code/measures.fasl \
	       /usr/local/lisp/code/measures.fasl.old
	cp -p measures.fasl /usr/local/lisp/code/

clean: FORCE
	rm -f *.fasl

FORCE:
