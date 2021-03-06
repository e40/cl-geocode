
default: all

MLISP_ARGS = -qq -batch -W -e '(require :asdf)' 

%.fasl : %.asd
	mlisp $(MLISP_ARGS) -e '(asdf:compile-system :$*)' -kill

all: clean cl-geocode.fasl test
	cat package.fasl geocode.fasl > cl-geocode.fasl

test: FORCE
	mlisp $(MLISP_ARGS) -L test.cl -kill

install: FORCE
	-mv -f /usr/local/lisp/code/cl-geocode.fasl \
	       /usr/local/lisp/code/cl-geocode.fasl.old
	cp -p cl-geocode.fasl /usr/local/lisp/code/

clean: FORCE
	rm -f *.fasl

FORCE:
