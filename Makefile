
default: all

MLISP_ARGS = -qq -batch -W -e '(require :asdf)' 

%.fasl : %.asd
ifdef SBCL
	sbcl --eval '(require :asdf)' --load cl-geocode.asd --eval '(asdf:compile-system :$*)' --eval '(quit)'
else
	mlisp $(MLISP_ARGS) -e '(asdf:compile-system :$*)' -kill
endif

all: clean cl-geocode.fasl test
	cat package.fasl geocode.fasl > cl-geocode.fasl

test: FORCE
ifdef SBCL
else
	mlisp $(MLISP_ARGS) -L test.cl -kill
endif

install: FORCE
	-mv -f /usr/local/lisp/code/cl-geocode.fasl \
	       /usr/local/lisp/code/cl-geocode.fasl.old
	cp -p cl-geocode.fasl /usr/local/lisp/code/

clean: FORCE
	rm -f *.fasl

FORCE:
