
default: all

%.fasl : %.asd
	mlisp -qq -batch -L asdf-setup -W \
	      -e '(asdf:compile-system :$*)' \
	      -kill

all: clean cl-geocode.fasl
	cat package.fasl geocode.fasl > cl-geocode.fasl

install: FORCE
	-mv -f /usr/local/lisp/code/cl-geocode.fasl \
	       /usr/local/lisp/code/cl-geocode.fasl.old
	cp -p cl-geocode.fasl /usr/local/lisp/code/

clean: FORCE
	rm -f *.fasl

FORCE:
