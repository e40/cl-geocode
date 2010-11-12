
SOURCE_FILES = Makefile cl-geocode.cl readme.txt cl-geocode.asd

default: all

include ../makefile.include

all: clean cl-geocode.fasl
	cat package.fasl geocode.fasl > cl-geocode.fasl

install: FORCE
	-mv -f /usr/local/lisp/code/cl-geo.fasl \
	       /usr/local/lisp/code/cl-geo.fasl.old
	cp -p cl-geocode.fasl /usr/local/lisp/code/

clean: FORCE
	rm -f *.fasl

FORCE:
