all: build

build:
	@dune build
	@ln -s _build/default/src/facile.cma facile.cma
	@ln -s _build/default/src/facile.cmxa facile.cmxa
	@ln -s _build/default/src/facile.a facile.a

clean:
	@dune clean
	@rm -f facile.cma facile.cmxa facile.a

install:
	@dune install

uninstall:
	@dune uninstall

.PHONY: all clean install uninstall
