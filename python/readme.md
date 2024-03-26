# Python constraint programming library

OCaml Facile library is an excellent tool for solving constraint programming problems in OCaml. Facile stands for "Functional Constraint Library". Besides, "facile" means "easy" in French!

This code is a Cython-based wrapping in Python of basic functionalities of this library.

The documentation (installation, quickstart and examples) has been moved to [readthedocs.org](http://facile.readthedocs.io/).

## Things left to do

- Find a way to better parametrize the resolution process (heuristics, variable choice order, etc.)
- Study the feasibility of building your own constraints in pure Python.

## Compilation

- `opam switch create 5.1.1`
- `opam install dune`
- in `ocaml/facile`: `dune build; dune install`
- in `python/binding`: `dune build`
- in `python`: `poetry install`
- check: `pytest`
- check few examples: `for i in examples; do python $i; done`
- finally: `poetry build`
