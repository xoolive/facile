Python wrapping for OCaml Facile library
========================================

OCaml Facile library is an excellent tool for solving constraint programming problems in OCaml. Facile stands for "Functional Constraint Library". Besides, "facile" means "easy" in French!

This code is a Cython-based wrapping in Python of basic functionalities of this library. My approach has been pragmatic: I have been wrapping elements as soon as the need arises.

#### Examples

In order of complexity, you can read the following example programs, (almost) all coming from the set of examples from the original Facile library:

- [`examples/basic.py`](examples/basic.py), basic example problems;
- [`examples/seven_eleven.py`](examples/seven_eleven.py), find four numbers such that their sum is 711 and their product is 711000000;
- [`examples/coins.py`](examples/coins.py), which coins do you need to give back change for any amount between 0 and maxval, using a given set of coins;
- [`examples/golomb.py`](examples/golomb.py), the Golomb ruler;
- [`examples/queens.py`](examples/queens.py), the n-queen problem;
- [`examples/magical.py`](examples/magical.py), a magical sequence where `a[i]` is equal to the number of `i` in `a`;
- [`examples/marriage.py`](examples/marriage.py), a problem of stable marriages;
- [`examples/buckets.py`](examples/buckets.py), a problem of filling buckets;
- [`examples/tiles.py`](examples/tiles.py), placing tiles on a board;
- [`examples/golf.py`](examples/golf.py), the golf tournament problem.

#### Installation

In order to compile the tool, I recommend the following method:

```sh
# Install opam, then have PIC-compiled version of OCaml:
opam switch 4.01.0+PIC # anterior version may work as well, +PIC is essential

# Follow opam instructions to configure the environment

# Install ocamlfind and facile library
opam install ocamlfind
opam install facile

# Build/install the Python library (recent version of setuptools needed)
python setup.py install

#  (optional) Prepare a package
python setup.py bdist_wheel
```

After installing the package, you can try basic problems in the `examples` directory.

I detailed my approach and some lessons learned [here](lessons.md)

#### Things left to do

- Study the feasibility of building your own constraints in pure Python.

