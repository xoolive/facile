Python wrapping for OCaml Facile library
========================================

OCaml Facile library is an excellent tool for solving constraint programming problems in OCaml. Facile stands for "Functional Constraint Library". Besides, "facile" means "easy" in French!

This code is a Cython-based wrapping in Python of basic functionalities of this library. My approach has been pragmatic: I only wrapped what I needed.

In order to compile the tool, I recommend the following method:

```sh
# Install opam, then have PIC-compiled version of OCaml:
opam switch 4.01.0+PIC # anterior version may work as well, +PIC is essential

# Follow opam instructions to configure the environment

# Install OCaml Facile library
opam install facile

# Build/install the Python library (recent version of setuptools needed)
python setup.py install

#  (optional) Prepare a package
python setup.py bdist_wheel
```

I detailed my approach and some lessons learned [here](lessons.md)
