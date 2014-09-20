Python wrapping for OCaml Facile library
========================================

OCaml Facile library is an excellent tool for solving constraint programming problems in OCaml. Facile stands for "Functional Constraint Library". Besides, "facile" means "easy" in French!

This code is a Cython-based wrapping in Python of basic functionalities of this library. My approach has been pragmatic: I only wrapped what I needed.

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

- Improve `setup.py` for managing build dependencies
- Clean a messy `value` becomes `value*` thing between the `.h` and the `.pxd` because of Cython mess with the `__getval()` function returning `value*` type objects.
- Study the feasibility of building your own constraints in pure Python.

