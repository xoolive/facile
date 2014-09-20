### Lessons learned while coding Python wrapping for OCaml Facile library

My approach for wrapping OCaml Facile library has been pragmatic: write test cases according to my needs and wrap only the functionalities I need.

I won't develop here the interface choices I made for the wrapping: I just kept in mind that I should wrap as less as possible and rely on the specificities of Python language to build a pleasant experience.

#### Position independent code (`-fPIC`)

I mainly develop on two different environments: Ubuntu 64 bits at work and Mac OS at home. My OCaml environment on Ubuntu always came from apt repositories, and from brew on Mac OS. The package facile is shipped through `opam`, the OCaml equivalent for `pip`.

Things worked always earlier on my Mac until I realised the OCaml shipped through brew is compiled with `-fPIC` option, but not the ones in apt repositories. This is not a problem in most cases, but Cython build dynamic libraries and `-fPIC` becomes essential in 64 bits architectures.

#### OCaml Garbage collector (GC)

(many thanks to @bvaugon for his help)

Basically, I use Python objects to wrap OCaml `value` type objects. An OCaml value can be an integer or a pointer to an OCaml structure. I had in mind how Python GC works by counting references to objects. OCaml GC moves objects in addition, so when you keep a pointer to an OCaml structure in your Python object and OCaml GC messes things up... you get the picture!

The keywords are `caml_register_global_root(v)` and `caml_remove_global_root`. The idea is to allocate memory in C and to keep there a pointer to a `value` (which is a pointer to an OCaml structure itself). When the GC moves things, it will update the reference in the C memory as well.

#### `Makefile`, `scons`, `setuptools` etc.

How to build things up when you've got OCaml files (the callback registrations), C files (the interface) and Cython pyrex file to compile into a Python compiled dynamic library, linked with Python, OCaml, and Facile library?

I started compiling things manually with a `Makefile`. With `Cython`, when you want to keep things clean in your `$PYTHONPATH`, building a  `setup.py` is a must.

So I had the cythoning and the link edition in a `setup.py`, and the compilation of the interface in the `Makefile`, but things would get messy: two commands to type (`Makefile` and `setup.py`) and the link edition would not happen if `facile.pyx` had not been modified. I tried to play with `scons` but it is unnecessary complicated to build OCaml with it. I prefer [`ocaml-cmake`](https://github.com/ocaml-cmake/ocaml-cmake)!

I eventually imitated how Cython extends the `build_ext` command in order to compile OCaml directly in the `setup.py`. I kept it simple, so the link edition still won't happen if any of the `.c` or `.pyx` file didn't change.

#### Wrap up everything

I like wheels (`pip install wheel`) when packaging Python libraries. I didn't expect it to not be able to differentiate between different GLIBC versions. I have to install this `facile` wheel on Debian Stable (Wheezy) distributions which are still on an antediluvian version of the GLIBC: the installation fails. After compiling things on my own Debian Stable virtual machine, I found that even the `wheel` name is the same. Since `wheel` is not even installed on the machines I will have students try the library, I will just stick to `python-eggs` for this year.

