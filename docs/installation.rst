Installation
============

Facile is provided for Python 2.7, 3.4 and 3.5, for Linux, MacOS (≥ 10.10) and
Windows (no 2.7).

::

    pip install facile

The Linux version cannot be provided as wheel file: it has been compiled under a
Debian Stable (Wheezy) environment for an older version of glibc.

::

    easy_install facile

*If the automatic installation process does not work on your
environment, considering compiling your own version.*

Compilation
^^^^^^^^^^^

In order to compile the tool, I recommend the following method:

.. code:: sh

    # Install opam, then have PIC-compiled version of OCaml:
    opam switch 4.01.0+PIC # other versions may work as well

    # Follow opam instructions to configure the environment

    # Install ocamlfind and facile library
    opam install ocamlfind
    opam install facile

    # Build/install the Python library
    python setup.py install

    #  (optional) Prepare a package
    python setup.py bdist_wheel

After installing the package, you can try basic problems in the
``examples`` directory.

I detailed my approach and some lessons learned
`here <http://www.xoolive.org/2014/09/20/python-wrapping-for-ocaml-facile-library.html>`__

For building Windows packages, I use cross-compilation from Linux
or Mac (see the ``python setup.py cross`` command)

*Do not hesitate to share if you find a better compilation experience.*
