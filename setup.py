from setuptools import setup
from setuptools.extension import Extension
from distutils.command import build_ext
from distutils.core import Command
from Cython.Build import cythonize

import os
import os.path
import platform
import sysconfig
import sys

import time

# I know, it's bad! Feel free to improve...
bpath = "build/temp.%s-%s.%s" % (sysconfig.get_platform(),
                                 sys.version_info[0], sys.version_info[1])

if not os.path.exists("build"):
    os.mkdir("build")
if not os.path.exists(bpath):
    os.mkdir(bpath)

ocamlpath = os.popen("ocamlc -where").readline().strip()
if ocamlpath == "":
    print ("ocamlc not found")
    raise SystemError

ocamlopt = os.popen("which ocamlopt").readline().strip()
if ocamlopt == "":
    print ("ocamlopt not found")
    raise SystemError

# Rely on ocamlfind to find facile, but you can add some hints if need be
facilepath = os.popen("ocamlfind query facile").readline().strip()
if facilepath == "":
    print ("ocamlfind or facile not found")
    if os.path.exists(ocamlpath + "/facile.cma"):
        facilepath = ocamlpath
    else:
        raise SystemError

INCLUDE = [ocamlpath]

try:
    import numpy
    INCLUDE.append(numpy.get_include())
except ImportError:
    print ("numpy is required")
    raise

mlobject = "%s/interface_ml.o" % bpath
asmrunlib = ocamlpath + "/libasmrun.a"
compileargs = ["-fPIC"]

# Check timestamps for OCaml file
exists = not os.path.exists(mlobject)
if exists or os.path.getmtime("interface.ml") > os.path.getmtime(mlobject):
    print("Compiling interface.ml")
    os.system(("%s -output-obj -I %s -o %s %s/facile.cmxa interface.ml") %
              (ocamlopt, facilepath, mlobject, facilepath))
    now = time.time()
    os.utime("facile.pyx", (now, now))

extensions = [
    Extension("facile",
              ["facile.pyx", "interface_c.c"],
              language="c",
              include_dirs=INCLUDE,
              extra_compile_args=compileargs,
              extra_link_args=[mlobject, asmrunlib,]
              )
]

class mrclean(Command):
    description = "Custom clean command for OCaml objects"
    user_options = []

    def initialize_options(self):
        self.cwd = None

    def finalize_options(self):
        self.cwd = os.getcwd()

    def run(self):
        assert os.getcwd() == self.cwd, 'Must be in: %s' % self.cwd
        os.system('rm -rf *.cm* *.o *.obj %s' % mlobject)

cmdclass = {}
cmdclass['clean'] = mrclean

setup(name="facile",
      version="1.1",
      author="Xavier Olive",
      author_email="xo.olive@gmail.com",
      description="Wrapping for OCaml Facile library into Python",
      ext_modules=cythonize(extensions),
      cmdclass=cmdclass
      )
