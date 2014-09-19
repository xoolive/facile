from setuptools import setup
from setuptools.extension import Extension
from distutils.command import build_ext
from distutils.core import Command
from Cython.Build import cythonize

import os
import os.path

import sysconfig
import sys

# I know, it's bad! Feel free to improve...
bpath = "build/temp.%s-%s.%s" % (sysconfig.get_platform(),
                                 sys.version_info[0], sys.version_info[1])

if not os.path.exists("build"):
    os.mkdir("build")
if not os.path.exists(bpath):
    os.mkdir(bpath)

ocamlpath = os.popen("ocamlc -where").readline().strip()
if ocamlpath == "":
    print "ocamlc not found"
    raise SystemError

ocamlopt = os.popen("which ocamlopt").readline().strip()
if ocamlopt == "":
    print "ocamlc not found"
    raise SystemError

facilepath = os.popen("ocamlfind query facile").readline().strip()
if facilepath == "":
    print "ocamlfind or facile not found"
    raise SystemError

INCLUDE = [ocamlpath]

try:
    import numpy
    INCLUDE.append(numpy.get_include())
except ImportError:
    print "numpy is required"
    raise

extensions = [
    Extension("facile",
              ["facile.pyx", "interface_c.c"],
              language="c",
              include_dirs=INCLUDE,
              extra_compile_args=["-fPIC"],
              extra_link_args=["%s/interface_ml.o" % bpath,
                               ocamlpath+"/libasmrun.a"]
              )
]


# TODO add dependency to interface.ml for rebuilding facile.so
class ocamlbuild(build_ext.build_ext):
    def run(self):
        print "compiling interface.ml"
        os.system(("%s -output-obj -I %s -o %s/interface_ml.o %s/facile.cmxa" +
                  " interface.ml") %
                  (ocamlopt, facilepath, bpath, facilepath))
        build_ext.build_ext.run(self)


class mrclean(Command):
    description = "custom clean command for OCaml objects"
    user_options = []

    def initialize_options(self):
        self.cwd = None

    def finalize_options(self):
        self.cwd = os.getcwd()

    def run(self):
        assert os.getcwd() == self.cwd, 'Must be in: %s' % self.cwd
        os.system('rm -rf *.cm* *.o %s' % bpath)

cmdclass = {}
cmdclass['build_ext'] = ocamlbuild
cmdclass['clean'] = mrclean

setup(name="facile",
      version="1.0",
      author="Xavier Olive",
      author_email="xo.olive@gmail.com",
      description="Wrapping for OCaml Facile library into Python",
      ext_modules=cythonize(extensions),
      cmdclass=cmdclass
      )
