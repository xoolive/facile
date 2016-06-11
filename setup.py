from setuptools import setup
from setuptools.extension import Extension
from distutils.command import build_ext
from distutils.core import Command
from Cython.Build import cythonize

import os
import os.path
import platform
import sysconfig
import distutils
import sys

import time

def get_requirements():
    return [line.strip() for line in open("requirements.txt")]

def get_long_description():
    import codecs
    with codecs.open('readme.md', encoding='utf-8') as f:
        readme = f.read()
    try:
        from pypandoc import convert
        return convert(readme, 'rst', 'md')
    except ImportError:
        return ""

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

# First check if the environment variable is set
clang = False
try:
    if os.environ['CC'] == "clang":
        clang = True
except KeyError:
    pass

# I am not sure that is how setuptools find the proper compiler, but it sure
# looks like so
if clang or distutils.sysconfig_get_config_vars()['CC'] == 'clang':
    try:
        _ = os.environ['CFLAGS']
    except KeyError:
        os.environ['CFLAGS'] = ""
    # Flag for numpy
    os.environ['CFLAGS'] += " -Wno-unused-function"
    # Mute the ugly trick for value/value*
    os.environ['CFLAGS'] += " -Wno-int-conversion"
    os.environ['CFLAGS'] += " -Wno-incompatible-pointer-types"

extensions = [
    Extension("facile",
              ["facile.pyx", "interface_c.c"],
              language="c",
              include_dirs=INCLUDE,
              extra_compile_args=compileargs,
              extra_link_args=[mlobject, asmrunlib, ]
              )
]

class clean(Command):
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
cmdclass['clean'] = clean

setup(name="facile",
      version="1.3",
      author="Xavier Olive",
      author_email="xo.olive@gmail.com",
      description="Python constraint programming library",
      long_description=get_long_description(),
      license="LGPL 3.0",
      url="https://github.com/xoolive/facile",
      cmdclass=cmdclass,
      ext_modules=cythonize(extensions),
      install_requires=get_requirements(),
      test_suite="doctests",
      )
