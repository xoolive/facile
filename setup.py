from setuptools import setup
from setuptools.extension import Extension
from distutils.core import Command
from Cython.Build import cythonize

import os
import os.path
import sys
import time
import sysconfig


def get_long_description():
    import codecs
    with codecs.open('readme.md', encoding='utf-8') as f:
        readme = f.read()
    try:
        from pypandoc import convert
        return convert(readme, 'rst', 'md')
    except ImportError:
        return ""


def ocaml_config(prefix="", bpath=None):

    if bpath is None:
        # I know, it's bad! Feel free to improve...
        bpath = "build/temp.%s-%s.%s" % (sysconfig.get_platform(),
                                         sys.version_info[0],
                                         sys.version_info[1])

    if not os.path.exists("build"):
        os.mkdir("build")
    if not os.path.exists(bpath):
        os.mkdir(bpath)

    mlobject = "%s/interface_ml.o" % bpath

    ocamlpath = os.popen("%socamlopt -where" % prefix).readline().strip()
    if ocamlpath == "":
        raise SystemError("%socamlopt not found" % prefix)

    asmrunlib = ocamlpath + "/libasmrun.a"

    # Rely on ocamlfind to find facile, but you can add some hints if need be
    facilepath = os.popen("%socamlfind query facile" % prefix).readline()
    facilepath = facilepath.strip()

    # Check timestamps for OCaml file
    exists = not os.path.exists(mlobject)
    if exists or os.path.getmtime("interface.ml") > os.path.getmtime(mlobject):
        print("Compiling interface.ml")
        cmd = ("%socamlfind ocamlopt -package facile -linkpkg -output-obj" +
               " -o %s interface.ml") % (prefix, mlobject)
        print(cmd)
        os.system(cmd)
        now = time.time()
        os.utime("facile.pyx", (now, now))

    return ocamlpath, mlobject, asmrunlib


if sys.platform != "win32":
    ocamlpath, mlobject, asmrunlib = ocaml_config()
    compileargs = ["-fPIC"]
    INCLUDE = [ocamlpath]
else:
    pass

try:
    os.environ['CFLAGS']
except KeyError:
    os.environ['CFLAGS'] = ""

try:
    compiler = sysconfig.get_config_vars()['CC']
    compiler = os.environ['CC']
except:
    pass

# Flag for array
os.environ['CFLAGS'] += " -Wno-unused-function"
# Mute the ugly trick for value/value*
os.environ['CFLAGS'] += " -Wno-int-conversion"
os.environ['CFLAGS'] += " -Wno-incompatible-pointer-types"

# Compiler specific
if compiler == "clang":
    # Other warning on a Python flag (not my fault...)
    os.environ['CFLAGS'] += " -Wno-unknown-warning-option"

# gcc
if "gcc" in compiler:
    os.environ['CFLAGS'] += " -Wno-strict-prototypes"

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
        os.system('rm -rf *.cm* *.o *.obj *.so %s' % mlobject)


class cross(Command):
    description = "Cross-compilation for Windows"
    user_options = []

    def initialize_options(self):
        self.cwd = None

    def finalize_options(self):
        self.cwd = os.getcwd()

    def run(self):
        self.tmp = "build/temp.win32-%s.%s" % (sys.version_info[:2])
        self.lib = "build/lib.win32-%s.%s" % (sys.version_info[:2])

        if not os.path.exists(self.tmp):
            os.mkdir(self.tmp)
        if not os.path.exists(self.lib):
            os.mkdir(self.lib)

        config = ocaml_config(prefix="i686-w64-mingw32-", bpath=self.tmp)
        ocamlpath, mlobject, asmrunlib = config

        cmd = ("i686-w64-mingw32-gcc %s -c facile.c -o %s/facile.o " +
               "-I cross-compile/win32-py%s.%s/include -I %s") % \
            (os.environ['CFLAGS'], self.tmp, sys.version_info[0],
             sys.version_info[1], ocamlpath)

        print(cmd)
        os.system(cmd)

        cmd = ("i686-w64-mingw32-gcc %s -c interface_c.c -o %s/interface_c.o" +
               " -I cross-compile/win32-py%s.%s/include -I %s") % \
            (os.environ['CFLAGS'], self.tmp, sys.version_info[0],
             sys.version_info[1], ocamlpath)

        print(cmd)
        os.system(cmd)

        pyd_file = "facile.pyd"
        if sys.version_info[:2] > (3, 4):
            pyd_file = "facile.cp%s%s-win32.pyd" % sys.version_info[:2]

        cmd = ("flexlink -maindll -chain mingw %s/facile.o %s/interface_c.o " +
               "%s/interface_ml.o -o %s/%s %s " +
               "cross-compile/win32-py%s.%s/libpython%s%s.a -- -static-libgcc")
        cmd = cmd % (self.tmp, self.tmp, self.tmp, self.lib, pyd_file,
                     asmrunlib, sys.version_info[0], sys.version_info[1],
                     sys.version_info[0], sys.version_info[1])

        print(cmd)
        os.system(cmd)


cmdclass = {}
cmdclass['clean'] = clean
cmdclass['cross'] = cross

setup(name="facile",
      version="1.4.1",
      author="Xavier Olive",
      author_email="xo.olive@gmail.com",
      description="Python constraint programming library",
      long_description=get_long_description(),
      license="LGPL 3.0",
      url="https://github.com/xoolive/facile",
      cmdclass=cmdclass,
      ext_modules=cythonize(extensions, language_level=3),
      test_suite="doctests",
      )
