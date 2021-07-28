import os
import shutil
import sys
import sysconfig
import time
from distutils.command import build_ext
from distutils.core import Distribution, Extension

from Cython.Build import cythonize


def ocaml_config(bpath=None):

    if bpath is None:
        # I know, it's bad! Feel free to improve...
        bpath = "build/temp.%s-%s.%s" % (
            sysconfig.get_platform(),
            sys.version_info[0],
            sys.version_info[1],
        )

    if not os.path.exists("build"):
        os.mkdir("build")
    if not os.path.exists(bpath):
        os.mkdir(bpath)

    ext_obj = "o"
    if sysconfig.get_platform().startswith("win"):
        ext_obj = "obj"

    mlobject = f"{bpath}/interface_ml.{ext_obj}"

    ocamlpath = os.popen("opam exec -- ocamlopt -where").readline().strip()
    if ocamlpath == "":
        raise SystemError("opam exec -- ocamlopt not found")

    static_obj = "a"
    if sysconfig.get_platform().startswith("win"):
        static_obj = "lib"
    asmrunlib = f"{ocamlpath}/libasmrun.{static_obj}"

    # Rely on ocamlfind to find facile, but you can add some hints if need be
    # facilepath = os.popen("opam exec -- ocamlfind query facile").readline()
    # facilepath = facilepath.strip()

    # Check timestamps for OCaml file
    exists = not os.path.exists(mlobject)
    if exists or os.path.getmtime("interface/interface.ml") > os.path.getmtime(
        mlobject
    ):
        print("Compiling interface.ml")
        cmd = (
            "opam exec -- ocamlfind ocamlopt"  # " -runtime-variant _pic"
            " -package facile -linkpkg -output-obj -verbose"
            f" -o {mlobject} interface/interface.ml"
        )
        print(cmd)
        os.system(cmd)
        now = time.time()
        os.utime("facile/core.pyx", (now, now))

    return ocamlpath, mlobject, asmrunlib


def build():
    ocamlpath, mlobject, asmrunlib = ocaml_config()

    compiler = sysconfig.get_config_var("CC")
    compiler = "" if compiler is None else compiler
    compileargs = sysconfig.get_config_var("CFLAGS")
    compileargs = "" if compileargs is None else compileargs

    if sys.platform != "win32":
        # Flag for array
        compileargs += " -Wno-unused-function"
        # Mute the ugly trick for value/value*
        compileargs += " -Wno-int-conversion"
        compileargs += " -Wno-incompatible-pointer-types"
        # assignment discards 'const' qualifier from pointer target type
        compileargs += " -Wno-discarded-qualifiers"

    extensions = [
        Extension(
            "facile.core",
            ["facile/core.pyx", "interface/interface_c.c"],
            language="c",
            include_dirs=[ocamlpath],
            extra_compile_args=compileargs.split(),
            extra_link_args=[mlobject, asmrunlib],
        )
    ]
    ext_modules = cythonize(
        extensions,
        compiler_directives={"binding": True, "language_level": 3},
    )

    distribution = Distribution(
        {"name": "extended", "ext_modules": ext_modules}
    )
    distribution.package_dir = "extended"

    cmd = build_ext.build_ext(distribution)
    cmd.verbose = True
    cmd.ensure_finalized()
    cmd.run()

    # Copy built extensions back to the project
    for output in cmd.get_outputs():
        relative_extension = os.path.relpath(output, cmd.build_lib)
        shutil.copyfile(output, relative_extension)
        mode = os.stat(relative_extension).st_mode
        mode |= (mode & 0o444) >> 2
        os.chmod(relative_extension, mode)


if __name__ == "__main__":
    build()
