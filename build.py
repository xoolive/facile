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

    mlobject = f"{bpath}/interface_ml.o"

    ocamlpath = os.popen("opam exec -- ocamlopt -where").readline().strip()
    if ocamlpath == "":
        raise SystemError("opam exec -- ocamlopt not found")

    asmrunlib = ocamlpath + "/libasmrun.a"

    # Rely on ocamlfind to find facile, but you can add some hints if need be
    facilepath = os.popen("opam exec -- ocamlfind query facile").readline()
    facilepath = facilepath.strip()

    # Check timestamps for OCaml file
    exists = not os.path.exists(mlobject)
    if exists or os.path.getmtime("interface/interface.ml") > os.path.getmtime(
        mlobject
    ):
        print("Compiling interface.ml")
        cmd = (
            "opam exec -- ocamlfind ocamlopt -runtime-variant _pic"
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
    compileargs = ["-fPIC"]
    INCLUDE = [ocamlpath]

    try:
        os.environ["CFLAGS"]
    except KeyError:
        os.environ["CFLAGS"] = ""

    try:
        compiler = sysconfig.get_config_vars()["CC"]
        compiler = os.environ["CC"]
    except Exception:
        compiler = ""

    # Flag for array
    os.environ["CFLAGS"] += " -Wno-unused-function"
    # Mute the ugly trick for value/value*
    os.environ["CFLAGS"] += " -Wno-int-conversion"
    os.environ["CFLAGS"] += " -Wno-incompatible-pointer-types"
    # assignment discards 'const' qualifier from pointer target type
    os.environ["CFLAGS"] += " -Wno-discarded-qualifiers"

    # Compiler specific
    if compiler == "clang":
        # Other warning on a Python flag (not my fault...)
        os.environ["CFLAGS"] += " -Wno-unknown-warning-option"

    # gcc
    if "gcc" in compiler:
        os.environ["CFLAGS"] += " -Wno-strict-prototypes"

    extensions = [
        Extension(
            "facile.core",
            ["facile/core.pyx", "interface/interface_c.c"],
            language="c",
            include_dirs=INCLUDE,
            extra_compile_args=compileargs,
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
