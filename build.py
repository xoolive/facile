import os
import shutil
import sys
import sysconfig
import time
from distutils.command import build_ext
from distutils.core import Distribution, Extension
from typing import List, Tuple

from Cython.Build import cythonize


def ocaml_config(build_path=None) -> Tuple[List[str], List[str]]:

    include_dirs: List[str] = []
    extra_link_args: List[str] = []

    if build_path is None:
        build_path = "build/temp.{}-{}".format(
            sysconfig.get_platform(),
            sysconfig.get_config_var("py_version_short"),
        )

    if not os.path.exists("build"):
        os.mkdir("build")
    if not os.path.exists(build_path):
        os.mkdir(build_path)

    ext_obj = "o"
    if sysconfig.get_platform().startswith("win"):
        ext_obj = "obj"

    mlobject = f"{build_path}/interface_ml.{ext_obj}"
    extra_link_args.append(mlobject)

    ocamlpath = os.popen("opam exec -- ocamlopt -where").readline().strip()
    if ocamlpath == "":
        raise SystemError("ocamlopt not found")
    include_dirs.append(ocamlpath)

    static_obj = "a"
    if sysconfig.get_platform().startswith("win"):
        static_obj = "lib"

    extra_link_args.append(f"{ocamlpath}/libasmrun.{static_obj}")
    if sysconfig.get_platform().startswith("win"):
        extra_link_args.append(f"{ocamlpath}/flexdll/flexdll_msvc64.obj")
        extra_link_args.append(f"{ocamlpath}/flexdll/flexdll_initer_msvc64.obj")

    # Rely on ocamlfind to find facile, but you can add some hints if need be
    # facilepath = os.popen("opam exec -- ocamlfind query facile").readline()
    # facilepath = facilepath.strip()

    # Check timestamps for OCaml file
    exists = os.path.exists(mlobject)
    ml_mtime = os.path.getmtime("interface/interface.ml")
    obj_mtime = os.path.getmtime(mlobject) if exists else 0
    if not exists or ml_mtime > obj_mtime:
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

    return include_dirs, extra_link_args


def build():
    include_dirs, extra_link_args = ocaml_config()

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
        compileargs += " -std=c99"

    extensions = [
        Extension(
            "facile.core",
            ["facile/core.pyx", "interface/interface_c.c"],
            language="c",
            include_dirs=include_dirs,
            extra_compile_args=compileargs.split(),
            extra_link_args=extra_link_args,
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
