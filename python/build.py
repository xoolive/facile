from __future__ import annotations

import os
import shutil
import sys
import sysconfig

from Cython.Build import cythonize
from setuptools import Distribution, Extension
from setuptools.command import build_ext


def ocaml_config(build_path: None | str = None) -> tuple[list[str], list[str]]:
    include_dirs: list[str] = []
    extra_link_args: list[str] = []

    if build_path is None:
        build_path = (
            build_ext.build_ext(Distribution())
            .get_finalized_command("build_ext")
            .build_temp
        )

    if not os.path.exists("build"):
        os.mkdir("build")
    if not os.path.exists(build_path):
        os.mkdir(build_path)

    ext_obj = "o"
    if sysconfig.get_platform().startswith("win"):
        ext_obj = "obj"

    mlobject = f"{build_path}/interface_ml.{ext_obj}"
    mlobject = "binding/binding.exe.o"
    extra_link_args.append(mlobject)

    ocamlpath = os.popen("opam exec -- ocamlopt -where").readline().strip()
    if ocamlpath == "":
        raise SystemError("ocamlopt not found")
    include_dirs.append(ocamlpath)

    static_obj = "a"
    if sysconfig.get_platform().startswith("win"):
        static_obj = "lib"

    # if sysconfig.get_platform().startswith("linux"):
    #     extra_link_args.append(f"{ocamlpath}/libasmrun_pic.{static_obj}")
    # else:
    #     extra_link_args.append(f"{ocamlpath}/libasmrun.{static_obj}")

    if sysconfig.get_platform().startswith("win"):
        extra_link_args.append(f"{ocamlpath}/flexdll/flexdll_msvc64.obj")
        extra_link_args.append(f"{ocamlpath}/flexdll/flexdll_initer_msvc64.obj")

    # Rely on ocamlfind to find facile, but you can add some hints if need be
    # facilepath = os.popen("opam exec -- ocamlfind query facile").readline()
    # facilepath = facilepath.strip()

    # Check timestamps for OCaml file
    # exists = os.path.exists(mlobject)
    # ml_mtime = os.path.getmtime("interface/interface.ml")
    # obj_mtime = os.path.getmtime(mlobject) if exists else 0
    # if not exists or ml_mtime > obj_mtime:
    #     print("Compiling interface.ml")
    #     cmd = (
    #         "opam exec -- ocamlfind ocamlopt"
    #         " -package facile -linkpkg -output-obj -verbose"
    #         f" -o {mlobject} interface/interface.ml"
    #     )
    #     os.system(cmd)
    #     now = time.time()
    #     os.utime("facile/core.pyx", (now, now))

    return include_dirs, extra_link_args


def build() -> None:
    # include_dirs, extra_link_args = ocaml_config()

    ocamlpath = os.popen("opam exec -- ocamlopt -where").readline().strip()
    if ocamlpath == "":
        raise SystemError("ocamlopt not found")

    # compiler = sysconfig.get_config_var("CC")
    # compiler = "" if compiler is None else compiler

    compileargs = sysconfig.get_config_var("CFLAGS")
    compileargs = "" if compileargs is None else compileargs

    if sys.platform == "linux":
        # Flag for array
        compileargs += " -Wno-unused-function"
        # Mute the ugly trick for value/value*
        compileargs += " -Wno-int-conversion"
        compileargs += " -Wno-incompatible-pointer-types"
        # assignment discards 'const' qualifier from pointer target type
        compileargs += " -Wno-discarded-qualifiers"
        compileargs += " -std=c99"
    elif sys.platform == "darwin":
        # Flag for array
        compileargs += " -Wno-unused-function"
        # Mute the ugly trick for value/value*
        compileargs += " -Wno-int-conversion"
        compileargs += " -Wno-incompatible-pointer-types"
        compileargs += " -Wno-unreachable-code-fallthrough"
        compileargs += " -std=c99"

    extensions = [
        Extension(
            "facile.core",
            ["facile/core.pyx", "binding/facile.c"],
            language="c",
            include_dirs=[".", ocamlpath],
            extra_compile_args=compileargs.split(),
            extra_link_args=["binding/binding.exe.o"],
        )
    ]

    ext_modules = cythonize(
        extensions,
        compiler_directives={"binding": True, "language_level": 3},
    )

    distribution = Distribution(
        {"name": "extended", "ext_modules": ext_modules}
    )

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
