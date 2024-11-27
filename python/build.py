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

    # mlobject = f"{build_path}/interface_ml.{ext_obj}"
    mlobject = "binding/_build/default/binding.exe.o"
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

    extra_link_args: list[str] = []
    mlobject = "binding/_build/default/binding.exe.o"

    if sys.platform == "linux":
        # Flag for array
        compileargs += " -Wno-unused-function"
        # Mute the ugly trick for value/value*
        compileargs += " -Wno-int-conversion"
        compileargs += " -Wno-incompatible-pointer-types"
        # assignment discards 'const' qualifier from pointer target type
        compileargs += " -Wno-discarded-qualifiers"
        compileargs += " -std=c99"
        extra_link_args.append(mlobject)
    elif sys.platform == "darwin":
        # Flag for array
        compileargs += " -Wno-unused-function"
        # Mute the ugly trick for value/value*
        compileargs += " -Wno-int-conversion"
        compileargs += " -Wno-incompatible-pointer-types"
        compileargs += " -Wno-unreachable-code-fallthrough"
        compileargs += " -std=c99"
        extra_link_args.append(mlobject)
    # elif sys.platform.startswith("win"):
    elif sysconfig.get_platform().startswith("win"):
        ocamlpath = os.popen("opam exec -- ocamlopt -where").readline().strip()
        if ocamlpath == "":
            raise SystemError("ocamlopt not found")
        extra_link_args.append(mlobject + "bj")  # .obj
        # libraries=["ws2_32", "version"],  # Link Windows libraries
        # extra_link_args.append(f"{ocamlpath}/flexdll/flexdll_msvc64.obj")
        # extra_link_args.append(f"{ocamlpath}/flexdll/flexdll_initer_msvc64.obj")

    extensions = [
        Extension(
            "facile.core",
            ["facile/core.pyx", "binding/facile.c"],
            language="c",
            include_dirs=[".", ocamlpath],
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

    class CustomBuildExt(build_ext.build_ext):
        def initialize(self):
            print(f"{sysconfig.get_platform()=}")
            if sysconfig.get_platform().startswith("win"):
                from setuptools._distutils._msvccompiler import _find_exe
                from setuptools._distutils.ccompiler import new_compiler

                # Get Python's include and library paths
                # include_dir = sysconfig.get_path("include")
                # plat_include_dir = sysconfig.get_path("platinclude")
                # library_dirs = [
                #     sysconfig.get_config_var("LIBDIR"),
                #     sysconfig.get_config_var("LIBPL"),
                # ]

                # # Add include directories
                # self.compiler.add_include_dir(include_dir)
                # if plat_include_dir:
                #     self.compiler.add_include_dir(plat_include_dir)

                # # Add library directories
                # for lib_dir in library_dirs:
                #     if lib_dir:
                #         self.compiler.add_library_dir(lib_dir)

                # Override the linker with flexlink.exe
                print(f"{self.compiler=}")
                self.compiler = new_compiler()
                print(f"{self.compiler=}")

                super().initialize()

                print(f"{self.compiler=}")
                print(f"{self.compiler.linker=}")

                self.compiler.linker = os.environ["LDSHARED"]

                print(f"{self.compiler=}")
                print(f"{self.compiler.linker=}")
            else:
                super().initialize()

    cmd = CustomBuildExt(distribution)
    cmd.verbose = True
    cmd.ensure_finalized()
    print(cmd.compiler)
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
