from __future__ import annotations

import functools
import operator
import os
import shutil
import sys
import sysconfig

from Cython.Build import cythonize
from setuptools import Distribution, Extension
from setuptools.command import build_ext


def flexlink_library_dir_option(self, dir):
    # replace /LIB: by -L for flexlink
    return "-L" + dir


def flexlink_spawn(self, cmd):
    # even if the environment variable is set, it would fail if not passed here
    flexdir = "d:/a/facile/facile/_opam/lib/ocaml/flexdll/"
    env = dict(os.environ, PATH=self._paths, FLEXDIR=flexdir)
    with self._fallback_spawn(cmd, env) as fallback:
        from setuptools._distutils.ccompiler import CCompiler

        # super() doesn't work here
        return CCompiler.spawn(self, cmd, env=env)
    return fallback.value


def flexlink_link(
    self,
    target_desc,
    objects,
    output_filename,
    output_dir=None,
    libraries=None,
    library_dirs=None,
    runtime_library_dirs=None,
    export_symbols=None,
    debug=False,
    extra_preargs=None,
    extra_postargs=None,
    build_temp=None,
    target_lang=None,
):
    # Rewrite the link() method for flexlink
    from setuptools._distutils._log import log
    from setuptools._distutils._msvccompiler import gen_lib_options
    from setuptools._distutils.errors import DistutilsExecError, LinkError

    if not self.initialized:
        self.initialize()
    objects, output_dir = self._fix_object_args(objects, output_dir)
    fixed_args = self._fix_lib_args(
        libraries, library_dirs, runtime_library_dirs
    )
    libraries, library_dirs, runtime_library_dirs = fixed_args

    if runtime_library_dirs:
        self.warn(
            "I don't know what to do with 'runtime_library_dirs': "
            + str(runtime_library_dirs)
        )

    lib_opts = gen_lib_options(
        self, library_dirs, runtime_library_dirs, libraries
    )
    if output_dir is not None:
        output_filename = os.path.join(output_dir, output_filename)

    if self._need_link(objects, output_filename):
        ldflags = ["-v", "-maindll", "-chain", "msvc64"]

        export_opts = functools.reduce(
            operator.iadd,
            (["-export", sym] for sym in (export_symbols or [])),
            [],
        )

        ld_args = (
            ldflags + lib_opts + export_opts + objects + ["-o", output_filename]
        )

        if extra_preargs:
            ld_args[:0] = extra_preargs
        if extra_postargs:
            ld_args.extend(extra_postargs)

        output_dir = os.path.dirname(os.path.abspath(output_filename))
        self.mkpath(output_dir)
        try:
            log.debug('Executing "%s" %s', self.linker, " ".join(ld_args))
            self.spawn([self.linker, *ld_args])
        except DistutilsExecError as msg:
            raise LinkError(msg)
    else:
        log.debug("skipping %s (up-to-date)", output_filename)


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
    ext_obj = "o"
    if sysconfig.get_platform().startswith("win"):
        ext_obj = "obj"
    mlobject = f"binding/_build/default/binding.exe.{ext_obj}"
    extra_link_args.append(mlobject)

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
    # elif sys.platform.startswith("win"):
    elif sysconfig.get_platform().startswith("win"):
        ocamlpath = os.popen("opam exec -- ocamlopt -where").readline().strip()
        if ocamlpath == "":
            raise SystemError("ocamlopt not found")
        compileargs += (
            " /LD"  # include dll information
            " /wd4090"  # C4090: '=': different 'const' qualifiers
            " /wd4024"  # different types for formal and actual parameter
            " /wd4047"  # 'value *' differs in levels of indirection
        )
        # directly in the flexlink format
        extra_link_args += [
            "-lvcruntime",
            "-lversion",
            "-luuid",
            "-lkernel32",
            "-luser32",
            "-ladvapi32",
            "-lws2_32",
        ]

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
        def build_extensions(self):
            if sysconfig.get_platform().startswith("win"):
                flexlink_path = (
                    os.popen("opam exec -- where flexlink.exe")
                    .readline()
                    .strip()
                )

                self.compiler.initialize()

                self.compiler.linker = flexlink_path

                # with /GL (by default), the symbols are not found by flexlink
                self.compiler.compile_options = [
                    "/nologo",
                    "/O2",
                    "/W3",
                    "/Gy",  # replace GL with Gy
                    "/DNDEBUG",
                    "/MD",
                ]

                self.compiler.__class__.link = flexlink_link
                self.compiler.__class__.spawn = flexlink_spawn
                self.compiler.__class__.library_dir_option = (
                    flexlink_library_dir_option
                )

            super().build_extensions()

    cmd = CustomBuildExt(distribution)
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
