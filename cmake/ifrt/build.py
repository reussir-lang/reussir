#!/usr/bin/env python3
"""Build upstream IFRT IR and serialization support with CMake's LLVM."""

import argparse
import filecmp
import json
import os
from pathlib import Path
import shlex
import shutil
import stat
import subprocess


def write(path, content):
    path.parent.mkdir(parents=True, exist_ok=True)
    if not path.exists() or path.read_text() != content:
        path.write_text(content)


def copy(source, destination):
    destination = Path(destination)
    destination.parent.mkdir(parents=True, exist_ok=True)
    if not destination.exists() or not filecmp.cmp(source, destination, shallow=False):
        if destination.exists():
            destination.chmod(destination.stat().st_mode | stat.S_IWUSR)
        shutil.copy2(source, destination)
    return str(destination)


def link(source, destination):
    destination.parent.mkdir(parents=True, exist_ok=True)
    if destination.is_symlink():
        if destination.readlink() == source:
            return
        destination.unlink()
    destination.symlink_to(source, target_is_directory=True)


def compiler_launcher(work, compiler, launcher):
    if not launcher:
        return compiler
    # Bazel's local C++ toolchain accepts a compiler path, not CMake's launcher
    # list. Resolve the launcher now: compile actions have a restricted PATH.
    executable = shutil.which(launcher[0])
    if executable is None:
        raise RuntimeError("compiler launcher not found: " + launcher[0])
    wrapper = work / (compiler.name + "-launcher")
    write(wrapper, '#!/bin/sh\nexec ' + shlex.join(
        [executable, *launcher[1:], str(compiler)]) + ' "$@"\n')
    wrapper.chmod(0o755)
    return wrapper


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ("work", "xla", "stablehlo", "stablehlo-build", "tblgen",
                 "tblgen-rule", "bazel", "cc", "cxx", "output", "serdes-output"):
        parser.add_argument("--" + name, required=True, type=Path)
    for name in ("llvm-include", "mlir-include"):
        parser.add_argument("--" + name, required=True, nargs="+", type=Path)
    parser.add_argument("--jobs", required=True, type=int)
    parser.add_argument("--host-library", required=True, nargs="+", type=Path)
    parser.add_argument("--host-library-dir", nargs="*", type=Path, default=[])
    parser.add_argument("--cxx-flags", default="")
    parser.add_argument("--compiler-launcher", nargs="*", default=[])
    args = parser.parse_args()
    adapter = Path(__file__).resolve().parent
    workspace = args.work / "workspace"

    # An isolated copy permits build overlays without touching source overrides.
    # copy() preserves unchanged files so Bazel's incremental checks stay cheap.
    overlays = {
        "xla/python/ifrt/ir/BUILD",
        "xla/python/ifrt/ir/transforms/passes.td",
        "xla/python/ifrt/ir/ifrt_ops.td", "third_party/extensions/llvm.bzl",
        "third_party/shardy/temporary.patch",
    }

    def ignore(directory, names):
        relative = Path(directory).relative_to(args.xla)
        return [name for name in names if name == ".git" or name.startswith("bazel-")
                or str(relative / name) in overlays]

    shutil.copytree(args.xla, workspace, dirs_exist_ok=True,
                    copy_function=copy, ignore=ignore)
    package = Path("xla/python/ifrt/ir")
    write(workspace / package / "BUILD",
          (args.xla / package / "BUILD").read_text() + "\n" +
          (adapter / "ir.BUILD").read_text())
    copy(adapter / "Register.cpp", workspace / "xla/python/ifrt/ir/reussir_registration.cc")
    driver = (args.xla / package / "tests/ifrt-translate.cc").read_text()
    entry = "int main(int argc, char** argv)"
    if driver.count(entry) != 1:
        raise RuntimeError("IFRT translate entry point changed; update the adapter")
    # Outlining clones referenced Shardy meshes into atom modules. Upstream's
    # serializer supports them, but its test driver's input registry omits SDY.
    registry = "registry.insert<IfrtDialect, VifrtDialect, mlir::func::FuncDialect>();"
    if driver.count(registry) != 1:
        raise RuntimeError("IFRT translate registry changed; update the adapter")
    driver = driver.replace(registry,
        "registry.insert<IfrtDialect, VifrtDialect, mlir::func::FuncDialect, "
        "mlir::sdy::SdyDialect>();")
    write(workspace / package / "reussir_translate.cc",
          driver.replace(entry, "int reussirIfrtTranslateMain(int argc, char** argv)"))

    # VIFRT-only input has not loaded FuncDialect through parsing. The inverse
    # conversion creates func.func/call/return and must declare that dependency.
    passes_td = Path("xla/python/ifrt/ir/transforms/passes.td")
    write(workspace / passes_td, (args.xla / passes_td).read_text().replace(
        'let dependentDialects = ["xla::ifrt::IfrtDialect"];',
        'let dependentDialects = ["xla::ifrt::IfrtDialect", "mlir::func::FuncDialect"];',
    ))

    # LLVM 23's Symbol interface already provides name and visibility. Newer
    # MLIR splits these traits out; select the equivalent spelling by feature.
    symbol_traits = "".join((p / "mlir/IR/SymbolInterfaces.td").read_text()
                            for p in args.mlir_include
                            if (p / "mlir/IR/SymbolInterfaces.td").exists())
    td = Path("xla/python/ifrt/ir/ifrt_ops.td")
    definitions = (args.xla / td).read_text()
    shardy_patch = Path("third_party/shardy/temporary.patch")
    shardy_compat = (args.xla / shardy_patch).read_text()
    if "def SymbolName " not in symbol_traits:
        definitions = definitions.replace("[SymbolName, SymbolVisibility, Symbol]", "[Symbol]")
        shardy_compat += "\n" + (adapter / "shardy-llvm23.patch").read_text()
    write(workspace / td, definitions)
    write(workspace / shardy_patch, shardy_compat)

    llvm = args.work / "llvm"
    write(llvm / "MODULE.bazel", 'module(name = "llvm-project")\n')
    for kind, includes in (("llvm", args.llvm_include), ("mlir", args.mlir_include)):
        names = []
        for i, include in enumerate(dict.fromkeys(includes)):
            names.append("include" + str(i))
            link(include, llvm / kind / names[-1])
        build = (adapter / (kind + ".BUILD")).read_text()
        write(llvm / kind / "BUILD.bazel", build.replace("@INCLUDES@", json.dumps(names)))
    copy(args.tblgen_rule, llvm / "mlir/tblgen.bzl")
    wrapper = llvm / "mlir/mlir-tblgen.sh"
    write(wrapper, '#!/bin/sh\nexec ' + shlex.quote(str(args.tblgen)) + ' "$@"\n')
    wrapper.chmod(0o755)

    stablehlo = args.work / "stablehlo"
    write(stablehlo / "MODULE.bazel", 'module(name = "stablehlo")\n')
    copy(adapter / "stablehlo.BUILD", stablehlo / "BUILD.bazel")
    link(args.stablehlo, stablehlo / "src")
    link(args.stablehlo_build, stablehlo / "gen")

    # Keep the extension's repository identity, but avoid fetching/configuring
    # upstream LLVM just to override it with installed headers afterward.
    write(workspace / "third_party/extensions/llvm.bzl", '''
load("@bazel_tools//tools/build_defs/repo:local.bzl", "local_repository")
def _impl(ctx):
    local_repository(name = "llvm-project", path = %s)
llvm_extension = module_extension(implementation = _impl)
''' % json.dumps(str(llvm)))

    env = dict(os.environ,
               CC=str(compiler_launcher(args.work, args.cc, args.compiler_launcher)),
               CXX=str(compiler_launcher(args.work, args.cxx, args.compiler_launcher)))
    # CI starts the S3-backed sccache server before the build. Forward its
    # connection/configuration to target and host actions by name, not value.
    # Credentials remain with the already-running server.
    cache_env = [name for name in (
        "SCCACHE_CONF", "SCCACHE_SERVER_PORT", "SCCACHE_SERVER_UDS",
        "SCCACHE_IDLE_TIMEOUT", "SCCACHE_LOG", "SCCACHE_ERROR_LOG",
    ) if name in env]
    subprocess.run([
        str(args.bazel), "--output_user_root=" + str(args.work / "cache"),
        "--max_idle_secs=60", "build", "--config=clang_local",
        *["--action_env=" + name for name in cache_env],
        *["--host_action_env=" + name for name in cache_env],
        "--override_repository=+third_party_ext+stablehlo=" + str(stablehlo),
        *["--cxxopt=" + flag for flag in shlex.split(args.cxx_flags)
          if not flag.startswith("-Werror")],
        # Upstream XLA currently has an invalid std::to_underlying alias in its
        # C++23 branch. Its C++20 path works with the same compiler and C++ ABI.
        "--cxxopt=-std=c++20", "--host_cxxopt=-std=c++20",
        "--copt=-Wno-error", "--cxxopt=-Wno-error",
        # XLA's HLO exporter builds a host TableGen helper. It must use the
        # installed compiler libraries too, rather than a second LLVM build.
        *["--host_linkopt=" + str(lib) for lib in args.host_library],
        *["--host_linkopt=-Wl,-rpath," + str(directory)
          for directory in dict.fromkeys(
              [lib.parent for lib in args.host_library] + args.host_library_dir)],
        "--jobs=" + str(args.jobs), "//xla/python/ifrt/ir:reussir_ifrt",
        "//xla/python/ifrt/ir:reussir_serdes_archive",
    ], cwd=workspace, env=env, check=True)
    copy(workspace / "bazel-bin/xla/python/ifrt/ir/libreussir_ifrt.a", args.output)
    # Preserve upstream's alwayslink requirement without whole-archiving the
    # entire support closure. This TU registers the standard IFRT serializers.
    copy(workspace / "bazel-bin/xla/python/ifrt/ir/libifrt_ir_program_serdes.lo",
         args.serdes_output)


if __name__ == "__main__":
    main()
