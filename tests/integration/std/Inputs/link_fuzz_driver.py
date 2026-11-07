#!/usr/bin/env python3
"""Link a Rust `fuzz_target!()` driver against a `rene`-built Reussir staticlib."""

import argparse
import json
import os
import subprocess
import sys
import time


def add_native_archive(cmd: list[str], archive: str, whole_archive: bool) -> None:
    parent = os.path.dirname(archive) or "."
    name = os.path.basename(archive)
    if sys.platform == "darwin":
        if whole_archive:
            cmd.append(f"-Clink-arg=-Wl,-force_load,{archive}")
        else:
            cmd.append(f"-Clink-arg={archive}")
        return
    cmd.extend(["-L", f"native={parent}"])
    mods = "static:+whole-archive,+verbatim" if whole_archive else "static:+verbatim"
    cmd.extend(["-l", f"{mods}={name}"])


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--rene", required=True)
    parser.add_argument("--manifest-path", required=True)
    parser.add_argument("--build-dir", required=True)
    parser.add_argument("--profile", default="release")
    parser.add_argument("--staticlib", required=True)
    parser.add_argument("--driver", required=True)
    parser.add_argument("--library-path", required=True)
    parser.add_argument("-o", "--output", required=True)
    args = parser.parse_args()

    inspect_cmd = [
        args.rene,
        "inspect",
        "--manifest-path",
        args.manifest_path,
        "--build-dir",
        args.build_dir,
        "--frozen",
        "--commands",
        "--profile",
        args.profile,
    ]
    inspect_out = ""
    for attempt in range(600):
        proc = subprocess.run(inspect_cmd, capture_output=True, text=True)
        if proc.returncode == 0:
            inspect_out = proc.stdout
            break
        if "build directory is in use" in proc.stderr and attempt < 599:
            time.sleep(0.05)
            continue
        sys.stderr.write(proc.stderr)
        return proc.returncode
    report = json.loads(inspect_out)
    plan = report["plan"]
    rustc = plan["env"]["REUSSIR_RUSTC"]

    polyffi_libdirs: list[str] = []
    link_libs: list[str] = []
    for node in plan["nodes"]:
        for cmd_entry in node.get("commands", []):
            argv = cmd_entry.get("argv", [])
            for i, tok in enumerate(argv[:-1]):
                if tok == "--polyffi-libdir" and argv[i + 1] not in polyffi_libdirs:
                    polyffi_libdirs.append(argv[i + 1])
                elif tok == "--link-lib" and argv[i + 1] not in link_libs:
                    link_libs.append(argv[i + 1])

    if not link_libs:
        for node in reversed(plan["nodes"][:-1]):
            artifacts = node.get("artifacts", {})
            archive = artifacts.get("archive")
            if archive and archive not in link_libs:
                link_libs.append(archive)

    cmd = [rustc, "--edition", "2024", args.driver, "-o", args.output]
    add_native_archive(cmd, args.staticlib, whole_archive=True)
    for lib in link_libs:
        add_native_archive(cmd, lib, whole_archive=False)

    for libdir in polyffi_libdirs:
        rcgmp = os.path.join(libdir, "libRCGMPRuntime.a")
        if os.path.isfile(rcgmp):
            add_native_archive(cmd, rcgmp, whole_archive=False)
            cmd.extend(["-l", "dylib=gmp"])
            break

    fuzzer_rlib = os.path.join(args.library_path, "libreussir_fuzzer.rlib")
    fuzzer_rmeta = os.path.join(args.library_path, "libreussir_fuzzer.rmeta")
    cmd.append(f"--extern=reussir_fuzzer={fuzzer_rlib}")
    if os.path.isfile(fuzzer_rmeta):
        cmd.append(f"--extern=reussir_fuzzer={fuzzer_rmeta}")
    cmd.extend(["-L", f"dependency={args.library_path}"])

    for libdir in polyffi_libdirs:
        rt_rlib = os.path.join(libdir, "libreussir_rt.rlib")
        rt_rmeta = os.path.join(libdir, "libreussir_rt.rmeta")
        if os.path.isfile(rt_rlib):
            cmd.append(f"--extern=reussir_rt={rt_rlib}")
            if os.path.isfile(rt_rmeta):
                cmd.append(f"--extern=reussir_rt={rt_rmeta}")
            break

    for libdir in polyffi_libdirs:
        cmd.extend(["-L", f"dependency={libdir}"])

    res = subprocess.run(cmd)
    return res.returncode


if __name__ == "__main__":
    raise SystemExit(main())
