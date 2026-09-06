#!/usr/bin/env python3
"""Check compiler stage failures and preservation of the last good executable."""

import argparse
from contextlib import nullcontext
import json
import os
from pathlib import Path
import resource
import shutil
import signal
import subprocess
import tempfile


ROOT = Path(__file__).resolve().parent.parent
BUILD = ROOT / "build"
TOOLS = "/usr/lib/llvm-18/bin:/usr/bin:/bin"
SOURCE = "function int main() { print(7); return 0; }"


def cases():
    tests = [dict(name="valid", diagnostic=None, calls=["llc", "clang"]),
             dict(name="valid_fixture", fixture="valid", diagnostic=None,
                  calls=["llc", "clang"]),
             dict(name="invalid_module", fixture="invalid",
                  diagnostic="LLVM module verification failed", calls=[]),
             dict(name="missing_main", source="function int helper() { return 0; }",
                  diagnostic="Tool 'clang' failed with exit status 1", calls=["llc", "clang"],
                  detail="undefined reference to `main'"),
             dict(name="ir_open", prepare="ir_open",
                  diagnostic="Cannot open IR file 'output.ll'", calls=[]),
             dict(name="ir_write", limit_file_size=True,
                  diagnostic="Cannot write or close IR file 'output.ll'", calls=[]),
             dict(name="object_copy", prepare="object_copy",
                  diagnostic="Cannot write object file 'output.o'", calls=["llc"]),
             dict(name="missing_output_parent", target="missing_parent",
                  diagnostic="Cannot create temporary output directory", calls=[], fresh_only=True),
             dict(name="output_directory", target="directory",
                  diagnostic="Output executable path is not a regular file", calls=[], fresh_only=True),
             dict(name="publish_failure", clang="publish_failure",
                  diagnostic="Cannot replace output executable", calls=["llc", "clang"], fresh_only=True)]
    for tool in ("llc", "clang"):
        before = [] if tool == "llc" else ["llc"]
        code = 23 if tool == "llc" else 29
        for mode, diagnostic in (
            ("missing", f"Tool '{tool}' not found on PATH"),
            ("bad_interpreter", f"Cannot execute '{tool}'"),
            ("fail", f"Tool '{tool}' failed with exit status {code}"),
            ("crash", f"Tool '{tool}' terminated abnormally"),
            ("partial_failure", f"Tool '{tool}' failed with exit status {code}"),
            ("no_output", f"Tool '{tool}' did not produce a regular output file"),
            ("directory", f"Tool '{tool}' did not produce a regular output file"),
            ("symlink", f"Tool '{tool}' did not produce a regular output file"),
            ("empty", f"Tool '{tool}' did not produce a nonempty output file"),
        ):
            calls = before if mode in ("missing", "bad_interpreter") else [*before, tool]
            tests.append(dict(name=f"{tool}_{mode}", diagnostic=diagnostic,
                              calls=calls, **{tool: mode}))
    tests.append(dict(name="clang_nonexecutable", clang="nonexecutable",
                      diagnostic="Tool 'clang' did not produce an executable file",
                      calls=["llc", "clang"], native_permissions=True))
    tests.append(dict(name="stale_object", llc="no_output", prepare="stale_object",
                      diagnostic="Tool 'llc' did not produce a regular output file", calls=["llc"]))
    for alias in ("ir", "object", "source", "hardlink", "symlink"):
        tests.append(dict(name=f"output_alias_{alias}", target=alias,
                          diagnostic="Conflicting compiler paths", calls=[], fresh_only=True))
    return tests


def clear_intermediates():
    for path in (BUILD / "output.ll", BUILD / "output.o"):
        if path.is_dir() and not path.is_symlink():
            path.rmdir()  # Only empty directories deliberately created by a case.
        else:
            path.unlink(missing_ok=True)


def child_limits(limit_file_size):
    resource.setrlimit(resource.RLIMIT_CORE, (0, 0))
    if limit_file_size:
        signal.signal(signal.SIGXFSZ, signal.SIG_IGN)
        resource.setrlimit(resource.RLIMIT_FSIZE, (64, 64))


def run(command, directory, prefix, env, limit_file_size=False):
    result = subprocess.run(command, cwd=BUILD, env=env, text=True,
                            stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=60,
                            preexec_fn=lambda: child_limits(limit_file_size))
    (directory / f"{prefix}.stdout.txt").write_text(result.stdout)
    (directory / f"{prefix}.stderr.txt").write_text(result.stderr)
    (directory / f"{prefix}.status.txt").write_text(f"{result.returncode}\n")
    return result


def check(case, directory, level, previous, target_directory=None):
    clear_intermediates()
    source = directory / "input program.donato"
    source.write_text(case.get("source", SOURCE))
    source_bytes = source.read_bytes()
    target = (target_directory or directory) / "program with spaces"
    target_kind = case.get("target")
    if target_kind == "missing_parent":
        target = directory / "missing" / "program"
    elif target_kind == "directory":
        target.mkdir()
        (target / "keep").write_text("destination blocker\n")
    elif target_kind in ("ir", "object", "source", "hardlink", "symlink"):
        if target_kind in ("ir", "object"):
            target = BUILD / ("output.ll" if target_kind == "ir" else "output.o")
            target.write_bytes(b"existing artifact\n")
        elif target_kind == "source":
            target = source
        else:
            intermediate = BUILD / "output.ll"
            target.write_bytes(b"existing executable\n")
            if target_kind == "hardlink":
                os.link(target, intermediate)
            else:
                intermediate.symlink_to(target)
    elif previous:
        shutil.copy2(previous, target)
    original = (target.read_bytes(), target.stat().st_mode) if target.is_file() else None
    if case.get("prepare") == "ir_open":
        (BUILD / "output.ll").mkdir()
    elif case.get("prepare") == "object_copy":
        (BUILD / "output.o").mkdir()
    elif case.get("prepare") == "stale_object":
        (BUILD / "output.o").write_bytes(b"old object must never be linked\n")

    tool_dir = directory / "tools"
    tool_dir.mkdir()
    for tool in ("llc", "clang"):
        mode = case.get(tool, "real")
        wrapper = tool_dir / tool
        if mode == "missing":
            continue
        if mode == "bad_interpreter":
            wrapper.write_text("#!/nonexistent-dtc-test-interpreter\n")
        else:
            # Copy so executable permissions work on both ext4 and WSL mounts.
            shutil.copyfile(ROOT / "scripts/fixtures/driver-tool.py", wrapper)
        wrapper.chmod(0o755)
    env = dict(os.environ, PATH=str(tool_dir) + ("" if "missing" in
               (case.get("llc"), case.get("clang")) else ":" + TOOLS),
               DTC_TEST_TRACE=str(directory / "trace.jsonl"),
               DTC_TEST_SOURCE=str(source), DTC_TEST_TARGET=str(target),
               DTC_TEST_LLC_MODE=case.get("llc", "real"),
               DTC_TEST_CLANG_MODE=case.get("clang", "real"))
    relative_target = os.path.relpath(target, BUILD)
    if "fixture" in case:
        command = [str(BUILD / "check-codegen-driver"), case["fixture"], str(level), relative_target]
    else:
        command = [str(BUILD / "dtc"), "-O", str(level), "-o", relative_target, str(source)]
    result = run(command, directory, "compile", env, case.get("limit_file_size", False))
    trace_path = directory / "trace.jsonl"
    trace = [json.loads(line) for line in trace_path.read_text().splitlines()] if trace_path.exists() else []
    calls = [entry["tool"] for entry in trace]
    if calls != case["calls"]:
        raise RuntimeError(f"wrong stage sequence: {calls}; stderr={result.stderr!r}")
    if list(directory.rglob(".dtc-*")) or list(target.parent.glob(".dtc-*")):
        raise RuntimeError("temporary compilation directory was not cleaned up")
    if source.read_bytes() != source_bytes:
        raise RuntimeError("compilation overwrote its input")
    if case["diagnostic"]:
        if (result.returncode != 1 or "Error in codegen:: " not in result.stderr
                or case["diagnostic"] not in result.stderr
                or f"Output executable '{relative_target}' was not updated." not in result.stderr
                or case.get("detail", "") not in result.stderr):
            raise RuntimeError(f"wrong rejection: status={result.returncode}, stderr={result.stderr!r}")
        if "LLVM ERROR" in result.stderr or "terminate called" in result.stderr:
            raise RuntimeError("failure reached an uncaught exception or LLVM fatal handler")
        if original:
            if not target.is_file() or (target.read_bytes(), target.stat().st_mode) != original:
                raise RuntimeError("failure changed the previous output")
        elif target_kind == "directory" or case.get("clang") == "publish_failure":
            if (target / "keep").read_text() != "destination blocker\n":
                raise RuntimeError("failure changed the destination directory")
        elif target.exists():
            raise RuntimeError("failed compilation published an executable")
        if case.get("fixture") == "invalid" and any(
                (BUILD / name).exists() for name in ("output.ll", "output.o")):
            raise RuntimeError("invalid module produced an intermediate")
        return
    if result.returncode != 0 or result.stderr or not target.is_file():
        raise RuntimeError(f"valid compilation failed: {result.returncode}, {result.stderr!r}")
    llc, clang = trace
    object_path = llc["args"][llc["args"].index("-o") + 1]
    executable_path = clang["args"][clang["args"].index("-o") + 1]
    if object_path not in clang["args"] or executable_path == str(target):
        raise RuntimeError("tools did not use private compilation products")
    if f"-O{level}" not in llc["args"]:
        raise RuntimeError("optimization level was not forwarded")
    shutil.copyfile(BUILD / "output.ll", directory / "output.ll")
    shutil.copyfile(BUILD / "output.o", directory / "output.o")
    verified = run(["/usr/lib/llvm-18/bin/opt", "-passes=verify", "-disable-output", "output.ll"],
                   directory, "verify", env)
    executed = run([str(target)], directory, "execute", env)
    expected = "" if case.get("fixture") == "valid" else "value: 7\n"
    if verified.returncode or verified.stderr or executed.returncode or executed.stderr or executed.stdout != expected:
        raise RuntimeError("valid program failed IR verification or execution")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("level", type=int, choices=range(4))
    args = parser.parse_args()
    for binary in ("dtc", "check-codegen-driver"):
        if not (BUILD / binary).is_file():
            parser.error("first run: cmake --build build --target dtc check-codegen-driver --parallel 2")
    directory = BUILD / f"driver-O{args.level}"
    directory.mkdir(exist_ok=True)
    # Unique runs retain earlier logs and avoid deleting someone else's results.
    work = Path(tempfile.mkdtemp(prefix="run-", dir=directory))
    backup = work / "saved-intermediates"
    backup.mkdir()
    saved = []
    results = []
    try:
        for name in ("output.ll", "output.o"):
            path = BUILD / name
            if path.is_dir() and not path.is_symlink():
                raise RuntimeError(f"existing {path} is a directory; inspect it before testing")
            if path.exists() or path.is_symlink():
                path.rename(backup / name)
                saved.append(name)
        # A real, different executable proves replacement and preservation.
        old = work / "previous"
        old.mkdir()
        check(dict(name="seed", source="function int main() { return 0; }",
                   fixture="valid", diagnostic=None, calls=["llc", "clang"]), old, args.level, None)
        previous = old / "program with spaces"
        for case in cases():
            for stale in (False,) if case.get("fresh_only") else (False, True):
                name = case["name"] + ("_existing" if stale else "_fresh")
                case_dir = work / name
                case_dir.mkdir()
                try:
                    # WSL's Windows mounts may ignore chmod without metadata.
                    # Exercise actual Unix permissions on the Linux filesystem.
                    native = (tempfile.TemporaryDirectory(prefix="dtc-driver-", dir="/tmp")
                              if case.get("native_permissions") else nullcontext(str(case_dir)))
                    with native as target_directory:
                        check(case, case_dir, args.level, previous if stale else None,
                              Path(target_directory))
                    results.append(dict(name=name, passed=True))
                except (OSError, RuntimeError, subprocess.TimeoutExpired) as error:
                    results.append(dict(name=name, passed=False, error=str(error)))
                    print(f"FAIL {name}: {error}", flush=True)
    finally:
        # Leave preexisting directory blockers alone if setup itself failed.
        if results or (work / "previous").exists():
            clear_intermediates()
        for name in saved:
            (backup / name).rename(BUILD / name)
    (work / "results.json").write_text(json.dumps(results, indent=2) + "\n")
    passed = sum(result["passed"] for result in results)
    print(f"{passed}/{len(results)} driver checks passed at -O{args.level}; logs: {work}")
    return 0 if results and passed == len(results) else 1


if __name__ == "__main__":
    raise SystemExit(main())
