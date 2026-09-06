#!/usr/bin/env python3
"""Check exact CLI arguments, early rejection and the level sent to real LLVM tools."""

import argparse
import json
import os
from pathlib import Path
import resource
import shutil
import subprocess
import tempfile


ROOT = Path(__file__).resolve().parent.parent
BUILD = ROOT / "build"
TOOLS = "/usr/lib/llvm-18/bin:/usr/bin:/bin"
LEVEL_ERROR = "Error: -O requires a decimal integer between 0 and 3.\n"
SOURCE = "function int main() { print(7); return 7; }\n"


def cases():
    tests = []

    def valid(name, options, level):
        tests.append(dict(name=name, options=options, level=level, source=True))

    def invalid(name, options, kind="level", source=True, detail=None):
        for existing in (False, True):
            tests.append(dict(name=name + ("_existing" if existing else "_fresh"),
                              options=options, kind=kind, source=source,
                              detail=detail, existing=existing))

    for name, value in (
        ("letters", "abc"), ("huge", "99999999999999999999999999"),
        ("int_overflow", "2147483648"), ("int_underflow", "-2147483649"),
        ("suffix", "2junk"), ("fraction", "1.5"), ("exponent", "1e0"),
        ("negative", "-1"), ("above_range", "4"), ("empty", ""),
        ("space_only", " "), ("leading_space", " 2"), ("trailing_space", "2 "),
        ("tab", "\t2"), ("newline", "2\n"), ("plus", "+2"),
        ("hex", "0x2"), ("double_sign", "--1"), ("flags_suffix", "3tf"),
    ):
        invalid(f"{name}_separate", ["-O", value])
        if value:
            invalid(f"{name}_compact", ["-O" + value])

    invalid("invalid_then_valid", ["-O", "abc", "-O3"])
    invalid("valid_then_invalid", ["-O3", "-O", "2junk"])
    invalid("flag_as_value", ["-O", "-tf"])
    invalid("missing_level", ["-O"], "usage", source=False,
            detail="option requires an argument -- 'O'")
    invalid("missing_output", ["-o"], "usage", source=False,
            detail="option requires an argument -- 'o'")
    invalid("missing_source", ["-O2"], "usage", source=False)
    invalid("unknown_option", ["-z"], "usage", detail="invalid option -- 'z'")
    invalid("help", ["--help"], "help", source=False)
    # Preserve the existing --help prescan, which takes precedence over parsing.
    invalid("help_with_bad_level", ["-Oabc", "--help"], "help", source=False)

    valid("default", [], 0)
    for level in range(4):
        valid(f"separate_{level}", ["-O", str(level)], level)
        valid(f"compact_{level}", [f"-O{level}"], level)
    valid("leading_zero", ["-O", "02"], 2)
    valid("negative_zero", ["-O-0"], 0)
    valid("many_leading_zeros", ["-O", "0000000000000000000000000000003"], 3)
    valid("repeated_separate", ["-O", "0", "-O", "3"], 3)
    valid("repeated_compact", ["-O3", "-O0"], 0)
    valid("repeated_mixed", ["-O1", "-O", "2", "-O3"], 3)
    valid("combined_flags", ["-tf"], 0)
    valid("combined_flags_with_level", ["-tf", "-O3"], 3)
    valid("separate_flags", ["-t", "-f", "-O", "2"], 2)
    valid("end_of_options", ["-O2", "--"], 2)
    return tests


def run(command, directory, step, env, timeout=60):
    result = subprocess.run(command, cwd=BUILD, env=env, text=True,
                            capture_output=True, timeout=timeout)
    (directory / f"{step}.stdout.txt").write_text(result.stdout)
    (directory / f"{step}.stderr.txt").write_text(result.stderr)
    (directory / f"{step}.status.txt").write_text(str(result.returncode) + "\n")
    return result


def check(case, directory, tool_dir):
    source = directory / "input program.donato"
    source.write_text(SOURCE)
    binary = directory / "program"
    ir, obj = BUILD / "output.ll", BUILD / "output.o"
    products = (binary, ir, obj)
    for product in products:
        product.unlink(missing_ok=True)
    previous = {}
    if case.get("existing"):
        for product in products:
            product.write_bytes(f"previous {product.name}\n".encode())
            previous[product] = (product.read_bytes(), product.stat().st_mtime_ns)

    trace_file = directory / "trace.jsonl"
    env = dict(os.environ, PATH=str(tool_dir) + ":" + TOOLS, LC_ALL="C",
               DTC_TEST_TRACE=str(trace_file), DTC_TEST_LLC_MODE="real",
               DTC_TEST_CLANG_MODE="real")
    command = [str(BUILD / "dtc"), "-o", str(binary.relative_to(BUILD)), *case["options"]]
    if case["source"]:
        command.append(str(source))
    (directory / "command.json").write_text(json.dumps(command, indent=2) + "\n")
    result = run(command, directory, "compile", env, timeout=5 if "kind" in case else 60)
    trace = [json.loads(line) for line in trace_file.read_text().splitlines()] if trace_file.exists() else []
    if source.read_text() != SOURCE:
        raise RuntimeError("compiler changed its input")

    if "kind" in case:
        if result.returncode != 1 or trace:
            raise RuntimeError(f"wrong rejection: status={result.returncode}, tools={trace}, stderr={result.stderr!r}")
        if case["kind"] == "help":
            if result.stderr or not result.stdout.startswith("Usage:") or "-O <level>" not in result.stdout:
                raise RuntimeError(f"wrong help output: {result.stdout!r}, {result.stderr!r}")
        elif result.stdout or (case["kind"] == "level" and result.stderr != LEVEL_ERROR):
            raise RuntimeError(f"wrong diagnostic: stdout={result.stdout!r}, stderr={result.stderr!r}")
        elif case["kind"] == "usage":
            if "Usage:" not in result.stderr or (case["detail"] and case["detail"] not in result.stderr):
                raise RuntimeError(f"wrong usage diagnostic: {result.stderr!r}")
        for product in products:
            if product in previous:
                if not product.is_file() or (product.read_bytes(), product.stat().st_mtime_ns) != previous[product]:
                    raise RuntimeError(f"rejection changed existing {product.name}")
            elif product.exists():
                raise RuntimeError(f"rejection created {product.name}")
        return

    if result.returncode != 0 or result.stderr or not all(path.is_file() for path in products):
        raise RuntimeError(f"valid compilation failed: status={result.returncode}, stderr={result.stderr!r}")
    if [entry["tool"] for entry in trace] != ["llc", "clang"]:
        raise RuntimeError(f"wrong tool sequence: {trace}")
    levels = [argument for argument in trace[0]["args"] if argument.startswith("-O")]
    if levels != [f"-O{case['level']}"]:
        raise RuntimeError(f"wrong optimization level sent to llc: {levels}")
    shutil.copyfile(ir, directory / "output.ll")
    shutil.copyfile(obj, directory / "output.o")
    verified = run(["/usr/lib/llvm-18/bin/opt", "-passes=verify", "-disable-output", str(ir)],
                   directory, "verify", env)
    if verified.returncode != 0 or verified.stderr:
        raise RuntimeError(f"LLVM verification failed: {verified.stderr}")
    executed = run([str(binary)], directory, "execute", env)
    if executed.returncode != 7 or executed.stdout != "value: 7\n" or executed.stderr:
        raise RuntimeError(f"wrong program result: status={executed.returncode}, stdout={executed.stdout!r}, stderr={executed.stderr!r}")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.parse_args()
    if not (BUILD / "dtc").is_file():
        parser.error("Build dtc first; see README.md.")
    resource.setrlimit(resource.RLIMIT_CORE, (0, 0))
    results = BUILD / "cli"
    results.mkdir(exist_ok=True)
    work = Path(tempfile.mkdtemp(prefix="run-", dir=results))
    tool_dir = work / "tools"
    tool_dir.mkdir()
    for tool in ("llc", "clang"):
        wrapper = tool_dir / tool
        shutil.copyfile(ROOT / "scripts/fixtures/driver-tool.py", wrapper)
        wrapper.chmod(0o755)
    originals = {}
    for name in ("output.ll", "output.o"):
        product = BUILD / name
        if product.exists():
            if not product.is_file():
                parser.error(f"Expected a regular build/{name}; inspect it before testing.")
            backup = work / ("previous-" + name)
            shutil.copy2(product, backup)
            originals[product] = backup
    records = []
    try:
        for case in cases():
            directory = work / case["name"]
            directory.mkdir()
            try:
                check(case, directory, tool_dir)
                record = dict(name=case["name"], passed=True)
                print(f"PASS {case['name']}", flush=True)
            except (RuntimeError, OSError, subprocess.TimeoutExpired) as error:
                record = dict(name=case["name"], passed=False, error=str(error))
                print(f"FAIL {case['name']}: {error}", flush=True)
            records.append(record)
            (work / "results.json").write_text(json.dumps(records, indent=2) + "\n")
    finally:
        for name in ("output.ll", "output.o"):
            product = BUILD / name
            if product in originals:
                shutil.copy2(originals[product], product)
            else:
                product.unlink(missing_ok=True)
    passed = sum(record["passed"] for record in records)
    print(f"{passed}/{len(records)} CLI cases passed. Logs: {work}")
    return int(passed != len(records))


if __name__ == "__main__":
    raise SystemExit(main())
