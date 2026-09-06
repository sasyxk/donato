#!/usr/bin/env python3
"""Check function classification and LLVM names under different heap patterns."""

import argparse
import importlib.util
import json
import os
from pathlib import Path
import re
import resource
import subprocess
import sys
import textwrap


ROOT = Path(__file__).resolve().parent.parent
sys.dont_write_bytecode = True
spec = importlib.util.spec_from_file_location("control_flow_checks", ROOT / "scripts/check-control-flow.py")
checks = importlib.util.module_from_spec(spec)
spec.loader.exec_module(checks)


def cases():
    return (
        dict(name="main", source="""
            function int main() { print(42); return 0; }
        """, values=[42], symbols=("main",)),
        dict(name="ordinary_functions", source="""
            function int factorial(int x) {
                if (x == 0) { return 1; }
                else { return x * factorial(x - 1); }
            }
            function void show(int x) { print(x); return; }
            function int main() {
                show(factorial(5)); show(42); return 0;
            }
        """, values=[120, 42], symbols=("factorial", "show", "main")),
        dict(name="classes_and_ordinary_functions", source="""
            function int read(int x) { return x + 1; }
            class Box {
                int value;
            public:
                Box(int initial) { this.value = initial; return; }
                function int read() { return this.value; }
            }
            class Other {
                int value;
            public:
                Other(int initial) { this.value = initial; return; }
                function int read() { return this.value; }
            }
            function int main() {
                print(read(5));
                auto b = new Box(7); ref Box box = *b;
                auto o = new Other(11); ref Other other = *o;
                print(box.read()); print(other.read()); print(read(8));
                delete b; delete o; return 0;
            }
        """, values=[6, 7, 11, 9], symbols=(
            "read", "Box_Create_Default", "Box_read",
            "Other_Create_Default", "Other_read", "main")),
    )


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("level", nargs="?", type=int, choices=range(4), default=0)
    parser.add_argument("--build-dir", type=Path, default=ROOT / "build",
                        help="Directory containing dtc, with runtime sources at ../src/error_handling")
    args = parser.parse_args()
    checks.BUILD = args.build_dir.resolve()
    if not (checks.BUILD / "dtc").is_file():
        parser.error("Build dtc first; see README.md.")
    resource.setrlimit(resource.RLIMIT_CORE, (0, 0))
    results = checks.BUILD / f"function-initialization-O{args.level}"
    sources = results / "sources"
    sources.mkdir(parents=True, exist_ok=True)
    records = []
    for case in cases():
        source = sources / (case["name"] + ".donato")
        source.write_text(textwrap.dedent(case["source"]).strip() + "\n")
        for pattern in (0, 1, 85, 170):
            name = f"{case['name']}_heap{pattern}"
            directory = results / name
            directory.mkdir(exist_ok=True)
            env = dict(os.environ, PATH="/usr/lib/llvm-18/bin:/usr/bin:/bin",
                       MALLOC_PERTURB_=str(pattern))
            try:
                checks.check(dict(case, flags=(), merges=None, timeout=10),
                             source, directory, args.level, env)
                ir = (directory / "output.ll").read_text()
                symbols = set(re.findall(r"^define\b[^@\n]*@([A-Za-z_][A-Za-z_0-9]*)\(",
                                         ir, re.MULTILINE))
                missing = set(case["symbols"]) - symbols
                unexpected = {"_" + symbol for symbol in case["symbols"]} & symbols
                if missing or unexpected:
                    raise RuntimeError(f"wrong function names: missing={sorted(missing)}, "
                                       f"unexpected={sorted(unexpected)}")
                records.append(dict(name=name, heap_pattern=pattern, passed=True))
                print(f"PASS {name}", flush=True)
            except (RuntimeError, OSError, subprocess.TimeoutExpired) as error:
                records.append(dict(name=name, heap_pattern=pattern, passed=False, error=str(error)))
                print(f"FAIL {name}: {error}", flush=True)
    (results / "results.json").write_text(json.dumps(records, indent=2) + "\n")
    passed = sum(record["passed"] for record in records)
    print(f"{passed}/{len(records)} function-initialization cases passed "
          f"(-O {args.level}). Logs: {results}")
    return int(passed != len(records))


if __name__ == "__main__":
    raise SystemExit(main())
