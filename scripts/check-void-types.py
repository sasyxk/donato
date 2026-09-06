#!/usr/bin/env python3
"""Check that void is allowed only as a plain function return type."""

import argparse
import importlib.util
import json
import os
from pathlib import Path
import resource
import subprocess


ROOT = Path(__file__).resolve().parent.parent
BUILD = ROOT / "build"
spec = importlib.util.spec_from_file_location("control_flow_checks", ROOT / "scripts/check-control-flow.py")
checks = importlib.util.module_from_spec(spec)
spec.loader.exec_module(checks)


def cases():
    tests = []
    value_error = "'void' is only allowed as a non-reference function return type"
    ref_error = "'ref void' is not a valid return type"
    pointer_error = "Pointers to 'void' are not supported"
    main = "function int main() { print(7); return 0; }"
    class_prefix = "class Box { public: Box() { return; } "

    def invalid(name, source, diagnostic=value_error):
        tests.append(dict(name=name, source=source, diagnostic=diagnostic,
                          phase="parsing", flags=(), timeout=5))

    def valid(name, source, values):
        tests.append(dict(name=name, source=source, values=values, merges=None, flags=()))

    forms = ("void", "void*", "void**", "ref void", "ref void*", "ref void**")
    for index, form in enumerate(forms):
        for position, prefix in (("first", ""), ("second", "int before, ")):
            parameters = prefix + form + " value"
            invalid(f"parameter_function_{index}_{position}",
                    f"function int eval({parameters}) {{ return 0; }}" + main)
            invalid(f"parameter_method_{index}_{position}", class_prefix +
                    f"function int eval({parameters}) {{ return 0; }} }}" + main)
            invalid(f"parameter_constructor_{index}_{position}",
                    f"class Box {{ public: Box({parameters}) {{ return; }} }}" + main)
        invalid(f"local_{index}", f"function int main() {{ {form} value = 0; return 0; }}")
        if form != "void":
            diagnostic = ref_error if form.startswith("ref") else pointer_error
            for context, prefix, suffix in (("function", "", ""), ("method", class_prefix, "}")):
                invalid(f"return_{context}_{index}", prefix +
                        f"function {form} eval() {{ return; }}" + suffix + main, diagnostic)

    for index, form in enumerate(("void", "void*", "void**")):
        for position, before in (("first", ""), ("second", "int before; ")):
            field = before + form + " value;"
            invalid(f"field_struct_{index}_{position}", f"struct Box {{ {field} }}" + main)
            for access, prefix in (("implicit", ""), ("explicit", "private: ")):
                invalid(f"field_class_{access}_{index}_{position}",
                        f"class Box {{ {prefix}{field} public: Box() {{ return; }} }}" + main)
        invalid(f"nullptr_{index}",
                f"function int main() {{ auto p = nullptr<{form}>; return 0; }}")

    for name, source, diagnostic in (
        ("parameter_comments", "function int eval(ref /* comment */ void /* * */ value) { return 0; }" + main, value_error),
        ("field_line_comment", "struct Box { int first; // comment\r\nvoid value; }" + main, value_error),
        ("nullptr_comments", "function int main() { auto p = nullptr< /* comment */ void /**/ ** >; return 0; }", value_error),
        ("return_ref_comments", "function ref /* comment */ void eval() { return; }" + main, ref_error),
        ("return_pointer_comments", "function void /* comment */ ** eval() { return; }" + main, pointer_error),
        ("method_ref_comments", class_prefix + "function ref /* comment */ void eval() { return; } }" + main, ref_error),
        ("method_pointer_comments", class_prefix + "function void /* comment */ * eval() { return; } }" + main, pointer_error),
        ("parameter_unnamed", "function int eval(void) { return 0; }" + main, value_error),
    ):
        invalid(name, source, diagnostic)

    for name, prefix, diagnostic in (
        ("parameter", "function int eval(void", value_error),
        ("return_ref", "function ref void", ref_error),
        ("return_pointer", "function void*", pointer_error),
        ("method_return_ref", class_prefix + "function ref void", ref_error),
        ("method_return_pointer", class_prefix + "function void*", pointer_error),
        ("return_missing_name", "function void", "Unexpected end of input"),
    ):
        for ending, suffix in (("eof", ""), ("lf", "\n"), ("crlf", "\r\n")):
            invalid(f"{name}_{ending}", prefix + suffix, diagnostic)

    valid("void_function", """
        function void increment(ref int value) { value = value + 1; return; }
        function void ping() { print(7); return; }
        function int main() { int x = 8; ping(); increment(x); print(x); return 0; }
    """, [7, 9])
    valid("void_method_constructor", """
        class Box {
        public:
            Box() { return; }
            function void ping() { print(7); return; }
            function void relay() { this.ping(); return; }
        }
        function int main() {
            auto p = new Box(); ref Box box = *p;
            box.ping(); box.relay(); delete p; return 0;
        }
    """, [7, 7])
    valid("void_function_at_eof", main + "function void unused() { return; }/**/", [7])
    valid("void_class_at_eof", main + class_prefix + "function void ping() { return; } }", [7])
    valid("ordinary_ref_and_pointer_returns", """
        function ref int alias(ref int value) { return value; }
        function ref int* pointeralias(ref int* value) { return value; }
        function int* pointer(int* value) { return value; }
        function int** pointers(int** value) { return value; }
        function int main() {
            int x = 7; ref int y = alias(x); y = 9; print(x);
            int z = 11; int* p = &x; ref int* q = pointeralias(p); q = &z;
            print(*pointer(p)); print(**pointers(&p));
            int* empty = pointer(nullptr<int>); print(if empty then 1 else 0);
            return 0;
        }
    """, [9, 11, 11, 0])
    valid("ordinary_method_reference_returns", """
        class Box {
            int value;
            int* pointer;
        public:
            Box(int* p) { this.value = 7; this.pointer = p; return; }
            function ref int get() { return this.value; }
            function ref int* getpointer() { return this.pointer; }
        }
        function int main() {
            int x = 11; auto p = new Box(&x); ref Box box = *p;
            ref int value = box.get(); value = 9; print(box.get());
            ref int* pointer = box.getpointer(); print(*pointer);
            delete p; return 0;
        }
    """, [9, 11])
    valid("ordinary_value_returns", """
        function bool truth(bool value) { return value; }
        function double decimal(double value) { return value; }
        function int main() {
            print(truth(true)); print(if decimal(1.5) == 1.5 then 7 else 0); return 0;
        }
    """, [1, 7])
    return tests


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("level", nargs="?", type=int, choices=range(4), default=0)
    args = parser.parse_args()
    if not (BUILD / "dtc").is_file():
        parser.error("Build dtc first; see README.md.")
    resource.setrlimit(resource.RLIMIT_CORE, (0, 0))
    env = dict(os.environ, PATH="/usr/lib/llvm-18/bin:/usr/bin:/bin")
    results = BUILD / f"void-types-O{args.level}"
    sources = results / "sources"
    sources.mkdir(parents=True, exist_ok=True)
    records = []
    for case in cases():
        source = sources / (case["name"] + ".donato")
        source.write_bytes(case["source"].encode())  # Preserve EOF and CRLF exactly.
        directory = results / case["name"]
        directory.mkdir(exist_ok=True)
        try:
            # Reuse the existing checks for diagnostics, artifacts, IR and execution.
            checks.check(case, source, directory, args.level, env)
            records.append(dict(name=case["name"], passed=True))
            print(f"PASS {case['name']}", flush=True)
        except (RuntimeError, OSError, subprocess.TimeoutExpired) as error:
            records.append(dict(name=case["name"], passed=False, error=str(error)))
            print(f"FAIL {case['name']}: {error}", flush=True)
    (results / "results.json").write_text(json.dumps(records, indent=2) + "\n")
    passed = sum(record["passed"] for record in records)
    print(f"{passed}/{len(records)} void-type cases passed (-O {args.level}). Logs: {results}")
    return int(passed != len(records))


if __name__ == "__main__":
    raise SystemExit(main())
