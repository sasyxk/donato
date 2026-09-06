#!/usr/bin/env python3
"""Check grouped operands and optional parentheses in inline if conditions."""

import argparse
import importlib.util
import json
import operator
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

    def valid(name, source, values):
        tests.append(dict(name=name, source=source, values=values, merges=None, flags=()))

    def invalid(name, source, diagnostic, phase="parsing"):
        tests.append(dict(name=name, source=source, diagnostic=diagnostic,
                          phase=phase, flags=(), timeout=5))

    def main(body):
        return checks.function(body + "\nreturn 0;", "main", "")

    def condition(text):
        return "if " + text + " then 1 else 0"

    for name, op, compare in (("eq", "==", operator.eq), ("ne", "!=", operator.ne),
                              ("lt", "<", operator.lt), ("le", "<=", operator.le),
                              ("gt", ">", operator.gt), ("ge", ">=", operator.ge)):
        for form, text in (("left", f"(x + 1) {op} 3"),
                           ("nestedleft", f"(((x + 1))) {op} (3)"),
                           ("whole", f"((x + 1) {op} 3)"),
                           ("plain", f"x + 1 {op} 3")):
            valid(f"compare_{name}_{form}", checks.program(
                "return " + condition(text) + ";", [0, 2, 4]),
                [int(compare(x + 1, 3)) for x in (0, 2, 4)])

    valid("arithmetic_continuations", main("int x = 8;\n" + "\n".join(
        "print(" + condition(text) + ");" for text in (
            "(x) + 2 * 3 == 14", "(x) - 2 - 1 == 5", "(x) / 2 * 3 == 12",
            "(x + 1) * 2 + 3 == 21", "(x) + 2 * 3 != 30", "(x) - 8"))),
          [1, 1, 1, 1, 1, 0])
    valid("boolean_continuations", main("\n".join("print(" + condition(text) + ");"
          for text in ("(true) * false", "(false) + true", "(true) + false * false",
                       "((false))", "(true)", "(false) == false"))), [0, 1, 1, 0, 1, 1])
    valid("integer_truth", checks.program("return " + condition("((x))") + ";", [-1, 0, 2]), [1, 0, 1])
    valid("unary_and_right_operand", checks.program(
        "return " + condition("(-x) < (-1)") + ";", [0, 2, 4]), [0, 1, 1])
    valid("nested_if_left", checks.program(
        "return " + condition("(if x > 0 then x else 0) == 2") + ";", [0, 2, 4]), [0, 1, 0])
    valid("nested_if_wrapped", checks.program(
        "return " + condition("((if (x + 1) > 2 then 1 else 0) == 1)") + ";", [0, 2, 4]), [0, 1, 1])
    valid("nested_if_right", checks.program(
        "return " + condition("(x + 1) == (if x > 0 then 3 else 1)") + ";", [0, 2, 4]), [1, 1, 0])
    valid("then_else_binding", checks.program(
        "return if (x) > 0 then if (x + 1) > 2 then 3 else 4 else 5;", [0, 1, 2]), [5, 4, 3])
    valid("else_if_binding", checks.program(
        "return if (x) > 0 then 1 else if (x + 1) > 0 then 2 else 3;", [-2, 0, 2]), [3, 2, 1])
    valid("let_left", checks.program(
        "return " + condition("(let y = x + 1 in y) > 2") + ";", [0, 2, 4]), [0, 1, 1])
    valid("let_wrapped", checks.program(
        "return " + condition("((let y = x + 1 in y) > 2)") + ";", [0, 2, 4]), [0, 1, 1])
    valid("calls_and_single_evaluation", checks.function(
        "x = x + 1; return x;", "tick", "ref int x") + main("""
        int x = 0;
        print(if (tick(x)) > 0 then 7 else 9); print(x);
        print(if ((tick(x)) > 0) then 7 else 9); print(x);
        print(if (x) > 0 then tick(x) else tick(x)); print(x);
    """), [7, 1, 7, 2, 3, 3])
    valid("references_and_nullptr", main("""
        int x = 2; ref int alias = x; int* p = &x;
        print(if (alias) > 1 then 1 else 0);
        print(if (*p) > 1 then 1 else 0);
        print(if (&x) == p then 1 else 0);
        print(if (nullptr<int>) == nullptr<int> then 1 else 0);
        print(if ((nullptr<int>) == nullptr<int>) then 1 else 0);
        print(x);
    """), [1, 1, 1, 1, 1, 2])
    valid("class_method", """
        class Box {
            int value;
        public:
            Box(int x) { this.value = x; return; }
            function int eval() { return if (this.value + 1) > 2 then 1 else 0; }
        }
    """ + main("auto p = new Box(2); ref Box box = *p; "
               "print(if (box.eval()) == 1 then 7 else 0); delete p;"), [7])
    valid("statement_conditions", main("""
        int x = 0;
        while ((x + 1) < 4) {
            if ((x + 1) > 1) { print(if (x + 1) > 2 then 7 else 8); }
            x = x + 1;
        }
        print(x);
    """), [8, 7, 3])
    valid("comments_in_lookahead", main("""
        int x = 2;
        print(if /* ( then */ (x /* ) then */ + 1) // ) then
            > 2 then 1 else 0);
        print(if ((x + 1) /* ) */ > 2) /* ( */ then 1 else 0);
        print(if (x) /* then */ + 1 > 2 then 1 else 0);
    """), [1, 1, 1])
    for depth in (16, 64):
        text = "(" * depth + "x + 1" + ")" * depth + " > 2"
        valid(f"deep_grouping_{depth}", checks.program("return " + condition(text) + ";", [0, 2]), [0, 1])
    nested = "x"
    for _ in range(24):
        nested = "if (" + nested + ") == 2 then 2 else 0"
    valid("deep_nested_inline_if", checks.program("return " + nested + ";", [0, 2]), [0, 2])
    valid("valid_at_eof", main("int x = 2; print(if (x + 1) > 2 then 1 else 0);").rstrip(), [1])

    prefix = "function int main() { print(if "
    suffix = "); return 0; }"
    for name, text, diagnostic in (
        ("chain", "(2) < 3 < 4 then 1 else 0", "Unexpected Token: '<'"),
        ("comparison_as_operand", "(2 < 3) == true then 1 else 0", "Unexpected Token: '<'"),
        ("nested_comparison_group", "((2 < 3)) then 1 else 0", "Unexpected Token: '<'"),
        ("missing_then", "(2) 1 else 0", "Unexpected Token: '1'"),
        ("missing_else", "(2) then 1", "Unexpected Token: ')'"),
        ("missing_left", "() > 2 then 1 else 0", "Unexpected factor"),
        ("missing_right", "(2) > then 1 else 0", "Unexpected factor"),
        ("extra_close", "(2)) then 1 else 0", "Unexpected Token: ')'"),
        ("unknown_inside", "(2 @ 3) then 1 else 0", "Unknown token: @"),
        ("unknown_after", "(2) @ then 1 else 0", "Unknown token: @"),
    ):
        invalid(name, prefix + text + suffix, diagnostic)
    invalid("generic_comparison", main("bool b = (2 < 3); print(b);"), "Unexpected Token: '<'")
    invalid("generic_comparison_argument", main("print((2) < 3);"), "Unexpected Token: '<'")
    invalid("diagnostic_position_after_lookahead",
            "function int main() {\n    print(if (\n        2 + ) then 1 else 0);\n    return 0;\n}",
            "[3, 13]")
    for name, ending in (("eof", ""), ("lf", "\n"), ("crlf", "\r\n")):
        for case, text in (("opening", "("), ("nested", "((2 + 1)"), ("inline", "(if true then 1")):
            invalid(f"unclosed_{case}_{name}", prefix + text + ending,
                    "Unexpected end of input: expected ')' in inline if condition")
        invalid(f"missing_then_{name}", prefix + "(2)" + ending, "Unexpected end of input")
        invalid(f"unclosed_comment_{name}", "function int main() {\n    print(if (2 /* unclosed" + ending,
                "Unterminated block comment [2, 17]", phase="tokenizer")
    invalid("unclosed_comment_after_group", "function int main() {\n    print(if (2) /* unclosed",
            "Unterminated block comment [2, 18]", phase="tokenizer")
    return tests


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("level", nargs="?", type=int, choices=range(4), default=0)
    args = parser.parse_args()
    if not (BUILD / "dtc").is_file():
        parser.error("Build dtc first; see README.md.")
    resource.setrlimit(resource.RLIMIT_CORE, (0, 0))
    env = dict(os.environ, PATH="/usr/lib/llvm-18/bin:/usr/bin:/bin")
    results = BUILD / f"inline-conditions-O{args.level}"
    sources = results / "sources"
    sources.mkdir(parents=True, exist_ok=True)
    records = []
    for case in cases():
        source = sources / (case["name"] + ".donato")
        source.write_bytes(case["source"].encode())  # Preserve EOF and CRLF exactly.
        directory = results / case["name"]
        directory.mkdir(exist_ok=True)
        try:
            checks.check(case, source, directory, args.level, env)
            records.append(dict(name=case["name"], passed=True))
            print(f"PASS {case['name']}", flush=True)
        except (RuntimeError, OSError, subprocess.TimeoutExpired) as error:
            records.append(dict(name=case["name"], passed=False, error=str(error)))
            print(f"FAIL {case['name']}: {error}", flush=True)
    (results / "results.json").write_text(json.dumps(records, indent=2) + "\n")
    passed = sum(record["passed"] for record in records)
    print(f"{passed}/{len(records)} inline-condition cases passed (-O {args.level}). Logs: {results}")
    return int(passed != len(records))


if __name__ == "__main__":
    raise SystemExit(main())
