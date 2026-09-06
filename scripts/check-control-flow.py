#!/usr/bin/env python3
"""Compile real Donato inputs and check output, parser diagnostics and LLVM IR."""

import argparse
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import textwrap


ROOT = Path(__file__).resolve().parent.parent
BUILD = ROOT / "build"


def function(body, name="eval", params="int x", result="int"):
    return f"function {result} {name}({params}) {{\n{body}\n}}\n"


def program(body, inputs):
    calls = "\n".join(f"print(eval({value}));" for value in inputs)
    return function(body) + function(calls + "\nreturn 0;", "main", "")


def tree(depth, first=0, open_leaf=None):
    if depth == 0:
        return "value = 900;" if first == open_leaf else f"return {first};"
    split = first + 2 ** (depth - 1)
    left = tree(depth - 1, first, open_leaf)
    right = tree(depth - 1, split, open_leaf)
    return f"if (x < {split}) {{\n{left}\n}} else {{\n{right}\n}}"


def cases():
    tests = []

    def valid(name, source, values, merges=None, flags=()):
        tests.append(dict(name=name, source=source, values=values,
                          merges=merges, flags=flags))

    def invalid(name, source, diagnostic):
        tests.append(dict(name=name, source=source, diagnostic=diagnostic,
                          flags=()))

    for type_name in ("int8", "int16", "int32", "int64", "int", "bool"):
        if type_name == "bool":
            inputs, expected, zero = ["true", "false"], [1, 0], "false"
        else:
            inputs, expected, zero = [5, 0, -1, 2], [1, 0, 1, 1], "0"
            if type_name in ("int64", "int"):
                inputs.append(4294967296)
                expected.append(1)
        calls = "\n".join(
            f"{type_name} input{i} = {value}; print(eval(input{i}));"
            for i, value in enumerate(inputs)
        )
        for form, body in (
            ("if", "if (x) { return 1; } else { return 0; }"),
            ("while", f"int result = 0; while (x) {{ result = 1; x = {zero}; }} return result;"),
            ("inline", "return if x then 1 else 0;"),
        ):
            valid(f"condition_{type_name}_{form}",
                  function(body, params=f"{type_name} x")
                  + function(calls + "\nreturn 0;", "main", ""), expected)

    original = """
        auto start = x;
        auto y = start + 5;
        start = y + 2;
        if (start != 0) {
            auto z = start + y;
            return z;
        } else {
            auto z = start + 2;
            return z;
        }
    """
    valid("original_both_return", program(original, [5, -7]), [22, 2], 0)
    valid("original_checked", program(original, [5, -7]), [22, 2], 0,
          flags=("-f", "-t"))
    valid("if_without_else", program(
        "if (x > 0) { return 1; } return 2;", [1, 0]), [1, 2], 1)
    valid("then_returns", program(
        "if (x > 0) { return 1; } else { print(2); } return 3;", [1, 0]),
        [1, 2, 3], 1)
    valid("else_returns", program(
        "if (x > 0) { print(1); } else { return 2; } return 3;", [1, 0]),
        [1, 3, 2], 1)
    valid("closed_inner_then", program("""
        if (x > 0) {
            if (x > 1) { return 10; } else { return 11; }
        } else { print(20); }
        return 30;
    """, [2, 1, 0]), [10, 11, 20, 30], 1)
    valid("closed_inner_else", program("""
        if (x > 0) { print(20); } else {
            if (x < 0) { return 10; } else { return 11; }
        }
        return 30;
    """, [1, -1, 0]), [20, 30, 10, 11], 1)
    valid("open_nested_and_sequential", program("""
        auto value = 0;
        if (x > 0) {
            if (x > 1) { value = 10; } else { value = 11; }
            value = value + 1;
        } else {
            if (x < 0) { value = 20; } else { value = 21; }
            value = value + 2;
        }
        if (value > 20) { value = value + 100; }
        return value;
    """, [2, 1, -1, 0]), [11, 12, 122, 123], 4)

    for depth in (1, 3, 4, 5):
        inputs = list(range(2 ** depth))
        closed = tree(depth)
        valid(f"all_return_depth{depth}", program(closed, inputs), inputs, 0)
        # Exercise a surviving path at either extreme of the tree.
        for leaf in (0, 2 ** depth - 1):
            body = "auto value = 0;\n" + tree(depth, open_leaf=leaf) + "\nreturn value;"
            expected = [900 if value == leaf else value for value in inputs]
            valid(f"open_leaf{leaf}_depth{depth}", program(body, inputs), expected, depth)
        invalid(f"extra_return_depth{depth}", function(closed + "\nreturn x;"),
                "Unreachable statement 'return'")
        invalid(f"extra_decl_depth{depth}", function(closed + "\nint i = 0;"),
                "Unreachable statement 'int'")
        invalid(f"missing_path_depth{depth}", function(
            "auto value = 0;\n" + tree(depth, open_leaf=2 ** depth - 1)),
            "Function 'eval' can reach the end without a return")

    for depth in (3, 4, 5):
        body = "return 10;"
        for threshold in reversed(range(1, depth + 1)):
            body = f"if (x >= {threshold}) {{\n{body}\n}}"
        valid(f"no_else_depth{depth}", program(body + "\nreturn 20;", range(depth + 1)),
              [20] * depth + [10], depth)

        # Put unreachable code inside a then, an else or a while at each depth.
        for wrapper in ("then", "else", "while"):
            body = "return 1; print(999);"
            for _ in range(depth):
                if wrapper == "then":
                    body = f"if (x > 0) {{ {body} }} else {{ return 2; }}"
                elif wrapper == "else":
                    body = f"if (x > 0) {{ return 2; }} else {{ {body} }}"
                else:
                    body = f"while (x > 0) {{ {body} }}"
            invalid(f"dead_{wrapper}_depth{depth}", function(body + "\nreturn 3;"),
                    "Unreachable statement 'print'")

    valid("while_direct_return", program(
        "while (x > 0) { return 10; } return 20;", [1, 0]), [10, 20], 0)
    valid("while_closed_if", program("""
        while (x > 0) {
            if (x > 1) { return 10; } else { return 11; }
        }
        return 20;
    """, [2, 1, 0]), [10, 11, 20], 0)
    valid("while_partial_return", program("""
        while (x > 0) {
            if (x == 2) { return x; } else { x = x - 1; }
        }
        return 99;
    """, [4, 1, 0]), [2, 99, 99], 1)
    loops = "return 10;"
    for _ in range(5):
        loops = f"while (x > 0) {{ {loops} }}"
    valid("while_depth5", program(loops + "\nreturn 20;", [1, 0]), [10, 20], 0)
    valid("return_inline_if", program("""
        if (x > 0) {
            return if (x > 1) then 10 else 11;
        } else {
            return if (x < 0) then 20 else 21;
        }
    """, [2, 1, -1, 0]), [10, 11, 20, 21])
    valid("inline_reference_original", function("""
        int x = 7;
        int* p = &x;
        print(if false then 9 else *p);
        return 0;
    """, "main", ""), [7])
    reference_helper = function("return value;", "getref", "ref int value", "ref int")
    for name, expression, expected in (
        ("deref_then", "if (x > 0) then *p else 9", [7, 9]),
        ("deref_else", "if (x > 0) then 9 else *p", [9, 7]),
        ("deref_both", "if (x > 0) then *p else *q", [7, 13]),
        ("call_then", "if (x > 0) then getref(first) else 9", [7, 9]),
        ("call_else", "if (x > 0) then 9 else getref(first)", [9, 7]),
        ("call_both", "if (x > 0) then getref(first) else getref(second)", [7, 13]),
    ):
        valid(f"inline_reference_{name}", reference_helper + program(f"""
            int first = 7;
            int second = 13;
            int* p = &first;
            int* q = &second;
            return {expression};
        """, [1, 0]), expected)
    valid("comments_after_returns", program("""
        if (x > 0) { return 1; /* comment */ } else {
            return 2; // comment
        } /* after a closed if */
    """, [1, 0]), [1, 2], 0)
    valid("void_both_return", function("""
        if (x > 0) { print(1); return; } else { print(2); return; }
    """, result="void") + function("eval(1); eval(0); return 0;", "main", ""),
          [1, 2], 0)

    class_source = """
        class Choice {
            int value;
        public:
            Choice(int x) {
                if (x > 0) { this.value = 10; return; }
                else { this.value = 20; return; }
            }
            function int choose(int x) {
                BODY
            }
            function void show(int x) {
                if (x > 0) { print(this.value); return; }
                else { print(99); return; }
            }
        }
        function int main() {
            auto a = new Choice(1);
            auto b = new Choice(0);
            ref Choice first = *a;
            ref Choice second = *b;
            first.show(1);
            second.show(1);
            first.show(0);
            CALLS
            delete a;
            delete b;
            return 0;
        }
    """
    valid("class_returns_depth5", class_source.replace("BODY", tree(5)).replace(
        "CALLS", "\n".join(f"print(first.choose({x}));" for x in range(32))),
        [10, 20, 99] + list(range(32)), 0)

    for name, statement, token in (
        ("return", "return missing;", "return"),
        ("print", "print(999);", "print"),
        ("decl", "int i = 0;", "int"),
        ("auto", "auto i = 0;", "auto"),
        ("assign", "x = 2;", "x"),
        ("if", "if (x > 0) { return 2; }", "if"),
        ("while", "while (x > 0) { return 2; }", "while"),
        ("call", "missing();", "missing"),
        ("malformed", "print(;", "print"),
    ):
        invalid(f"after_return_{name}", function("return 1;\n" + statement),
                f"Unreachable statement '{token}'")

    for name, body in (
        ("plain", "print(1);"),
        ("one_branch", "if (x > 0) { return 1; } else { print(2); }"),
        ("no_else", "if (x > 0) { return 1; }"),
        ("while", "while (x > 0) { return 1; }"),
        ("while_true", "while (true) { return 1; }"),
    ):
        invalid(f"missing_return_{name}", function(body),
                "Function 'eval' can reach the end without a return")
    invalid("void_missing_return", function("print(1);", result="void"),
            "Function 'eval' can reach the end without a return")
    invalid("void_dead_code", function("return; print(1);", result="void"),
            "Unreachable statement 'print'")

    def class_case(constructor="return;", method="return 1;"):
        return f"""class Sample {{
            int value;
        public:
            Sample() {{ {constructor} }}
            function int eval() {{ {method} }}
        }}"""

    invalid("constructor_missing_return", class_case(constructor="this.value = 1;"),
            "Constructor 'Sample' can reach the end without a return")
    invalid("constructor_dead_code", class_case(constructor="return; this.value = 1;"),
            "Unreachable statement 'this'")
    invalid("method_missing_return", class_case(method="print(1);"),
            "Function 'eval' can reach the end without a return")
    invalid("method_dead_code", class_case(method="return 1; print(2);"),
            "Unreachable statement 'print'")
    for name, body in (
        ("function", ""),
        ("then", "if (x > 0) {} return 1;"),
        ("else", "if (x > 0) { return 1; } else {}"),
        ("while", "while (x > 0) {} return 1;"),
    ):
        invalid(f"empty_{name}", function(body), "Unexpected Statement Token: '}'")
    invalid("empty_constructor", class_case(constructor=""), "Unexpected Statement Token: '}'")
    invalid("empty_method", class_case(method=""), "Unexpected Statement Token: '}'")
    invalid("missing_brace", "function int eval(int x) { return x;",
            "Unexpected end of input: expected '}'")
    invalid("nested_function", function("function int other() { return 1; } return 0;"),
            "Unexpected Statement token 'FUNCTION'")
    return tests


def run(command, directory, prefix, env):
    with (directory / f"{prefix}.stdout.txt").open("w") as out, \
         (directory / f"{prefix}.stderr.txt").open("w") as err:
        result = subprocess.run(command, cwd=BUILD, env=env, stdout=out,
                                stderr=err, timeout=60)
    return (result.returncode,
            (directory / f"{prefix}.stdout.txt").read_text(),
            (directory / f"{prefix}.stderr.txt").read_text())


def check(case, source, directory, level, env):
    binary = directory / "program"
    # Only remove these known products; never accept an executable from a prior run.
    for product in (binary, BUILD / "output.ll", BUILD / "output.o"):
        product.unlink(missing_ok=True)
    status, _, errors = run(
        [str(BUILD / "dtc"), "-O", str(level), *case["flags"], "-o",
         str(binary.relative_to(BUILD)), str(source)], directory, "compile", env)
    if "diagnostic" in case:
        if status != 1 or "Error in parsing::" not in errors or case["diagnostic"] not in errors:
            raise RuntimeError(f"wrong parser result (status {status}): {errors.strip()}")
        if binary.exists() or (BUILD / "output.ll").exists():
            raise RuntimeError("parser rejection produced codegen output")
        return
    if status != 0 or errors or not binary.is_file():
        raise RuntimeError(f"compilation failed (status {status}): {errors.strip()}")
    ir = BUILD / "output.ll"
    shutil.copyfile(ir, directory / "output.ll")
    status, _, errors = run(["opt", "-passes=verify", "-disable-output", str(ir)],
                            directory, "verify", env)
    if status != 0 or errors:
        raise RuntimeError(f"LLVM verification failed: {errors.strip()}")
    text = ir.read_text()
    if "No predecessors!" in text:
        raise RuntimeError("IR contains an unreachable block")
    if case["merges"] is not None:
        count = len(re.findall(r"^merge\d*:", text, re.MULTILINE))
        if count != case["merges"]:
            raise RuntimeError(f"expected {case['merges']} if merges, got {count}")
    expected = "".join(f"value: {value}\n" for value in case["values"])
    (directory / "expected.txt").write_text(expected)
    status, output, errors = run([str(binary)], directory, "run", env)
    if status != 0 or errors or output != expected:
        raise RuntimeError(f"unexpected execution: status={status}, stdout={output!r}, stderr={errors!r}")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("level", nargs="?", type=int, choices=range(4), default=0)
    args = parser.parse_args()
    if not (BUILD / "dtc").is_file():
        parser.error("Build dtc first; see README.md.")
    env = dict(os.environ, PATH="/usr/lib/llvm-18/bin:" + os.environ.get("PATH", ""))
    results = BUILD / f"control-flow-O{args.level}"
    sources = results / "sources"
    sources.mkdir(parents=True, exist_ok=True)
    records = []
    for case in cases():
        source = sources / (case["name"] + ".donato")
        source.write_text(textwrap.dedent(case["source"]).strip() + "\n")
        directory = results / case["name"]
        directory.mkdir(exist_ok=True)
        try:
            check(case, source, directory, args.level, env)
            record = dict(name=case["name"], passed=True)
            print(f"PASS {case['name']}", flush=True)
        except (RuntimeError, OSError, subprocess.TimeoutExpired) as error:
            record = dict(name=case["name"], passed=False, error=str(error))
            print(f"FAIL {case['name']}: {error}", flush=True)
        records.append(record)
    (results / "results.json").write_text(json.dumps(records, indent=2) + "\n")
    passed = sum(record["passed"] for record in records)
    print(f"{passed}/{len(records)} control-flow cases passed (-O {args.level}). Logs: {results}")
    return int(passed != len(records))


if __name__ == "__main__":
    raise SystemExit(main())
