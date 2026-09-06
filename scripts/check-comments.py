#!/usr/bin/env python3
"""Check comment parsing, opening locations and exact EOF bytes with real inputs."""

import argparse
import json
import os
from pathlib import Path
import resource
import shutil
import subprocess


ROOT = Path(__file__).resolve().parent.parent
BUILD = ROOT / "build"


def cases():
    tests = []
    main = b"function int main() { return 0; }"
    printing_main = b"function int main() { print(7); return 0; }"

    def valid(name, source, output="value: 7\n"):
        tests.append(dict(name=name, source=source, output=output))

    def invalid(name, source, diagnostic):
        tests.append(dict(name=name, source=source, diagnostic=diagnostic))

    def unterminated(name, source, row, col):
        invalid(name, source,
                f"Error in tokenizer:: Unterminated block comment [{row}, {col}]\n")

    for place, prefix, col in (
        ("start", b"", 1),
        ("after_return", b"function int main() { return 0; ", 33),
        ("after_function", main + b" ", 35),
    ):
        for tail_name, tail in (
            ("bare", b""), ("space", b" "), ("letter", b" text"),
            ("digit", b" 7"), ("brace", b" }"), ("star", b"*"),
            ("slash", b"/"), ("unknown", b" @"),
        ):
            for ending_name, ending in (("eof", b""), ("lf", b"\n"), ("crlf", b"\r\n")):
                unterminated(f"{place}_{tail_name}_{ending_name}",
                             prefix + b"/*" + tail + ending, 1, col)

    unterminated("multiline_lf", b"\n\n    /* first\nsecond }", 3, 5)
    unterminated("multiline_crlf", b"\r\n\r\n    /* first\r\nsecond }", 3, 5)
    unterminated("after_closed_comment", b"/**/ /*", 1, 6)
    unterminated("after_line_comment", b"// /* ignored\n  /*", 2, 3)
    unterminated("after_multiline_comment", b"/* closed\n */\n    /*", 3, 5)
    unterminated("after_function_multiline", main + b"\n/* first\n }", 2, 1)
    unterminated("second_trailing_comment", main + b"/**//*", 1, 38)
    unterminated("opening_inside_comment", b"/* outer /*", 1, 1)
    unterminated("class_prescan", b"class Sample {\npublic:\n"
                 b"    Sample() { return;\n        /* } }\n", 4, 9)

    valid("without_comments", printing_main)
    for name, comment in (
        ("empty", b"/**/"), ("stars", b"/***/"),
        ("text", b"/* comment */"),
        ("multiline", b"/* first\nsecond */"),
        ("multiline_crlf", b"/* first\r\nsecond */"),
        ("consecutive", b"/**//**/"),
    ):
        valid(f"closed_{name}_at_eof", printing_main + comment)

    valid("before_first_token", b"/* comment */" + printing_main)
    valid("after_return", b"function int main() { print(7); return 0; /* } @ */ }")
    valid("line_after_return", b"function int main() { print(7); return 0; // } @\n}")
    valid("after_terminating_if", b"function int eval(bool x) {\n"
          b"if (x) { return 7; } else { return 9; } /* } @ */\n}\n"
          b"function int main() { print(eval(true)); print(eval(false)); return 0; }",
          "value: 7\nvalue: 9\n")
    valid("between_tokens", b"function/**/int/**/main() { print(8/**//**/ / /**/2); return 0; }",
          "value: 4\n")
    valid("non_nested", b"/* outer /* inner */" + printing_main)
    valid("delimiters_inside_comment", b"/* // { } return 9; @ * /\n */" + printing_main)
    valid("line_comment_at_eof", printing_main + b"// /* @ }")
    valid("line_comment_before_code", b"// /* ignored\r\n" + printing_main)
    valid("division", b"function int main() { print(8 / 2); return 0; }", "value: 4\n")

    # Preserve ordinary parser diagnostics and the trailing slash operator.
    invalid("slash_at_eof", main + b"/",
            "Error in parsing:: Unexpected Statement Token: '/' [1, 34]\n")
    invalid("missing_semicolon", b"function int main() { return 0 /**/ }",
            "Error in parsing:: Unexpected tokens '}' after expression [1, 37]\n")
    invalid("unknown_first_token", b"@",
            "Error in parsing:: Unknown token: @ [1, 1]\n")
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
    ir = BUILD / "output.ll"
    obj = BUILD / "output.o"
    # Remove only these known products so stale artifacts cannot pass a check.
    for product in (binary, ir, obj):
        product.unlink(missing_ok=True)
    status, output, errors = run(
        [str(BUILD / "dtc"), "-O", str(level), "-o",
         str(binary.relative_to(BUILD)), str(source)], directory, "compile", env)
    if "diagnostic" in case:
        if status != 1 or output or errors != case["diagnostic"]:
            raise RuntimeError(f"wrong rejection: status={status}, stdout={output!r}, stderr={errors!r}")
        if any(product.exists() for product in (binary, ir, obj)):
            raise RuntimeError("rejected source produced codegen output")
        return
    if status != 0 or errors or not all(product.is_file() for product in (binary, ir, obj)):
        raise RuntimeError(f"compilation failed or artifacts missing (status {status}): {errors.strip()}")
    shutil.copyfile(ir, directory / "output.ll")
    shutil.copyfile(obj, directory / "output.o")
    status, _, errors = run(["opt", "-passes=verify", "-disable-output", str(ir)],
                            directory, "verify", env)
    if status != 0 or errors:
        raise RuntimeError(f"LLVM verification failed: {errors.strip()}")
    (directory / "expected.txt").write_text(case["output"])
    status, output, errors = run([str(binary)], directory, "run", env)
    if status != 0 or errors or output != case["output"]:
        raise RuntimeError(f"unexpected execution: status={status}, stdout={output!r}, stderr={errors!r}")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("level", nargs="?", type=int, choices=range(4), default=0)
    args = parser.parse_args()
    if not (BUILD / "dtc").is_file():
        parser.error("Build dtc first; see README.md.")
    # Malformed inputs must fail cleanly; suppress core files if a regression aborts.
    resource.setrlimit(resource.RLIMIT_CORE, (0, 0))
    env = dict(os.environ, PATH="/usr/lib/llvm-18/bin:" + os.environ.get("PATH", ""))
    results = BUILD / f"comments-O{args.level}"
    sources = results / "sources"
    sources.mkdir(parents=True, exist_ok=True)
    records = []
    for case in cases():
        source = sources / (case["name"] + ".donato")
        # EOF and CRLF are part of the input: never strip or append a newline.
        source.write_bytes(case["source"])
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
    print(f"{passed}/{len(records)} comment cases passed (-O {args.level}). Logs: {results}")
    return int(passed != len(records))


if __name__ == "__main__":
    raise SystemExit(main())
