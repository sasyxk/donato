#!/usr/bin/env python3
"""Compile real Donato inputs and check output, compiler diagnostics and LLVM IR."""

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

    def valid(name, source, values, merges=None, flags=(), observer=None):
        tests.append(dict(name=name, source=source, values=values,
                          merges=merges, flags=flags, observer=observer))

    def invalid(name, source, diagnostic, timeout=60, phase="parsing"):
        tests.append(dict(name=name, source=source, diagnostic=diagnostic,
                          flags=(), timeout=timeout, phase=phase))

    print_class = """
        class PrintBox {
            TYPE value;
        public:
            PrintBox(TYPE initial) { this.value = initial; return; }
            function TYPE read() { return this.value; }
            function ref TYPE get() { return this.value; }
            function void show() { print(this.value); return; }
        }
    """
    for type_name, bits in (("int8", 8), ("int16", 16), ("int32", 32),
                            ("int64", 64), ("int", 64), ("bool", 1)):
        if type_name == "bool":
            literals, numbers = ["false", "true"], [0, 1]
            first, next_value = "false", "true"
            first_number, next_number = 0, 1
        else:
            numbers = [-(2 ** (bits - 1)), 2 ** (bits - 1) - 1, -1, 0, 1]
            literals = [str(number) for number in numbers]
            first, next_value = "-1", "7"
            first_number, next_number = -1, 7
        body = "\n".join(
            f"{type_name} x{i} = {literal}; print(x{i}); print(x{i});"
            for i, literal in enumerate(literals)
        )
        for suffix, flags in (("", ()), ("_checked", ("-t", "-f"))):
            valid(f"print_bounds_{type_name}{suffix}",
                  function(body + "\nreturn 0;", "main", ""),
                  [number for number in numbers for _ in range(2)], flags=flags)

        # Calling typed functions after print checks that the original type and
        # address remain usable; aliases also observe later writes to storage.
        helpers = function("return x;", "echo", f"{type_name} x", type_name)
        helpers += function("count = count + 1; return x;", "touch",
                            f"ref {type_name} x, ref int count", f"ref {type_name}")
        valid(f"print_references_{type_name}", helpers + function(f"""
            {type_name} x = {first};
            {type_name}* p = &x;
            ref {type_name} alias = x;
            int count = 0;
            print(alias);
            print(*p);
            print(touch(x, count));
            print(count);
            print(echo(x));
            alias = {next_value};
            print(touch(x, count));
            print(count);
            print(*p);
            print(echo(x));
            return 0;
        """, "main", ""),
              [first_number] * 3 + [1, first_number, next_number, 2,
                                    next_number, next_number])

        aggregate = f"struct PrintPair {{ {type_name} value; {type_name} guard; }}\n"
        aggregate += print_class.replace("TYPE", type_name)
        valid(f"print_fields_{type_name}", aggregate + function(f"""
            {type_name} start = {first};
            {type_name} next = {next_value};
            PrintPair* pairPointer = new PrintPair(start, next);
            ref PrintPair pair = *pairPointer;
            PrintBox* boxPointer = new PrintBox(start);
            ref PrintBox box = *boxPointer;
            print(pair.value);
            print(pair.guard);
            print(box.read());
            print(box.get());
            box.show();
            ref {type_name} alias = box.get();
            alias = next;
            print(box.get());
            box.show();
            print(pair.value);
            print(pair.guard);
            delete pairPointer;
            delete boxPointer;
            return 0;
        """, "main", ""),
              [first_number, next_number] + [first_number] * 3
              + [next_number] * 2 + [first_number, next_number])

    valid("print_literals", function("""
        print(42); print(-1); print(false); print(true);
        print(if true then false else true);
        return 0;
    """, "main", ""), [42, -1, 0, 1, 0])
    valid("print_promoted_expression", function("""
        int8 x = -1;
        print(x + 0);
        print(x);
        return 0;
    """, "main", ""), [-1, -1])

    for name, declarations, body, actual_type in (
        ("double_literal", "", "print(1.5);", "double"),
        ("double_variable", "", "double x = 1.5; print(x);", "double"),
        ("double_expression", "", "print(1.5 + 2.5);", "double"),
        ("double_reference", "function ref double get(ref double x) { return x; }",
         "double x = 1.5; print(get(x));", "double"),
        ("double_method", print_class.replace("TYPE", "double").replace(
            "print(this.value);", ""),
         "PrintBox* p = new PrintBox(1.5); ref PrintBox box = *p; print(box.get());",
         "double"),
        ("pointer", "", "int x = 7; int* p = &x; print(p);", "PointerType to int64"),
        ("null_pointer", "", "print(nullptr<int>);", "PointerType to int64"),
        ("pointer_reference", "function ref int* get(ref int* x) { return x; }",
         "int* p = nullptr<int>; print(get(p));", "PointerType to int64"),
        ("struct", "struct PrintPair { int value; }",
         "PrintPair* p = new PrintPair(7); ref PrintPair pair = *p; print(pair);",
         "PrintPairstruct"),
        ("class", print_class.replace("TYPE", "int"),
         "PrintBox* p = new PrintBox(7); ref PrintBox box = *p; print(box);",
         "classPrintBox"),
    ):
        invalid(f"print_reject_{name}", declarations + function(body + " return 0;", "main", ""),
                "print only supports signed integers and bool; got " + actual_type,
                phase="codegen")
    invalid("print_reject_void", "function void nothing() { return; }"
            + function("print(nothing()); return 0;", "main", ""),
            "VoidType does not support createValue.", phase="codegen")

    top_level_error = "Expected 'function', 'struct' or 'class' at top level, got "
    main_function = function("print(1); return 0;", "main", "")
    helper_function = function("return 1;", "helper", "")
    for name, statement, token in (
        ("variable", "int x = 1;", "int"),
        ("auto", "auto x = 1;", "auto"),
        ("reference", "ref int x = 1;", "ref"),
        ("assignment", "x = 1;", "x"),
        ("print", "print(1);", "print"),
        ("call", "helper();", "helper"),
        ("return", "return 0;", "return"),
        ("if", "if (true) { print(1); }", "if"),
        ("while", "while (false) { print(1); }", "while"),
        ("delete", "delete nullptr<int>;", "delete"),
    ):
        for place, prefix, suffix in (
            ("before_function", "", main_function),
            ("between_functions", helper_function, main_function),
            ("after_function", helper_function + main_function, ""),
        ):
            invalid(f"top_level_{name}_{place}", prefix + statement + "\n" + suffix,
                    top_level_error + f"'{token}'", timeout=5)

    for kind, declaration in (
        ("struct", "struct Sample { int value; }\n"),
        ("class", "class Sample { public: Sample() { return; } }\n"),
    ):
        for name, statement, token in (
            ("variable", "int x = 1;", "int"),
            ("aggregate_pointer", "Sample* x = nullptr<Sample>;", "Sample"),
        ):
            invalid(f"top_level_{name}_after_{kind}",
                    declaration + statement + "\n" + main_function,
                    top_level_error + f"'{token}'", timeout=5)

    for name, source in (("empty", b""), ("comments_only", b"/* closed */ // eof")):
        invalid(f"top_level_{name}", source, top_level_error + "''", timeout=5)
    for keyword, declaration in (
        ("function", helper_function),
        ("struct", "struct Sample { int value; }"),
        ("class", "class Sample { public: Sample() { return; } }"),
    ):
        invalid(f"top_level_nested_{keyword}",
                function(declaration + "\nreturn 0;", "main", ""),
                f"Unexpected Statement token '{keyword.upper()}'", timeout=5)
    valid("top_level_local_variable", function("int x = 1; print(x); return 0;", "main", ""), [1])

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

    double_calls = """
        print(eval(1.0, 2.0));
        print(eval(0.0, 0.0));
        print(eval(-1.0, -1.0));
        print(eval(2.0, 1.0));
        return 0;
    """
    for form, body in (
        ("if", "if (x == y) { return 1; } else { return 0; }"),
        ("while", "while (x == y) { return 1; } return 0;"),
        ("inline", "return if x == y then 1 else 0;"),
    ):
        valid(f"double_equality_{form}",
              function(body, params="double x, double y")
              + function(double_calls, "main", ""), [0, 1, 1, 0])

    double_exports = ""
    for name, expression in (
        ("dadd", "x + y"), ("dsub", "x - y"),
        ("dmul", "x * y"), ("ddiv", "x / y"),
        ("dchain", "(x + y) * (x - y) / y"),
    ):
        double_exports += function(f"return {expression};", name,
                                   "double x, double y", "double")
    for name, expression in (("dneg", "-x"), ("dcube", "x * x * x")):
        double_exports += function(f"return {expression};", name, "double x", "double")
    for name, operator in (
        ("deq", "=="), ("dneq", "!="), ("dlt", "<"),
        ("dlte", "<="), ("dgt", ">"), ("dgte", ">="),
    ):
        double_exports += function(f"return if x {operator} y then 1 else 0;",
                                   name, "double x, double y")
    valid("double_c_observer", double_exports + function("return 0;", "main", ""), [],
          observer=ROOT / "scripts" / "fixtures" / "double-observer.c")

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

    reference_class = """
        class Box {
            TYPE value;
        public:
            Box(TYPE initial) { this.value = initial; return; }
            function TYPE read() { return this.value; }
            function ref TYPE get() { return this.value; }
            function ref TYPE forward() { return this.get(); }
            function void set(TYPE next) { this.value = next; return; }
        }
    """
    valid("class_reference_original", reference_class.replace("TYPE", "int")
          + function("""
              Box* p = new Box(7);
              ref Box box = *p;
              ref int alias = box.get();
              alias = 9;
              print(alias);
              print(box.read());
              delete p;
              return 0;
          """, "main", ""), [9, 9])

    for name, type_name, setup in (
        ("int8", "int8", "int8 start = 7; int8 next = 9;"),
        ("int16", "int16", "int16 start = 7; int16 next = 9;"),
        ("int32", "int32", "int32 start = 7; int32 next = 9;"),
        ("int64", "int64", "int64 start = 7; int64 next = 9;"),
        ("int", "int", "int start = 7; int next = 9;"),
        ("double", "double", "double start = 7.0; double next = 9.0;"),
        ("bool", "bool", "bool start = false; bool next = true;"),
        ("pointer", "int*", "int first = 7; int second = 9; "
         "int* start = &first; int* next = &second;"),
        ("pointer_pointer", "int**", "int first = 7; int second = 9; "
         "int* p = &first; int* q = &second; int** start = &p; int** next = &q;"),
    ):
        source = function("return value;", "getref", f"ref {type_name} value",
                          f"ref {type_name}") + reference_class.replace("TYPE", type_name)
        body = setup + """
            auto allocation = new Box(start);
            ref Box box = *allocation;
            TYPE copy = box.get();
            ref TYPE alias = box.get();
            alias = next;
            print(if box.read() == next then 1 else 0);
            print(if box.get() == next then 1 else 0);
            print(if copy == start then 1 else 0);
            ref TYPE forwarded = box.forward();
            forwarded = start;
            print(if box.read() == start then 1 else 0);
            box.set(next);
            print(if alias == next then 1 else 0);
            ref TYPE freealias = getref(alias);
            freealias = start;
            print(if box.read() == start then 1 else 0);
            delete allocation;
            return 0;
        """.replace("TYPE", type_name)
        valid(f"class_reference_{name}", source + function(body, "main", ""), [1] * 6)

    for kind, declaration, read, update in (
        ("struct", "struct Item { int value; }", "return item.value;", "alias.value = 9;"),
        ("class", """class Item {
            int value;
        public:
            Item(int initial) { this.value = initial; return; }
            function int read() { return this.value; }
            function void set(int next) { this.value = next; return; }
        }""", "return item.read();", "alias.set(9);"),
    ):
        source = declaration + """
            class Holder {
                Item* value;
            public:
                Holder(Item* initial) { this.value = initial; return; }
                function ref Item get() { return *this.value; }
                function int read() { ref Item item = *this.value; READ }
            }
        """.replace("READ", read)
        valid(f"class_reference_{kind}", source + function("""
            auto item = new Item(7);
            auto allocation = new Holder(item);
            ref Holder holder = *allocation;
            ref Item alias = holder.get();
            UPDATE
            print(holder.read());
            delete allocation;
            delete item;
            return 0;
        """.replace("UPDATE", update), "main", ""), [9])

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

    eof_class_prefix = b"class Broken { public: Broken() { return; } "
    for name, source, delimiter, context in (
        ("constructor_parameters", b"class Broken { public: Broken(",
         ")", "parameters of constructor 'Broken'"),
        ("constructor_body", b"class Broken { public: Broken() { return;",
         "}", "body of constructor 'Broken'"),
        ("method_parameters", eof_class_prefix + b"function int get(",
         ")", "parameters of method 'Broken.get'"),
        ("method_body", eof_class_prefix + b"function int get() { return 1;",
         "}", "body of method 'Broken.get'"),
        ("ref_method_parameters", eof_class_prefix + b"function ref int get(",
         ")", "parameters of method 'Broken.get'"),
        ("ref_method_body", eof_class_prefix + b"function ref int get() { return value;",
         "}", "body of method 'Broken.get'"),
        ("ref_pointer_parameters", eof_class_prefix + b"function ref int** get(",
         ")", "parameters of method 'Broken.get'"),
        ("ref_pointer_body", eof_class_prefix + b"function ref int** get() { return value;",
         "}", "body of method 'Broken.get'"),
    ):
        for ending_name, ending in (("eof", b""), ("lf", b"\n"), ("crlf", b"\r\n")):
            invalid(f"eof_{name}_{ending_name}", source + ending,
                    f"Unexpected end of input: expected '{delimiter}' in {context}", timeout=5)

    for depth in (1, 2, 5):
        for kind, prefix, statement, context in (
            ("constructor", b"class Broken { public: Broken() { ", b"return;",
             "constructor 'Broken'"),
            ("method", eof_class_prefix + b"function int get() { ", b"return 1;",
             "method 'Broken.get'"),
        ):
            invalid(f"eof_{kind}_nested_{depth}",
                    prefix + b"if (true) { " * depth + statement + b" }" * depth,
                    f"Unexpected end of input: expected '}}' in body of {context}", timeout=5)

    invalid("eof_parameters_after_block_comment",
            b"class Broken { public: Broken(/* ) } */",
            "Unexpected end of input: expected ')' in parameters of constructor 'Broken'", timeout=5)
    invalid("eof_body_after_block_comment",
            eof_class_prefix + b"function int get() { return 1; /* } */",
            "Unexpected end of input: expected '}' in body of method 'Broken.get'", timeout=5)
    invalid("eof_body_after_line_comment",
            eof_class_prefix + b"function int get() { return 1; // }",
            "Unexpected end of input: expected '}' in body of method 'Broken.get'", timeout=5)
    invalid("eof_class_after_constructor", eof_class_prefix,
            "Unexpected Statement Token: ''", timeout=5)
    invalid("eof_class_after_method", eof_class_prefix + b"function int get() { return 1; }",
            "Unexpected end of input", timeout=5)
    invalid("eof_before_constructor_parameters", b"class Broken { public: Broken",
            "Unexpected end of input", timeout=5)
    invalid("eof_before_constructor_body", b"class Broken { public: Broken()",
            "Unexpected end of input", timeout=5)

    valid("class_eof_closed_comments", """class Sample {
        int value;
    public:
        Sample(int x /* ) } */) { this.value = x; return; /* } */ }
        function int get(/* ) } */) { return this.value; /* } */ }
    }
    function int main() {
        auto p = new Sample(7);
        ref Sample object = *p;
        print(object.get());
        delete p;
        return 0;
    }""", [7])
    for name, methods in (("constructor", b""),
                          ("method", b"function int get() { return 7; }")):
        valid(f"class_eof_complete_{name}",
              b"function int main() { print(7); return 0; }\n"
              b"class Sample { public: Sample() { return; } " + methods + b"}", [7])
    return tests


def run(command, directory, prefix, env, timeout=60):
    with (directory / f"{prefix}.stdout.txt").open("w") as out, \
         (directory / f"{prefix}.stderr.txt").open("w") as err:
        result = subprocess.run(command, cwd=BUILD, env=env, stdout=out,
                                stderr=err, timeout=timeout)
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
         str(binary.relative_to(BUILD)), str(source)], directory, "compile", env,
        timeout=case.get("timeout", 60))
    if "diagnostic" in case:
        prefix = f"Error in {case['phase']}::"
        if status != 1 or prefix not in errors or case["diagnostic"] not in errors:
            raise RuntimeError(f"wrong {case['phase']} result (status {status}): {errors.strip()}")
        if case["phase"] == "codegen" and errors != f"{prefix} {case['diagnostic']}\n":
            raise RuntimeError(f"unexpected codegen diagnostic: {errors.strip()}")
        if any(product.exists() for product in (binary, BUILD / "output.ll", BUILD / "output.o")):
            raise RuntimeError("rejected source produced codegen output")
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
    if case.get("observer"):
        check_observer(case["observer"], directory, env)


def check_observer(source, directory, env):
    # Rename only main in a copy of the object emitted by dtc.
    original = directory / "output.o"
    renamed = directory / "observer.o"
    binary = directory / "observer"
    for product in (original, renamed, binary):
        product.unlink(missing_ok=True)
    shutil.copyfile(BUILD / "output.o", original)
    commands = (
        ("rename", ["objcopy", "--redefine-sym", "main=donatomain", str(original), str(renamed)]),
        ("link", ["clang", "-std=c17", "-Wall", "-Wextra", "-Werror", str(source), str(renamed),
                  str(ROOT / "src" / "error_handling" / "errors.c"), "-lm", "-o", str(binary)]),
        ("run", [str(binary)]),
    )
    for step, command in commands:
        status, output, errors = run(command, directory, f"observer-{step}", env)
        if status != 0 or errors:
            raise RuntimeError(f"C observer {step} failed (status {status}): {errors.strip()}")
        if step == "run" and output != "PASS double observer\n":
            raise RuntimeError(f"unexpected C observer output: {output!r}")


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
        if isinstance(case["source"], bytes):
            # EOF cases preserve the exact final bytes, including LF and CRLF.
            source.write_bytes(case["source"])
        else:
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
