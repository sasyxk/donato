# Donato examples

These source files and their expected output are versioned so a fresh clone can be tried immediately. Build the compiler using the [root README](../README.md), then run from the repository root inside Ubuntu:

```bash
bash scripts/check-examples.sh 0
bash scripts/check-examples.sh 3
```

The script compiles and runs each example, compares its output with `expected/<name>.txt`, and keeps executables and logs under `build/examples-O<level>/`. Expected files contain the generated program's output; allocation statistics printed by `dtc` are recorded separately.

| Example | Demonstrates |
| --- | --- |
| `hello.donato` | Entry point, `print(42)` and `return 0` |
| `example1.donato` | Initialization, inline `if`, nested `let`, primitive types |
| `example2.donato` | Functions, references, value parameters and `while` |
| `example3.donato` | Recursive Fibonacci |
| `example4.donato` | Structs, pointer chains and references |
| `example5.donato` | Recursive linked list of structs and deallocation |
| `example6.donato` | Classes, constructors and a linked list |
| `example7.donato` | Doubly linked list, insertion and reverse traversal |
| `example8.donato` | Cycle detection and removal in a linked list |

`example1.donato` through `example8.donato` preserve the user-provided sources. Some original comments describe earlier behavior: narrowing checks require `-t`, and nested `let` bindings restore the outer scope when they end. These examples exercise particular paths; their success does not certify memory-leak freedom or every operation supported by each class.
