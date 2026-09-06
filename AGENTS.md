# Repository instructions

## Repository skills

- For Git commit requests, use the repository's [commit skill](.agents/skills/commit/SKILL.md) to review the scope, select validation and write the commit message.
- For GitHub issues, feature requests and milestones, use the repository's [github-issues skill](.agents/skills/github-issues/SKILL.md) for authentication checks, publication commands and links to branches, PRs and commits.

## Project and sources of truth

- This repository implements the compiler for **Donato**, a new programming language, in C++ with an LLVM backend. The compiler executable is `dtc`; Donato source files use `.donato`.
- All currently available information about Donato lives in this codebase. Do not look for external Donato specifications or assume that syntax and semantics match another language. External documentation is appropriate for dependencies such as C++, LLVM and CMake.
- Read the relevant implementation before changing language behavior. `ProgramL-implemented.md` documents the current grammar; keep it consistent with the implementation. `ProgramL.md` is an initial design draft, not a definitive specification of implemented features, and must remain intact unless the user requests otherwise. Distinguish proposed features from behavior actually implemented in the tokenizer, parser, AST, type system and code generator.
- Read `README.md` for the existing build and usage instructions, checking them against the current code when details differ.

## Development conventions

- Develop the compiler in **C++26**, using only features supported by the selected Clang version and C++ standard library. Preserve the existing C runtime helpers and their integration where applicable.
- Keep changes consistent with the surrounding code: naming, formatting, header/source organization, architecture and error handling. Reuse existing abstractions and keep changes focused on the requested task.
- Follow the existing module boundaries: `src/parser` for tokenization and parsing, `src/ast` for AST nodes, `src/type` and `src/value` for types and values, `src/codegen` for LLVM integration, and `src/error_handling` for runtime support.

## Local environment and platform

- Environment checked on 2026-09-05: **Ubuntu 24.04.1 under WSL 2**, distribution name `Ubuntu`, with **Clang 23.1.0** installed from the official release in `/opt/llvm-23.1.0`, **LLVM 18.1.3** backend libraries, **CMake 3.28.3** and Make. The existing `/usr/bin/clang` and `llc` remain version 18 for Donato's executable-generation workflow.
- On this Windows PC, build and run through Ubuntu/WSL. From PowerShell in the repository root, prefix individual Linux commands with `wsl --distribution Ubuntu --exec`, or enter Ubuntu with `wsl --distribution Ubuntu`.
- The current build and execution workflow targets Linux. Native Windows support is unverified and requires portability work, including POSIX `unistd.h`/`getopt` usage and the executable-generation commands. Do not present WSL verification as native Windows verification.
- Build dependencies include a C/C++ compiler, CMake, LLVM development libraries, ZLIB, CURL and zstd. Generating a Donato executable also requires `llc` and `clang` on the Linux `PATH`.

## C++ standard and build commands

- `CMakeLists.txt` requires `-std=c++26`, checks that the compiler accepts it, and applies it to `dtc`. Keep C++26 as the project's build mode.
- Use **Clang 23.1.0** from `/opt/llvm-23.1.0/bin` for the C/C++ build. The LLVM libraries used by Donato's backend remain LLVM 18; select them explicitly with `LLVM_DIR` as below. Updating the C++ compiler does not require migrating the backend's LLVM API.
- Clang's C++26 implementation is partial. Verify support before using a newer feature, including its availability in the selected standard library. Do not assume that selecting C++26 makes every C++26 feature available. See the [Clang implementation status](https://clang.llvm.org/cxx_status.html).
- The explicit compiler option supports the installed CMake 3.28.3, whose `CXX_STANDARD=26` setting cannot select compiler flags. Do not replace it with that setting without also verifying the CMake version.

Run from the repository root inside Ubuntu/WSL:

```bash
cmake -S . -B build \
  -DCMAKE_C_COMPILER=/opt/llvm-23.1.0/bin/clang \
  -DCMAKE_CXX_COMPILER=/opt/llvm-23.1.0/bin/clang++ \
  -DLLVM_DIR=/usr/lib/llvm-18/lib/cmake/llvm
cmake --build build --parallel 2
```

When changing compilers in an existing build, add `--fresh` to the configuration command to clear the old compiler selection and cached flags. The README also documents the equivalent commands to run directly from Windows PowerShell.

To compile an existing Donato source file and execute the generated program:

```bash
cd build
env PATH=/usr/lib/llvm-18/bin:/usr/bin:/bin ./dtc -O 3 -o hello ../examples/hello.donato
./hello
```

Run `dtc` from the repository's `build/` directory: `src/codegen/codegen.cpp` currently locates runtime sources through `../src/error_handling`. Source paths passed to `dtc` are relative to that working directory. The default executable name is `output`; the implementation also supports `-o <name>`. Choose simple output names without spaces or shell syntax. Compile sequentially because `output.ll` and `output.o` are shared intermediate files. The CLI supports `-O 0` through `-O 3` (passed to `llc`), `-t` for signed narrowing checks and `-f` for signed overflow and integer division checks; see the README for details.

## Verification

- Baseline verified on 2026-09-05: CMake configuration and the complete `dtc` build succeeded with Clang 23.1.0 and `-std=c++26` on all 17 C++ translation units. All eight user-provided Donato examples compiled and ran at `-O 0`, with the expected output and exit status 0.
- Keep analysis reports local in the ignored `report/` directory. Do not commit or publish them on GitHub.
- After compiler changes, rebuild and exercise the affected behavior with a small `.donato` program based on implemented syntax. Check compiler diagnostics, executable generation and the generated program's result.
- The versioned `examples/` directory contains the eight original examples plus `hello.donato`, with expected output in `examples/expected/`. From the repository root, run `bash scripts/check-examples.sh 0` and `bash scripts/check-examples.sh 3` sequentially. Both runs passed all nine programs on 2026-09-05. The script selects LLVM 18 tools and saves logs under `build/examples-O<level>/`.
- Check diagnostics and the creation of a fresh executable: `dtc` currently ignores some external tool failures and LLVM verification results. `--help` currently returns status 1.
- Keep build products and temporary examples under the ignored `build/` directory. Keep reusable examples and their expected text output under versioned `examples/`; do not ignore `.donato` source files.
- Report the commands actually run and their results. Distinguish compilation success from successful execution and state any verification that could not be completed.
