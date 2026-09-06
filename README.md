# Donato Language

**Donato** is a programming language developed for my master's thesis, carried out
at the University of Twente and presented at the University of Parma. Its compiler,
`dtc`, is written in C++ and translates `.donato` sources through a parser and AST
into LLVM IR, then into Linux executables.

The implemented language is documented in [ProgramL-implemented.md](ProgramL-implemented.md)
and demonstrated in [examples/](examples/). [ProgramL.md](ProgramL.md) is the original
design draft and includes features that are not implemented. This repository is
the source of truth for Donato; there is no external language specification.

## Supported setup

The verified configuration is **Ubuntu 24.04 on x86_64**, including Ubuntu inside
**WSL 2 on Windows**, with:

| Component | Version / purpose |
| --- | --- |
| C++ build mode | C++26 (`-std=c++26`) |
| C/C++ compiler used by CMake | Clang 23.1.0 in `/opt/llvm-23.1.0` |
| C++ standard library | Ubuntu's libstdc++ 13 |
| Donato backend libraries | LLVM 18 (18.1.3 in the verified environment) |
| Donato executable generation | LLVM 18 `llc` and Clang 18 |
| Build tools | CMake 3.28.3 and Make |

Clang 23 builds the compiler itself; LLVM 18 libraries and tools generate Donato
programs. These versions are selected separately in the commands below.
[C++26 support in Clang is partial](https://clang.llvm.org/cxx_status.html);
new C++ code must use features available in both the compiler and standard library.

The documented workflow produces Linux executables. Native Windows builds and
other distributions or architectures have not been verified.

## Windows: prepare Ubuntu through WSL

If Ubuntu 24.04 is already installed in WSL 2, use that distribution. Otherwise,
run this in **PowerShell as Administrator**, restart if requested, then complete
Ubuntu's first-run username and password setup:

```powershell
wsl --install -d Ubuntu-24.04
```

See Microsoft's [WSL installation instructions](https://learn.microsoft.com/en-us/windows/wsl/install)
for Windows requirements. List installed distributions and open yours:

```powershell
wsl --list --verbose
$Distro = 'Ubuntu-24.04'
wsl -d $Distro
```

Use the exact listed name: on the development PC it is `Ubuntu`, so use
`$Distro = 'Ubuntu'` there. Inside Ubuntu, follow the Linux setup below.
If you already have a checkout on Windows, open PowerShell in that repository
before entering Ubuntu and skip the clone step. WSL then works on the same files.

## Linux / Ubuntu: install and build

Run these commands in **Bash inside Ubuntu**, not directly in PowerShell.
The setup requires Internet access and `sudo` for system dependencies. Check the
environment first: `cat /etc/os-release` should show Ubuntu 24.04 and `uname -m`
should show `x86_64`.

### 1. Install prerequisites

```bash
sudo apt update
sudo apt install -y git curl ca-certificates xz-utils cmake make \
  libstdc++-13-dev clang-18 llvm-18 llvm-18-dev \
  zlib1g-dev libcurl4-openssl-dev libzstd-dev
```

These packages include the development headers needed by CMake and the Linux
compiler/linker dependencies. If Ubuntu cannot find `clang-18` or `llvm-18-dev`,
enable the Ubuntu `universe` repository and repeat the installation:

```bash
sudo apt install -y software-properties-common
sudo add-apt-repository -y universe
sudo apt update
```

### 2. Get the repository

For a new checkout:

```bash
git clone https://github.com/sasyxk/donato.git
cd donato
```

SSH also works if your GitHub key is configured:
`git clone git@github.com:sasyxk/donato.git`.
For an existing checkout, enter its root directory instead.

### 3. Install Clang 23.1.0

Use the official [LLVM 23.1.0 Linux X64 release](https://github.com/llvm/llvm-project/releases/tag/llvmorg-23.1.0).
The archive is about 2 GB and the extracted toolchain occupies about 12 GB;
allow additional space for Ubuntu packages and the build. The following block,
run from the repository root, downloads it under the ignored `build/` directory,
checks its SHA-256 and installs it under `/opt`. It skips installation if the
compiler already exists at that path.

```bash
(
  set -eu
  if [ "$(uname -m)" != x86_64 ]; then
    echo "These instructions require Linux x86_64." >&2
    exit 1
  fi
  if [ ! -x /opt/llvm-23.1.0/bin/clang++ ]; then
    if [ -e /opt/llvm-23.1.0 ]; then
      echo "Incomplete installation at /opt/llvm-23.1.0; inspect it before retrying." >&2
      exit 1
    fi
    mkdir -p build/downloads
    archive=build/downloads/LLVM-23.1.0-Linux-X64.tar.xz
    curl --fail --location --retry 3 \
      https://github.com/llvm/llvm-project/releases/download/llvmorg-23.1.0/LLVM-23.1.0-Linux-X64.tar.xz \
      --output "$archive"
    printf '%s  %s\n' \
      18da30f77f475688a18f7704d23f9f155ae007ed9922dbed6850a9419d9fec8c \
      "$archive" | sha256sum --check
    sudo mkdir /opt/llvm-23.1.0
    sudo tar -xJf "$archive" -C /opt/llvm-23.1.0 --strip-components=1 --no-same-owner
    rm -- "$archive"
  fi
  /opt/llvm-23.1.0/bin/clang++ --version
)
```

Check the selected tools before configuring:

```bash
/opt/llvm-23.1.0/bin/clang++ --version
/usr/lib/llvm-18/bin/llvm-config --version
/usr/lib/llvm-18/bin/llc --version
/usr/lib/llvm-18/bin/clang --version
cmake --version
```

The first command should report 23.1.0; the three LLVM 18 commands should report
18.x. Installing Clang 23 this way does not change the system's default compiler.

### 4. Build dtc

From the repository root:

```bash
cmake -S . -B build \
  -DCMAKE_C_COMPILER=/opt/llvm-23.1.0/bin/clang \
  -DCMAKE_CXX_COMPILER=/opt/llvm-23.1.0/bin/clang++ \
  -DLLVM_DIR=/usr/lib/llvm-18/lib/cmake/llvm
cmake --build build --parallel 2
```

CMake checks support for `-std=c++26` and applies it to `dtc`. When changing
compilers in an existing build, use `cmake --fresh -S . -B build` with the same
`-D` arguments to clear the previous compiler selection; `--fresh` requires
CMake 3.24 or newer. Rebuild after changing C++ sources.

## Compile and run a Donato program

From the repository root in Ubuntu:

```bash
cd build
env PATH=/usr/lib/llvm-18/bin:/usr/bin:/bin ./dtc -O 3 -o hello ../examples/hello.donato && ./hello
```

The program prints:

```text
value: 42
```

`dtc` also prints its own allocation statistics during compilation. The sample is:

```text
function int main() {
    print(42);
    return 0;
}
```

**Run `dtc` from this repository's `build/` directory.** The compiler currently
finds the runtime C sources through `../src/error_handling`. Source paths are
relative to that working directory. Quote source and output paths containing
spaces and use Linux forward slashes. Tools receive separate arguments directly,
without a shell command.

The default executable name is `output`; run it with `./output`. Each compilation
also writes `output.ll` and `output.o` in the working directory, even with `-o`.
Compile programs sequentially in that directory. Editing a `.donato` file
requires recompiling that file, without rebuilding `dtc`. Input, output and
intermediate paths must identify distinct files.

LLVM verification, IR writing, object generation and linking errors stop
compilation with status `1`. `dtc` replaces the requested executable only after
all stages succeed, using a temporary directory on the destination filesystem.
If executable generation fails, an existing executable is preserved and the
diagnostic says it was not updated; intermediate files may be incomplete or left
from a previous run.
Run the program only when compilation succeeds, for example:

```bash
env PATH=/usr/lib/llvm-18/bin:/usr/bin:/bin ./dtc -o hello ../examples/hello.donato && ./hello
```

### Compiler options

```text
./dtc [-O <0|1|2|3>] [-t] [-f] [-o <name>] <file.donato>
```

| Option | Current behavior |
| --- | --- |
| `-O 0`, `-O 1`, `-O 2`, `-O 3` | Select the optimization level passed to LLVM's `llc`. Default: `0`. The compact form `-O3` also works. |
| `-o <name>` | Name of the generated executable. Default: `output`. |
| `-t` | Enable runtime checks for data loss when narrowing signed integers. Off by default. |
| `-f` | Enable runtime checks for signed arithmetic overflow and integer division by zero. Off by default. |
| `--help` | Print usage. Currently exits with status `1`. |

The entire `-O` value must be a decimal integer between 0 and 3. Empty values,
non-numeric text, suffixes such as `2junk`, fractions, oversized integers,
whitespace and a leading `+` are rejected with a diagnostic and status `1` before
compilation. Leading zeros and negative zero remain valid: `02` selects level 2
and `-0` selects level 0. If `-O` is repeated, every value must be valid and the
last one selects the optimization level.

The optimization setting controls [LLVM machine-code generation via `llc`](https://llvm.org/docs/CommandGuide/llc.html#options).
It does not select the CMake build type or add a full LLVM IR optimization pipeline.
For example, compile a program with level 3 and both runtime checks:

```bash
env PATH=/usr/lib/llvm-18/bin:/usr/bin:/bin ./dtc -O3 -t -f -o checked ../examples/hello.donato && ./checked
```

### From Windows PowerShell, staying in the project folder

After completing the Ubuntu setup, keep PowerShell in the repository root.
Choose your installed distribution name and configure:

```powershell
$Distro = 'Ubuntu'
wsl -d $Distro --exec cmake --fresh -S . -B build '-DCMAKE_C_COMPILER=/opt/llvm-23.1.0/bin/clang' '-DCMAKE_CXX_COMPILER=/opt/llvm-23.1.0/bin/clang++' '-DLLVM_DIR=/usr/lib/llvm-18/lib/cmake/llvm'
wsl -d $Distro --exec cmake --build build --parallel 2
```

Use `Ubuntu-24.04` instead if that is your distribution's name. Keep the `-D`
arguments quoted in PowerShell. After configuration, only the build command is
needed for subsequent C++ changes.

Compile and run the included sample:

```powershell
wsl -d $Distro --cd "$PWD\build" --exec env PATH=/usr/lib/llvm-18/bin:/usr/bin:/bin ./dtc -O 3 -o hello ../examples/hello.donato
if ($LASTEXITCODE -eq 0) { wsl -d $Distro --cd "$PWD\build" --exec ./hello }
```

`--cd` makes the Linux process run in `build/` while PowerShell stays in the
repository root. Output appears directly in the Windows terminal. Replace
`../examples/hello.donato` with your source file's path relative to `build/`.
These are Linux executables running through WSL.

## Check the included examples

From the repository root in Ubuntu:

```bash
bash scripts/check-examples.sh 0
bash scripts/check-examples.sh 3
```

Or from PowerShell in the same folder:

```powershell
wsl -d $Distro --exec bash scripts/check-examples.sh 0
wsl -d $Distro --exec bash scripts/check-examples.sh 3
```

Each run should report `9/9 examples passed`. The script checks compilation,
creation of a fresh executable, execution status and exact output against
[examples/expected/](examples/expected/). Executables and logs go to
`build/examples-O0/` or `build/examples-O3/`. See the [example catalogue](examples/README.md).

### Check control flow and numeric operations

After rebuilding `dtc`, run the input/output checks with Python 3 from the
repository root in Ubuntu:

```bash
python3 scripts/check-control-flow.py 0
python3 scripts/check-control-flow.py 3
```

From PowerShell, prefix each command with `wsl -d $Distro --exec`.
The script generates `.donato` programs under `build/control-flow-O<level>/sources/`,
compiles them sequentially and compares actual output with expected output.
It covers `if`/`else` trees up to five levels, partially returning branches,
loops, methods, constructors, and inline `if` branches with pointer dereferences
or calls returning `ref` values. Method reference-return tests check mutation
through aliases for all primitive types, pointers and aggregates, including
references forwarded through another method. It also checks integer conditions
at every supported width (including the `int` alias) in `if`, `while` and inline `if`,
with boolean controls. Invalid programs must fail in the expected parsing or
code-generation phase with the expected diagnostic, before LLVM IR, an object
or an executable is generated.
Valid programs must also pass LLVM 18 `opt` verification and checks for unnecessary
merge blocks. Each case retains its source, logs and, when valid, IR, executable
and expected output under `build/control-flow-O<level>/`.

Print checks cover signed integers of every supported width and booleans,
including signed limits, repeated printing, and `-t`/`-f` checks. Smaller integers
are promoted only for the runtime call, and booleans print `0` or `1`. References,
dereferences, struct fields and class methods check that printing preserves
storage and types, and an expression with side effects is evaluated once.
Unsupported types such as `double`, pointers and aggregates must produce a
controlled code-generation diagnostic with status `1` and no new artifacts.

Top-level checks reject variables and other statements before, between and after
functions, and variable declarations after structs and classes. They require
status `1`, a diagnostic naming the allowed declarations and no new IR, object
or executable. Empty and comment-only inputs and declarations nested inside a
function are also rejected; local variables remain valid.

Type-name checks reject duplicate struct and class names during parsing, including
conflicts between the two categories in either declaration order, equal layouts,
unused types and declarations separated by functions. Each rejection must return
status `1`, identify the duplicate and produce no new IR, object or executable.
Valid controls use distinct, case-sensitive names in allocations, fields, methods
and function parameters, and check pointers to the type being declared.

Pointer-negation checks require unary `-p` to return a boolean: true for null and
false otherwise. They cover primitive and aggregate pointers, depths through six
levels, aliases, fields, value/reference returns, single evaluation, conditions,
assignments and boolean combinations. They also preserve numeric/boolean negation
and the rejection of mixed types and binary pointer arithmetic. Valid cases must
verify LLVM IR and run fresh executables with exact output; rejected cases require
a controlled diagnostic, status `1` and no new compilation artifacts.

Parameter and argument list checks reject trailing commas in functions, methods,
constructors and `new`, including calls through `this`, comments after commas,
and LF/CRLF endings. Leading and repeated commas and comma-only lists are rejected
too. Valid controls exercise empty lists, nested calls, references, pointers and
comments between elements. Truncated lists must fail with a parsing diagnostic,
status `1` and no new IR, object or executable.

Incomplete class tests check constructor and method parameter lists and bodies,
including nested braces, comments and reference return types. These inputs
preserve EOF, LF and CRLF exactly and must fail with status `1`, the expected
syntax diagnostic and no new IR, object or executable. A five-second compilation
timeout makes a parser hang fail the test. Valid classes ending exactly at EOF
must still compile and run normally.

The same suite tests `double` equality in `if`, `while` and inline `if`. Its
[C observer](scripts/fixtures/double-observer.c) independently checks arithmetic,
chained expressions, all six comparisons, signed zero, infinities and NaN.
It copies the object generated by `dtc`, renames only its `main` symbol with
`objcopy` (from `binutils`), and links the observer with Clang 18 and the original
C runtime. The checks compare numeric results without using Donato's equality
or `print`, which does not support `double`; observer build and execution logs
stay beside the case.

The parser rejects code after a return or an `if`/`else` that closes both paths.
Functions must return on every structurally checked path; a final `if`/`else`
whose branches both return needs no extra return. A `while` is conservatively
allowed to fall through. See [the implemented language rules](ProgramL-implemented.md).

### Check comments and lexical diagnostics

After rebuilding `dtc`, run these regression checks sequentially in Ubuntu:

```bash
python3 scripts/check-comments.py 0
python3 scripts/check-comments.py 3
```

From PowerShell, prefix each command with `wsl -d $Distro --exec`.
The script preserves the exact source bytes, including missing final newlines
and CRLF endings. It checks unterminated block comments before the first token,
after `return`, after a complete function and during class scanning. Every such
input must exit with status `1`, report `Unterminated block comment` at the opening
`/*` (one-based line and byte column), and produce no IR, object or executable.

Valid controls cover empty and non-nested comments, closing delimiters at EOF,
comments after terminating branches, line comments and division. Each valid
program must generate a fresh executable, pass LLVM 18 IR verification, and run
with the expected output and exit status. Sources, logs and valid compilation
artifacts stay under `build/comments-O<level>/`.

### Check void type restrictions

After rebuilding `dtc`, run these regressions sequentially in Ubuntu:

```bash
python3 scripts/check-void-types.py 0
python3 scripts/check-void-types.py 3
```

From PowerShell, prefix each command with `wsl -d $Distro --exec`.
`void` is allowed only as a plain return type for functions and methods. The
parser rejects `ref void`, pointers to `void` at any depth, `nullptr<void>` and
`void` parameters, fields and local variables. The checks cover functions,
methods and constructors, first and second positions, comments and exact EOF,
LF and CRLF endings. Every invalid source must report a parsing error, return
status `1` and produce no new IR, object or executable.
Valid controls preserve void functions/methods, constructors, ordinary value
and reference returns, pointers and `nullptr<int>`. They reuse the control-flow
suite's IR verification and execution checks. Sources, diagnostics and valid
artifacts stay under `build/void-types-O<level>/`.

### Check inline condition parentheses

After rebuilding `dtc`, run these checks sequentially in Ubuntu:

```bash
python3 scripts/check-inline-conditions.py 0
python3 scripts/check-inline-conditions.py 3
```

From PowerShell, prefix each command with `wsl -d $Distro --exec`.
The cases cover grouped operands and whole conditions, all six comparisons,
arithmetic precedence, nested inline `if`/`let`, calls, references, methods and
comments. Valid programs must produce fresh executables, pass LLVM IR verification
and run with the exact expected output. Invalid programs must return status `1`
with a diagnostic and no new IR, object or executable. EOF cases preserve LF/CRLF
and missing final newlines and have a five-second compilation timeout. Comparisons
as general expressions and chained comparisons remain rejected. Sources and logs
stay under `build/inline-conditions-O<level>/`.

### Check command-line arguments

After rebuilding `dtc`, run these checks in Ubuntu:

```bash
python3 scripts/check-cli.py
```

From PowerShell, prefix the command with `wsl -d $Distro --exec`.
The suite passes exact argument arrays to fresh compiler processes, including
empty strings and whitespace, and covers all four levels in separate and compact
forms, repeated options, malformed values, missing arguments, `--help`, unknown
options and combined `-tf` flags. Rejected commands must return `1`, report the
expected diagnostic, call neither `llc` nor `clang` and preserve existing output
files or leave fresh paths absent. The existing driver tool fixture records
arguments before executing the real LLVM 18 tools, so valid cases also verify
the effective optimization level. Generated programs must pass IR verification,
print `value: 7` and exit with `7`, separately from `dtc`'s successful status `0`.
Sources, exact commands and logs stay in unique directories under `build/cli/`.
Run this suite sequentially with other compiler checks because intermediates
are shared within the build directory.

### Check compiler driver failures

Build the optional driver fixture and run these checks sequentially in Ubuntu:

```bash
cmake --build build --target dtc check-codegen-driver --parallel 2
python3 scripts/check-driver.py 0
python3 scripts/check-driver.py 3
```

From PowerShell, prefix each command with `wsl -d $Distro --exec`.
The fixture passes a deliberately invalid LLVM module to the production driver,
so verifier coverage does not depend on a particular Donato code-generation bug.
Other cases exercise real linking without `main`, missing tools, tool startup
errors, nonzero exits, signals, missing or unusable tool outputs, IR open/write
errors, object copying, conflicting paths and final executable replacement.
Tool stand-ins and file-size limits affect only test subprocesses; system tools
are not modified. Each failure must return `1`, identify the failed stage, stop
later stages and preserve the previous executable when one exists. Valid
controls verify LLVM IR and run fresh executables, including paths with spaces.
The suite saves and restores existing `output.ll` and `output.o`. Sources and logs
remain in unique run directories under `build/driver-O<level>/`.

## Known workflow limitations

- If `llc` or runtime linking fails, check the selected LLVM 18 tools and the
  working directory. The `env PATH=...` commands above select the expected tools.
- LLVM 18 headers may emit deprecation warnings with Clang 23 and libstdc++ 13.
  Such warnings occurred in the successful verified build.
- `--help` returning status 1 is current CLI behavior, not an installation test.

## Repository contents and Git

Keep source code, `examples/*.donato`, expected text output, documentation
and `AGENTS.md` in Git. Analysis reports stay local in the ignored `report/`
directory and must not be committed. [.gitignore](.gitignore) also excludes `build/`
(including downloaded archives, validation logs and executables), local CMake
build directories, accidental root build outputs and editor files.
Example sources are deliberately not ignored. [.gitattributes](.gitattributes)
keeps Bash scripts and expected output in LF format after Windows checkouts.

For an agent working on the compiler, start with [AGENTS.md](AGENTS.md), follow
the setup above, and run the examples after relevant compiler changes. Consult
the implementation before changing Donato syntax or semantics.
