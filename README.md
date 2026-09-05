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
env PATH=/usr/lib/llvm-18/bin:/usr/bin:/bin ./dtc -O 3 -o hello ../examples/hello.donato
./hello
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
relative to that working directory. Quote source paths containing spaces and use
Linux forward slashes. Keep output names simple, such as `hello` or
`my_program`: the linker command does not quote output names.

The default executable name is `output`; run it with `./output`. Each compilation
also overwrites `output.ll` and `output.o` in the working directory, even with
`-o`. Compile programs sequentially in that directory. Editing a `.donato` file
requires recompiling that file, without rebuilding `dtc`.

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

The optimization setting controls [LLVM machine-code generation via `llc`](https://llvm.org/docs/CommandGuide/llc.html#options).
It does not select the CMake build type or add a full LLVM IR optimization pipeline.
For example, compile a program with level 3 and both runtime checks:

```bash
env PATH=/usr/lib/llvm-18/bin:/usr/bin:/bin ./dtc -O3 -t -f -o checked ../examples/hello.donato
./checked
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
wsl -d $Distro --cd "$PWD\build" --exec ./hello
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

### Check returns and nested control flow

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
loops, methods and constructors. Invalid programs must fail during parsing with
the expected diagnostic, before LLVM IR or an executable is generated.
Valid programs must also pass LLVM 18 `opt` verification and checks for unnecessary
merge blocks. Each case retains its source, logs and, when valid, IR, executable
and expected output under `build/control-flow-O<level>/`.

The parser rejects code after a return or an `if`/`else` that closes both paths.
Functions must return on every structurally checked path; a final `if`/`else`
whose branches both return needs no extra return. A `while` is conservatively
allowed to fall through. See [the implemented language rules](ProgramL-implemented.md).

## Known workflow limitations

- Compiler exit status alone is insufficient: the current implementation does
  not propagate all LLVM verification or external tool failures. Inspect
  diagnostics and ensure a new executable was generated before running it.
  An old executable can remain after failure; the example checker removes its
  previous binary before each compilation.
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
