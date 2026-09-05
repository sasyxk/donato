#!/usr/bin/env bash
set -euo pipefail

level=${1:-0}
if [[ $# -gt 1 || ! "$level" =~ ^[0-3]$ ]]; then
    printf 'Usage: bash scripts/check-examples.sh [0|1|2|3]\n' >&2
    exit 2
fi

cd "$(dirname "${BASH_SOURCE[0]}")/../build"
export PATH="/usr/lib/llvm-18/bin:$PATH"
if [[ ! -x ./dtc ]]; then
    printf 'Build dtc first; see README.md.\n' >&2
    exit 1
fi

results="examples-O$level"
mkdir -p "$results"
failed=0
total=0
for source in ../examples/*.donato; do
    name=$(basename "$source" .donato)
    binary="$results/$name"
    total=$((total + 1))
    rm -f -- "$binary"
    if ! timeout 60 ./dtc -O "$level" -o "$binary" "$source" \
        > "$results/$name.compile.stdout.txt" 2> "$results/$name.compile.stderr.txt"; then
        printf 'FAIL %s: compilation failed; see build/%s/%s.compile.stderr.txt\n' "$name" "$results" "$name"
        failed=$((failed + 1))
        continue
    fi
    if [[ ! -x "$binary" || -s "$results/$name.compile.stderr.txt" ]]; then
        printf 'FAIL %s: executable missing or compilation diagnostics present\n' "$name"
        failed=$((failed + 1))
        continue
    fi
    if ! timeout 15 "./$binary" > "$results/$name.stdout.txt" 2> "$results/$name.stderr.txt"; then
        printf 'FAIL %s: execution failed; see build/%s/%s.stderr.txt\n' "$name" "$results" "$name"
        failed=$((failed + 1))
        continue
    fi
    if [[ -s "$results/$name.stderr.txt" ]] || ! diff -u "../examples/expected/$name.txt" "$results/$name.stdout.txt"; then
        printf 'FAIL %s: unexpected output\n' "$name"
        failed=$((failed + 1))
        continue
    fi
    printf 'PASS %s (-O %s)\n' "$name" "$level"
done

printf '%s/%s examples passed. Logs: build/%s/\n' "$((total - failed))" "$total" "$results"
[[ "$failed" -eq 0 ]]
