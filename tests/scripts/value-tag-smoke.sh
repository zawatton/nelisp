#!/usr/bin/env bash
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

cargo build --workspace --release --quiet

NELISP=target/release/nelisp
RUNTIME=target/release/nelisp-runtime

run_case() {
    local name="$1"
    local expected="$2"
    local fixture="tests/fixtures/${name}.el"
    local image="/tmp/doc47-stage8b-${name}.bin"

    rm -f "$image"
    "$NELISP" mint-eval-file "$fixture" "$image" >/dev/null

    local exit_code=0
    "$RUNTIME" boot-from-image "$image" || exit_code=$?
    if [[ "$exit_code" -ne "$expected" ]]; then
        echo "FAIL: $name expected $expected got $exit_code" >&2
        exit 1
    fi

    echo "OK: $name -> exit $exit_code"
}

run_case value-int 42
run_case value-t 1
run_case value-float 4
run_case value-vector 3
run_case value-string 8
run_case value-symbol 3

echo
echo "All value-tag smoke cases passed."
