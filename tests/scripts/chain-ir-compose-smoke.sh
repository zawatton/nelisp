#!/usr/bin/env bash
# Doc 47 Stage 9d — ChainOp IR composer smoke.
#
# Drives `mint-chain SRC OPS OUT' for a matrix of (heap_int, ops)
# pairs and asserts the boot exit = expected i8/u8 wrap of the
# predicted i64 (POSIX exit codes are 8-bit, so negative results
# wrap; positive results in [0, 255] pass through).
#
# Coverage targets the new Stage 9d primitives (sub/mul/neg) and
# combinations that exercise the compose() composer over 5+ ops.
set -euo pipefail
cd "$(git rev-parse --show-toplevel)"
cargo build --workspace --release --quiet
NELISP=target/release/nelisp
RUNTIME=target/release/nelisp-runtime

# Convert a signed i64 to its 8-bit unsigned exit-code representation.
# POSIX wait(2) returns the low 8 bits of `_exit(n)' as the exit code.
expected_exit() {
    local n="$1"
    python3 -c "import sys; n=int(sys.argv[1]); print(n & 0xFF)" "$n"
}

run_case() {
    local label="$1" src="$2" ops="$3" predicted="$4"
    local out_bin="/tmp/doc47-stage9d-${label}.bin"
    rm -f "$out_bin"

    "$NELISP" mint-chain "$src" "$ops" "$out_bin" >/dev/null

    local actual=0
    "$RUNTIME" boot-from-image "$out_bin" || actual=$?
    local expected
    expected="$(expected_exit "$predicted")"

    if [[ "$actual" -ne "$expected" ]]; then
        echo "FAIL ${label}: src='${src}' ops='${ops}' predicted=${predicted} expected_exit=${expected} got=${actual}" >&2
        exit 1
    fi
    echo "OK ${label}: ${src} | ${ops} -> ${predicted} (exit ${actual})"
}

# heap | ops | predicted-i64
run_case sub_basic    "20"            "sub:7"                  13
run_case mul_basic    "6"             "mul:7"                  42
run_case neg_basic    "10"            "neg"                   -10
run_case add_then_mul "3"             "add:2 mul:5"            25
run_case sub_then_neg "100"           "sub:50 neg"            -50
run_case full_chain   "5"             "add:3 mul:2 neg"       -16
run_case fib_then_arith "(progn (defun f (n) (if (< n 2) n (+ (f (- n 1)) (f (- n 2))))) (f 7))" "mul:3 sub:1" 38
run_case empty_chain  "42"            ""                       42

echo
echo "Stage 9d ChainOp IR composer: all cases passed."
