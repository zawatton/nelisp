#!/usr/bin/env bash
# Doc 47 Stage 9e — AST → ChainOp translator smoke.
#
# Drives `mint-from-ast LAMBDA HEAP-SRC OUT' for a matrix of
# (lambda body, heap_int, expected i64) triples.  The translator walks
# the lambda body, derives ChainOps, composes the function body, and
# the runtime boot exits with `body(heap_int)' wrapped to the OS
# 8-bit exit code.  If the boot exit equals the predicted value (mod
# 256), the AST → asm path is verified end-to-end for that body shape.
#
# This is the first surface where the driver hands the build-tool a
# real Elisp lambda and gets back a runnable native body — no textual
# OPS spec mediation (= Stage 9d) and no canned asset (= Stage 7a).
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
    local label="$1" lambda="$2" heap="$3" predicted="$4"
    local out_bin="/tmp/doc47-stage9e-${label}.bin"
    rm -f "$out_bin"

    "$NELISP" mint-from-ast "$lambda" "$heap" "$out_bin" >/dev/null

    local actual=0
    "$RUNTIME" boot-from-image "$out_bin" || actual=$?
    local expected
    expected="$(expected_exit "$predicted")"

    if [[ "$actual" -ne "$expected" ]]; then
        echo "FAIL ${label}: lambda='${lambda}' heap='${heap}' predicted=${predicted} expected_exit=${expected} got=${actual}" >&2
        exit 1
    fi
    echo "OK ${label}: ${lambda} | n=${heap} -> ${predicted} (exit ${actual})"
}

# label                | lambda                                 | heap | expected
run_case identity        "(lambda (n) n)"                          "42"   42
run_case add_imm         "(lambda (n) (+ n 5))"                    "10"   15
run_case sub_imm         "(lambda (n) (- n 7))"                    "20"   13
run_case mul_imm         "(lambda (n) (* n 3))"                    "6"    18
run_case unary_neg       "(lambda (n) (- n))"                      "10"  -10
run_case mul_then_add    "(lambda (n) (+ (* n 3) 5))"              "7"    26
run_case add_then_mul    "(lambda (n) (* (+ n 1) 2))"              "3"    8
run_case sub_then_mul    "(lambda (n) (- (* n 5) 1))"              "4"    19
run_case constant_first  "(lambda (n) (+ 5 n))"                    "10"   15
run_case variadic_add    "(lambda (n) (+ n 3 -1 5))"               "10"   17
run_case neg_then_add    "(lambda (n) (+ (- n) 10))"               "3"    7
run_case full_arith      "(lambda (n) (+ (* (- n 1) 5) 7))"        "4"    22
# heap from a real eval: fib(7) = 13, then lambda doubles + 1 → 27
run_case eval_heap       "(lambda (n) (+ (* n 2) 1))" "(progn (defun f (n) (if (< n 2) n (+ (f (- n 1)) (f (- n 2))))) (f 7))" 27

echo
echo "Stage 9e AST → ChainOp translator: all cases passed."
