#!/usr/bin/env bash
# Doc 47 Stage 9f — IfLtImm structured branch smoke.
#
# Drives `mint-from-ast LAMBDA HEAP-SRC OUT' for guarded lambdas and
# asserts the boot exit equals the predicted i64 wrapped to 8 bits.
# Coverage targets the new IfLtImm ChainOp variant: simple guard,
# negate-on-negative (= abs), nested-if dispatch, edge thresholds.
#
# IfLtImm is the first ChainOp that owns sub-chain payload; this smoke
# is the structural regression gate for the recursive `compose'.
set -euo pipefail
cd "$(git rev-parse --show-toplevel)"
cargo build --workspace --release --quiet
NELISP=target/release/nelisp
RUNTIME=target/release/nelisp-runtime

expected_exit() {
    local n="$1"
    python3 -c "import sys; n=int(sys.argv[1]); print(n & 0xFF)" "$n"
}

run_case() {
    local label="$1" lambda="$2" heap="$3" predicted="$4"
    local out_bin="/tmp/doc47-stage9f-${label}.bin"
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

# Basic guard: doubled when >= 2.
LAMBDA_BASIC='(lambda (n) (if (< n 2) n (* n 2)))'
run_case basic_lt2_n0   "$LAMBDA_BASIC" 0   0
run_case basic_lt2_n1   "$LAMBDA_BASIC" 1   1
run_case basic_lt2_n2   "$LAMBDA_BASIC" 2   4
run_case basic_lt2_n5   "$LAMBDA_BASIC" 5   10
run_case basic_lt2_n10  "$LAMBDA_BASIC" 10  20

# Abs: negate when < 0, identity otherwise.
LAMBDA_ABS='(lambda (n) (if (< n 0) (- n) n))'
run_case abs_neg3       "$LAMBDA_ABS"  -3   3
run_case abs_neg1       "$LAMBDA_ABS"  -1   1
run_case abs_zero       "$LAMBDA_ABS"  0    0
run_case abs_pos5       "$LAMBDA_ABS"  5    5
run_case abs_pos100     "$LAMBDA_ABS"  100  100

# Nested: 3 piecewise zones.
LAMBDA_NESTED='(lambda (n) (if (< n 2) n (if (< n 5) (* n 2) (* n 3))))'
run_case nested_n0      "$LAMBDA_NESTED" 0   0
run_case nested_n1      "$LAMBDA_NESTED" 1   1
run_case nested_n2      "$LAMBDA_NESTED" 2   4
run_case nested_n4      "$LAMBDA_NESTED" 4   8
run_case nested_n5      "$LAMBDA_NESTED" 5   15
run_case nested_n10     "$LAMBDA_NESTED" 10  30

# Mixed arithmetic in branches: small adds 100, large subtracts 50.
LAMBDA_MIXED='(lambda (n) (if (< n 10) (+ n 100) (- n 50)))'
run_case mixed_n0       "$LAMBDA_MIXED"  0   100
run_case mixed_n9       "$LAMBDA_MIXED"  9   109
run_case mixed_n10      "$LAMBDA_MIXED"  10  -40
run_case mixed_n100     "$LAMBDA_MIXED"  100 50

# Negative threshold guards.
LAMBDA_NEG='(lambda (n) (if (< n -3) (- n) n))'
run_case neg_thr_below  "$LAMBDA_NEG"   -10  10
run_case neg_thr_above  "$LAMBDA_NEG"   -3   -3
run_case neg_thr_zero   "$LAMBDA_NEG"   0    0

# HEAP-SRC from real recursive eval — fib(7) = 13, > 5, hits *3 branch.
LAMBDA_FIB_GUARD='(lambda (n) (if (< n 5) (* n 2) (* n 3)))'
run_case eval_heap_fib7 "$LAMBDA_FIB_GUARD" "(progn (defun f (n) (if (< n 2) n (+ (f (- n 1)) (f (- n 2))))) (f 7))" 39

echo
echo "Stage 9f IfLtImm structured branch: all cases passed."
