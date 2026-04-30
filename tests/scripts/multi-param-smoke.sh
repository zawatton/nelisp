#!/usr/bin/env bash
# Doc 47 Stage 9g — multi-parameter lambda + Save / OpSaved smoke.
#
# Drives `mint-from-ast LAMBDA HEAP-SRC OUT' for multi-param lambdas
# whose heap is laid out as N consecutive tagged-int words.  The
# translator emits LoadHeapIndex(i) for param i; binary `(OP a b)' is
# composed as `Save b; Load a; OP-Saved'; the secondary register
# (r10 / x10) absorbs one stash.
#
# This smoke is the regression gate for:
#   - LoadHeapIndex(i) byte encoding for both archs
#   - Save / AddSaved / SubSaved / MulSaved primitives
#   - IfLtImm.param_index dispatching on multi-param argv
#   - lower_int_array_heap producing N-word tagged-int heaps
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
    local out_bin="/tmp/doc47-stage9g-${label}.bin"
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
    echo "OK ${label}: ${lambda} | ${heap} -> ${predicted} (exit ${actual})"
}

# Single-param backward compat (= Stage 9e/f shapes still work after
# the predict_chain_value / heap-as-array refactor).
run_case single_old      "(lambda (n) (+ n 5))"           "10"           15
run_case single_neg      "(lambda (n) (- n))"             "10"          -10
run_case single_if       "(lambda (n) (if (< n 2) n (* n 2)))" "5"       10

# 2-param binary on bare param symbols.
run_case binary_add      "(lambda (a b) (+ a b))"         "(list 10 7)"  17
run_case binary_sub      "(lambda (a b) (- a b))"         "(list 20 7)"  13
run_case binary_mul      "(lambda (a b) (* a b))"         "(list 6 7)"   42
run_case binary_neg_imm  "(lambda (a b) (- a b))"         "(list 7 20)"  -13
run_case binary_zero_a   "(lambda (a b) (+ a b))"         "(list 0 99)"  99
run_case binary_zero_b   "(lambda (a b) (* a b))"         "(list 99 0)"  0

# Self-reference both sides (= effective 2x / 0 / x^2).
run_case self_double     "(lambda (n) (+ n n))"           "21"           42
run_case self_subtract   "(lambda (n) (- n n))"           "100"          0
run_case self_square     "(lambda (n) (* n n))"           "12"           144

# 3-param lambda where body uses only some.
run_case three_first_two "(lambda (a b c) (+ a b))"       "(list 5 6 999)" 11
run_case three_last_only "(lambda (a b c) (* c 4))"       "(list 99 99 6)" 24

# IfLtImm with param_index != 0 (= guard on second param).
run_case if_on_b_low     "(lambda (a b) (if (< b 10) a (- a)))" "(list 30 5)"  30
run_case if_on_b_high    "(lambda (a b) (if (< b 10) a (- a)))" "(list 30 50)" -30

# IfLtImm with binary-saved combine inside branches.
run_case if_then_mul     "(lambda (a b) (if (< a 5) (* a b) (+ a b)))" "(list 3 7)" 21
run_case if_else_add     "(lambda (a b) (if (< a 5) (* a b) (+ a b)))" "(list 8 7)" 15

# HEAP-SRC from real eval — fib(7)=13 + literal 5 → list of 2 ints.
run_case eval_heap_pair  "(lambda (a b) (* a b))" \
    "(list (progn (defun f (n) (if (< n 2) n (+ (f (- n 1)) (f (- n 2))))) (f 7)) 5)" \
    65

echo
echo "Stage 9g multi-param + Save/OpSaved: all cases passed."
