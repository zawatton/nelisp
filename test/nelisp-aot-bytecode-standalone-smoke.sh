#!/bin/sh
# nelisp-aot-bytecode-standalone-smoke.sh --- regression for the build-time AOT
# of GNU bytecode (scripts/nelisp-aot-bytecode.el).
#
# Runs test/nelisp-aot-bytecode-standalone-smoke.el on a built standalone
# binary, then re-runs it under four deliberate defects (negative controls) and
# requires each to fail with its signature check:
#   detached  table detached at runtime        -> "FAIL entry-taken-total"
#   partial   one installed slot cleared       -> "FAIL install-count"
#   nolean    lean-entry disable flag set      -> "FAIL lean-entry-taken"
#   guard     mutation check fed an equal copy -> "FAIL mutated-install-refused"
#   defunnohit defun slot cleared             -> "FAIL install-count"
#   bqnohit   backquote-process slot cleared    -> "FAIL bq-entry-taken"
# If NELISP_AOT_SMOKE_NOSF_BIN names an AOT binary without the special-form inline
# answer of `wf_aot_adl', the smoke must fail there with "FAIL adl-special-form-inline".
# If NELISP_AOT_SMOKE_BASE_BIN names a binary built without AOT (for example
# the commit before the AOT change), the smoke must also fail there with
# "FAIL aot-table-present".
#
# Usage: NELISP_BIN=target/nelisp test/nelisp-aot-bytecode-standalone-smoke.sh

set -eu

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo=${NELISP_ROOT:-$(CDPATH= cd -- "$script_dir/.." && pwd)}
binary=${NELISP_BIN:-$repo/target/nelisp}
smoke=$script_dir/nelisp-aot-bytecode-standalone-smoke.el
out_dir=$(mktemp -d "${TMPDIR:-/tmp}/nelisp-aot-smoke.XXXXXX")
trap 'rm -rf "$out_dir"' EXIT HUP INT TERM

if [ ! -x "$binary" ]; then
    echo "GATE-SKIP NELISP_BIN is not executable: $binary"
    exit 0
fi

findings=0

# run_case NAME BINARY CONTROL: leaves the exit status in $rc, output in $out_dir/NAME.out
run_case() {
    rc=0
    NELISP_AOT_SMOKE_CONTROL=$3 timeout 600 "$2" --load "$smoke" \
        >"$out_dir/$1.out" 2>&1 || rc=$?
}

run_case clean "$binary" ""
if [ "$rc" = 0 ] && grep -q '^AOT-SMOKE aot=11 .* fail=0$' "$out_dir/clean.out"; then
    echo "PASS aot smoke: $(grep '^AOT-SMOKE aot=' "$out_dir/clean.out")"
else
    echo "FAIL aot smoke (rc=$rc)"; tail -15 "$out_dir/clean.out"
    findings=$((findings + 1))
fi

control() {  # control NAME BINARY CONTROL SIGNATURE
    run_case "$1" "$2" "$3"
    if [ "$rc" != 0 ] && grep -q "^FAIL $4" "$out_dir/$1.out"; then
        echo "PASS negative control $1 (red as required: FAIL $4)"
    else
        echo "FAIL negative control $1 did not go red with FAIL $4 (rc=$rc)"
        tail -5 "$out_dir/$1.out"
        findings=$((findings + 1))
    fi
}
control detached "$binary" detached entry-taken-total
control partial "$binary" partial install-count
control nolean "$binary" nolean lean-entry-taken
control defunnohit "$binary" defunnohit install-count
control bqnohit "$binary" bqnohit bq-entry-taken
control guard "$binary" guard mutated-install-refused
if [ -n "${NELISP_AOT_SMOKE_BASE_BIN:-}" ]; then
    control nobuild "$NELISP_AOT_SMOKE_BASE_BIN" "" aot-table-present
else
    echo "NOTE baseline-binary control skipped (set NELISP_AOT_SMOKE_BASE_BIN)"
fi

# A binary built before the special-form inline answer must go red on its check.
if [ -n "${NELISP_AOT_SMOKE_NOSF_BIN:-}" ]; then
    control nosfinline "$NELISP_AOT_SMOKE_NOSF_BIN" "" adl-special-form-inline
else
    echo "NOTE special-form control skipped (set NELISP_AOT_SMOKE_NOSF_BIN)"
fi

[ "$findings" = 0 ] || exit 1
