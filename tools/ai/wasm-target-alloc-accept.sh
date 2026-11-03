#!/usr/bin/env bash
# Invoke under heavy-run with ulimit -v 8388608; children retain that slot.
set -u
mkdir -p target/wasm-target-alloc || exit $?
prefix=(timeout 600 emacs -Q --batch -L lisp -L src -L test -L tools/ai
        --eval '(setq load-prefer-newer t)' -l lisp/nelisp-asm-wasm.el -l lisp/nelisp-aot-compiler.el)
run() {
    local label=$1
    shift
    "$@" > "target/wasm-target-alloc/$label.log" 2>&1
    local rc=$?
    printf '%s rc=%d\n' "$label" "$rc"
    printf '%d\n' "$rc" > "target/wasm-target-alloc/$label.rc"
    return "$rc"
}
run ert "${prefix[@]}" -l wasm-target-alloc-test --eval '(ert-run-tests-batch-and-exit "wasm-target-alloc")' || exit $?
run current "${prefix[@]}" -l tools/ai/wasm-target-alloc-probe.el || exit $?
run check timeout 600 node --disable-wasm-trap-handler tools/ai/wasm-target-alloc-check.mjs
