#!/usr/bin/env bash
# Invoke under heavy-run and ulimit -v 8388608; children retain the parent slot.
set -u
out=target/wasm-genuine-alloc
mkdir -p "$out/tmp"
export TMPDIR="$PWD/$out/tmp"
prefix=(timeout 900 emacs -Q --batch -L lisp -L src -L test -L tools/ai --eval '(setq load-prefer-newer t)' -l lisp/nelisp-asm-wasm.el -l lisp/nelisp-aot-compiler.el)
status=0
run() {
 local label=$1; shift
 "$@" > "$out/$label.log" 2>&1
 local rc=$?
 echo "$label rc=$rc"
 echo "$rc" > "$out/$label.rc"
 if (( rc != 0 && status == 0 )); then status=$rc; fi
}
run init-ert "${prefix[@]}" -l wasm-target-init-test --eval '(ert-run-tests-batch-and-exit "wasm-target-init")'
run alloc-ert "${prefix[@]}" -l wasm-target-alloc-test --eval '(ert-run-tests-batch-and-exit "wasm-target-alloc")'
run rootstack-ert "${prefix[@]}" -l wasm-rootstack-test -l wasm-rootstack-full-test --eval '(ert-run-tests-batch-and-exit "wasm-rootstack")'
run asm-ert "${prefix[@]}" -l test/nelisp-asm-wasm-test.el --eval '(ert-run-tests-batch-and-exit t)'
# This file contains no wasm-specific names/forms; run all of it (selector t).
run compiler-ert "${prefix[@]}" -l test/nelisp-aot-compiler-test.el --eval '(ert-run-tests-batch-and-exit t)'
run genuine-ert "${prefix[@]}" -l test/nelisp-aot-wasm-genuine-alloc-rev2-test.el --eval '(ert-run-tests-batch-and-exit "nelisp-wasm-genuine-alloc")'
run compile timeout 900 make compile
exit "$status"
