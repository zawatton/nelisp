#!/usr/bin/env bash
# Invoke under heavy-run with ulimit -v 8388608; children retain that slot.
set -u
# Existing native-copy regression cases use this scratch parent too.
mkdir -p target/wasm-target-alloc target/wasm-rootstack || exit $?
prefix=(timeout 600 emacs -Q --batch -L lisp -L src -L test -L tools/ai
        --eval '(setq load-prefer-newer t)' -l lisp/nelisp-asm-wasm.el -l lisp/nelisp-aot-compiler.el)
status=0
run() {
    local label=$1
    shift
    "$@" > "target/wasm-target-alloc/regression-$label.log" 2>&1
    local rc=$?
    if (( rc != 0 && status == 0 )); then status=$rc; fi
    printf '%s rc=%d\n' "$label" "$rc"
    printf '%d\n' "$rc" > "target/wasm-target-alloc/regression-$label.rc"
}
run init-ert "${prefix[@]}" -l wasm-target-init-test --eval '(ert-run-tests-batch-and-exit "wasm-target-init")'
run init-probe "${prefix[@]}" -l tools/ai/wasm-target-init-probe.el
run init-check timeout 600 node --disable-wasm-trap-handler tools/ai/wasm-target-init-check.mjs
run rootstack-ert "${prefix[@]}" -l wasm-rootstack-test -l wasm-rootstack-full-test --eval '(ert-run-tests-batch-and-exit "wasm-rootstack")'
run rootstack-fixture "${prefix[@]}" -l wasm-rootstack-fixture -f wasm-rootstack-fixture-build
run rootstack-check timeout 600 node --disable-wasm-trap-handler tools/ai/wasm-rootstack-check.mjs
exit "$status"
