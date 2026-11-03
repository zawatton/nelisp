#!/usr/bin/env bash
# Run under heavy-run.sh with ulimit -v 8388608; children keep that slot.
set -u
out=target/wasm-intern-symbol
mkdir -p "$out/tmp"
export TMPDIR="$PWD/$out/tmp"
prefix=(timeout 600 emacs -Q --batch -L lisp -L src -L test -L tools/ai --eval '(setq load-prefer-newer t wasm-intern-symbol--library-only t)' -l lisp/nelisp-asm-wasm.el -l lisp/nelisp-aot-compiler.el)
run() {
 local label=$1; shift
 "$@" > "$out/$label.log" 2>&1
 local rc=$?
 echo "$rc" > "$out/$label.rc"
 echo "$label rc=$rc"
 return "$rc"
}
run current "${prefix[@]}" -l wasm-intern-symbol-probe -f wasm-intern-symbol-build || exit $?
run check timeout 600 node --disable-wasm-trap-handler tools/ai/wasm-intern-symbol-check.mjs || exit $?
run controls "${prefix[@]}" -l wasm-intern-symbol-controls -f wasm-intern-symbol-controls || exit $?
run ert "${prefix[@]}" -l wasm-intern-symbol-test --eval '(ert-run-tests-batch-and-exit "^wasm-intern-symbol/")' || exit $?
rg -q '^GATE-COUNT checked=[1-9][0-9]* findings=0$' "$out/check.log" || exit $?
if rg -q '^intern-red=' "$out/check.log"; then echo 'red line survived adapter'; exit 1; fi
rg -q '^genuine-intern-init=expected-compiles diagnostic=nil' "$out/current.log" || exit $?
echo 'control=adapter-restored rc=0 intern-red-lines=0'
exit 0
