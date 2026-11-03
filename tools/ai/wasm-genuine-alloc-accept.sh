#!/usr/bin/env bash
# Invoke under heavy-run with ulimit -v 8388608; child jobs keep the parent slot.
set -u
out=target/wasm-genuine-alloc
mkdir -p "$out/tmp"
export TMPDIR="$PWD/$out/tmp"
prefix=(timeout 300 emacs -Q --batch -L lisp -L src -L test -L tools/ai --eval '(setq load-prefer-newer t wasm-genuine-alloc--library-only t)' -l lisp/nelisp-asm-wasm.el -l lisp/nelisp-aot-compiler.el)
run() {
 local label=$1; shift
 "$@" > "$out/$label.log" 2>&1
 local rc=$?
 echo "$rc" > "$out/$label.rc"
 echo "$label rc=$rc"
 return "$rc"
}
run default-build "${prefix[@]}" -l wasm-genuine-alloc-probe --eval '(wasm-genuine-alloc-build nil "target/wasm-genuine-alloc/default.wasm")' || exit $?
run default-red node --disable-wasm-trap-handler tools/ai/wasm-genuine-alloc-check.mjs "$out/default.wasm" "$out/default.wasm.json" --red
rc=$?
if (( rc == 0 )); then echo 'default mode unexpectedly green'; exit 1; fi
test "$rc" -eq 1 || exit 1
for spec in nl_alloc_consbox:- nl_alloc_vector:0 nl_alloc_vector:-1 nl_alloc_vector:1 nl_alloc_vector:3 nl_alloc_vector:8; do
 rg -q "^alloc-arena-red=consumer=${spec%:*} capacity=${spec#*:} object=[0-9]* in-arena=0$" "$out/default-red.log" || exit 1
done
test "$(rg -c '^alloc-arena-red=' "$out/default-red.log")" -eq 6 || exit 1
rg -q '^genuine-alloc-check: default consumers collected=6; expected outside-arena red$' "$out/default-red.log" || exit 1
cp "$out/default-red.log" "$out/red.log"
cp "$out/default-red.rc" "$out/red.rc"
run green-build "${prefix[@]}" -l wasm-genuine-alloc-probe --eval '(wasm-genuine-alloc-build t "target/wasm-genuine-alloc/green.wasm")' || exit $?
cmp "$out/default.wasm.source.eld" "$out/green.wasm.source.eld" || exit $?
run green node --disable-wasm-trap-handler tools/ai/wasm-genuine-alloc-check.mjs || exit $?
run controls "${prefix[@]}" -l wasm-genuine-alloc-controls -f wasm-genuine-alloc-controls || exit $?
run focused "${prefix[@]}" -l test/nelisp-aot-wasm-genuine-alloc-rev2-test.el --eval '(ert-run-tests-batch-and-exit "nelisp-wasm-genuine-alloc")' || exit $?
exit 0
