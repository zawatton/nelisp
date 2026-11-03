#!/usr/bin/env bash
# Hold one top-level heavy-run slot across both compiler and Node.
set -u
: "${NOTES_ROOT:?Set NOTES_ROOT to the Notes workspace}"
ulimit -v 8388608
"$NOTES_ROOT/bin/heavy-run.sh" bash -c '
 set -u
 out=target/wasm-genuine-alloc
 mkdir -p "$out/tmp"
 export TMPDIR="$PWD/$out/tmp"
 timeout 600 emacs -Q --batch -L lisp -L src -L test -L tools/ai --eval "(setq load-prefer-newer t)" -l lisp/nelisp-asm-wasm.el -l lisp/nelisp-aot-compiler.el -l tools/ai/wasm-genuine-alloc-probe.el > "$out/current.log" 2>&1 &&
 timeout 600 node --disable-wasm-trap-handler tools/ai/wasm-genuine-alloc-check.mjs > "$out/check.log" 2>&1
 rc=$?
 if (( rc == 0 )); then
  rg -q "^GATE-COUNT checked=[1-9][0-9]* findings=0$" "$out/check.log"
  rc=$?
 fi
 echo "$rc" > "$out/s13.rc"
 exit "$rc"
'
