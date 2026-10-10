#!/usr/bin/env bash
set -euo pipefail
root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
cd "$root"
binary=${1:-target/nelisp}
work=$(mktemp -d "$root/target/aot-evalorder-XXXXXX")
export NELISP_REPO_ROOT="$root" AOT_ORDER_ARTIFACT="$work/probe.neln"
timeout 1800 "${EMACS:-emacs}" -Q --batch -L lisp -L src \
  -l "${AOT_ORDER_COMPILER:-lisp/nelisp-aot-compiler.el}" \
  -l test/standalone-aot-evalorder-compile.el \
  > "$work/compile.out" 2> "$work/compile.err"
timeout 1800 "${EMACS:-emacs}" -Q --batch \
  --eval '(let (trace) (let ((value (+ (progn (push 7 trace) 7) (progn (push 5 trace) 5)))) (princ (format "Host +: %S/%S\n" value trace))))' > "$work/host.out"
timeout 1800 "$binary" --eval '(load (concat (getenv "NELISP_REPO_ROOT") "/test/standalone-aot-evalorder-driver.el") nil nil t)' \
  > "$work/run.out" 2> "$work/run.err"
cat "$work/host.out" "$work/run.out"
test ! -s "$work/run.err"
grep -qx 'AOT-EVALORDER-PASS cases=12' "$work/run.out"
printf 'AOT-EVALORDER-EVIDENCE=%s\n' "$work"
