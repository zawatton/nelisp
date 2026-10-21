#!/usr/bin/env bash
# GNU compiles the fixture; standalone checks consume the exact function objects.
set -euo pipefail
cd "$(dirname "$0")/../.."
ulimit -v 8388608
GNU=${GNU_EMACS:-/usr/local/bin/emacs}
out=target/f-buffer-bridge
mkdir -p "$out/fixtures" target/tmp
export TMPDIR="$PWD/target/tmp"
if [ "${1:-}" = prepare ]; then
  cp test/fixtures/buffer-function-cell.el "$out/fixtures/"
  "$GNU" -Q --batch -f batch-byte-compile "$out/fixtures/buffer-function-cell.el"
  "$GNU" -Q --batch --load "$out/fixtures/buffer-function-cell.elc" --eval '
    (with-temp-file "test/fixtures/buffer-function-cell.eld"
      (insert ";;; -*- lexical-binding: t; -*- GNU 31.1 batch-byte-compile; regenerate with tools/ai/f-buffer-bridge-check.sh prepare\n")
      (let ((print-escape-nonascii t) (print-escape-multibyte t)
            (print-escape-control-characters t) (print-escape-newlines t))
        (dolist (name (quote (fbb-with fbb-normal fbb-throw fbb-signal fbb-nested
                             fbb-killed fbb-excursion fbb-restriction fbb-loop)))
          (prin1 (list (quote fset) (list (quote quote) name) (symbol-function name)) (current-buffer))
          (insert "\n"))))'
  "$GNU" -Q --batch --load test/standalone-buffer-function-cell-check.el >test/standalone-buffer-function-cell-check.expected
  exit
fi
bin=$(realpath "${1:?usage: f-buffer-bridge-check.sh prepare | BIN [LABEL]}")
label=${2:-semantic}
args=()
# Fast source-only iteration; final acceptance still uses rebuilt binaries.
if [ "${FBB_RELOAD:-0}" = 1 ]; then
  python3 - <<'PYCODE'
from pathlib import Path
text = Path('scripts/nelisp-stdlib-prelude.el').read_text()
start = text.index("(let* ((set-value (symbol-function 'set-buffer))")
end = text.index("(let ((set-value (symbol-function 'nelisp--bytecode-set-buffer))", start)
Path('target/f-buffer-bridge/reload.el').write_text(
    '(load "lisp/nelisp-bytecode-buffer.el" nil t t)\n' +
    text[start:end] + '\n(load "lisp/nelisp-bytecode-cleanup.el" nil t t)\n')
PYCODE
  args=(--load "$out/reload.el")
fi
"$GNU" -Q --batch --load test/standalone-buffer-function-cell-check.el >"$out/gnu.log" 2>"$out/gnu.err"
test ! -s "$out/gnu.err"
timeout 60 "$bin" "${args[@]}" --load test/standalone-buffer-function-cell-check.el >"$out/$label.log" 2>"$out/$label.err"
test ! -s "$out/$label.err"
cmp "$out/gnu.log" "$out/$label.log"
printf 'BUFFER-CELL PASS %s\n' "$label"
