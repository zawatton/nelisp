#!/usr/bin/env bash
# nelisp-async-editor-smoke.sh -- the async editor edits a nelisp-buffer from
# real stdin keystrokes, driven entirely by nelisp-async-run-tty.
#
# Feeds a key script (printable + control keys) and asserts the resulting
# buffer text / point.  Pure-pipe input is deterministic; interactive raw-mode
# use and timer multiplexing are covered elsewhere.
#
# Usage: tools/nelisp-async-editor-smoke.sh [path-to-nelisp]
set -euo pipefail
cd "$(dirname "$0")/.."

NELISP="${1:-./target/nelisp}"
GEN="${NELISP_GENERATOR:-../nemacs-next/vendor/emacs-lisp/emacs-lisp/generator.el}"

if [ ! -x "$NELISP" ]; then echo "SKIP: $NELISP not built"; exit 0; fi
if [ ! -f "$GEN" ]; then echo "SKIP: generator.el not found at $GEN"; exit 0; fi

LOADS="(load \"scripts/nelisp-stdlib-prelude.el\") \
(load \"src/nelisp-gc.el\") (load \"src/nelisp-buffer.el\") (load \"$GEN\") \
(load \"packages/nelisp-actor/src/nelisp-actor.el\") \
(load \"packages/nelisp-eventloop/src/nelisp-eventloop.el\") \
(load \"packages/nelisp-eventloop/src/nelisp-async.el\") \
(load \"examples/nelisp-async-editor.el\")"

run_case () { # $1 = key bytes (printf escapes), $2 = expected RESULT substring
  local keys="$1" want="$2"
  local out
  out="$(printf "$keys" | "$NELISP" --eval "(progn $LOADS (princ (nae-run-batch)))" 2>&1 | tail -1)"
  echo "[async-editor-smoke] keys=$(printf '%q' "$keys") -> $out"
  case "$out" in
    *"$want"*) ;;
    *) echo "FAIL: expected substring '$want'"; exit 1 ;;
  esac
}

# 'a' 'b' 'c'  C-b C-b  'Z'  C-q  -> "aZbc", point 3
run_case 'abc\002\002Z\021' 'text=aZbc point=3'
# 'h' 'i'  C-f(no-op at end)  DEL  'X'  C-q -> "hX", point 3
run_case 'hi\006\177X\021'   'text=hX point=3'
# plain typing + quit
run_case 'hello\021'         'text=hello point=6'

echo "PASS"
