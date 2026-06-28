#!/usr/bin/env bash
# nelisp-async-tty-smoke.sh -- end-to-end check that the async TTY input layer
# delivers real stdin bytes to the event-loop actor.
#
# Pipes bytes into `nelisp-async-run-tty': each byte must dispatch through the
# key-binding table to its command, and EOF must stop the loop.  Pure-pipe
# input is deterministic (all bytes buffered, drained in order, then EOF), so
# this is a stable CI smoke; interactive timer/keystroke multiplexing is
# covered by the ERT timer suite + manual TTY use.
#
# Usage: tools/nelisp-async-tty-smoke.sh [path-to-nelisp]
set -euo pipefail
cd "$(dirname "$0")/.."

NELISP="${1:-./target/nelisp}"
GEN="${NELISP_GENERATOR:-../nemacs-next/vendor/emacs-lisp/emacs-lisp/generator.el}"

if [ ! -x "$NELISP" ]; then
  echo "SKIP: $NELISP not built (run: make standalone-reader)"; exit 0
fi
if [ ! -f "$GEN" ]; then
  echo "SKIP: generator.el not found at $GEN"; exit 0
fi

LOADS="(load \"scripts/nelisp-stdlib-prelude.el\") \
(load \"src/nelisp-gc.el\") (load \"src/nelisp-buffer.el\") (load \"$GEN\") \
(load \"packages/nelisp-actor/src/nelisp-actor.el\") \
(load \"packages/nelisp-eventloop/src/nelisp-eventloop.el\") \
(load \"packages/nelisp-eventloop/src/nelisp-async.el\")"

OUT="$(printf 'abc' | "$NELISP" --eval "(progn $LOADS
  (nelisp-actor--reset) (nelisp-async-reset-timers)
  (setq nelisp-eventloop--bindings (make-hash-table :test (quote equal)))
  (let ((seen nil))
    (nelisp-eventloop-bind \"a\" (lambda (_e) (setq seen (cons (quote a) seen))))
    (nelisp-eventloop-bind \"b\" (lambda (_e) (setq seen (cons (quote b) seen))))
    (nelisp-eventloop-bind \"c\" (lambda (_e) (setq seen (cons (quote c) seen))))
    (let* ((main (nelisp-eventloop-spawn-main-actor))
           (res (nelisp-async-run-tty main)))
      (format \"RESULT[%s %s]\" res (reverse seen)))))" 2>&1 | tail -1)"

echo "[async-tty-smoke] $OUT"
case "$OUT" in
  *"RESULT[:eof (a b c)]"*) echo "PASS"; exit 0 ;;
  *) echo "FAIL: expected 'RESULT[:eof (a b c)]'"; exit 1 ;;
esac
