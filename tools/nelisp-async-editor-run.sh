#!/usr/bin/env bash
# nelisp-async-editor-run.sh -- launch the async editor on a REAL terminal.
#
# Run this directly in your own terminal (NOT through a pipe / captured shell):
#   bash tools/nelisp-async-editor-run.sh
#
# Keys:  printable -> insert,  C-f/C-b -> move,  Backspace/DEL -> delete,
#        C-l -> redraw,  C-q -> quit.  A 0.5s timer updates the "ticks=" status.
#
# The editor puts the terminal into raw mode and restores it on C-q (via an
# unwind-protect).  If something goes wrong and your shell looks broken
# afterwards (no echo / no newline), run:  stty sane   (or:  reset).
set -euo pipefail
cd "$(dirname "$0")/.."

NELISP="${NELISP:-./target/nelisp}"
GEN="${NELISP_GENERATOR:-}"
if [ -z "$GEN" ]; then
  for c in \
    ../nemacs-next/vendor/emacs-lisp/emacs-lisp/generator.el \
    ../nelisp-emacs.wt-c1/vendor/emacs-lisp/emacs-lisp/generator.el \
    ../nelisp-emacs-lib/src/generator.el; do
    [ -f "$c" ] && { GEN="$c"; break; }
  done
fi

if [ ! -x "$NELISP" ]; then echo "ERROR: $NELISP not built (run: make standalone-reader)"; exit 1; fi
if [ -z "$GEN" ] || [ ! -f "$GEN" ]; then
  echo "ERROR: generator.el not found; set NELISP_GENERATOR=/path/to/generator.el"; exit 1
fi
if [ ! -t 0 ]; then
  echo "ERROR: stdin is not a terminal. Run this directly in a terminal, not via a pipe."; exit 1
fi

echo "[async-editor] starting (C-q to quit) ; generator=$GEN"
"$NELISP" --eval "(progn
  (load \"scripts/nelisp-stdlib-prelude.el\")
  (load \"src/nelisp-gc.el\") (load \"src/nelisp-buffer.el\") (load \"$GEN\")
  (load \"packages/nelisp-actor/src/nelisp-actor.el\")
  (load \"packages/nelisp-eventloop/src/nelisp-eventloop.el\")
  (load \"packages/nelisp-eventloop/src/nelisp-async.el\")
  (load \"packages/nelisp-redisplay/src/nelisp-redisplay.el\")
  (load \"examples/nelisp-async-editor.el\")
  (nae-run-interactive))"

# Belt-and-suspenders: make sure the terminal is sane on exit.
stty sane 2>/dev/null || true
echo "[async-editor] exited."
