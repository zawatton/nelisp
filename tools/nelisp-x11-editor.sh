#!/usr/bin/env bash
# nelisp-x11-editor.sh -- run the gap-buffer editor inside a real X window,
# drawn entirely by the pure-elisp X11 client (no Xlib, no C).
#
# Keys: printable / Return / Backspace; arrows or C-f C-b C-p C-n / C-a C-e;
#       C-d delete; C-k kill-line, C-y yank; C-/ undo; C-q quit.
#
# Run on the machine hosting the X server (display :0).  Optional: a file to
# load (no save key wired in the X editor yet -- C-x C-s is TTY-only for now).
#   bash tools/nelisp-x11-editor.sh [FILE]
set -euo pipefail
cd "$(dirname "$0")/.."

NELISP="${NELISP:-./target/nelisp}"
FILE="${1:-}"
if [ ! -x "$NELISP" ]; then echo "ERROR: $NELISP not built"; exit 1; fi
if [ ! -S /tmp/.X11-unix/X0 ]; then echo "ERROR: no X server socket /tmp/.X11-unix/X0"; exit 1; fi

FILEARG="nil"
[ -n "$FILE" ] && FILEARG="\"$FILE\""

echo "[x11-editor] opening editor window (C-q to quit)..."
"$NELISP" --eval "(progn
  (load \"scripts/nelisp-stdlib-prelude.el\")
  (load \"src/nelisp-buffer.el\")
  (load \"examples/nelisp-async-editor.el\")
  (load \"packages/nelisp-x11/src/nelisp-x11.el\")
  (load \"examples/nelisp-x11-editor.el\")
  (nelisp-x11-editor $FILEARG))"
echo "[x11-editor] done."
