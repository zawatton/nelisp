#!/usr/bin/env bash
# nelisp-x11-dired.sh -- browse a directory in an X window (pure-elisp X11).
#
# Directory entries are read with getdents64 + stat (no host fs API) and shown
# ls -l-style.  Keys: Down/C-n/n, Up/C-p/p move; RET / Right enter dir or open
# a file in the X editor; Left / u parent; C-q quit.
#
# Run on the host display.  Optional starting directory.
#   bash tools/nelisp-x11-dired.sh [DIR]
set -euo pipefail
cd "$(dirname "$0")/.."

NELISP="${NELISP:-./target/nelisp}"
DIR="${1:-.}"
if [ ! -x "$NELISP" ]; then echo "ERROR: $NELISP not built"; exit 1; fi
if [ ! -S /tmp/.X11-unix/X0 ]; then echo "ERROR: no X server socket /tmp/.X11-unix/X0"; exit 1; fi

echo "[x11-dired] browsing '$DIR' (RET enter/open, u parent, C-q quit)..."
"$NELISP" --eval "(progn
  (load \"scripts/nelisp-stdlib-prelude.el\")
  (load \"src/nelisp-buffer.el\")
  (load \"examples/nelisp-async-editor.el\")
  (load \"packages/nelisp-x11/src/nelisp-x11.el\")
  (load \"examples/nelisp-x11-editor.el\")
  (load \"packages/nelisp-dired/src/nelisp-dired.el\")
  (load \"examples/nelisp-x11-dired.el\")
  (nelisp-x11-dired \"$DIR\"))"
echo "[x11-dired] done."
