#!/usr/bin/env bash
# nelisp-x11-hello.sh -- open a real X window from the pure-elisp X11 client.
#
# Talks the X11 core protocol directly over /tmp/.X11-unix/X0 (no Xlib, no C).
# A window appears with a greeting; press any key in it to close.
#
# Run on the machine hosting the X server (display :0).  No DISPLAY needed --
# the client connects to the socket directly.
set -euo pipefail
cd "$(dirname "$0")/.."

NELISP="${NELISP:-./target/nelisp}"
if [ ! -x "$NELISP" ]; then echo "ERROR: $NELISP not built"; exit 1; fi
if [ ! -S /tmp/.X11-unix/X0 ]; then echo "ERROR: no X server socket /tmp/.X11-unix/X0"; exit 1; fi

echo "[x11-hello] opening a window (press a key in it to quit)..."
"$NELISP" --eval "(progn
  (load \"scripts/nelisp-stdlib-prelude.el\")
  (load \"packages/nelisp-x11/src/nelisp-x11.el\")
  (nelisp-x11-hello))"
echo "[x11-hello] done."
