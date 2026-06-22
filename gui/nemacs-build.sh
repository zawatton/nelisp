#!/bin/bash
set -euo pipefail
EL="$1"; VAR="$2"
SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
NELISP_SNAP=${NELISP_SNAP:-/tmp/nelisp-snap}
NEMACS_TRANSPORT_DIR=${NEMACS_TRANSPORT_DIR:-/tmp}
NEMACS_X_DISPLAY_NUM=${NEMACS_X_DISPLAY_NUM:-0}
mkdir -p "$NEMACS_TRANSPORT_DIR"
NEMACS_TRANSPORT_DIR=$(CDPATH= cd -- "$NEMACS_TRANSPORT_DIR" && pwd)
if [ -n "${NEMACS_ARTIFACT_DIR:-}" ]; then
  mkdir -p "$NEMACS_ARTIFACT_DIR"
  NEMACS_ARTIFACT_DIR=$(CDPATH= cd -- "$NEMACS_ARTIFACT_DIR" && pwd)
elif [ "$NEMACS_TRANSPORT_DIR" = "$(CDPATH= cd -- "${TMPDIR:-/tmp}" && pwd)" ]; then
  NEMACS_ARTIFACT_DIR=$(CDPATH= cd -- "${TMPDIR:-/tmp}" && pwd)
else
  NEMACS_ARTIFACT_DIR="$NEMACS_TRANSPORT_DIR/.nemacs-artifacts"
  mkdir -p "$NEMACS_ARTIFACT_DIR"
fi
NEMACS_NATIVE_BIN=${NEMACS_NATIVE_BIN:-$NEMACS_ARTIFACT_DIR/nemacs-win.bin}
NEMACS_CONFIG_PATH=${NEMACS_CONFIG_PATH:-$NEMACS_ARTIFACT_DIR/nemacs.cfg}
NEMACS_NATIVE_TRANSPORT_FILE=${NEMACS_NATIVE_TRANSPORT_FILE:-$NEMACS_ARTIFACT_DIR/nemacs-win.transport-dir}
NEMACS_NATIVE_XDISPLAY_FILE=${NEMACS_NATIVE_XDISPLAY_FILE:-$NEMACS_ARTIFACT_DIR/nemacs-win.x-display}
NEMACS_NATIVE_CONFIG_FILE=${NEMACS_NATIVE_CONFIG_FILE:-$NEMACS_ARTIFACT_DIR/nemacs-win.config-path}
mkdir -p "$(dirname -- "$NEMACS_NATIVE_BIN")" "$(dirname -- "$NEMACS_CONFIG_PATH")"
export NEMACS_TRANSPORT_DIR NEMACS_CONFIG_PATH NEMACS_NATIVE_BIN NEMACS_X_DISPLAY_NUM
cd "$SCRIPT_DIR"
if [ "${NEMACS_SYNC_NELISP:-0}" = 1 ] || [ ! -x "$NELISP_SNAP/nelisp" ] || [ ! -f "$NELISP_SNAP/lisp/nelisp-aot-compiler.el" ]; then
  "$SCRIPT_DIR/scripts/sync-nelisp-snap.sh"
fi
if [ "${NEMACS_BUILD_SKIP_GUI_PKILL:-0}" != "1" ]; then
  for p in $(pgrep -f "$NEMACS_NATIVE_BIN" 2>/dev/null || true); do kill "$p" 2>/dev/null || true; done
fi
sleep 1
rm -f "$NEMACS_NATIVE_BIN" "$NEMACS_NATIVE_TRANSPORT_FILE" "$NEMACS_NATIVE_XDISPLAY_FILE" "$NEMACS_NATIVE_CONFIG_FILE"
NEMACS_NATIVE_BIN_EL=$(printf '%s' "$NEMACS_NATIVE_BIN" | sed 's/\\/\\\\/g; s/"/\\"/g')
ERR=$(timeout 240 /usr/bin/emacs --batch -L "$NELISP_SNAP/src" -L "$NELISP_SNAP/lisp" -l nelisp-aot-compiler -l "$EL" -l "$SCRIPT_DIR/nemacs-editor-transport.el" --eval "(nelisp-aot-compile-sexp $VAR \"$NEMACS_NATIVE_BIN_EL\")" 2>&1 | grep -iE 'error|arity|unsupported|unknown|void' | grep -iv mozc | head -3 || true)
if [ ! -f "$NEMACS_NATIVE_BIN" ]; then echo "COMPILE-FAILED: $ERR"; exit 1; fi
chmod +x "$NEMACS_NATIVE_BIN"
printf '%s\n' "$NEMACS_TRANSPORT_DIR" >"$NEMACS_NATIVE_TRANSPORT_FILE"
printf '%s\n' "$NEMACS_X_DISPLAY_NUM" >"$NEMACS_NATIVE_XDISPLAY_FILE"
printf '%s\n' "$NEMACS_CONFIG_PATH" >"$NEMACS_NATIVE_CONFIG_FILE"
if [ "${NEMACS_BUILD_SMOKE:-1}" = 1 ]; then
  smoke_log="${TMPDIR:-/tmp}/nemacs-build-smoke.$$"
  NEMACS_SYNC_NELISP=0 \
    NEMACS_TRANSPORT_DIR="$NEMACS_TRANSPORT_DIR" \
    NEMACS_CONFIG_PATH="$NEMACS_CONFIG_PATH" \
    NEMACS_NATIVE_BIN="$NEMACS_NATIVE_BIN" \
    NEMACS_NATIVE_TRANSPORT_FILE="$NEMACS_NATIVE_TRANSPORT_FILE" \
    NEMACS_NATIVE_XDISPLAY_FILE="$NEMACS_NATIVE_XDISPLAY_FILE" \
    NEMACS_NATIVE_CONFIG_FILE="$NEMACS_NATIVE_CONFIG_FILE" \
    setsid bash bin/nemacs >"$smoke_log" 2>&1 </dev/null &
  sleep 4
  alive=0
  for p in $(pgrep -f "$NEMACS_NATIVE_BIN" 2>/dev/null || true); do
    [ -n "$p" ] && alive=1
  done
  if [ "$alive" = 1 ]; then echo "OK alive"; else echo "CRASHED:"; head -3 "$smoke_log"; exit 1; fi
fi
