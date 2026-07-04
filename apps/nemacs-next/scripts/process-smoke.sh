#!/usr/bin/env sh
set -eu

ROOT=${NEMACS_NEXT_ROOT:-$(CDPATH= cd -- "$(dirname -- "$0")/../../.." && pwd -P)}
NELISP_BIN=${NELISP_BIN:-${NELISP:-}}
BOOTSTRAP_REPL=${NEMACS_BOOTSTRAP_REPL:-$ROOT/build/nemacs-bootstrap.repl}

if [ -z "$NELISP_BIN" ]; then
  if [ -x "$ROOT/../nelisp/target/nelisp" ]; then
    NELISP_BIN=$ROOT/../nelisp/target/nelisp
  elif [ -x "$ROOT/vendor/nelisp/target/nelisp" ]; then
    NELISP_BIN=$ROOT/vendor/nelisp/target/nelisp
  else
    NELISP_BIN=$ROOT/../nelisp/target/nelisp
  fi
fi

if [ ! -x "$NELISP_BIN" ]; then
  echo "nemacs-next-process-smoke: nelisp binary is not executable: $NELISP_BIN" >&2
  exit 1
fi

if [ ! -r "$BOOTSTRAP_REPL" ]; then
  echo "nemacs-next-process-smoke: bootstrap REPL is not readable: $BOOTSTRAP_REPL" >&2
  echo "nemacs-next-process-smoke: run make build-nelisp-bootstrap first" >&2
  exit 1
fi

tmp=${TMPDIR:-/tmp}/nemacs-next-process-smoke.$$.repl
out=${TMPDIR:-/tmp}/nemacs-next-process-smoke.$$.out
rm -f "$tmp" "$out"
trap 'rm -f "$tmp" "$out"' EXIT

cat "$BOOTSTRAP_REPL" > "$tmp"
cat >> "$tmp" <<EOF
(setq load-path (list "$ROOT/src" "$ROOT/apps/nemacs-next/lisp"))
(load "$ROOT/apps/nemacs-next/lisp/nemacs-next-protocol.el")
(nelisp--write-stdout-bytes (nemacs-next-protocol-handle-message-line (quote (:type hello))))
(nelisp--write-stdout-bytes (nemacs-next-protocol-handle-message-line (quote (:type command :name create-buffer :buffer-name "nemacs-next-process-smoke"))))
(nelisp--write-stdout-bytes (nemacs-next-protocol-handle-message-line (quote (:type command :name insert-text :text "abc"))))
(nelisp--write-stdout-bytes (nemacs-next-protocol-handle-message-line (quote (:type command :name goto-char :position 1))))
(nelisp--write-stdout-bytes (nemacs-next-protocol-handle-message-line (quote (:type command :name forward-char :count 2))))
(nelisp--write-stdout-bytes (nemacs-next-protocol-handle-message-line (quote (:type command :name backward-char :count 1))))
(nelisp--write-stdout-bytes (nemacs-next-protocol-handle-message-line (quote (:type command :name delete-char :count 1))))
(nelisp--write-stdout-bytes (nemacs-next-protocol-handle-message-line (quote (:type command :name forward-char :count 999))))
,quit
EOF

set +e
timeout 60s "$NELISP_BIN" --repl --no-prompt --no-print < "$tmp" > "$out" 2>&1
rc=$?
set -e

if [ "$rc" -ne 0 ]; then
  cat "$out" >&2
  echo "nemacs-next-process-smoke: fail rc=$rc" >&2
  exit 1
fi

lines=$(wc -l < "$out" | tr -d ' ')
if [ "$lines" != "8" ]; then
  cat "$out" >&2
  echo "nemacs-next-process-smoke: fail lines=$lines expected=8" >&2
  exit 1
fi

if ! grep -q '"type":"hello"' "$out"; then
  cat "$out" >&2
  echo "nemacs-next-process-smoke: fail missing hello response" >&2
  exit 1
fi

if ! grep -q '"buffer-name":"nemacs-next-process-smoke"' "$out"; then
  cat "$out" >&2
  echo "nemacs-next-process-smoke: fail missing buffer snapshot" >&2
  exit 1
fi

if ! grep -q '"text":"abc"' "$out"; then
  cat "$out" >&2
  echo "nemacs-next-process-smoke: fail missing inserted text snapshot" >&2
  exit 1
fi

if ! grep -q '"point":1,"point-min"' "$out"; then
  cat "$out" >&2
  echo "nemacs-next-process-smoke: fail missing goto-char point=1 snapshot" >&2
  exit 1
fi

if ! grep -q '"point":3,"point-min"' "$out"; then
  cat "$out" >&2
  echo "nemacs-next-process-smoke: fail missing forward-char point=3 snapshot" >&2
  exit 1
fi

if ! grep -q '"text":"ac"' "$out"; then
  cat "$out" >&2
  echo "nemacs-next-process-smoke: fail missing post-delete-char text=ac snapshot" >&2
  exit 1
fi

if [ "$(grep -c '"type":"error"' "$out")" != "1" ]; then
  cat "$out" >&2
  echo "nemacs-next-process-smoke: fail expected exactly one out-of-range error response" >&2
  exit 1
fi

if ! grep -q '"code":"out-of-range"' "$out"; then
  cat "$out" >&2
  echo "nemacs-next-process-smoke: fail missing out-of-range error code" >&2
  exit 1
fi

echo "nemacs-next-process-smoke: json-lines=$lines ok"
