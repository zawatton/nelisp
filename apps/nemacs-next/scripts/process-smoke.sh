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
m3_file=${TMPDIR:-/tmp}/nemacs-next-process-smoke.$$.txt
rm -f "$tmp" "$out" "$m3_file"
trap 'rm -f "$tmp" "$out" "$m3_file"' EXIT
printf 'seed' > "$m3_file"

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
(nelisp--write-stdout-bytes (nemacs-next-protocol-handle-message-line (quote (:type command :name yank))))
(nelisp--write-stdout-bytes (nemacs-next-protocol-handle-message-line (quote (:type command :name newline))))
(nelisp--write-stdout-bytes (nemacs-next-protocol-handle-message-line (quote (:type command :name undo))))
(nelisp--write-stdout-bytes (nemacs-next-protocol-handle-message-line (quote (:type command :name kill-region :start 1 :end 2))))
(nelisp--write-stdout-bytes (nemacs-next-protocol-handle-message-line (quote (:type command :name yank))))
(nelisp--write-stdout-bytes (nemacs-next-protocol-handle-message-line (quote (:type command :name kill-line))))
(nelisp--write-stdout-bytes (nemacs-next-protocol-handle-message-line (quote (:type command :name yank))))
(nelisp--write-stdout-bytes (nemacs-next-protocol-handle-message-line (quote (:type command :name kill-region))))
(nelisp--write-stdout-bytes (nemacs-next-protocol-handle-message-line (quote (:type command :name create-buffer :buffer-name "nemacs-next-process-smoke-undo-empty"))))
(nelisp--write-stdout-bytes (nemacs-next-protocol-handle-message-line (quote (:type command :name undo))))
(nelisp--write-stdout-bytes (nemacs-next-protocol-handle-message-line (quote (:type command :name snapshot))))
(nelisp--write-stdout-bytes (nemacs-next-protocol-handle-message-line (quote (:type command :name find-file :path "$m3_file"))))
(nelisp--write-stdout-bytes (nemacs-next-protocol-handle-message-line (quote (:type command :name insert-text :text "!"))))
(nelisp--write-stdout-bytes (nemacs-next-protocol-handle-message-line (quote (:type command :name save-buffer))))
(nelisp--write-stdout-bytes (nemacs-next-protocol-handle-message-line (quote (:type command :name complete :purpose buffer :input "nemacs-next-process-smoke."))))
(nelisp--write-stdout-bytes (nemacs-next-protocol-handle-message-line (quote (:type command :name complete :input "fi" :collection ("find-file" "save-buffer" "switch-to-buffer")))))
(nelisp--write-stdout-bytes (nemacs-next-protocol-handle-message-line (quote (:type command :name switch-to-buffer :buffer-name "nemacs-next-process-smoke"))))
(nelisp--write-stdout-bytes (nemacs-next-protocol-handle-message-line (quote (:type command :name kill-buffer :buffer-name "nemacs-next-process-smoke.$$.txt"))))
(nelisp--write-stdout-bytes (nemacs-next-protocol-handle-message-line (quote (:type command :name kill-buffer :buffer-name "missing-buffer"))))
(nelisp--write-stdout-bytes (nemacs-next-protocol-handle-message-line (quote (:type command :name frame-snapshot :width 24 :height 4))))
(nelisp--write-stdout-bytes (nemacs-next-protocol-handle-message-line (quote (:type command :name menu))))
(nelisp--write-stdout-bytes (nemacs-next-protocol-handle-message-line (quote (:type resize :width 22 :height 3))))
(nelisp--write-stdout-bytes (nemacs-next-protocol-handle-message-line (quote (:type input :event (:text "x")))))
(nelisp--write-stdout-bytes (nemacs-next-protocol-handle-message-line (quote (:type input :event (:commit "y")))))
(nelisp--write-stdout-bytes (nemacs-next-protocol-handle-message-line (quote (:type command :name clipboard-read))))
(nelisp--write-stdout-bytes (nemacs-next-protocol-handle-message-line (quote (:type command :name clipboard-write :text "copy-me"))))
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
if [ "$lines" != "34" ]; then
  cat "$out" >&2
  echo "nemacs-next-process-smoke: fail lines=$lines expected=34" >&2
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

if [ "$(grep -c '"type":"error"' "$out")" != "5" ]; then
  cat "$out" >&2
  echo "nemacs-next-process-smoke: fail expected exactly five error responses (out-of-range, empty-kill-ring, bad-command, no-further-undo-information, no-such-buffer)" >&2
  exit 1
fi

if ! grep -q '"code":"out-of-range"' "$out"; then
  cat "$out" >&2
  echo "nemacs-next-process-smoke: fail missing out-of-range error code" >&2
  exit 1
fi

if ! grep -q '"code":"empty-kill-ring"' "$out"; then
  cat "$out" >&2
  echo "nemacs-next-process-smoke: fail missing empty-kill-ring error code for yank on an empty kill ring" >&2
  exit 1
fi

if ! grep -qF '"text":"a\nc"' "$out"; then
  cat "$out" >&2
  echo "nemacs-next-process-smoke: fail missing newline-inserted text=a\\nc snapshot" >&2
  exit 1
fi

if ! grep -q '"code":"bad-command"' "$out"; then
  cat "$out" >&2
  echo "nemacs-next-process-smoke: fail missing bad-command error code for kill-region without :start/:end" >&2
  exit 1
fi

if ! grep -qF '"text":"c"' "$out"; then
  cat "$out" >&2
  echo "nemacs-next-process-smoke: fail missing post-kill-region text=c snapshot" >&2
  exit 1
fi

if ! grep -qF '"text":"a"' "$out"; then
  cat "$out" >&2
  echo "nemacs-next-process-smoke: fail missing post-kill-line text=a snapshot" >&2
  exit 1
fi

if ! grep -q '"code":"no-further-undo-information"' "$out"; then
  cat "$out" >&2
  echo "nemacs-next-process-smoke: fail missing no-further-undo-information error code for undo on a fresh buffer" >&2
  exit 1
fi

if ! grep -q "\"file-name\":\"$m3_file\"" "$out"; then
  cat "$out" >&2
  echo "nemacs-next-process-smoke: fail missing find-file snapshot for M3 file" >&2
  exit 1
fi

if ! grep -q '"text":"seed!"' "$out"; then
  cat "$out" >&2
  echo "nemacs-next-process-smoke: fail missing post-M3 append text=seed! snapshot" >&2
  exit 1
fi

if ! grep -q "\"saved-file\":\"$m3_file\"" "$out"; then
  cat "$out" >&2
  echo "nemacs-next-process-smoke: fail missing save-buffer saved-file" >&2
  exit 1
fi

if [ "$(cat "$m3_file")" != "seed!" ]; then
  cat "$out" >&2
  echo "nemacs-next-process-smoke: fail M3 save-buffer did not write seed!" >&2
  exit 1
fi

if ! grep -q '"type":"minibuffer"' "$out"; then
  cat "$out" >&2
  echo "nemacs-next-process-smoke: fail missing minibuffer completion response" >&2
  exit 1
fi

if ! grep -q '"candidates":\["find-file"\]' "$out"; then
  cat "$out" >&2
  echo "nemacs-next-process-smoke: fail missing generic completion candidate find-file" >&2
  exit 1
fi

if ! grep -q '"code":"no-such-buffer"' "$out"; then
  cat "$out" >&2
  echo "nemacs-next-process-smoke: fail missing no-such-buffer error for kill-buffer" >&2
  exit 1
fi

if ! grep -q '"frame":{"id":"main"' "$out"; then
  cat "$out" >&2
  echo "nemacs-next-process-smoke: fail missing M4 frame snapshot/delta payload" >&2
  exit 1
fi

if ! grep -q '"type":"menu"' "$out"; then
  cat "$out" >&2
  echo "nemacs-next-process-smoke: fail missing M4 menu model" >&2
  exit 1
fi

if ! grep -q '"command":"find-file"' "$out"; then
  cat "$out" >&2
  echo "nemacs-next-process-smoke: fail missing menu protocol command target" >&2
  exit 1
fi

if [ "$(grep -c '"type":"delta"' "$out")" != "3" ]; then
  cat "$out" >&2
  echo "nemacs-next-process-smoke: fail expected three M4 delta responses (resize, keyboard input, IME commit)" >&2
  exit 1
fi

if [ "$(grep -c '"type":"request"' "$out")" != "2" ]; then
  cat "$out" >&2
  echo "nemacs-next-process-smoke: fail expected two M4 clipboard requests" >&2
  exit 1
fi

echo "nemacs-next-process-smoke: json-lines=$lines ok"
