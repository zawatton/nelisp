#!/usr/bin/env sh
set -eu

ROOT=${NEMACS_NEXT_ROOT:-$(CDPATH= cd -- "$(dirname -- "$0")/../../.." && pwd -P)}
NELISP_BIN=${NELISP_BIN:-${NELISP:-}}
BOOTSTRAP_REPL=${NEMACS_BOOTSTRAP_REPL:-$ROOT/build/nemacs-bootstrap.repl}
FIXTURE_DIR=${NEMACS_USER_EMACS_DIRECTORY:-$ROOT/apps/nemacs-next/fixtures/theme-smoke}

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
  echo "nemacs-next-theme-smoke: nelisp binary is not executable: $NELISP_BIN" >&2
  exit 1
fi

if [ ! -r "$BOOTSTRAP_REPL" ]; then
  echo "nemacs-next-theme-smoke: bootstrap REPL is not readable: $BOOTSTRAP_REPL" >&2
  echo "nemacs-next-theme-smoke: run make build-nelisp-bootstrap first" >&2
  exit 1
fi

tmp=${TMPDIR:-/tmp}/nemacs-next-theme-smoke.$$.repl
out=${TMPDIR:-/tmp}/nemacs-next-theme-smoke.$$.out
err=${TMPDIR:-/tmp}/nemacs-next-theme-smoke.$$.err
tui_out=${TMPDIR:-/tmp}/nemacs-next-theme-smoke.$$.tui.out
rm -f "$tmp" "$out" "$err" "$tui_out"
trap 'rm -f "$tmp" "$out" "$err" "$tui_out"' EXIT

cat "$BOOTSTRAP_REPL" > "$tmp"
cat >> "$tmp" <<EOF
(setq load-path (list "$ROOT/src" "$ROOT/apps/nemacs-next/lisp"))
(load "$ROOT/src/emacs-special-buffers.el")
(load "$ROOT/src/nemacs-loadup.el")
(unless nemacs-initialized (nemacs-init t))
(load "$ROOT/apps/nemacs-next/lisp/nemacs-next-protocol.el")
(nelisp-ec-with-current-buffer nemacs--initial-buffer
  (nelisp-ec-erase-buffer)
  (nelisp-ec-insert "theme smoke")
  (emacs-buffer-put-text-property 1 6 'face 'nemacs-theme-smoke-face nemacs--initial-buffer))
(setq nemacs-next-theme-smoke-frame (nemacs-next-session-frame-snapshot 40 4))
(setq nemacs-next-theme-smoke-run (car (plist-get (car (plist-get (plist-get nemacs-next-theme-smoke-frame :frame) :viewport)) :face-runs)))
(nelisp--write-stdout-bytes (concat "enabled-themes=" (if (memq 'nemacs-demo custom-enabled-themes) "nemacs-demo" "missing") "\n"))
(nelisp--write-stdout-bytes (concat "face-foreground=" (emacs-faces-attribute 'nemacs-theme-smoke-face :foreground nil t) "\n"))
(nelisp--write-stdout-bytes (concat "face-weight=" (symbol-name (emacs-faces-attribute 'nemacs-theme-smoke-face :weight nil t)) "\n"))
(nelisp--write-stdout-bytes (concat "run-foreground=" (plist-get nemacs-next-theme-smoke-run :foreground) "\n"))
(disable-theme 'nemacs-demo)
(nelisp--write-stdout-bytes (concat "after-disable-foreground=" (emacs-faces-attribute 'nemacs-theme-smoke-face :foreground nil t) "\n"))
,quit
EOF

set +e
NEMACS_USER_EMACS_DIRECTORY=$FIXTURE_DIR \
  timeout 90s "$NELISP_BIN" --repl --no-prompt --no-print \
  < "$tmp" > "$out" 2> "$err"
rc=$?
set -e

if [ "$rc" -ne 0 ]; then
  cat "$out" >&2
  cat "$err" >&2
  echo "nemacs-next-theme-smoke: session fail rc=$rc" >&2
  exit 1
fi

for expected in \
  'enabled-themes=nemacs-demo' \
  'face-foreground=#5fd7ff' \
  'face-weight=bold' \
  'run-foreground=#5fd7ff' \
  'after-disable-foreground=#ff0000'; do
  if ! grep -q "^$expected$" "$out"; then
    cat "$out" >&2
    cat "$err" >&2
    echo "nemacs-next-theme-smoke: missing $expected" >&2
    exit 1
  fi
done

{
  printf '\030\003'
} | NEMACS_USER_EMACS_DIRECTORY=$FIXTURE_DIR \
    NEMACS_NEXT_TUI_DRAW=always \
    NEMACS_NEXT_TUI_WIDTH=80 \
    NEMACS_NEXT_TUI_HEIGHT=4 \
    COLORTERM=truecolor \
    timeout 90s "$ROOT/apps/nemacs-next/frontends/tui/nemacs-next-tui" \
    > "$tui_out" 2>&1

if ! grep -q "$(printf '\033\\[38;2;95;215;255m')" "$tui_out"; then
  cat "$tui_out" >&2
  echo "nemacs-next-theme-smoke: TUI output missing truecolor foreground SGR" >&2
  exit 1
fi

cat "$out"
echo "tui-ansi=ESC[38;2;95;215;255m"
echo "nemacs-next-theme-smoke: custom-theme-load-path init.el load-theme ok"
