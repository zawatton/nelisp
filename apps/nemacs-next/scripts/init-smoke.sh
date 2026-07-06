#!/usr/bin/env sh
set -eu

ROOT=${NEMACS_NEXT_ROOT:-$(CDPATH= cd -- "$(dirname -- "$0")/../../.." && pwd -P)}
NELISP_BIN=${NELISP_BIN:-${NELISP:-}}
BOOTSTRAP_REPL=${NEMACS_BOOTSTRAP_REPL:-$ROOT/build/nemacs-bootstrap.repl}
FIXTURE_DIR=${NEMACS_USER_EMACS_DIRECTORY:-$ROOT/apps/nemacs-next/fixtures/init-smoke}

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
  echo "nemacs-next-init-smoke: nelisp binary is not executable: $NELISP_BIN" >&2
  exit 1
fi

if [ ! -r "$BOOTSTRAP_REPL" ]; then
  echo "nemacs-next-init-smoke: bootstrap REPL is not readable: $BOOTSTRAP_REPL" >&2
  echo "nemacs-next-init-smoke: run make build-nelisp-bootstrap first" >&2
  exit 1
fi

tmp=${TMPDIR:-/tmp}/nemacs-next-init-smoke.$$.repl
out=${TMPDIR:-/tmp}/nemacs-next-init-smoke.$$.out
err=${TMPDIR:-/tmp}/nemacs-next-init-smoke.$$.err
tui_out=${TMPDIR:-/tmp}/nemacs-next-init-smoke.$$.tui.out
tmp_q=${TMPDIR:-/tmp}/nemacs-next-init-smoke.$$.q.repl
out_q=${TMPDIR:-/tmp}/nemacs-next-init-smoke.$$.q.out
err_q=${TMPDIR:-/tmp}/nemacs-next-init-smoke.$$.q.err
rm -f "$tmp" "$out" "$err" "$tui_out" "$tmp_q" "$out_q" "$err_q"
trap 'rm -f "$tmp" "$out" "$err" "$tui_out" "$tmp_q" "$out_q" "$err_q"' EXIT

cat "$BOOTSTRAP_REPL" > "$tmp"
cat >> "$tmp" <<EOF
(setq load-path (list "$ROOT/src" "$ROOT/apps/nemacs-next/lisp"))
(load "$ROOT/src/emacs-special-buffers.el")
(load "$ROOT/src/nemacs-loadup.el")
(load "$ROOT/src/emacs-startup-screen.el")
(unless nemacs-initialized (nemacs-init))
(load "$ROOT/apps/nemacs-next/lisp/nemacs-next-protocol.el")
(setq nemacs-next-init-smoke-frame-config (nemacs-next-session-frame-config))
(setq nemacs-next-init-smoke-frame (nemacs-next-session-frame-snapshot 40 6))
(setq nemacs-next-init-smoke-scratch-text (nelisp-ec-with-current-buffer nemacs--initial-buffer (nelisp-ec-buffer-string)))
(setq nemacs-next-init-smoke-messages-text (let ((buf (emacs-special-buffers-ensure-buffer messages-buffer-name))) (nelisp-ec-with-current-buffer buf (nelisp-ec-buffer-string))))
(nelisp--write-stdout-bytes (concat "frame-config-tool-bar-lines=" (number-to-string (plist-get nemacs-next-init-smoke-frame-config :tool-bar-lines)) "\n"))
(nelisp--write-stdout-bytes (concat "frame-toolbar-present=" (if (plist-get (plist-get nemacs-next-init-smoke-frame :frame) :toolbar) "yes" "no") "\n"))
(nelisp--write-stdout-bytes (concat "scratch=" (if (equal nemacs-next-init-smoke-scratch-text "fixture scratch message\n") "custom" "bad") "\n"))
(nelisp--write-stdout-bytes (concat "messages=" (if (string-match "init loaded" nemacs-next-init-smoke-messages-text) "init-loaded" "bad") "\n"))
(nelisp--write-stdout-bytes (concat "splash=" (if (or (emacs-startup-screen-buffer) (equal (nelisp-ec-buffer-name (nelisp-ec-current-buffer)) emacs-startup-screen-buffer-name)) "bad-splash-shown" "suppressed") "\n"))
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
  echo "nemacs-next-init-smoke: session fail rc=$rc" >&2
  exit 1
fi

for expected in \
  'frame-config-tool-bar-lines=0' \
  'frame-toolbar-present=no' \
  'scratch=custom' \
  'messages=init-loaded' \
  'splash=suppressed'; do
  if ! grep -q "^$expected$" "$out"; then
    cat "$out" >&2
    cat "$err" >&2
    echo "nemacs-next-init-smoke: missing $expected" >&2
    exit 1
  fi
done

# --- paired verification: UX #18 session A -------------------------
#
# Same fixture, but `init-file-user' is forced nil before `nemacs-init'
# runs (= the -q/-Q CLI gate's effect at the nemacs-loadup level).  The
# fixture's early-init.el/init.el side effects (tool-bar-lines=0, custom
# scratch text, "init loaded" message) must NOT appear this time.

cat "$BOOTSTRAP_REPL" > "$tmp_q"
cat >> "$tmp_q" <<EOF
(setq load-path (list "$ROOT/src" "$ROOT/apps/nemacs-next/lisp"))
(load "$ROOT/src/emacs-special-buffers.el")
(load "$ROOT/src/nemacs-loadup.el")
(load "$ROOT/src/emacs-startup-screen.el")
(setq init-file-user nil)
(unless nemacs-initialized (nemacs-init))
(load "$ROOT/apps/nemacs-next/lisp/nemacs-next-protocol.el")
(setq nemacs-next-init-smoke-frame-config-q (nemacs-next-session-frame-config))
(setq nemacs-next-init-smoke-scratch-text-q (nelisp-ec-with-current-buffer nemacs--initial-buffer (nelisp-ec-buffer-string)))
(setq nemacs-next-init-smoke-messages-text-q (let ((buf (emacs-special-buffers-ensure-buffer messages-buffer-name))) (nelisp-ec-with-current-buffer buf (nelisp-ec-buffer-string))))
(nelisp--write-stdout-bytes (concat "q-frame-config-tool-bar-lines=" (number-to-string (or (plist-get nemacs-next-init-smoke-frame-config-q :tool-bar-lines) -1)) "\n"))
(nelisp--write-stdout-bytes (concat "q-scratch=" (if (equal nemacs-next-init-smoke-scratch-text-q "fixture scratch message\n") "bad-fixture-loaded" "default") "\n"))
(nelisp--write-stdout-bytes (concat "q-messages=" (if (string-match "init loaded" nemacs-next-init-smoke-messages-text-q) "bad-fixture-loaded" "no-init-message") "\n"))
(setq nemacs-next-init-smoke-splash-text-q (if (emacs-startup-screen-buffer) (nelisp-ec-with-current-buffer (emacs-startup-screen-buffer) (nelisp-ec-buffer-string)) ""))
(nelisp--write-stdout-bytes (concat "q-splash-current=" (if (equal (nelisp-ec-buffer-name (nelisp-ec-current-buffer)) emacs-startup-screen-buffer-name) "gnu-emacs" "bad") "\n"))
(nelisp--write-stdout-bytes (concat "q-splash-body=" (if (and (string-match "Welcome to nemacs" nemacs-next-init-smoke-splash-text-q) (string-match "ABSOLUTELY NO WARRANTY" nemacs-next-init-smoke-splash-text-q)) "welcome" "bad") "\n"))
(nelisp--write-stdout-bytes (concat "q-splash-read-only=" (if (and (boundp (quote buffer-read-only)) buffer-read-only) "t" "bad") "\n"))
,quit
EOF

set +e
NEMACS_USER_EMACS_DIRECTORY=$FIXTURE_DIR \
  timeout 90s "$NELISP_BIN" --repl --no-prompt --no-print \
  < "$tmp_q" > "$out_q" 2> "$err_q"
rc_q=$?
set -e

if [ "$rc_q" -ne 0 ]; then
  cat "$out_q" >&2
  cat "$err_q" >&2
  echo "nemacs-next-init-smoke: -q equivalent session fail rc=$rc_q" >&2
  exit 1
fi

for expected in \
  'q-scratch=default' \
  'q-messages=no-init-message' \
  'q-splash-current=gnu-emacs' \
  'q-splash-body=welcome' \
  'q-splash-read-only=t'; do
  if ! grep -q "^$expected$" "$out_q"; then
    cat "$out_q" >&2
    cat "$err_q" >&2
    echo "nemacs-next-init-smoke: missing $expected" >&2
    exit 1
  fi
done

if grep -q '^q-frame-config-tool-bar-lines=0$' "$out_q"; then
  cat "$out_q" >&2
  echo "nemacs-next-init-smoke: early-init.el side effect (tool-bar-lines=0) leaked with init-file-user nil" >&2
  exit 1
fi

{
  printf '\030\003'
} | NEMACS_USER_EMACS_DIRECTORY=$FIXTURE_DIR \
    NEMACS_NEXT_TUI_DRAW=never \
    NEMACS_NEXT_TUI_WIDTH=80 \
    timeout 90s "$ROOT/apps/nemacs-next/frontends/tui/nemacs-next-tui" \
    > "$tui_out" 2>&1

if grep -q 'New File\|Open\|Dired\|Save\|Search' "$tui_out"; then
  cat "$tui_out" >&2
  echo "nemacs-next-init-smoke: TUI rendered toolbar despite tool-bar-lines=0" >&2
  exit 1
fi

echo "nemacs-next-init-smoke: frame-config-tool-bar-lines=0"
echo "nemacs-next-init-smoke: tui-toolbar-hidden ok"
echo "nemacs-next-init-smoke: scratch=custom"
echo "nemacs-next-init-smoke: messages=init-loaded"
echo "nemacs-next-init-smoke: q-scratch=default"
echo "nemacs-next-init-smoke: q-messages=no-init-message"
echo "nemacs-next-init-smoke: splash=suppressed"
echo "nemacs-next-init-smoke: q-splash-current=gnu-emacs"
echo "nemacs-next-init-smoke: q-splash-body=welcome"
