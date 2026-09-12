#!/bin/sh
# nelisp-ai-repl-script-smoke.sh --- `nelisp-ai.sh repl --script FILE'
#
# The REPL reads one line per form.  `--script' is the supported way to feed
# it ordinary Elisp whose forms span several lines.  This smoke runs a real
# standalone session and checks the user-facing loop:
#
#   multi-line defun -> multi-line caller -> a form that signals ->
#   the NEXT form still runs -> state from earlier in the script survives
#
# It also runs the SAME file through the plain line-by-line pipe and requires
# that path to fail.  Without that control the smoke would keep passing if
# `--script' silently stopped doing anything.

set -eu

smoke_script_dir=$(cd "$(dirname "$0")" && pwd)
smoke_root=$(cd "$smoke_script_dir/.." && pwd)
cd "$smoke_root"

smoke_dir=$(mktemp -d "${TMPDIR:-/tmp}/nelisp-ai-repl-script-smoke.XXXXXX")
smoke_keep=${NELISP_AI_REPL_SCRIPT_SMOKE_KEEP:-0}
smoke_status=0
cleanup_smoke() {
    smoke_status=$?
    trap - EXIT
    if [ "$smoke_keep" = 1 ]; then
        printf 'smoke evidence retained: %s\n' "$smoke_dir" >&2
    else
        rm -rf "$smoke_dir"
    fi
    exit "$smoke_status"
}
trap cleanup_smoke EXIT

smoke_input="$smoke_dir/setup.el"
smoke_out="$smoke_dir/script.out"
smoke_control_out="$smoke_dir/control.out"

# Every form below is deliberately written across more than one line, which
# is exactly what a line-oriented REPL cannot take.
cat > "$smoke_input" <<'EOF'
;; A comment between forms must not become a form of its own.
(defun nelisp-repl-script-smoke--step (x)
  (if (= x 1)
      (error "script smoke: deliberate failure")
    (+ x 10)))

(setq nelisp-repl-script-smoke--state
      (list "retained"
            (+ 1 2)))

(princ
 (format "SCRIPT-OK-1 %S\n"
         (nelisp-repl-script-smoke--step 5)))

(princ "SCRIPT-QUOTE \"quoted\" and\na second line\n")

(princ "SCRIPT-NONASCII 日本語\n")

(nelisp-repl-script-smoke--step 1)

(princ
 (format "SCRIPT-AFTER-ERROR %S\n"
         (car nelisp-repl-script-smoke--state)))
EOF

printf 'running --script session\n' >&2
tools/ai/nelisp-ai.sh repl --no-prompt --script "$smoke_input" \
    < /dev/null > "$smoke_out" 2>&1 || true

smoke_require() {
    if ! grep -q -- "$1" "$smoke_out"; then
        printf 'nelisp-ai-repl-script-smoke: missing %s\n' "$1" >&2
        printf -- '--- session output ---\n' >&2
        cat "$smoke_out" >&2
        exit 1
    fi
}

smoke_require 'SCRIPT-OK-1 15'
smoke_require 'SCRIPT-QUOTE "quoted" and'
smoke_require 'a second line'
smoke_require 'SCRIPT-NONASCII 日本語'
smoke_require 'script smoke: deliberate failure'
# The form after the failing one still runs, and a value set before the
# failure is still there: the error was contained to its own form.
smoke_require 'SCRIPT-AFTER-ERROR "retained"'

# Order matters: the continuation marker must come after the failure.
smoke_failure_line=$(grep -n 'script smoke: deliberate failure' "$smoke_out" \
    | head -1 | cut -d: -f1)
smoke_after_line=$(grep -n 'SCRIPT-AFTER-ERROR' "$smoke_out" \
    | head -1 | cut -d: -f1)
if [ "$smoke_after_line" -le "$smoke_failure_line" ]; then
    printf 'nelisp-ai-repl-script-smoke: continuation marker precedes the failure\n' >&2
    exit 1
fi

# Control: the same file fed line by line must NOT complete.
printf 'running line-by-line control\n' >&2
cat "$smoke_input" | tools/ai/nelisp-ai.sh repl --no-prompt \
    > "$smoke_control_out" 2>&1 || true
if grep -q 'SCRIPT-AFTER-ERROR' "$smoke_control_out"; then
    printf 'nelisp-ai-repl-script-smoke: the control path completed, so this smoke proves nothing\n' >&2
    cat "$smoke_control_out" >&2
    exit 1
fi

printf 'nelisp-ai-repl-script-smoke: ok\n'
