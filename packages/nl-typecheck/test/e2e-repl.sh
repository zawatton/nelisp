#!/bin/sh
# Doc 236 S4/S5/S7 end-to-end sessions.
# Usage: e2e-repl.sh hook-off|input-check|redefine|recover|standalone [BIN]
MODE="$1"; BIN="$2"
cd "$(dirname "$0")/../../.." || exit 1
ROOT=$(pwd)
EMACS=${EMACS:-emacs}
T=$(mktemp -d) || exit 1
trap 'rm -rf "$T"' EXIT
LOADS="-L lisp -L packages/nl-prelude/src -L packages/nl-safe/src -L packages/nl-static/src -L packages/nl-typecheck/src"
run_host() {
  (ulimit -v 8388608; $EMACS --batch -Q $LOADS -l packages/nl-typecheck/test/e2e-repl.el "$@")
}
case "$MODE" in
  hook-off)
    run_host hook-off "$T/out" || exit 1
    cmp "$T/out.plain" "$T/out.driver" || exit 1
    test -s "$T/out.plain" || exit 1 ;;
  input-check|redefine|recover)
    run_host "$MODE" || exit 1 ;;
  standalone)
    test -x "$BIN" || { echo "e2e-repl: NELISP_BIN not executable: $BIN"; exit 2; }
    SETUP="(progn (setq nl-e2e-root \"$ROOT\") (load \"$ROOT/packages/nl-typecheck/test/e2e-repl-standalone-setup.el\"))"
    CORPUS='(+ 1 2)
(setq e2e-x 5)
(list e2e-x "a" (quote b))
(defun e2e-f (n) (* n 3))
(e2e-f 4)
(car (quote (1 2)))
(concat "a" "b")'
    # Hook nil: transcript identical with and without nl-typecheck loaded.
    printf '"marker"\n%s\n' "$CORPUS" | (ulimit -v 8388608; "$BIN" --repl --no-prompt) 2>&1 | sed '0,/^"marker"$/d' > "$T/base" || exit 1
    printf '%s\n"marker"\n%s\n' "$SETUP" "$CORPUS" | (ulimit -v 8388608; "$BIN" --repl --no-prompt) 2>&1 | sed '0,/^"marker"$/d' > "$T/loaded" || exit 1
    test -s "$T/base" || { echo "e2e-repl: empty baseline transcript"; exit 1; }
    cmp "$T/base" "$T/loaded" || { echo "e2e-repl: hook-nil transcript differs"; exit 1; }
    # Hook enabled: clashing input refused, valid input evaluated.
    printf '%s\n' "$SETUP" '(nelisp-typecheck-repl-mode 1)' '(setq nelisp-typecheck-strict t)' \
      '(defun clash-fn (n) (declare (ftype (function (integer) integer))) (* n 2))' \
      '(progn (setq probe-flag t) (clash-fn "s"))' \
      "(list 'probe (boundp 'probe-flag))" \
      '(clash-fn 4)' | (ulimit -v 8388608; "$BIN" --repl --no-prompt) > "$T/hooked" 2>&1
    cat "$T/hooked"
    grep -q '<repl>:[0-9]*:[0-9]*: error: argument 1 of clash-fn' "$T/hooked" || exit 1
    grep -q 'refused' "$T/hooked" || exit 1
    grep -q '^(probe nil)$' "$T/hooked" || exit 1
    tail -1 "$T/hooked" | grep -q '^8$' || exit 1 ;;
  *) echo "usage: e2e-repl.sh MODE [BIN]"; exit 2 ;;
esac
exit 0
