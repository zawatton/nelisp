#!/bin/sh
# Doc 236 S6 end-to-end: entry checks.  Usage: e2e-entry.sh differential | standalone BIN
MODE="$1"; BIN="$2"
cd "$(dirname "$0")/../../.." || exit 1
ROOT=$(pwd)
EMACS=${EMACS:-emacs}
T=$(mktemp -d) || exit 1
trap 'rm -rf "$T"' EXIT
host() { (ulimit -v 8388608; $EMACS --batch -Q -L lisp -L packages/nl-typecheck/src -L packages/nl-prelude/src -L packages/nl-safe/src -L packages/nl-static/src -l packages/nl-typecheck/test/e2e-entry.el -- "$ROOT" "$1" "$2"); }
case "$MODE" in
  differential)
    host on 0 > "$T/on" 2>"$T/on.err" || exit 1
    host off 0 > "$T/off" 2>"$T/off.err" || exit 1
    cmp "$T/on" "$T/off" || { diff "$T/on" "$T/off" | head -5; exit 1; }
    grep -q '^(3 ' "$T/on" || exit 1
    host on 1 > "$T/bad-on" 2>"$T/bad-on.err" || exit 1
    host off 1 > "$T/bad-off" 2>"$T/bad-off.err" || exit 1
    grep -q 'SIGNALLED (wrong-type-argument (lambda (x) (or (integerp x) (stringp x))) 1.5)' "$T/bad-on" || exit 1
    grep -q 'bad call returned' "$T/bad-off" || exit 1 ;;
  standalone)
    test -x "$BIN" || { echo "e2e-entry: NELISP_BIN not executable: $BIN"; exit 2; }
    OUT=$( (ulimit -v 8388608; "$BIN" -l packages/nl-typecheck/test/e2e-entry.el -- "$ROOT" on 1) 2>&1 ) || { echo "$OUT"; exit 1; }
    echo "$OUT"
    echo "$OUT" | grep -q 'SIGNALLED (wrong-type-argument (lambda (x) (or (integerp x) (stringp x))) 1.5)' || exit 1 ;;
  *) echo "usage: e2e-entry.sh differential | standalone BIN"; exit 2 ;;
esac
exit 0
