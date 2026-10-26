#!/bin/sh
# Regenerate the "GENERATED nelisp-timefns" block of scripts/nelisp-stdlib-prelude.el from
# lisp/nelisp-timefns.el (GNU Emacs byte-compiles each defun; see timefns-genblock.el).
# usage: tools/ai/timefns-genblock.sh [PRELUDE]   (env EMACS, default emacs)
set -e
ROOT=$(cd "$(dirname "$0")/../.." && pwd)
PRELUDE=${1:-$ROOT/scripts/nelisp-stdlib-prelude.el}
EMACS=${EMACS:-emacs}
TMP=$(mktemp); OUT=$(mktemp)
LC_ALL=C "$EMACS" -Q --batch -l "$ROOT/tools/ai/timefns-genblock.el" "$ROOT/lisp/nelisp-timefns.el" "$TMP"
awk -v blk="$TMP" '
  /^;;; BEGIN GENERATED nelisp-timefns/ { print; while ((getline line < blk) > 0) print line; skip=1; next }
  /^;;; END GENERATED nelisp-timefns/ { skip=0 }
  !skip { print }' "$PRELUDE" > "$OUT"
grep -q "BEGIN GENERATED nelisp-timefns" "$PRELUDE" || { echo "markers missing in $PRELUDE" >&2; exit 1; }
cat "$OUT" > "$PRELUDE"
rm -f "$TMP" "$OUT"
echo "regenerated nelisp-timefns block in $PRELUDE"
