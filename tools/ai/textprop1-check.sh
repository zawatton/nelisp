#!/bin/sh
# usage: textprop1-check.sh corpus|fuzz|smoke BINARY [OVERLAY.el]
# corpus: tools/ai/textprop1-corpus.el on GNU Emacs and BINARY, must be equal except ALLOW (cases outside the
#         text-property engine: char-property functions and insert-buffer-substring are not in the standalone
#         prelude, buffer-string ignores narrowing (lane buflocal1), format keeps one interval per piece).
# fuzz:   tools/ai/textprop1-fuzz.el, three seeds, identical to GNU Emacs.
cd "$(dirname "$0")/../.." || exit 1
MODE=$1; BIN=$2; OV=$3
T=${TMPDIR:-/tmp}/textprop1-check.$$; mkdir -p "$T"; trap 'rm -rf "$T"' EXIT
run_nl() { ulimit -v 8388608; if [ -n "$OV" ]; then timeout 300 "$BIN" --load "$OV" --load "$1" -- ; else timeout 300 "$BIN" --load "$1" -- ; fi; }
case $MODE in
corpus)
  ALLOW='^(nscpc|pscpc|b-nscpc|b-get-pt|b-insert-buffer-substring|b-narrow-get|format-prop-template) '
  /usr/local/bin/emacs -Q --batch -l tools/ai/textprop1-corpus.el > "$T/g" 2>/dev/null
  run_nl tools/ai/textprop1-corpus.el > "$T/n" 2>"$T/e"
  diff "$T/g" "$T/n" | grep '^<' | sed 's/^< //' | grep -Ev "$ALLOW" > "$T/d"
  n=$(wc -l < "$T/d"); echo "corpus mismatches=$n cases=$(wc -l < "$T/g")"; head -5 "$T/d" | cut -c1-200; [ "$n" = 0 ] ;;
fuzz)
  bad=0
  for s in 7 11 23; do
    TF_SEED=$s TF_RUNS=40 /usr/local/bin/emacs -Q --batch -l tools/ai/textprop1-fuzz.el > "$T/g$s" 2>/dev/null
    TF_SEED=$s TF_RUNS=40 run_nl tools/ai/textprop1-fuzz.el > "$T/n$s" 2>/dev/null
    [ "$(diff "$T/g$s" "$T/n$s" | grep -c "^<")" = 0 ] || bad=$((bad+1))
  done
  echo "fuzz seeds differing=$bad of 3"; [ "$bad" = 0 ] ;;
smoke)
  run_nl scripts/standalone-textprop-smoke.el 2>&1 | grep TEXTPROP-SMOKE | tee "$T/s"; grep -q 'mismatches=0' "$T/s" ;;
esac
