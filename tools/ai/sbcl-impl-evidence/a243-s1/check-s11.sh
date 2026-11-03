#!/bin/bash
# Doc 243 S1.1: environment reinit cost and equality.
# Env: COLD_INIT_BIN (candidate, default build-on56), COLD_INIT_REF (default build-on33),
#      COLD_INIT_BUILDS (build root), NL243_W (worktree root override, for tests outside the worktree).
set -u; export LC_ALL=C
HERE=$(cd "$(dirname "$0")" && pwd)
NB=${COLD_INIT_BUILDS:-$HOME/.cache/tmp/nelisp-sbcl-impl-builds}
BIN=${COLD_INIT_BIN:-$NB/build-on56/nelisp-dynamic}
REF=${COLD_INIT_REF:-$NB/build-on33/nelisp-dynamic}
W=${NL243_W:-$(cd "$HERE/../../../.." && pwd)}
NOP=$HERE/../a242-s2/nop.el; [ -r "$NOP" ] || NOP=$W/tools/ai/sbcl-impl-evidence/a242-s2/nop.el
for x in "$BIN" "$REF" "$NOP" "$HERE/env-probe.el"; do [ -e "$x" ] || { echo "FAIL: missing $x"; echo S1.1-FAIL; exit 1; }; done
command -v valgrind >/dev/null || { echo "FAIL: valgrind not installed"; echo S1.1-FAIL; exit 1; }
mkdir -p "$HOME/.cache/tmp"; T=$(mktemp -d "$HOME/.cache/tmp/a243s11.XXXXXX"); trap 'rm -rf "$T"' EXIT
mkdir -p "$T/tmp" "$T/xdg"
fail=0; bad() { echo "FAIL: $*"; fail=1; }
# Deterministic synthetic environment: 60 vars with 64 chars derived from i.
SYNV=(); for i in $(seq 0 59); do SYNV+=("NL243_VAR_$i=$(printf '%064d' $((i*7919+13)) | tr 0-9 a-j)"); done
envof() { case $1 in
  MIN) echo "PATH=/usr/bin:/bin HOME=$HOME";;
  SYN) echo "PATH=/usr/bin:/bin HOME=$HOME ${SYNV[*]}";;
  ALT) echo "PATH=/usr/bin:/bin HOME=$HOME ${SYNV[*]} TMPDIR=$T/tmp XDG_CONFIG_HOME=$T/xdg";; esac; }
ir() { # BIN ENVNAME TAG -> Ir
  # shellcheck disable=SC2046
  (ulimit -v 8388608; env -i $(envof "$2") valgrind --tool=callgrind --callgrind-out-file="$T/cg.$3" "$1" --load "$NOP" >/dev/null 2>&1)
  sed -n 's/^summary: *\([0-9]*\).*/\1/p' "$T/cg.$3" | head -1; }
# (a) env-size sensitivity
CMIN=$(ir "$BIN" MIN cmin); CSYN=$(ir "$BIN" SYN csyn); RMIN=$(ir "$REF" MIN rmin); RSYN=$(ir "$REF" SYN rsyn)
if [ -z "$CMIN$CSYN$RMIN$RSYN" ] || [ -z "$CMIN" ] || [ -z "$CSYN" ] || [ -z "$RMIN" ] || [ -z "$RSYN" ]; then bad "(a) callgrind produced no total"
else
  CD=$((CSYN-CMIN)); RD=$((RSYN-RMIN))
  RATIO=$(awk -v c=$CD -v r=$RD 'BEGIN{ if (r>0) printf "%.4f", c/r; else print "nan"}')
  if [ "$RD" -gt 0 ] && [ "$(awk -v c=$CD -v r=$RD 'BEGIN{print (c<=0.25*r)?1:0}')" = 1 ]; then
    echo "PASS (a) delta Ir(SYN-MIN): cand $CD (MIN $CMIN SYN $CSYN) ref $RD (MIN $RMIN SYN $RSYN) ratio $RATIO <= 0.25"
  else
    echo "FAIL (a) delta Ir(SYN-MIN): cand $CD (MIN $CMIN SYN $CSYN) ref $RD (MIN $RMIN SYN $RSYN) ratio $RATIO > 0.25"; fail=1
  fi
fi
# (b) probe equality per environment
for e in MIN SYN ALT; do
  for w in cand ref; do
    b=$BIN; [ $w = ref ] && b=$REF
    # shellcheck disable=SC2046
    (cd "$T" && ulimit -v 8388608 && env -i $(envof $e) timeout 120 "$b" --load "$HERE/env-probe.el" > "$T/p.$e.$w" 2>&1 </dev/null); echo "rc=$?" >> "$T/p.$e.$w"
  done
  n=$(wc -l < "$T/p.$e.ref")
  if [ "$n" -ge 10 ] && grep -q '^rc=0$' "$T/p.$e.ref" && cmp -s "$T/p.$e.cand" "$T/p.$e.ref"; then echo "PASS (b) probe equal under $e ($n lines)"
  else echo "FAIL (b) probe differs or failed under $e (ref lines $n)"; diff "$T/p.$e.cand" "$T/p.$e.ref" | head -4 | cut -c1-160; fail=1; fi
done
# negative control for the comparison itself
cp "$T/p.MIN.ref" "$T/neg"; printf x >> "$T/neg"; cmp -s "$T/neg" "$T/p.MIN.ref" && bad "(b) cmp did not detect a change"
[ $fail = 0 ] && { echo S1.1-PASS; exit 0; }
echo S1.1-FAIL; exit 1
