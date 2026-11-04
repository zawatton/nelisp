#!/bin/bash
# Doc 243 S4 helper: net Ir per call of a lambda kind (closed | free) called through
# mapcar from a compiled function, net of an identical baseline that maps the builtin
# `1+'.  net = ((Ir_kind(2000) - Ir_kind(0)) - (Ir_base(2000) - Ir_base(0))) / 2000.
# Pass: candidate net <= 25% of the reference's net, and RESULT lines equal GNU Emacs
# for kind and base on both binaries.  Usage: check-net.sh KIND LABEL
# Env: COLD_INIT_BIN (default build-on56), COLD_INIT_REF (default build-on33), GNU_EMACS.
set -u
KIND=$1; LABEL=$2
HERE=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
NB=$HOME/.cache/tmp/nelisp-sbcl-impl-builds
BIN=${COLD_INIT_BIN:-$NB/build-on56/nelisp-dynamic}
REF=${COLD_INIT_REF:-$NB/build-on33/nelisp-dynamic}
GNU=${GNU_EMACS:-emacs-31.1}
for f in "$BIN" "$REF"; do [ -x "$f" ] || { echo "FAIL: not executable: $f"; echo "$LABEL-FAIL"; exit 1; }; done
T=$(mktemp -d "$HOME/.cache/tmp/a243s4net.XXXXXX") || exit 1; trap 'rm -rf "$T"' EXIT
for v in base "$KIND"; do for n in 0 2000; do sed "s/@N@/$n/" "$HERE/$v.el.in" > "$T/$v$n.el"; done; done
ir() { (ulimit -v 8388608; valgrind --tool=callgrind --callgrind-out-file="$T/cg.$1.$2" "$3" --load "$T/$2.el" > "$T/out.$1.$2" 2>/dev/null); sed -n 's/^summary: //p' "$T/cg.$1.$2"; }
ARGS=()
for side in ref cand; do b=$REF; [ $side = cand ] && b=$BIN
  for v in base "$KIND"; do for n in 0 2000; do ARGS+=("$(ir $side $v$n "$b")"); done; done; done
G1=$( (ulimit -v 8388608; "$GNU" -Q --batch -l "$T/base2000.el" 2>/dev/null) | grep -m1 '^RESULT')
G2=$( (ulimit -v 8388608; "$GNU" -Q --batch -l "$T/${KIND}2000.el" 2>/dev/null) | grep -m1 '^RESULT')
R=()
for side in ref cand; do for v in base "$KIND"; do R+=("$(grep -m1 -o 'RESULT [0-9]*' "$T/out.$side.${v}2000")"); done; done
python3 -I - "$LABEL" "$G1" "$G2" "${R[@]}" "${ARGS[@]}" <<'PY'
import sys
label, g1, g2, rb, rk, cb, ck = sys.argv[1:8]
r = list(map(int, sys.argv[8:]))
net = lambda a: ((a[3] - a[2]) - (a[1] - a[0])) / 2000
rn, cn = net(r[0:4]), net(r[4:8])
ok1 = cn <= 0.25 * rn
ok2 = bool(g1) and bool(g2) and g1 == rb == cb and g2 == rk == ck
print(f"{'PASS' if ok1 else 'FAIL'} (a) net Ir per call cand {cn:.0f} ref {rn:.0f} ratio {cn/rn:.3f} (max 0.25)")
print(f"{'PASS' if ok2 else 'FAIL'} (b) results GNU {g1!r}/{g2!r} ref {rb!r}/{rk!r} cand {cb!r}/{ck!r}")
print(f"{label}-PASS" if ok1 and ok2 else f"{label}-FAIL"); sys.exit(0 if ok1 and ok2 else 1)
PY
