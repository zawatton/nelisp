#!/bin/bash
# Doc 243 S5.1: compiling an anonymous lambda costs at most 25% of build45's
# Ir (net Ir per `nelisp--fx-register-fn' over 200 distinct closures), and
# the compiler's output for every named function equals build45's.
set -u
H=$(cd "$(dirname "$0")" && pwd)
B=${BUILDS:-$HOME/.cache/tmp/nelisp-sbcl-impl-builds}
REF=${REF_BIN:-$B/build-on45/nelisp-dynamic}
BIN=${CAND_BIN:-$B/build-on56/nelisp-dynamic}
T=$(mktemp -d) || exit 1; trap 'rm -rf "$T"' EXIT
for f in "$REF" "$BIN"; do [ -x "$f" ] || { echo "FAIL: missing $f"; exit 1; }; done
net() {
  local b=$1 r=()
  for n in 0 200; do
    sed "s/@N@/$n/" "$H/cost.el.in" > "$T/c$n.el"
    (ulimit -v 8388608; valgrind --tool=callgrind --callgrind-out-file="$T/cg" "$b" --load "$T/c$n.el" > "$T/o" 2>/dev/null)
    grep -q "^REG $n" "$T/o" || { echo "FAIL: probe output on $b"; exit 1; }
    r+=("$(sed -n 's/^summary: //p' "$T/cg")")
  done
  echo $(( (r[1] - r[0]) / 200 ))
}
a=$(net "$REF") || exit 1; b=$(net "$BIN") || exit 1
ratio=$(python3 -c "print(f'{$b/$a:.3f}')")
echo "net Ir per registration: build45 $a, candidate $b, ratio $ratio"
for x in REF BIN; do
  (ulimit -v 8388608; timeout 600 "${!x}" --load "$H/codes.el" 2>/dev/null | grep -v '^"' > "$T/$x.txt")
done
# Compare the functions both images compiled from the same source body: a
# later prelude change to an unrelated function (or `nelisp--fx-compile'
# itself, compiled only from build46 on) is not a compiler difference.
r=$(python3 -I - "$T/REF.txt" "$T/BIN.txt" <<'PY'
import sys
def load(p):
    d = {}
    for line in open(p, encoding='utf-8', errors='replace'):
        parts = line.rstrip('\n').split('\t', 2)
        if len(parts) == 3:
            d[(parts[0], parts[1])] = parts[2]
    return d
a, b = load(sys.argv[1]), load(sys.argv[2])
common = set(a) & set(b)
diff = sorted(k[0] for k in common if a[k] != b[k])
print(len(common), len(diff), len(a), len(b), ' '.join(diff[:5]))
PY
)
set -- $r
if [ "${2:-1}" = 0 ] && [ "${1:-0}" -gt 1000 ]; then echo "compiler output equal on $1 functions with the same body (reference $3, candidate $4 compiled)"
else echo "FAIL: compiler output differs on ${2:-?} of ${1:-?} same-body functions: ${*:5}"; exit 1; fi
python3 -c "import sys; sys.exit(0 if $b <= 0.25*$a else 1)" || { echo "FAIL: ratio $ratio > 0.25"; exit 1; }
echo PASS
