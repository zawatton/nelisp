#!/bin/sh
# nl-build-incr-gate.sh --- post-build gate for the incremental build (design2 A5).
#   sample TREE OUTDIR N     V1: first N units compiled with the closure key and
#                            with NELISP_STANDALONE_FULL_KEY=1 (cold-equivalent
#                            global key); payload sha256 must agree per unit.
#   expected EDITED TRACE UNITS BEFOREDIR AFTERDIR
#                            V2: expected rebuild list for a one-defun edit.
#                            EDITED = symbol name; TRACE = nl-key-trace.sh output
#                            (post-key functions); UNITS = file, one unit per line;
#                            BEFOREDIR/AFTERDIR = UNIT_SOURCE_DUMP dirs.
#                            EDITED in TRACE => every unit; else units whose dump differs.
#   check EXPECTED BUILD_TRACE.jsonl
#                            observed misses (phase cache-compile, event end) must equal
#                            EXPECTED exactly; empty observed with a non-empty expected is RED.
#   selftest                 fixtures: right trace GREEN, poisoned trace RED.
# Exit 0 = GREEN, 1 = RED, 2 = usage.  Never builds anything itself except `sample`.
set -u
here=$(cd "$(dirname "$0")" && pwd)
cmd=${1:-}; [ $# -gt 0 ] && shift
py() { exec python3 -I - "$@"; }
case $cmd in
sample)
  tree=$1; out=$2; n=$3
  "$here/nl-gate-units.sh" "$tree" "$out/closure" "$n" || { echo "RED: closure-key run failed"; exit 1; }
  GU_ENV="NELISP_STANDALONE_FULL_KEY=1" "$here/nl-gate-units.sh" "$tree" "$out/full" "$n" || { echo "RED: full-key run failed"; exit 1; }
  py "$out/closure/units.tsv" "$out/full/units.tsv" <<'PY'
import sys
def rows(p):
    return {l.split("\t")[1]: l.split("\t")[3] for l in open(p) if l.count("\t") >= 4}
a, b = rows(sys.argv[1]), rows(sys.argv[2])
if not a or set(a) != set(b):
    print("RED: unit sets differ or empty", sorted(a), sorted(b)); sys.exit(1)
bad = [u for u in a if a[u] != b[u]]
print("RED: payload differs for %s" % bad if bad else "GREEN: %d units cold-equivalent" % len(a)); sys.exit(1 if bad else 0)
PY
  ;;
expected)
  py "$@" <<'PY'
import sys, os
edited, trace, units, bd, ad = sys.argv[1:6]
T = set(l.strip() for l in open(trace) if l.strip())
U = [l.strip() for l in open(units) if l.strip()]
if edited in T:
    out = U
else:
    def rd(d, u):
        p = os.path.join(d, u + ".el")
        return open(p, "rb").read() if os.path.exists(p) else None
    out = [u for u in U if rd(bd, u) != rd(ad, u)]
print("\n".join(sorted(out)))
PY
  ;;
check)
  py "$@" <<'PY'
import sys, json
exp = set(l.strip() for l in open(sys.argv[1]) if l.strip())
obs = set()
for l in open(sys.argv[2]):
    try: r = json.loads(l)
    except ValueError: continue
    if r.get("phase") == "cache-compile" and r.get("event") == "end" and r.get("unit"): obs.add(r["unit"])
if obs == exp:
    print("GREEN: %d rebuilt units match" % len(obs)); sys.exit(0)
print("RED: unexpected rebuilds %s, missing rebuilds %s" % (sorted(obs - exp), sorted(exp - obs))); sys.exit(1)
PY
  ;;
selftest)
  t=$(mktemp -d "${TMPDIR:-$HOME/.cache/tmp}/nlgate.XXXXXX") || exit 2
  mkdir -p "$t/b" "$t/a"; printf 'u1.o\nu2.o\nu3.o\n' > "$t/units"
  for u in u1 u2 u3; do echo "(src $u)" > "$t/b/$u.o.el"; cp "$t/b/$u.o.el" "$t/a/$u.o.el"; done
  echo "(src u2 edited)" > "$t/a/u2.o.el"
  printf 'f-closure\nf-key\n' > "$t/trace"; printf 'f-closure\n' > "$t/poison"   # poison: f-key dropped
  j() { for u in "$@"; do printf '{"phase":"cache-compile","event":"end","unit":"%s"}\n' "$u"; done; }
  rc=0
  "$0" expected f-producer "$t/trace" "$t/units" "$t/b" "$t/a" > "$t/e1"
  [ "$(cat "$t/e1")" = "u2.o" ] || { echo "FAIL producer edit expected list: $(cat "$t/e1")"; rc=1; }
  j u2.o > "$t/o1"; "$0" check "$t/e1" "$t/o1" >/dev/null || { echo "FAIL good producer run not GREEN"; rc=1; }
  "$0" expected f-key "$t/trace" "$t/units" "$t/b" "$t/a" > "$t/e2"
  [ "$(wc -l < "$t/e2" | tr -d ' ')" = 3 ] || { echo "FAIL closure edit should expect all"; rc=1; }
  j u1.o u2.o u3.o > "$t/o2"; "$0" check "$t/e2" "$t/o2" >/dev/null || { echo "FAIL all-miss run not GREEN"; rc=1; }
  # Poisoned trace: f-key is a real post-key function but missing from the trace.
  "$0" expected f-key "$t/poison" "$t/units" "$t/b" "$t/a" > "$t/e3"
  if "$0" check "$t/e3" "$t/o2" >/dev/null; then echo "FAIL poisoned trace not RED"; rc=1; else echo "ok   poisoned trace is RED"; fi
  # Negative control: a stale hit (nothing rebuilt) must be RED too.
  : > "$t/o3"; "$0" check "$t/e2" "$t/o3" >/dev/null && { echo "FAIL empty observed not RED"; rc=1; }
  [ $rc = 0 ] && echo "selftest GREEN"; rm -rf "$t"; exit $rc ;;
*) sed -n 2,17p "$0"; exit 2 ;;
esac
