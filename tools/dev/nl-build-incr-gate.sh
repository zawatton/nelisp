#!/bin/sh
# nl-build-incr-gate.sh --- post-build gate for the incremental build (design2 A5).
#   sample TREE OUTDIR N [FILE FROM TO]
#                            V1: first N units. Closure-key side runs on a WARM cache
#                            (filled by a prior run), after the optional donor edit
#                            (FILE under TREE, FROM->TO exactly once, in a scratch copy);
#                            FULL_KEY=1 side runs on an EMPTY cache.  Payload sha256
#                            must agree per unit, so a stale cache hit is RED.
#   compare CLOSURE_TSV FULL_TSV   the per-unit payload comparison used by `sample`.
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
  tree=$1; out=$2; n=$3; ef=${4:-}; efrom=${5:-}; eto=${6:-}
  # Closure-key side: WARM cache, populated by a first run on the unedited tree,
  # then run on a scratch copy carrying the donor edit (EF FROM TO, replaced once).
  # FULL_KEY side: EMPTY cache, same scratch tree.  Both sides starting from empty
  # caches (the old form) never consult a cache entry, so no stale hit could show.
  # SAMPLE_CACHE=DIR (G5): use that existing shared cache as the warm side instead of
  # populating one, so its entries are re-verified against a FULL_KEY recompile.
  rm -rf "$out/fullcache" "$out/scratch"
  if [ -n "${SAMPLE_CACHE:-}" ]; then
    mkdir -p "$out"; rm -rf "$out/cache"; cp -a "$SAMPLE_CACHE" "$out/cache" || exit 1
  else
    rm -rf "$out/cache"
    "$here/nl-gate-units.sh" "$tree" "$out/pop" "$n" "$out/cache" || { echo "RED: cache-populating run failed"; exit 1; }
  fi
  t2=$tree
  if [ -n "$ef" ]; then
    t2=$out/scratch; mkdir -p "$t2"
    (cd "$tree" && tar --exclude=./target --exclude=./.git -cf - .) | (cd "$t2" && tar -xf -) || { echo "RED: scratch copy failed"; exit 1; }
    python3 -I - "$t2/$ef" "$efrom" "$eto" <<'PY' || { echo "RED: edit did not apply exactly once"; exit 1; }
import sys
p, a, b = sys.argv[1:4]
t = open(p, encoding="utf-8").read()
if t.count(a) != 1: sys.exit(1)
open(p, "w", encoding="utf-8").write(t.replace(a, b))
PY
  fi
  "$here/nl-gate-units.sh" "$t2" "$out/closure" "$n" "$out/cache" || { echo "RED: warm closure-key run failed"; exit 1; }
  GU_ENV="NELISP_STANDALONE_FULL_KEY=1" "$here/nl-gate-units.sh" "$t2" "$out/full" "$n" "$out/fullcache" || { echo "RED: full-key run failed"; exit 1; }
  "$0" compare "$out/closure/units.tsv" "$out/full/units.tsv"
  ;;
compare)
  py "$@" <<'PY'
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
  # Sample fixture: a simulated unit cache.  A unit row is "unit<TAB>sha"; the
  # cache maps key(source) -> sha.  OLD form = both sides empty caches; NEW form =
  # warm closure side.  A poisoned entry (stale sha stored under the NEW key) must
  # be invisible to the old form and RED in the new one.
  sim() { # sim CACHE SRC OUT : fill OUT/units.tsv from CACHE (hit) or fresh compile
    mkdir -p "$1" "$3"; : > "$3/units.tsv"
    for u in u1 u2 u3; do
      k=$(printf '%s' "$(cat "$2/$u")" | sha1sum | cut -c1-12)
      if [ -f "$1/$u.$k" ]; then v=$(cat "$1/$u.$k"); else v=$(printf 'payload:%s' "$(cat "$2/$u")" | sha1sum | cut -c1-12); echo "$v" > "$1/$u.$k"; fi
      printf 'unit\t%s\tk\t%s\tc\n' "$u" "$v" >> "$3/units.tsv"
    done; }
  mkdir -p "$t/s0" "$t/s1"; for u in u1 u2 u3; do echo "body $u" > "$t/s0/$u"; echo "body $u" > "$t/s1/$u"; done
  echo "body u2 edited" > "$t/s1/u2"
  sim "$t/c" "$t/s0" "$t/w0"                              # warm the cache on the unedited tree
  nk=$(printf '%s' "$(cat "$t/s1/u2")" | sha1sum | cut -c1-12)
  echo stale000stale > "$t/c/u2.$nk"                      # poison: stale payload under the NEW key
  sim "$t/oldc" "$t/s1" "$t/oc"; sim "$t/oldf" "$t/s1" "$t/of"   # old form: two empty caches
  "$0" compare "$t/oc/units.tsv" "$t/of/units.tsv" >/dev/null && echo "ok   old form is GREEN on a poisoned cache (vacuous)" || { echo "FAIL old-form fixture"; rc=1; }
  sim "$t/c" "$t/s1" "$t/nc"; sim "$t/newf" "$t/s1" "$t/nf"       # new form: warm vs empty
  if "$0" compare "$t/nc/units.tsv" "$t/nf/units.tsv" >/dev/null; then echo "FAIL new form not RED on poisoned cache"; rc=1; else echo "ok   new form is RED on a poisoned cache"; fi
  # Negative control: no poison => the new form is GREEN.
  rm -f "$t/c/u2.$nk"; sim "$t/c" "$t/s1" "$t/nc2"
  "$0" compare "$t/nc2/units.tsv" "$t/nf/units.tsv" >/dev/null || { echo "FAIL new form RED without poison"; rc=1; }
  [ $rc = 0 ] && echo "selftest GREEN"; rm -rf "$t"; exit $rc ;;
*) sed -n 2,17p "$0"; exit 2 ;;
esac
