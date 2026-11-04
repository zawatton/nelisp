#!/bin/bash
# Doc 243 S3.1: end-to-end Ir and wall of a bare start under SYN, plus the Doc 242 checks on the candidate.
# Env: COLD_INIT_BIN (candidate, default build-on56), COLD_INIT_REF (default build-on33), COLD_INIT_BUILDS,
#      NL243_W (worktree root override), COLD_INIT_ROUNDS (15), A242_QUIET_LOAD/A242_QUIET_BUSY/A242_QUIET_WAIT,
#      COLD_INIT_SKIP_DOC242=1 skips part (c) (debugging only; never counts as PASS).
set -u; export LC_ALL=C
HERE=$(cd "$(dirname "$0")" && pwd)
NB=${COLD_INIT_BUILDS:-$HOME/.cache/tmp/nelisp-sbcl-impl-builds}
BIN=${COLD_INIT_BIN:-$NB/build-on56/nelisp-dynamic}
REF=${COLD_INIT_REF:-$NB/build-on33/nelisp-dynamic}
W=${NL243_W:-$(cd "$HERE/../../../.." && pwd)}
EV=$W/tools/ai/sbcl-impl-evidence
NOP=$HERE/../a242-s2/nop.el; [ -r "$NOP" ] || NOP=$EV/a242-s2/nop.el
DRIVER=$HOME/.cache/tmp/nelisp-cross-language-20261029-t8wGC4/driver.el
for x in "$BIN" "$REF" "$NOP"; do [ -e "$x" ] || { echo "FAIL: missing $x"; echo S3.1-FAIL; exit 1; }; done
[ -r "$DRIVER" ] || { echo "FAIL: pinned driver.el missing: $DRIVER (sha eadbaf40...); restore it, this script does not"; echo S3.1-FAIL; exit 1; }
for t in valgrind python3; do command -v $t >/dev/null || { echo "FAIL: $t not installed"; echo S3.1-FAIL; exit 1; }; done
mkdir -p "$HOME/.cache/tmp"; T=$(mktemp -d "$HOME/.cache/tmp/a243s31.XXXXXX"); trap 'rm -rf "$T"' EXIT
fail=0; invalid=0; bad() { echo "FAIL: $*"; fail=1; }
SYNV=(); for i in $(seq 0 59); do SYNV+=("NL243_VAR_$i=$(printf '%064d' $((i*7919+13)) | tr 0-9 a-j)"); done
ir() { (ulimit -v 8388608; env -i PATH=/usr/bin:/bin HOME="$HOME" "${SYNV[@]}" valgrind --tool=callgrind --callgrind-out-file="$T/cg.$2" "$1" --load "$NOP" >/dev/null 2>&1)
  sed -n 's/^summary: *\([0-9]*\).*/\1/p' "$T/cg.$2" | head -1; }
# Order (2026-11-03): (b) first, while no work of this script has raised the
# load average; then (c), whose Doc 242 checks wait out their own build-up
# before their timing; (a) last, since Ir does not depend on the load.  Every
# quiet guard waits up to 150 s (A242_QUIET_WAIT): the work itself takes
# ~50 s, so three full waits still end within the ledger's 600 s.
export A242_QUIET_WAIT=${A242_QUIET_WAIT:-150} A240_QUIET_WAIT=${A240_QUIET_WAIT:-150}
# (b) quiet paired wall, bare start under SYN
quiet() { python3 -I - "$1" "${A242_QUIET_LOAD:-0.5}" "${A242_QUIET_BUSY:-0.3}" <<'QP'
import sys, time, os
label, max_load, max_busy = sys.argv[1], float(sys.argv[2]), float(sys.argv[3])
def stat():
    v = list(map(int, open("/proc/stat").readline().split()[1:]))
    return sum(v), v[3] + v[4]
t0, i0 = stat(); time.sleep(2.0); t1, i1 = stat()
busy = ((t1 - t0) - (i1 - i0)) / (t1 - t0) * os.cpu_count()
load = float(open("/proc/loadavg").read().split()[0])
ok = load <= max_load and busy <= max_busy
print(f"QUIET {label}: load1 {load:.2f} (max {max_load}), busy CPUs {busy:.2f} (max {max_busy}) -> {'quiet' if ok else 'BUSY'}")
sys.exit(0 if ok else 1)
QP
}
WAITED=0; QOK=1
until quiet before >/dev/null; do
  [ "$WAITED" -lt "${A242_QUIET_WAIT:-60}" ] || { QOK=0; break; }
  sleep 10; WAITED=$((WAITED + 12))
done
if [ $QOK = 0 ]; then echo "INVALID (b) machine not quiet before timing (waited ${WAITED}s)"; invalid=1
else
  python3 -I - "$BIN" "$REF" "$NOP" "${COLD_INIT_ROUNDS:-15}" "${SYNV[@]}" <<'PY'
import os, resource, statistics, subprocess, sys, time
bin_, ref, nop, rounds = sys.argv[1:5]; syn = sys.argv[5:]
env = dict(a.split("=", 1) for a in syn); env.update(PATH="/usr/bin:/bin", HOME=os.environ["HOME"])
rounds = int(rounds)
cap = lambda: resource.setrlimit(resource.RLIMIT_AS, (8388608 * 1024,) * 2)
def run(b):
    t = time.perf_counter()
    p = subprocess.run([b, "--load", nop], env=env, stdin=subprocess.DEVNULL, capture_output=True, timeout=300, preexec_fn=cap)
    if p.returncode != 0: sys.exit(f"FAIL: {b} rc={p.returncode}")
    return (time.perf_counter() - t) * 1000
tb, tr = [], []
for r in range(rounds):
    order = [(bin_, tb), (ref, tr)] if r % 2 == 0 else [(ref, tr), (bin_, tb)]
    for b, acc in order: acc.append(run(b))
mb, mr = statistics.median(tb), statistics.median(tr)
print(f"(b) bare start SYN wall median ms: cand {mb:.1f} ref {mr:.1f} ratio {mb/mr:.4f} over {rounds} alternating rounds")
sys.exit(0 if mb <= 0.95 * mr else 1)
PY
  RC=$?
  if ! quiet after >/dev/null; then echo "INVALID (b) machine busy after timing"; invalid=1
  elif [ $RC = 0 ]; then echo "PASS (b) cand median at least 5% faster"
  else echo "FAIL (b) cand median not 5% faster than ref"; fail=1; fi
fi
# (c) Doc 242 checks on the candidate
if [ "${COLD_INIT_SKIP_DOC242:-0}" = 1 ]; then echo "SKIP (c) Doc 242 checks skipped by request"; fail=1
else
  export IMAGE_SIZE_OUT=$T/s41.txt IMAGE_SIZE_SCRATCH=$T/s41-scratch   # do not overwrite tracked evidence
  for c in a242-s2/check-s21.sh a242-s3/check-s31.sh; do
    K=$(basename "$(dirname "$BIN")" | sed -n 's/^build-on//p'); DRV=$NB/run/b$K; [ -r "$DRV/benchmark-driver-native-on.el" ] || DRV=$NB/run/b33
    if COLD_LOAD_BIN=$BIN COLD_LOAD_DRIVERS=$DRV bash "$EV/$c" > "$T/doc242.log" 2>&1; then echo "PASS (c) $c with COLD_LOAD_BIN=candidate"
    else echo "FAIL (c) $c with COLD_LOAD_BIN=candidate: $(tail -2 "$T/doc242.log" | tr '\n' ' ' | cut -c1-200)"; fail=1; fi
  done
fi
# (a) Ir under SYN
IC=$(ir "$BIN" c); IR=$(ir "$REF" r)
if [ -n "$IC" ] && [ -n "$IR" ] && [ "$(awk -v c=$IC -v r=$IR 'BEGIN{print (c<=0.85*r)?1:0}')" = 1 ]; then echo "PASS (a) Ir(SYN) cand $IC ref $IR ratio $(awk -v c=$IC -v r=$IR 'BEGIN{printf "%.4f", c/r}') <= 0.85"
else echo "FAIL (a) Ir(SYN) cand ${IC:-?} ref ${IR:-?} ratio $(awk -v c=${IC:-0} -v r=${IR:-1} 'BEGIN{printf "%.4f", c/r}') > 0.85"; fail=1; fi
if [ $fail = 0 ] && [ $invalid = 0 ]; then echo S3.1-PASS; exit 0; fi
[ $invalid = 1 ] && echo "S3.1-INVALID (rerun on a quiet machine)"
echo S3.1-FAIL; exit 1
