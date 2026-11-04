#!/bin/bash
# S4.1 E2E: bare start and Doc 240 app launcher (hash-get1024, N=16384), new image vs reference, paired.
# Env: IMAGE_SIZE_REF_BIN (default build-on28), IMAGE_SIZE_BIN (default build-on31),
#      IMAGE_SIZE_REF_DRIVERS (default run/b28), IMAGE_SIZE_DRIVERS (default run/b31),
#      IMAGE_SIZE_ROUNDS (default 15), IMAGE_SIZE_MARGIN (min relative improvement, default 0.05; identical binaries show ~3% positional bias),
#      IMAGE_SIZE_OUT (table path), IMAGE_SIZE_SCRATCH,
#      A240_QUIET_LOAD (max 1-min load, default 0.5), A240_QUIET_BUSY (max busy CPUs over 2 s, default 0.3),
#      A240_QUIET_WAIT (seconds to wait for a quiet machine, default 300).
# Quiet rule: the machine is sampled before and after timing; a busy sample or a running
# nelisp-dynamic (after waiting up to 300 s) makes the run INVALID (exit 1, never PASS).
set -u
ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/../../../.." && pwd)
B=$HOME/.cache/tmp/nelisp-sbcl-impl-builds
REF_BIN=${IMAGE_SIZE_REF_BIN:-$B/build-on28/nelisp-dynamic}
NEW_BIN=${IMAGE_SIZE_BIN:-$B/build-on31/nelisp-dynamic}
REF_DRV=${IMAGE_SIZE_REF_DRIVERS:-$B/run/b28}
NEW_DRV=${IMAGE_SIZE_DRIVERS:-$B/run/b31}
OUTTXT=${IMAGE_SIZE_OUT:-$ROOT/tools/ai/sbcl-impl-evidence/a241-s4/s41-b31.txt}
PRODUCE=$ROOT/tools/nelisp-application-image-produce.sh
LAUNCH=$ROOT/tools/nelisp-application-image-launch.sh
SCRATCH=${IMAGE_SIZE_SCRATCH:-$HOME/.cache/tmp/a241-s4/scratch}
bad() { echo "FAIL: $*"; exit 1; }
mkdir -p "$SCRATCH" || bad "scratch"
T=$(mktemp -d "$SCRATCH/chk.XXXXXX") || exit 1
trap 'rm -rf "$T"' EXIT
for f in "$REF_BIN" "$NEW_BIN" "$PRODUCE" "$LAUNCH"; do [ -x "$f" ] || bad "not executable: $f"; done
for f in "$REF_DRV/benchmark-driver-native-on.el" "$NEW_DRV/benchmark-driver-native-on.el"; do [ -r "$f" ] || bad "driver missing: $f"; done
REF_SHA=$(sha256sum "$REF_BIN" | cut -d' ' -f1); NEW_SHA=$(sha256sum "$NEW_BIN" | cut -d' ' -f1)
mkdir -p "$T/ref" "$T/new" "$T/lane-scratch"
echo '(kill-emacs 0)' >"$T/nop.el"
"$PRODUCE" "$REF_BIN" "$REF_DRV/benchmark-driver-native-on.el" hash-get1024 "$T/ref/hash-get1024.img" >/dev/null || bad "produce ref failed"
"$PRODUCE" "$NEW_BIN" "$NEW_DRV/benchmark-driver-native-on.el" hash-get1024 "$T/new/hash-get1024.img" >/dev/null || bad "produce new failed"
REF_IMG=$(stat -c %s "$T/ref/hash-get1024.img"); NEW_IMG=$(stat -c %s "$T/new/hash-get1024.img")
echo "image sizes: ref $REF_IMG B, new $NEW_IMG B"
quiet() { # LABEL -> 0 if quiet, else prints why and returns 1
  python3 -I - "$1" "${A240_QUIET_LOAD:-0.5}" "${A240_QUIET_BUSY:-0.3}" <<'QP'
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
WAITED=0
while pgrep -x nelisp-dynamic >/dev/null; do
  [ "$WAITED" -lt 300 ] || { echo "S4.1 INVALID: nelisp-dynamic still running after ${WAITED}s: $(pgrep -a -x nelisp-dynamic | head -3)"; exit 1; }
  sleep 10; WAITED=$((WAITED + 10))
done
WAITED=0
until quiet before; do
  [ "$WAITED" -lt "${A240_QUIET_WAIT:-300}" ] || { echo "S4.1 INVALID: machine not quiet before timing (waited ${WAITED}s)"; exit 1; }
  sleep 10; WAITED=$((WAITED + 12))
done
export NELISP_SCRATCH_ROOT=$T/lane-scratch BENCH_ROW=hash-get1024 BENCH_N=16384 BENCH_HIGH=16384 BENCH_WARMUPS=0
python3 -I - "$REF_BIN" "$NEW_BIN" "$REF_SHA" "$NEW_SHA" "$T" "$LAUNCH" "$OUTTXT" "$REF_IMG" "$NEW_IMG" \
    "${IMAGE_SIZE_ROUNDS:-15}" "${IMAGE_SIZE_MARGIN:-0.05}" <<'PY'
import os, re, resource, statistics, subprocess, sys, time
refbin, newbin, refsha, newsha, T, launch, outtxt, refimg, newimg, R, margin = sys.argv[1:]
R = int(R); margin = float(margin)
cap = lambda: resource.setrlimit(resource.RLIMIT_AS, (8388608 * 1024,) * 2)
def run(cmd, extra=None):
    env = dict(os.environ, **(extra or {}))
    t = time.perf_counter()
    p = subprocess.run(cmd, env=env, stdin=subprocess.DEVNULL, capture_output=True, text=True, timeout=700, preexec_fn=cap)
    return p, (time.perf_counter() - t) * 1000
def lenv(side, b, s):
    return {"NELISP_APP_IMAGE_DIR": f"{T}/{side}", "NELISP_LANE_IMAGE": b, "NELISP_LANE_IMAGE_SHA256": s}
CMD = {"ref-bare": ([refbin, "--load", f"{T}/nop.el"], {}),
       "new-bare": ([newbin, "--load", f"{T}/nop.el"], {}),
       "ref-app": ([launch, "--load", "PLACEHOLDER"], lenv("ref", refbin, refsha)),
       "new-app": ([launch, "--load", "PLACEHOLDER"], lenv("new", newbin, newsha))}
names = list(CMD)
rows = {k: [] for k in names}; res = {k: set() for k in names}
for i in range(R):
    order = names if i % 2 == 0 else names[::-1]
    for k in order:
        c, e = CMD[k]
        p, w = run(c, e)
        if p.returncode != 0:
            print(f"FAIL: {k} round {i+1} rc={p.returncode}: {p.stderr[-300:]}"); sys.exit(1)
        if k.endswith("app"):
            m = re.findall(r"BENCH-CHECK n=(\d+) result=(\S+) elapsed=", p.stdout)
            if len(m) != 1 or m[0][0] != "16384":
                print(f"FAIL: {k}: expected one BENCH-CHECK n=16384, got {m}"); sys.exit(1)
            res[k].add(m[0][1])
        rows[k].append(w)
med = lambda k: statistics.median(rows[k])
L = [f"S4.1 paired ref(build28) vs new(build31), {R} alternating rounds, wall = parent perf_counter (ms); margin {margin:.2%}",
     f"image sizes: ref {refimg} B, new {newimg} B; ref bin sha {refsha[:12]}, new bin sha {newsha[:12]}",
     "round  " + "  ".join(f"{k:>9}" for k in names)]
for i in range(R):
    L.append(f"{i+1:>5}  " + "  ".join(f"{rows[k][i]:>9.1f}" for k in names))
for k in names:
    L.append(f"{k:>9}: median {med(k):.1f} min {min(rows[k]):.1f} max {max(rows[k]):.1f}")
rb = med("new-bare") / med("ref-bare"); ra = med("new-app") / med("ref-app")
eq = len(res["ref-app"]) == 1 and res["ref-app"] == res["new-app"]
okb = rb < 1 - margin; oka = ra < 1 - margin
L += [f"bare ratio new/ref {rb:.4f}; launcher ratio new/ref {ra:.4f}; results ref {sorted(res['ref-app'])} new {sorted(res['new-app'])}",
      f"checks: results-equal {eq}, bare faster {okb}, launcher faster {oka}"]
open(outtxt, "w").write("\n".join(L) + "\n")
print("\n".join(L[-7:]))
if not (eq and okb and oka): print("S4.1 FAIL"); sys.exit(1)
print("S4.1 timing ok")
PY
RC=$?
quiet after || { echo "S4.1 INVALID: machine not quiet after timing (rc=$RC)"; exit 1; }
[ "$RC" = 0 ] || exit 1
echo "S4.1 PASS"
