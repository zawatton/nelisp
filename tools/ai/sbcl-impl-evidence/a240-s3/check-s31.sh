#!/bin/bash
# S3.1 E2E: GNU 31.1 vs full lane vs app launcher, hash-get1024 N=16384, 9 rotating rounds.
# Env: LAUNCHER (default launch.sh), A240_PRODUCE (producer under test), A240_S31_OUT (table path),
#      A240_GNU, A240_GNU_DRIVER, STARTUP_LANE,
#      A240_QUIET_LOAD (max 1-min load average, default 0.5),
#      A240_QUIET_BUSY (max busy CPUs over a 2 s /proc/stat sample, default 0.3),
#      A240_QUIET_WAIT (seconds to wait for a quiet machine before timing, default 300).
# Quiet-machine rule (user decision 2026-11-02): the timing is taken only on a quiet
# machine.  The machine is sampled before and after the timing rounds; if either
# sample is busy the run is INVALID and exits 1 (never PASS).  The 1.10 bound is not
# relaxed.
set -u
STARTUP_SCRATCH=${STARTUP_SCRATCH:-$HOME/.cache/tmp/a240-s3}
. "$(dirname "$0")/../a240-s2/common.sh"
PRODUCE=${A240_PRODUCE:-$PRODUCE}
LANE=${STARTUP_LANE:-$RUNDIR/lane-new}
GNU=${A240_GNU:-/usr/local/bin/emacs-31.1}
GDRV=${A240_GNU_DRIVER:-$HOME/.cache/tmp/nelisp-cross-language-20261029-t8wGC4/driver.el}
OUTTXT=${A240_S31_OUT:-$ROOT/tools/ai/sbcl-impl-evidence/a240-s3/s31-b28.txt}
[ -x "$LANE" ] || bad "lane wrapper missing: $LANE"
[ -x "$GNU" ] && [ -r "$GDRV" ] || bad "GNU emacs or driver missing"
if pgrep -x nelisp-dynamic >/dev/null; then bad "another nelisp-dynamic is running: $(pgrep -a -x nelisp-dynamic | head -3)"; fi
quiet() { # LABEL -> 0 if quiet, else prints why and returns 1
  python3 -I - "$1" "${A240_QUIET_LOAD:-0.5}" "${A240_QUIET_BUSY:-0.3}" <<'QP'
import sys, time
label, max_load, max_busy = sys.argv[1], float(sys.argv[2]), float(sys.argv[3])
def stat():
    f = open("/proc/stat").readline().split()[1:]
    v = list(map(int, f))
    return sum(v), v[3] + v[4]
t0, i0 = stat(); time.sleep(2.0); t1, i1 = stat()
hz_total = t1 - t0
import os
busy = (hz_total - (i1 - i0)) / hz_total * os.cpu_count()
load = float(open("/proc/loadavg").read().split()[0])
ok = load <= max_load and busy <= max_busy
print(f"QUIET {label}: load1 {load:.2f} (max {max_load}), busy CPUs {busy:.2f} (max {max_busy}) -> {'quiet' if ok else 'BUSY'}")
sys.exit(0 if ok else 1)
QP
}
IMGDIR=$T/img; mkdir -p "$IMGDIR" "$T/lane-scratch"
"$PRODUCE" "$BIN" "$RUNDIR/benchmark-driver-native-on.el" hash-get1024 "$IMGDIR/hash-get1024.img" >/dev/null || bad "produce failed ($PRODUCE)"
# Independent cold-load check that the driver helpers are carried compiled (not merely asserted by the producer).
cat >"$T/probe.el" <<'EL'
;;; -*- lexical-binding: t; -*-
(dolist (n '(bench-input bench-check-input bench-validate))
  (let ((r (gethash (nthcdr 3 (symbol-function n)) nelisp--fx-table)))
    (princ (format "FXSTATE %s %s\n" n (if (and r (not (eq r 'none))) "compiled" (or r "absent"))))))
(kill-emacs 0)
EL
FX=$( (ulimit -v 8388608; env -i HOME="$HOME" PATH="$PATH" BENCH_ENGINE=nelisp NELISP_PRODUCER_IMAGE_SHA256="$BSHA" BENCH_ROW=hash-get1024 timeout 600 "$BIN" --cold-load-from "$IMGDIR/hash-get1024.img" --load "$T/probe.el") 2>&1 | grep '^FXSTATE')
echo "$FX"
[ "$(echo "$FX" | grep -c ' compiled$')" = 3 ] || bad "driver helpers are not carried compiled in the image"
WAITED=0
until quiet before; do
  [ "$WAITED" -lt "${A240_QUIET_WAIT:-300}" ] || { echo "S3.1 INVALID: machine not quiet before timing (waited ${WAITED}s)"; exit 1; }
  sleep 10; WAITED=$((WAITED + 12))
done
export NELISP_APP_IMAGE_DIR=$IMGDIR NELISP_LANE_IMAGE=$BIN NELISP_LANE_IMAGE_SHA256=$BSHA
export NELISP_SCRATCH_ROOT=$T/lane-scratch BENCH_ROW=hash-get1024 BENCH_N=16384 BENCH_HIGH=16384 BENCH_WARMUPS=0
python3 -I - "$LANE" "$GNU" "$GDRV" "$OUTTXT" "${LCMD[@]}" <<'PY'
import os, re, resource, statistics, subprocess, sys, time
lane, gnu, gdrv, outtxt, *LC = sys.argv[1:]
cap = lambda: resource.setrlimit(resource.RLIMIT_AS, (8388608 * 1024,) * 2)
def run(cmd, extra=None):
    env = dict(os.environ, **(extra or {}))
    t = time.perf_counter()
    p = subprocess.run(cmd, env=env, stdin=subprocess.DEVNULL, capture_output=True, text=True, timeout=700, preexec_fn=cap)
    return p, (time.perf_counter() - t) * 1000
def parse(p, label):
    if p.returncode != 0:
        print(f"FAIL: {label} rc={p.returncode}: {p.stderr[-300:]}"); sys.exit(1)
    m = re.findall(r"BENCH-CHECK n=(\d+) result=(\S+) elapsed=([0-9.]+)", p.stdout)
    if len(m) != 1 or m[0][0] != "16384":
        print(f"FAIL: {label}: expected one BENCH-CHECK n=16384, got {m}"); sys.exit(1)
    return m[0][1], float(m[0][2]) / 16384 * 1e9
CMD = {"gnu": ([gnu, "-Q", "--batch", "-l", gdrv], {"BENCH_ENGINE": "gnu"}),
       "lane": ([lane, "--load", gdrv], {}),
       "app": (LC + ["--load", "PLACEHOLDER"], {})}
names = ["gnu", "lane", "app"]
rows = {k: [] for k in names}; res = {k: set() for k in names}
R = 9
for i in range(R):
    order = names[i % 3:] + names[:i % 3]
    if i % 2: order = order[::-1]
    for k in order:
        c, e = CMD[k]
        p, w = run(c, e)
        r, ns = parse(p, k); res[k].add(r); rows[k].append((w, ns))
med = lambda k, j: statistics.median(r[j] for r in rows[k])
mm = lambda k, j: (min(r[j] for r in rows[k]), max(r[j] for r in rows[k]))
ratio_w = med("app", 0) / med("gnu", 0)
ratio_k = med("app", 1) / med("lane", 1)
p, wflag = run(LC + ["--load", "PLACEHOLDER"], {"NELISP_APP_LAUNCH_TIMING": "1"})
parse(p, "timing run")
lt = re.search(r"LAUNCH-TIMING (.*)", p.stderr)
if not lt: print("FAIL: no LAUNCH-TIMING line"); sys.exit(1)
ok_eq = len(res["app"]) == 1 and res["app"] == res["lane"]
ok_wall = ratio_w <= 1.10
ok_k = abs(ratio_k - 1) <= 0.05
L = ["S3.1 hash-get1024 N=16384 build28, 9 rotating rounds; wall = parent perf_counter around the whole process (ms)",
     "round  gnu-wall  lane-wall  app-wall  gnu-ns/step  lane-ns/step  app-ns/step"]
for i in range(R):
    L.append(f"{i+1:>5}  {rows['gnu'][i][0]:>8.1f}  {rows['lane'][i][0]:>9.1f}  {rows['app'][i][0]:>8.1f}  {rows['gnu'][i][1]:>11.2f}  {rows['lane'][i][1]:>12.2f}  {rows['app'][i][1]:>11.2f}")
for k in names:
    L.append(f"{k:>5}: median wall {med(k,0):.1f} (min {mm(k,0)[0]:.1f} max {mm(k,0)[1]:.1f}); median ns/step {med(k,1):.2f} (min {mm(k,1)[0]:.2f} max {mm(k,1)[1]:.2f}); results {sorted(res[k])}")
L += [f"wall ratio app/gnu {ratio_w:.4f} (bound 1.10; 1.10 x gnu = {1.10*med('gnu',0):.1f} ms)",
      f"kernel ratio app/lane {ratio_k:.4f} (bound |r-1|<=0.05)",
      f"launcher TIMING (one extra run, wall {wflag:.1f} ms): {lt.group(1)}",
      f"checks: results-equal(app,lane) {ok_eq}, wall<=1.10xGNU {ok_wall}, kernel<=5% {ok_k}"]
open(outtxt, "w").write("\n".join(L) + "\n")
print("\n".join(L[-6:]))
if not (ok_eq and ok_wall and ok_k): print("S3.1 FAIL"); sys.exit(1)
print("S3.1 timing within bounds")
PY
RC=$?
quiet after || { echo "S3.1 INVALID: machine not quiet after timing (rc=$RC)"; exit 1; }
[ "$RC" = 0 ] || exit 1
echo "S3.1 PASS"
