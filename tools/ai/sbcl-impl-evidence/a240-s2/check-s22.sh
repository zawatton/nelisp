#!/bin/bash
# S2.2 E2E: full lane (lane-new) vs the app-image launcher, hash-get1024, 7 alternating pairs.
set -u
. "$(dirname "$0")/common.sh"
LANE=${STARTUP_LANE:-$RUNDIR/lane-new}
OUTTXT=$ROOT/tools/ai/sbcl-impl-evidence/a240-s2/s22-b28.txt
[ -x "$LANE" ] || bad "lane wrapper missing: $LANE"
# No other NeLisp process may disturb the timing (-x: exact process name, so this shell never matches itself).
if pgrep -x nelisp-dynamic >/dev/null; then bad "another nelisp-dynamic is running: $(pgrep -a -x nelisp-dynamic | head -3)"; fi
IMGDIR=${A240_S2_IMAGE_DIR:-$T/img}
if [ ! -s "$IMGDIR/hash-get1024.img" ]; then
  mkdir -p "$IMGDIR"
  "$PRODUCE" "$BIN" "$RUNDIR/benchmark-driver-native-on.el" hash-get1024 "$IMGDIR/hash-get1024.img" >/dev/null || bad "produce failed"
fi
mkdir -p "$T/lane-scratch"
export NELISP_APP_IMAGE_DIR=$IMGDIR NELISP_LANE_IMAGE=$BIN NELISP_LANE_IMAGE_SHA256=$BSHA
export NELISP_SCRATCH_ROOT=$T/lane-scratch BENCH_ROW=hash-get1024 BENCH_N=16384 BENCH_HIGH=16384 BENCH_WARMUPS=0
python3 -I - "$LANE" "$BIN" "$OUTTXT" "${LCMD[@]}" <<'PY'
import os, re, statistics, subprocess, sys, time
lane, binary, outtxt, *LC = sys.argv[1:]
def run(cmd, extra=None):
    env = dict(os.environ, **(extra or {}))
    t = time.perf_counter()
    p = subprocess.run(cmd, env=env, stdin=subprocess.DEVNULL, capture_output=True, text=True, timeout=700)
    wall = (time.perf_counter() - t) * 1000
    return p, wall
def parse(p, label):
    if p.returncode != 0:
        print(f"FAIL: {label} rc={p.returncode}: {p.stderr[-300:]}"); sys.exit(1)
    m = re.findall(r"BENCH-CHECK n=(\d+) result=(\S+) elapsed=([0-9.]+)", p.stdout)
    if len(m) != 1 or m[0][0] != "16384":
        print(f"FAIL: {label}: expected one BENCH-CHECK n=16384, got {m}"); sys.exit(1)
    return m[0][1], float(m[0][2]) / 16384 * 1e9
L = LC + ["--load", "PLACEHOLDER"]
F = [lane, "--load", "PLACEHOLDER"]
rows = {"full": [], "launch": []}
results = set()
for i in range(7):
    order = ("full", "launch") if i % 2 == 0 else ("launch", "full")
    for k in order:
        p, w = run(F if k == "full" else L)
        res, ns = parse(p, k)
        results.add(res); rows[k].append((w, ns))
med = lambda k, j: statistics.median(r[j] for r in rows[k])
ratio = med("launch", 1) / med("full", 1)
# one flagged run for the launcher's own parts, plus baselines
p, wflag = run(L, {"NELISP_APP_LAUNCH_TIMING": "1"})
parse(p, "timing run")
lt = re.search(r"LAUNCH-TIMING (.*)", p.stderr)
if not lt:
    print("FAIL: no LAUNCH-TIMING line"); sys.exit(1)
parts = dict((a, float(b)) for a, b in re.findall(r"(\S+)=([0-9.]+)ms", lt.group(1)))
inner = sum(parts.values())
_, wpy = run(["python3", "-I", "-c", "pass"])
bw = ["bwrap", "--ro-bind", "/", "/", "--proc", "/proc", "--dev", "/dev", "--unshare-user", "--unshare-pid",
      "--unshare-net", "--die-with-parent", "--new-session", "--", "/bin/bash", "-c",
      "ulimit -v 8388608; ulimit -c 0; exec \"$@\"", "lane", "/bin/true"]
bwall = [run(bw)[1] for _ in range(5)]
ok_eq = len(results) == 1
ok_wall = med("launch", 0) <= 150
ok_ratio = abs(ratio - 1) <= 0.05
lines = [f"S2.2 hash-get1024 N=16384 build28, 7 alternating pairs; wall = parent perf_counter around the whole process (ms)",
         "pair  full-lane-wall  launcher-wall  full-ns/step  launcher-ns/step"]
for i in range(7):
    lines.append(f"{i+1:>4}  {rows['full'][i][0]:>14.1f}  {rows['launch'][i][0]:>13.1f}  {rows['full'][i][1]:>12.2f}  {rows['launch'][i][1]:>16.2f}")
lines += [f"median wall: full {med('full',0):.1f} ms, launcher {med('launch',0):.1f} ms (bound 150)",
          f"median kernel ns/step: full {med('full',1):.2f}, launcher {med('launch',1):.2f}, ratio {ratio:.4f} (bound |r-1|<=0.05)",
          f"results: {sorted(results)}",
          "launcher own overhead, one run with NELISP_APP_LAUNCH_TIMING=1 (wall %.1f ms):" % wflag,
          "  " + ", ".join(f"{k} {v:.2f}" for k, v in parts.items()) + f" (sum in-process {inner:.1f})",
          f"  python start+exit (wall - in-process) {wflag - inner:.1f}; bare 'python3 -I -c pass' wall {wpy:.1f}",
          f"  bwrap+bash+exec /bin/true, median of 5: {statistics.median(bwall):.1f}; child phase minus that = binary cold-load + main {parts['child'] - statistics.median(bwall):.1f}",
          f"checks: results-equal {ok_eq}, median-wall<=150 {ok_wall}, kernel-ratio<=5% {ok_ratio}"]
open(outtxt, "w").write("\n".join(lines) + "\n")
print("\n".join(lines[-9:]))
if not (ok_eq and ok_wall and ok_ratio):
    print("S2.2 FAIL"); sys.exit(1)
print("S2.2 PASS")
PY
