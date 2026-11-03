#!/bin/bash
# Doc 241 S3.1 E2E. IMAGE_SIZE_BIN (default build31) = binary under test; its cold image sits beside it.
# (a) v4 layout + reloc section <= 2,190,696 B; (b) dense-v2 vs v4 equality probe; (c) S1.2/S2.2 gates (v4-aware);
# (d) paired cold-load timing vs build30 (quiet-machine guard as a240-s3/check-s31.sh): pass iff median(new) <= 1.02 x median(build30).
set -u
BIN=${IMAGE_SIZE_BIN:-$HOME/.cache/tmp/nelisp-sbcl-impl-builds/build-on31/nelisp-dynamic}
OLD=${IMAGE_SIZE_OLD_BIN:-$HOME/.cache/tmp/nelisp-sbcl-impl-builds/build-on30/nelisp-dynamic}
HERE=$(cd "$(dirname "$0")" && pwd)
bad() { echo "S3.1 FAIL: $*"; exit 1; }
[ -x "$BIN" ] && [ -x "$OLD" ] || bad "binary missing ($BIN / $OLD)"
IMG=""; for c in "$BIN.cold" "$BIN.img" "$BIN.image"; do [ -f "$c" ] && IMG=$c && break; done
[ -n "$IMG" ] || bad "no cold image beside $BIN"
python3 -I "$HERE/decode.py" --check "$IMG" || bad "(a) layout"
PROBE_BIN=$BIN bash "$HERE/probe-equal.sh" "$BIN" || bad "(b) equality probe"
IMAGE_SIZE_BIN=$BIN bash "$HERE/check-s22v4.sh" || bad "(c) S1.2/S2.2 gates"
quiet() {
  python3 -I - "$1" "${S31_QUIET_LOAD:-0.5}" "${S31_QUIET_BUSY:-0.3}" <<'QP'
import sys, time, os
label, max_load, max_busy = sys.argv[1], float(sys.argv[2]), float(sys.argv[3])
def stat():
    v = list(map(int, open("/proc/stat").readline().split()[1:])); return sum(v), v[3] + v[4]
t0, i0 = stat(); time.sleep(2.0); t1, i1 = stat()
busy = ((t1 - t0) - (i1 - i0)) / (t1 - t0) * os.cpu_count()
load = float(open("/proc/loadavg").read().split()[0])
ok = load <= max_load and busy <= max_busy
print(f"QUIET {label}: load1 {load:.2f} (max {max_load}), busy CPUs {busy:.2f} (max {max_busy}) -> {'quiet' if ok else 'BUSY'}")
sys.exit(0 if ok else 1)
QP
}
T=$(mktemp -d); trap 'rm -rf "$T"' EXIT
echo '(kill-emacs 0)' > "$T/nop.el"
WAITED=0
until quiet before; do
  [ "$WAITED" -lt "${S31_QUIET_WAIT:-300}" ] || { echo "S3.1 INVALID: machine not quiet before timing (waited ${WAITED}s)"; exit 1; }
  sleep 10; WAITED=$((WAITED + 12))
done
python3 -I - "$OLD" "$BIN" "$T/nop.el" <<'PY'
import os, resource, statistics, subprocess, sys, time
old, new, nop = sys.argv[1:]
cap = lambda: resource.setrlimit(resource.RLIMIT_AS, (8388608 * 1024,) * 2)
def run(b):
    t = time.perf_counter()
    p = subprocess.run([b, "--load", nop], stdin=subprocess.DEVNULL, capture_output=True, timeout=300, preexec_fn=cap)
    ms = (time.perf_counter() - t) * 1000
    if p.returncode != 0: print(f"FAIL: {b} rc={p.returncode} {p.stderr[-200:]!r}"); sys.exit(1)
    return ms
rows = {"old": [], "new": []}
for i in range(15):
    order = ("old", "new") if i % 2 == 0 else ("new", "old")
    for k in order: rows[k].append(run(old if k == "old" else new))
print("round  build30-ms  new-ms")
for i in range(15): print(f"{i+1:>5}  {rows['old'][i]:>10.1f}  {rows['new'][i]:>6.1f}")
mo, mn = statistics.median(rows["old"]), statistics.median(rows["new"])
print(f"median build30 {mo:.1f} ms, new {mn:.1f} ms, ratio {mn/mo:.4f} (bound 1.02)")
sys.exit(0 if mn <= mo * 1.02 else 1)
PY
RC=$?
quiet after || { echo "S3.1 INVALID: machine not quiet after timing (rc=$RC)"; exit 1; }
[ "$RC" = 0 ] || bad "(d) cold-load timing"
echo S3.1-PASS
