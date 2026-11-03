#!/bin/bash
# Doc 242 S2.1 E2E: fixed arena base + mmap cold-load.
# Env: COLD_LOAD_BIN (default build-on33), COLD_LOAD_REF (default build-on32),
#      COLD_LOAD_BUILDS (build root), COLD_LOAD_REF_VERIFY / COLD_LOAD_REF_GATES (build28 verify outputs),
#      COLD_LOAD_ROUNDS (default 15), A242_QUIET_LOAD (0.5), A242_QUIET_BUSY (0.3), A242_QUIET_WAIT (300 s).
# Quiet-machine rule: the paired wall is taken only on a quiet machine, sampled before and
# after; a busy sample makes the run INVALID (exit 1, never PASS).
set -u
NB=${COLD_LOAD_BUILDS:-$HOME/.cache/tmp/nelisp-sbcl-impl-builds}
BIN=${COLD_LOAD_BIN:-$NB/build-on33/nelisp-dynamic}
REF=${COLD_LOAD_REF:-$NB/build-on32/nelisp-dynamic}
HERE=$(cd "$(dirname "$0")" && pwd); EV=$HERE/..
ROOT=$(cd "$HERE/../../../.." && pwd) || exit 1
for x in "$BIN" "$REF"; do [ -x "$x" ] || { echo "FAIL: missing binary $x"; exit 1; }; done
BIN=$(readlink -f "$BIN"); REF=$(readlink -f "$REF")
for t in strace valgrind setarch python3; do command -v $t >/dev/null || { echo "FAIL: $t not installed"; exit 1; }; done
T=$(mktemp -d); trap 'rm -rf "$T"' EXIT
fail=0; bad() { echo "FAIL: $*"; fail=1; }
cap() { (ulimit -v 8388608; "$@"); }
image_of() { local b=$1 c i=; for c in "$b.cold" "$b.img" "$b.image"; do [ -f "$c" ] && i=$c && break; done
  if [ -z "$i" ]; then c=$(ls "$(dirname "$b")"/*.cold 2>/dev/null); [ "$(echo "$c" | wc -w)" = 1 ] && i=$c; fi; echo "$i"; }
IMG=$(image_of "$BIN"); [ -f "$IMG" ] || { echo "FAIL: no cold image beside $BIN"; exit 1; }
IMGN=$(basename "$IMG")
# (a) fast path: image fd is mmap'd, never read in bulk; arena sits at the fixed base
cap strace -f -e trace=open,openat,read,pread64,mmap,close -o "$T/fast.st" "$BIN" --load "$HERE/nop.el" >/dev/null 2>&1
python3 -I "$HERE/strace-check.py" "$T/fast.st" "$IMGN" fast || bad "(a) fast path: no image mmap or a bulk read of the image"
cap "$BIN" --load "$HERE/arena.el" 2>&1 | grep -qx 'ARENA-BASE 35184372088832' || bad "(a) arena is not at 0x200000000000"
echo "(a) done"
# (b) bare-start instruction count
ir() { (ulimit -v 8388608; valgrind --tool=callgrind --callgrind-out-file="$T/cg.$2" "$1" --load "$HERE/nop.el" >/dev/null 2>&1)
  sed -n 's/^summary: *\([0-9]*\).*/\1/p;s/^totals: *\([0-9]*\).*/\1/p' "$T/cg.$2" | head -1; }
IRB=$(ir "$BIN" bin); IRR=$(ir "$REF" ref)
echo "(b) bare start Ir: BIN ${IRB:-?} REF ${IRR:-?}"
{ [ -n "$IRB" ] && [ -n "$IRR" ] && [ "$IRB" -lt "$IRR" ]; } || bad "(b) Ir of BIN is not lower than REF"
# (c) forced fallback: relocation path works and reads the content
cap env NELISP_COLD_FORCE_RELOCATE=1 strace -f -e trace=open,openat,read,pread64,mmap,close -o "$T/reloc.st" "$BIN" --load "$HERE/nop.el" >/dev/null 2>&1
python3 -I "$HERE/strace-check.py" "$T/reloc.st" "$IMGN" reloc || bad "(c) forced fallback did not read the content"
cap env NELISP_COLD_FORCE_RELOCATE=1 "$BIN" --load "$EV/a241-s1/small.el" 2>&1 | grep -q INTERN-PROBE-PASS || bad "(c) intern/eval probe failed on the fallback path"
cap "$BIN" --load "$EV/a241-s1/small.el" 2>&1 | grep -q INTERN-PROBE-PASS || bad "(c) intern/eval probe failed on the fast path"
echo "(c) done"
# (d) fast path and forced fallback restore heaps that behave the same.
# A byte comparison of dumps is not meaningful here: the fallback moves the
# intern region to a kernel-chosen base so relocation really runs, the dense
# dump keeps absolute intern pointers, and startup rehashes address-keyed
# tables.  So both paths run the semantic-diff cases and the reader probe and
# their outputs must be byte-identical.
SD=$ROOT/tools/bench/semantic-diff
RP=${COLD_LOAD_READER_PROBE:-$HOME/.cache/tmp/nelisp-sbcl-impl-f12-20261031/tools/ai/sbcl-impl-evidence/s2-reader/reader-probe.el}
[ -d "$SD" ] && [ -r "$RP" ] || bad "(d) semantic-diff dir or reader probe missing"
dfail=0
for c in cases alloc-cases bignum-cases hash-string-cases print-cases; do
  for m in 0 1; do
    (cd "$SD" && ulimit -v 8388608 && env NELISP_COLD_FORCE_RELOCATE=$m timeout 300 "$BIN" --load "$SD/$c.el" < /dev/null > "$T/sd-$c-$m.out" 2> "$T/sd-$c-$m.err")
    echo "rc=$?" >> "$T/sd-$c-$m.out"
  done
  cmp -s "$T/sd-$c-0.out" "$T/sd-$c-1.out" || { echo "(d) $c differs between fast and fallback"; dfail=1; }
  [ "$(wc -c < "$T/sd-$c-0.out")" -gt 100 ] || { echo "(d) $c produced almost no output"; dfail=1; }
done
for m in 0 1; do
  (cd "$T" && ulimit -v 8388608 && env NELISP_COLD_FORCE_RELOCATE=$m timeout 300 "$BIN" --load "$RP" < /dev/null > "$T/rp-$m.out" 2>&1)
done
cmp -s "$T/rp-0.out" "$T/rp-1.out" || { echo "(d) reader probe differs between fast and fallback"; dfail=1; }
# negative control: a 1-byte change in one output must be detected by cmp
cp "$T/sd-cases-1.out" "$T/neg.out"; printf 'x' >> "$T/neg.out"
cmp -s "$T/sd-cases-0.out" "$T/neg.out" && { echo "(d) comparison did not detect a change"; dfail=1; }
[ "$dfail" = 0 ] || bad "(d) fast and fallback behave differently"
echo "(d) done"
# (e) dense v2 vs decoded v5, fresh processes
python3 -I "$HERE/decode.py" --selftest || bad "(e) decode.py selftest"
mkdir "$T/d" "$T/s"
(cd "$T/d" && ulimit -v 8388608 && env -i HOME="$HOME" PATH="$PATH" NELISP_COLD_IMAGE_FORMAT=dense timeout 600 setarch "$(uname -m)" -R "$BIN" --load "$EV/a241-s1/dump.el") >"$T/d.log" 2>&1
(cd "$T/s" && ulimit -v 8388608 && env -i HOME="$HOME" PATH="$PATH" NELISP_COLD_IMAGE_FORMAT=xxxxx timeout 600 setarch "$(uname -m)" -R "$BIN" --load "$EV/a241-s1/dump.el") >"$T/s.log" 2>&1
if [ -s "$T/d/img.cold" ] && [ -s "$T/s/img.cold" ]; then
  python3 -I "$HERE/decode.py" "$T/s/img.cold" "$T/dec.cold" || bad "(e) decode"
  python3 -I "$HERE/compare.py" "$T/dec.cold" "$T/d/img.cold" || bad "(e) decoded v5 differs from dense v2"
  cap "$BIN" --cold-load-from "$T/s/img.cold" --load "$EV/a241-s1/small.el" 2>&1 | grep -q INTERN-PROBE-PASS || bad "(e) v5 image does not load"
  cap "$BIN" --cold-load-from "$T/d/img.cold" --load "$EV/a241-s1/small.el" 2>&1 | grep -q 'cold-load rejected' || bad "(e) dense v2 image was not refused"
else bad "(e) dump produced no image"; fi
echo "(e) done"
# madvise guard: GC churn in a fast-path process
cap "$BIN" --load "$HERE/gc-probe.el" 2>&1 | grep -q GC-PROBE-PASS || bad "madvise guard: heap invariant broken after GC churn"
echo "madvise guard done"
# (f) correctness gates (verify22 must have been run for this build)
REFV=${COLD_LOAD_REF_VERIFY:-$NB/verify28}; REFG=${COLD_LOAD_REF_GATES:-$NB/gates-b28}
K=$(basename "$(dirname "$BIN")" | sed -n 's/^build-on//p')
NEWV=$NB/verify$K; NEWG=$NB/gates-b$K
if [ "$(cat "$NEWV/status" 2>/dev/null)" = CORRECTNESS-DONE ] && [ "$(cat "$NEWV/image.sha256" 2>/dev/null)" = "$(sha256sum "$BIN" | cut -d' ' -f1)" ]; then
  strip_rows() { sed -E 's/ elapsed=[0-9.]+//' "$1"; }
  strip_epoch() { sed -E 's/[0-9]+ -> [0-9]+/N -> N/; s/^epoch at end: [0-9]+/epoch at end: N/' "$1"; }
  cmp -s <(strip_rows "$REFV/rows.txt") <(strip_rows "$NEWV/rows.txt") || bad "(f) rows results differ"
  for f in sd.txt kd.txt reader-probe.out; do cmp -s "$REFV/$f" "$NEWV/$f" || bad "(f) $f differs"; done
  cmp -s <(strip_epoch "$REFV/epoch-probe.out") <(strip_epoch "$NEWV/epoch-probe.out") || bad "(f) epoch probe differs"
  cmp -s "$REFG/new.fail" "$NEWG/new.fail" || bad "(f) standalone gate failure list differs"
  D240=$EV/a240-s1; D240b=$EV/a240-s2
  for c in "$D240/check-s11.sh" "$D240/check-s12.sh" "$D240b/check-s21.sh"; do
    STARTUP_BIN=$BIN STARTUP_DRIVER_DIR=$NB/run/b$K bash "$c" >/dev/null 2>&1 || bad "(f) Doc 240 check failed: $c"
  done
else bad "(f) verify22 not done for $BIN ($NEWV) or it is for another binary"; fi
echo "(f) done"
[ $fail = 0 ] || { echo S2.1-FAIL; exit 1; }
# (g) quiet-machine paired wall
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
WAITED=0
until quiet before; do
  [ "$WAITED" -lt "${A242_QUIET_WAIT:-300}" ] || { echo "S2.1 INVALID: machine not quiet before timing (waited ${WAITED}s)"; exit 1; }
  sleep 10; WAITED=$((WAITED + 12))
done
python3 -I - "$BIN" "$REF" "$HERE/nop.el" "${COLD_LOAD_ROUNDS:-15}" <<'PY' || exit 1
import os, resource, statistics, subprocess, sys, time
bin_, ref, nop, rounds = sys.argv[1], sys.argv[2], sys.argv[3], int(sys.argv[4])
cap = lambda: resource.setrlimit(resource.RLIMIT_AS, (8388608 * 1024,) * 2)
def run(b):
    t = time.perf_counter()
    p = subprocess.run([b, "--load", nop], stdin=subprocess.DEVNULL, capture_output=True, timeout=300, preexec_fn=cap)
    if p.returncode != 0: sys.exit(f"FAIL: {b} rc={p.returncode}")
    return (time.perf_counter() - t) * 1000
tb, tr = [], []
for r in range(rounds):
    order = [(bin_, tb), (ref, tr)] if r % 2 == 0 else [(ref, tr), (bin_, tb)]
    for b, acc in order: acc.append(run(b))
mb, mr = statistics.median(tb), statistics.median(tr)
print(f"(g) bare start wall median ms: BIN {mb:.1f} REF {mr:.1f} over {rounds} alternating rounds")
sys.exit(0 if mb < mr else 1)
PY
RC=$?
quiet after || { echo "S2.1 INVALID: machine busy after timing"; exit 1; }
[ $RC = 0 ] || { echo "FAIL: (g) median(BIN) is not below median(REF)"; echo S2.1-FAIL; exit 1; }
echo S2.1-PASS
