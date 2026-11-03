#!/bin/bash
# Doc 240 S4.1 supplementary "app-lane" campaign.
# Mirrors ../s8-final-b28/campaign.sh (rows, N, warmups, GNU engine, drivers,
# paired_bench flags).  Differences: engine "old" is the full ON lane of the
# same binary (FULL_LANE), engine "new" is the Doc 240 launcher on per-row saved
# images produced before any timing.
# Usage: campaign-app.sh BINARY OUT [ROW...]
# Env: FULL_LANE (default: b28 lane-new wrapper), DRIVER_DIR (default: dirname of
# FULL_LANE; holds benchmark-driver-native-on.el and nested-primary-on.el),
# REPS (default 7; anything else is recorded and is not protocol).
set -u
[ $# -ge 2 ] || { echo "usage: $0 BINARY OUT [ROW...]" >&2; exit 2; }
BIN=$1; OUT=$2; shift 2
HERE=$(cd "$(dirname "$0")" && pwd) || exit 1
ROOT=$(cd "$HERE/../../../.." && pwd) || exit 1
LAUNCH=$ROOT/tools/nelisp-application-image-launch.sh
PRODUCE=$ROOT/tools/nelisp-application-image-produce.sh
X=$HOME/.cache/tmp/nelisp-v8-native-20261029/out/gnu-native-batch6/experimental-speedup-20261031
PB=$HOME/.codex/skills/nelisp-gnu-gap-bench/scripts/paired_bench.py
GNU=/usr/local/bin/emacs-31.1
FIX=$HOME/.cache/tmp/nelisp-v8-native-20261029/out/qualification/fixtures/application.el
FULL_LANE=${FULL_LANE:-$HOME/.cache/tmp/nelisp-sbcl-impl-builds/run/b28/lane-new}
DRIVER_DIR=${DRIVER_DIR:-$(dirname "$FULL_LANE")}
REPS=${REPS:-7}
case $REPS in ''|*[!0-9]*) echo "REPS must be a positive integer" >&2; exit 2 ;; esac
[ -x "$BIN" ] || { echo "binary not executable: $BIN" >&2; exit 1; }
[ -x "$FULL_LANE" ] || { echo "FULL_LANE not executable: $FULL_LANE" >&2; exit 1; }
[ ! -e "$OUT" ] || { echo "OUT exists: $OUT" >&2; exit 1; }
SHA=$(sha256sum "$BIN" | cut -d' ' -f1)
LSHA=$(sed -n 's/.*NELISP_LANE_IMAGE_SHA256=\([0-9a-f]*\).*/\1/p' "$FULL_LANE" | head -1)
[ "$LSHA" = "$SHA" ] || { echo "FULL_LANE sha '$LSHA' differs from binary sha $SHA" >&2; exit 1; }
ROWS=${*:-"integer-sum function-call2 nested-function-call2 vector-read vector-write vector-allocate64 string-slice16 string-concat16 hash-get1024"}
no_nelisp() {
  if pgrep -x nelisp-dynamic >/dev/null; then echo "refusing: a nelisp-dynamic process is running ($1)" >&2; return 1; fi
  return 0
}
no_nelisp "before production" || exit 1
mkdir -p "$OUT/images" "$OUT/scratch" || exit 1
LOG=$OUT/campaign.log
START=$(date -u +%Y-%m-%dT%H:%M:%SZ); LOAD0=$(cut -d' ' -f1-3 /proc/loadavg)
write_meta() { # END LOAD1
  python3 -I - "$OUT/campaign.json" "$BIN" "$SHA" "$LAUNCH" "$PRODUCE" "$FULL_LANE" "$REPS" "$START" "$LOAD0" "$1" "$2" "$ROWS" <<'PY' || return 1
import hashlib, json, sys
out, binary, sha, launch, produce, lane, reps, start, l0, end, l1, rows = sys.argv[1:]
h = lambda p: hashlib.sha256(open(p, "rb").read()).hexdigest()
json.dump({"binary": binary, "binary_sha256": sha, "launcher": launch, "launcher_sha256": h(launch),
           "producer": produce, "producer_sha256": h(produce), "full_lane": lane, "reps": int(reps),
           "rows": rows.split(), "start": start, "end": end or None,
           "loadavg_start": l0, "loadavg_end": l1 or None}, open(out, "w"), indent=1)
open(out, "a").write("\n")
PY
}
write_meta "" "" || exit 1
: >"$LOG"
for ROW in $ROWS; do
  case $ROW in
    nested-function-call2) PD=$DRIVER_DIR/nested-primary-on.el ;;
    integer-sum|function-call2|vector-read|vector-write|vector-allocate64|string-slice16|string-concat16|hash-get1024) PD=$DRIVER_DIR/benchmark-driver-native-on.el ;;
    *) echo "unknown row $ROW" >&2; exit 2 ;;
  esac
  if ! GNU_EMACS_BIN=$GNU V8_FIXTURE_MODULE=$FIX "$PRODUCE" "$BIN" "$PD" "$ROW" "$OUT/images/$ROW.img" >"$OUT/images/$ROW.produce.log" 2>&1; then
    echo "produce failed for $ROW: $(tail -2 "$OUT/images/$ROW.produce.log")" >&2; exit 1
  fi
done
(cd "$OUT/images" && sha256sum -- *.img *.img.main.el *.img.manifest >../images.sha256) || exit 1
no_nelisp "after production" || exit 1
cat >"$OUT/lane-app" <<W
#!/bin/sh
NELISP_APP_IMAGE_DIR=$OUT/images NELISP_LANE_IMAGE=$BIN NELISP_LANE_IMAGE_SHA256=$SHA NELISP_SCRATCH_ROOT=$OUT/scratch exec $LAUNCH "\$@"
W
chmod +x "$OUT/lane-app"
for ROW in $ROWS; do
  case $ROW in
    integer-sum) LOW=4000000 HIGH=16000000 ;;
    vector-read|vector-write|function-call2|nested-function-call2) LOW=1048576 HIGH=4194304 ;;
    vector-allocate64|string-concat16|string-slice16) LOW=262144 HIGH=1048576 ;;
    hash-get1024) LOW=16384 HIGH=65536 ;;
  esac
  if [ "$ROW" = nested-function-call2 ]; then
    DRIVER=$X/gnu-comparison/nested-primary-gnu.el; GE=$X/gnu-comparison/emacs-gnu
    export GNU_EMACS_BIN=$GNU V8_FIXTURE_MODULE=$FIX
  else
    DRIVER=$HOME/.cache/tmp/nelisp-cross-language-20261029-t8wGC4/driver.el; GE=$GNU
  fi
  export BENCH_ROW=$ROW BENCH_HIGH=$HIGH BENCH_WARMUPS=2 BENCH_ENGINE=gnu
  unset BENCH_BROKEN
  echo "== $ROW low=$LOW high=$HIGH" >>"$LOG"
  python3 "$PB" --engine "old=$FULL_LANE" --engine "new=$OUT/lane-app" --engine "gnu=$GE" \
    --driver "$DRIVER" --low "$LOW" --high "$HIGH" --reps "$REPS" --cpu 1 --out "$OUT/$ROW" >>"$LOG" 2>&1 || echo "ROW-FAILED $ROW" >>"$LOG"
done
write_meta "$(date -u +%Y-%m-%dT%H:%M:%SZ)" "$(cut -d' ' -f1-3 /proc/loadavg)" || exit 1
if grep -q '^ROW-FAILED' "$LOG"; then grep '^ROW-FAILED' "$LOG" >&2; exit 1; fi
exit 0
