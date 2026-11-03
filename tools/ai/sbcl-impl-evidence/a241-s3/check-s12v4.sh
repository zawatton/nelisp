#!/bin/bash
# Doc 241 S1.2. IMAGE_SIZE_BIN=<binary> (default: build29, the S1 build); IMAGE_SIZE_IMG optionally names the cold image.
BIN=${IMAGE_SIZE_BIN:-$HOME/.cache/tmp/nelisp-sbcl-impl-builds/build-on29/nelisp-dynamic}
HERE=$(cd "$(dirname "$0")" && pwd)
IMG=${IMAGE_SIZE_IMG:-}
if [ -z "$IMG" ]; then for c in "$BIN.cold" "$BIN.img" "$BIN.image"; do [ -f "$c" ] && IMG=$c && break; done; fi
if [ -z "$IMG" ]; then c=$(ls "$(dirname "$BIN")"/*.cold 2>/dev/null); [ "$(echo "$c" | wc -w)" = 1 ] && IMG=$c; fi
[ -f "$IMG" ] || { echo "no cold image beside $BIN (set IMAGE_SIZE_IMG)"; exit 1; }
python3 -I "$HERE/decode.py" --check "$IMG" || exit 1
"$HERE/probe-equal.sh" "$BIN" || exit 1
# Correctness against the reference build (build28 by default): the verify22
# outputs of both builds must agree (rows results without elapsed times, semantic
# diff, kernel diff, reader probe, epoch probe modulo epoch values, and the same
# standalone-gate failure list).  verify22.sh must have been run for both builds.
NB=${IMAGE_SIZE_BUILDS:-$HOME/.cache/tmp/nelisp-sbcl-impl-builds}
REF=${IMAGE_SIZE_REF_VERIFY:-$NB/verify28}; REFG=${IMAGE_SIZE_REF_GATES:-$NB/gates-b28}
K=$(basename "$(dirname "$BIN")" | sed -n 's/^build-on//p')
NEWV=${IMAGE_SIZE_NEW_VERIFY:-$NB/verify$K}; NEWG=${IMAGE_SIZE_NEW_GATES:-$NB/gates-b$K}
[ "$(cat "$NEWV/status" 2>/dev/null)" = CORRECTNESS-DONE ] || { echo "verify22 not done for $BIN ($NEWV)"; exit 1; }
[ "$(cat "$NEWV/image.sha256")" = "$(sha256sum "$BIN" | cut -d' ' -f1)" ] || { echo "verify output is for another binary"; exit 1; }
strip_rows() { sed -E 's/ elapsed=[0-9.]+//' "$1"; }
strip_epoch() { sed -E 's/[0-9]+ -> [0-9]+/N -> N/; s/^epoch at end: [0-9]+/epoch at end: N/' "$1"; }
cmp -s <(strip_rows "$REF/rows.txt") <(strip_rows "$NEWV/rows.txt") || { echo "rows results differ"; exit 1; }
for f in sd.txt kd.txt reader-probe.out; do cmp -s "$REF/$f" "$NEWV/$f" || { echo "$f differs"; exit 1; }; done
cmp -s <(strip_epoch "$REF/epoch-probe.out") <(strip_epoch "$NEWV/epoch-probe.out") || { echo "epoch probe differs"; exit 1; }
cmp -s "$REFG/new.fail" "$NEWG/new.fail" || { echo "standalone gate failure list differs"; exit 1; }
echo "verify22 outputs equal to the reference"
# Doc 240 checks against the new binary (drivers repinned by verify22 into run/b$K).
D240=$HERE/../a240-s1; D240b=$HERE/../a240-s2
for c in "$D240/check-s11.sh" "$D240/check-s12.sh" "$D240b/check-s21.sh"; do
  STARTUP_BIN=$BIN STARTUP_DRIVER_DIR=${IMAGE_SIZE_DRIVER_DIR:-$NB/run/b$K} bash "$c" > /dev/null 2>&1 \
    || { echo "Doc 240 check failed on $BIN: $c"; exit 1; }
done
echo "Doc 240 S1.1, S1.2, S2.1 pass on $BIN"
echo S1.2v4-PASS
