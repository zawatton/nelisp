#!/bin/bash
# Doc 243 S4.2 E2E: no behaviour change on the verify22 suite.  verify22 outputs of the
# candidate (rows results, sd/kd, reader probe, epoch probe) equal build33's.
# Env: COLD_INIT_BIN (default build-on50); reference verify dir COLD_INIT_REF_VERIFY (default verify33).
set -u
NB=$HOME/.cache/tmp/nelisp-sbcl-impl-builds
BIN=${COLD_INIT_BIN:-$NB/build-on50/nelisp-dynamic}
REF=${COLD_INIT_REF_VERIFY:-$NB/verify33}
K=$(basename "$(dirname "$BIN")" | sed -n 's/^build-on//p'); NEWV=$NB/verify$K
fail() { echo "FAIL: $*"; echo S4.2-FAIL; exit 1; }
[ "$(cat "$NEWV/status" 2>/dev/null)" = CORRECTNESS-DONE ] || fail "verify22 not done for $BIN ($NEWV)"
[ "$(cat "$NEWV/image.sha256")" = "$(sha256sum "$BIN" | cut -d' ' -f1)" ] || fail "verify output is for another binary"
strip_rows() { sed -E 's/ elapsed=[0-9.]+//' "$1"; }
strip_epoch() { sed -E 's/[0-9]+ -> [0-9]+/N -> N/; s/^epoch at end: [0-9]+/epoch at end: N/' "$1"; }
cmp -s <(strip_rows "$REF/rows.txt") <(strip_rows "$NEWV/rows.txt") || fail "rows results differ"
for f in sd.txt kd.txt reader-probe.out; do cmp -s "$REF/$f" "$NEWV/$f" || fail "$f differs"; done
cmp -s <(strip_epoch "$REF/epoch-probe.out") <(strip_epoch "$NEWV/epoch-probe.out") || fail "epoch probe differs"
echo "PASS verify22 outputs equal to $REF ($(wc -l < "$NEWV/rows.txt") rows)"; echo S4.2-PASS
