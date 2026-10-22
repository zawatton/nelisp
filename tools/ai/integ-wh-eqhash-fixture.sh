#!/usr/bin/env bash
# integ-wh-eqhash-fixture.sh [BIN]: the hashtab line's 56-case GNU fixture (tools/ai/eqhash-semantics.el against its
# GNU 31.1 expected output) on the merged dynamic binary.  Prints "EQHASH-FIXTURE n/56" and the names of the cases
# that differ; exit 0 only when all match.  Measured: hashtab lane tip 41/56, F-final stage 39, gc-port stage 37,
# callpath stage 37 (the unrooted-weak cases wait on the conservative stack scan; scan off, debug switch 28, gives 39).
cd "$(dirname "$0")/../.." || exit 2
BIN=$(realpath "${1:-${INTEG_WH_DYN:-target/integ/nelisp-dyn}}") || exit 2
OUT=target/integ-wh; mkdir -p "$OUT"
(ulimit -v 8388608; LC_ALL=C.UTF-8 timeout 600 "$BIN" --load tools/ai/eqhash-semantics.el -- > "$OUT/eqhash-fixture.out" 2> "$OUT/eqhash-fixture.err")
python3 - "$OUT/eqhash-fixture.out" <<'PY'
import sys
def load(p):
    return {l.split()[1]: l.rstrip("\n") for l in open(p, encoding="utf-8", errors="replace") if l.startswith("(eqhash ")}
want, got = load("tools/ai/eqhash-semantics.expected"), load(sys.argv[1])
bad = [n for n in want if got.get(n) != want[n]]
print("EQHASH-FIXTURE %d/%d" % (len(want) - len(bad), len(want)) + (" differ: " + " ".join(bad) if bad else ""))
sys.exit(1 if bad else 0)
PY
