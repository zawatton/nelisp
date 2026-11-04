#!/bin/sh
# S6.4/S6.5 evidence: build12 hash fast path (callgrind self cost per lookup, semantic diffs).
cd "$(dirname "$0")" || exit 1
per() { python3 -I - "$1" "$2" <<'P'
import sys
def load(f): return {l.split()[0]: int(l.split()[1]) for l in open(f)}
a, c = load(f"cg/self.{sys.argv[1]}.1000.txt"), load(f"cg/self.{sys.argv[1]}.5000.txt")
print(round((c.get(sys.argv[2], 0) - a.get(sys.argv[2], 0)) / 4000, 1))
P
}
mode=${1:-s64}
if [ "$mode" = s64 ]; then
  test "$(per 12 nl_alloc_bytes_uncheck)" = 0.0 && test "$(per 12 nl_alloc_bytes)" = 0.0 || exit 1
  # Negative reference: the pre-change image allocates on every lookup.
  python3 -I -c "import sys; sys.exit(0 if float('$(per 10 nl_alloc_bytes_uncheck)') > 100 else 1)" || exit 1
else
  python3 -I sd/compare.py sd/hash-gnu.out sd/b12-hash-string-cases.out | grep -qx 'compared A=261 B=261 mismatches=0' || exit 1
  python3 -I kd/compare.py kd/gnu.out kd/b12-nelisp.out | grep -qx 'compared A=121 B=121 mismatches=0' || exit 1
  grep -q 'HASH-CUSTOM equal=t hashcalls=1024 testcalls=1024' checks.out && test "$(grep -c 'equal=t' checks.out)" -eq 11 && test ! -s checks.err || exit 1
  grep -q '^grow.miss => (ok 858)$' sd/b12-hash-string-cases.out || exit 1
fi
echo PASS
