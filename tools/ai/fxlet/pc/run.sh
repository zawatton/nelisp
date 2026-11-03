#!/bin/bash
# usage: run.sh TAG BIN OUTDIR -- callgrind every mode at two trip counts (pinned to CPUs 0-7; run via heavy-run.sh).
H=$(cd "$(dirname "$0")" && pwd); TAG=$1; BIN=$2; O=$3; ulimit -v 8388608; mkdir -p "$O"
MODES="empty let1 let3 letstar3 letlocal setspecial"
for m in $MODES; do for n in 2000 8000; do
  if [ "$TAG" = gnu ]; then C=("$BIN" -Q --batch -l $H/drv.el); else C=("$BIN" --load $H/drv.el); fi
  PC_MODE=$m BENCH_N=$n timeout 600 taskset -c 0-7 valgrind --tool=callgrind --callgrind-out-file=$O/$TAG-$m-$n.cg "${C[@]}" > $O/$TAG-$m-$n.out 2>&1
done; done
python3 - "$O" "$TAG" "$MODES" <<'PY'
import sys
h, tag, modes = sys.argv[1], sys.argv[2], sys.argv[3].split()
def tot(f):
    for l in open(f, errors='replace'):
        if l.startswith('summary:') or l.startswith('totals:'): return int(l.split()[1])
r = {}
for m in modes:
    a, b = tot(f'{h}/{tag}-{m}-2000.cg'), tot(f'{h}/{tag}-{m}-8000.cg')
    res = [l.strip() for l in open(f'{h}/{tag}-{m}-8000.out') if 'PC-CHECK' in l]
    r[m] = ((b - a) / 6000, res[0] if res else 'NO-RESULT')
for m, (v, res) in r.items():
    print(f'PERCALL {tag} src {m} loop={v:.1f} op={v - r["empty"][0]:.1f} {res}')
PY
