#!/bin/bash
# usage: run.sh TAG BIN  -- callgrind every mode x flavour at two trip counts (one heavy-run job, CPUs 0-7).
H=$(cd "$(dirname "$0")" && pwd); TAG=$1; BIN=$2; ulimit -v 8388608
MODES="empty special setspecial local setlocal letspecial symconst"
for fl in src bc; do for m in $MODES; do for n in 2000 8000; do
  if [ "$TAG" = gnu ]; then C=("$BIN" -Q --batch -l $H/drv.el); else C=("$BIN" --load $H/drv.el); fi
  PC_FLAV=$fl PC_MODE=$m BENCH_N=$n timeout 600 valgrind --tool=callgrind --callgrind-out-file=$H/$TAG-$fl-$m-$n.cg "${C[@]}" > $H/$TAG-$fl-$m-$n.out 2>&1
done; done; done
python3 - "$H" "$TAG" "$MODES" <<'PY' > $H/$TAG-percall.txt
import sys
h, tag, modes = sys.argv[1], sys.argv[2], sys.argv[3].split()
def tot(f):
    for l in open(f, errors='replace'):
        if l.startswith('summary:') or l.startswith('totals:'): return int(l.split()[1])
for fl in ('src', 'bc'):
    r = {}
    for m in modes:
        a, b = tot(f'{h}/{tag}-{fl}-{m}-2000.cg'), tot(f'{h}/{tag}-{fl}-{m}-8000.cg')
        res = [l.strip() for l in open(f'{h}/{tag}-{fl}-{m}-8000.out') if 'PC-CHECK' in l]
        r[m] = ((b - a) / 6000, res[0] if res else 'NO-RESULT')
    for m, (v, res) in r.items():
        print(f'PERCALL {tag} {fl} {m} loop={v:.1f} op={v - r["empty"][0]:.1f} {res}')
PY
echo DONE >> $H/$TAG-percall.txt
