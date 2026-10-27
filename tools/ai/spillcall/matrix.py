#!/usr/bin/env python3
"""spillcall S4: paired timing (paired_bench, fresh processes, engines alternating, two-N difference) of the
43 integ-1051 matrix rows plus lists-memq-bc, engines v34 (baseline), cand, GNU, on one CPU.
usage: MCPU=13 python3 matrix.py CAND_BIN [ROW...]  -> target/spillcall/timing/out/ROW/report.json
       python3 matrix.py --agg                      -> timing/matrix.txt, timing/paired.txt"""
import json, os, subprocess, sys, statistics, time
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from rows import RES, V34, GNU, HR, WT, ROWS
M = os.path.join(RES, 'integ-1051/matrix')
OUT = os.path.join(WT, 'target/spillcall/timing')
PB = os.path.realpath(os.path.join(WT, '../../../../../.claude/skills/nelisp-gnu-gap-bench/scripts/paired_bench.py'))
FIVE = ['reg-sum-bc', 'r7-funcall-bc', 'r7-gethash-eq-fix', 'r3-mix', 'lists-memq-bc']
specs = json.load(open(M + '/specs.json')); cal = json.load(open(M + '/calib.json'))
table = [(s['id'], M + '/drv/' + s['id'] + '.el', int(float(f"{cal[s['id']][3]:.2g}"))) for s in specs]
table.append(('lists-memq-bc', ROWS['lists-memq-bc'][0], int(0.65e9 / ROWS['lists-memq-bc'][1])))
def run(cand, only):
    cpu = os.environ.get('MCPU', '13')
    for rid, drv, hi in table:
        if only and rid not in only: continue
        out = f'{OUT}/out/{rid}'
        if os.path.exists(out + '/report.json'): continue
        lo = hi // 4
        cmd = ['bash', '-c', 'ulimit -v 8388608; exec "$@"', 'x', HR, 'timeout', '1800', 'python3', PB,
               '--engine', f'v34={V34}', '--engine', f'cand={cand}', '--engine', f'gnu={GNU}',
               '--driver', drv, '--low', str(lo), '--high', str(hi), '--reps', '5', '--cpu', cpu, '--out', out]
        for att in range(6):
            subprocess.run(['rm', '-rf', out])
            r = subprocess.run(cmd, capture_output=True, text=True)
            if 'is busy' not in r.stdout + r.stderr: break
            print('busy retry', rid, att, flush=True); time.sleep(20)
        print(rid, lo, hi, 'rc', r.returncode, (r.stdout + r.stderr).strip().replace('\n', ' | ')[-200:], flush=True)
def agg():
    rows = []
    for rid, _, _ in table:
        p = f'{OUT}/out/{rid}/report.json'
        if not os.path.exists(p): rows.append((rid, None)); continue
        e = json.load(open(p))['engines']
        rows.append((rid, ({k: v['inner_ns_per_step']['median'] for k, v in e.items()},
                           e['cand']['inner_ns_per_step'].get('pairs_faster_than_v34'))))
    have = [(r, d) for r, d in rows if d]
    with open(f'{OUT}/matrix.txt', 'w') as f:
        f.write('# spillcall candidate vs v34 vs GNU 31.1: ns/trip, median of 5 paired reps, two-N difference, CPU %s\n' % os.environ.get('MCPU', '13'))
        f.write('# row cand/v34 cand/gnu v34/gnu cand_faster_pairs\n')
        worst = []
        for r, (m, pf) in sorted(have, key=lambda x: x[1][0]['cand'] / x[1][0]['v34']):
            q = m['cand'] / m['v34']
            f.write(f"{r:24s} {q:6.3f} {m['cand']/m['gnu']:6.2f} {m['v34']/m['gnu']:6.2f} {pf}/5\n")
            if q > 1.03 and r != 'lists-memq-bc': worst.append((r, q))
        miss = [r for r, d in rows if not d]
        f.write(f"geomean cand/v34 {statistics.geometric_mean([d[0]['cand']/d[0]['v34'] for _, d in have]):.3f} over {len(have)} rows; missing {miss}\n")
        f.write(f"rows over 1.03: {worst}\n")
        if not miss and not worst: f.write('MATRIX-OK\n')
    with open(f'{OUT}/paired.txt', 'w') as f:
        f.write('# five acceptance rows: ns/trip v34 cand gnu, cand/v34, cand/gnu, pairs cand faster than v34\n')
        ok = True; fast = True
        for r in FIVE:
            d = dict(rows).get(r)
            if not d: f.write(f'{r} MISSING\n'); ok = False; continue
            m, pf = d; q = m['cand'] / m['v34']; fast = fast and q <= 0.80
            f.write(f"{r:20s} {m['v34']:10.1f} {m['cand']:10.1f} {m['gnu']:10.1f}  {q:.3f}  {m['cand']/m['gnu']:.2f}  {pf}/5\n")
        if ok: f.write('PAIRED-OK\n')
        if ok and fast: f.write('SPEED20-OK\n')
if sys.argv[1] == '--agg': agg()
else: run(os.path.realpath(sys.argv[1]), sys.argv[2:])
