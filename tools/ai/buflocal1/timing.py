#!/usr/bin/env python3
"""Paired timing for the buflocal1 rows: v17 vs candidate vs GNU on one CPU.
usage: timing.py CPU OUTDIR ROWSET...   (env V17_BIN, CAND_BIN; run under ulimit -v 8388608 and heavy-run.sh)
Parity is checked per engine at the survey4 N0; an engine whose result differs from GNU's is not timed."""
import json, os, re, subprocess, sys, statistics
HERE = os.path.dirname(os.path.abspath(__file__))
S4 = os.environ.get('SURVEY4', os.path.abspath(HERE + '/../../../../resume-4943-20261024/target/survey4'))
PB = os.path.abspath(HERE + '/../../../../../../.claude/skills/nelisp-gnu-gap-bench/scripts/paired_bench.py')
if not os.path.exists(PB):
    PB = '/home/madblack-21/Cowork/Notes/.claude/skills/nelisp-gnu-gap-bench/scripts/paired_bench.py'
GNU = '/usr/local/bin/emacs'
LINE = re.compile(r'BENCH-CHECK n=(\d+) result=(.*) elapsed=([0-9.]+)\n')
cpu, outroot = sys.argv[1], sys.argv[2]; ids = sys.argv[3:]
engines = [('v17', os.environ['V17_BIN']), ('cand', os.environ['CAND_BIN'])]
specs = {s['id']: s for s in json.load(open(S4 + '/specs.json'))}
MX = os.environ.get('MATRIX', os.path.abspath(HERE + '/../../../../resume-4943-20261024/target/integ-1034/matrix'))
for rid, hi in (('reg-sum-bc', 3000000), ('r3-mix', 150000)):
    specs[rid] = dict(id=rid, driver=None, abs=MX + '/drv/%s.el' % rid, n0=hi // 1000)
for rid in ('reg-search', 'a2-loop'):
    specs[rid] = dict(id=rid, driver=None, abs=HERE + '/drv/%s.el' % rid, n0=200)
os.makedirs(outroot, exist_ok=True)

def run1(label, path, drv, n, tmo=90):
    cmd = ['taskset', '-c', cpu, 'timeout', str(tmo)] + ([GNU, '-Q', '--batch', '-l', drv] if label == 'gnu' else [path, '--load', drv])
    try:
        r = subprocess.run(cmd, env=dict(os.environ, BENCH_N=str(n)), stdin=subprocess.DEVNULL, capture_output=True, timeout=tmo + 10)
    except subprocess.TimeoutExpired:
        return dict(rc=124, result=None, el=None)
    out = r.stdout.decode('utf-8', 'replace'); m = LINE.search(out)
    return dict(rc=r.returncode, result=m[2] if m else None, el=float(m[3]) if m else None, err=r.stderr.decode('utf-8', 'replace')[-200:])

def calibrate(label, path, drv, n0):
    n = n0
    for _ in range(12):
        r = run1(label, path, drv, n)
        if r['rc'] != 0 or r['el'] is None: return None
        if r['el'] >= 0.04: break
        n = int(n * min(50, max(2, 0.08 / max(r['el'], 1e-6))))
    return r['el'] / n * 1e9

rows_path = os.path.join(outroot, 'rows.jsonl')
done = set()
if os.path.exists(rows_path):
    for l in open(rows_path):
        r = json.loads(l)
        if r.get('status') in ('ok', 'no-parity'): done.add(r['id'])
res = open(rows_path, 'a')
for i in ids:
    if i in done: continue
    sp = specs.get(i)
    if sp is None:
        print(i, 'no spec'); continue
    drv = sp['abs'] if sp.get('abs') else S4 + '/' + sp['driver']; n0 = sp['n0']
    g = run1('gnu', GNU, drv, n0)
    rec = dict(id=i, n0=n0, gnu_result=(g['result'] or '')[:120])
    ok = []
    for lab, p in engines:
        r = run1(lab, p, drv, n0)
        rec[lab + '_parity'] = (r['rc'] == 0 and r['result'] == g['result'])
        rec[lab + '_result'] = (r['result'] or r.get('err', ''))[:120]
        if rec[lab + '_parity']: ok.append((lab, p))
    if g['result'] is None or not ok:
        rec['status'] = 'no-parity'; res.write(json.dumps(rec) + '\n'); res.flush(); print(i, 'no-parity', rec); continue
    cal = {lab: calibrate(lab, p, drv, n0) for lab, p in ok}
    gns = calibrate('gnu', GNU, drv, n0)
    slow = max(v for v in cal.values() if v)
    hi = int(max(8, min(0.4e9 / max(gns, 0.5), 3e9 / max(slow, 0.5)))); hi = int(float('%.2g' % hi)); lo = max(2, hi // 4)
    od = os.path.join(outroot, i)
    subprocess.run(['rm', '-rf', od])
    cmd = ['python3', PB] + sum([['--engine', f'{l}={p}'] for l, p in ok], []) + ['--engine', f'gnu={GNU}', '--driver', drv,
           '--low', str(lo), '--high', str(hi), '--reps', '5', '--cpu', cpu, '--out', od]
    r = subprocess.run(cmd, capture_output=True, text=True)
    if not os.path.exists(od + '/report.json'):
        rec['status'] = 'bench-error'; rec['out'] = (r.stdout + r.stderr)[-300:]
    else:
        rep = json.load(open(od + '/report.json')); rec['status'] = 'ok'; rec['lo'] = lo; rec['hi'] = hi
        for lab in [l for l, _ in ok] + ['gnu']:
            rec[lab + '_ns'] = rep['engines'][lab]['inner_ns_per_step']['median']
        for lab in [l for l, _ in ok]:
            rec[lab + '_ratio'] = rec[lab + '_ns'] / rec['gnu_ns'] if rec['gnu_ns'] else None
    res.write(json.dumps(rec) + '\n'); res.flush()
    print(i, rec.get('status'), {k: (round(v, 1) if isinstance(v, float) else v) for k, v in rec.items() if k.endswith('_ns') or k.endswith('_ratio')}, flush=True)
