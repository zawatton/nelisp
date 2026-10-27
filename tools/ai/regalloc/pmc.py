#!/usr/bin/env python3
"""Per-trip hardware counters (two trip counts, median of REPS) on one CPU.
Usage: pmc.py CPU OUT.json ENGINE=PATH... -- ROW...
Events (Zen 3): front-end stall cycles 00A9, failed store-to-load forwards 0224, i-cache 32B fetch miss 0081.
Run under ulimit -v 8388608 and heavy-run.sh."""
import json, os, re, statistics, subprocess, sys
HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(os.path.dirname(HERE)))
WT = os.path.dirname(ROOT)  # agent-worktrees
R = WT + '/resume-4943-20261024/target'
PMC = ROOT + '/target/regalloc/pmcrun2'
EV = 'festall:00A9,stlfbad:0224,icmiss:0081'
ROWS = {
    'reg-sum-bc': (R + '/integ-1045/matrix/drv/reg-sum-bc.el', 750000, 3000000),
    'r3-mix': (R + '/integ-1045/matrix/drv/r3-mix.el', 37500, 150000),
    'memq-i': (R + '/resurvey23/survey2/drv/lists-memq-i.el', 62602, 250409),
    'aref-loop': (R + '/resurvey23/survey3/drv/ms-aref-loop-130k.el', 2, 8),
}
REPS = int(os.environ.get('PMC_REPS', '3'))
def one(path, drv, n, cpu):
    gnu = os.path.basename(path).startswith('emacs')
    cmd = ['taskset', '-c', cpu, 'timeout', '300', PMC] + ([path, '-Q', '--batch', '-l', drv] if gnu else [path, '--load', drv])
    r = subprocess.run(cmd, env=dict(os.environ, BENCH_N=str(n), PMC_EVENTS=EV), stdin=subprocess.DEVNULL,
                       capture_output=True, text=True, timeout=330)
    out = r.stdout + r.stderr
    chk = re.search(r'result=(\S+)', out)
    line = [l for l in out.splitlines() if 'cycles=' in l][-1]
    d = {k: int(v) for k, v in re.findall(r'(\w+)=(\d+)', line)}
    return d, (chk.group(1) if chk else None)
def main():
    cpu, outp = sys.argv[1], sys.argv[2]
    i = sys.argv.index('--'); engs = [a.split('=', 1) for a in sys.argv[3:i]]; rows = sys.argv[i + 1:]
    res = json.load(open(outp)) if os.path.exists(outp) else {}
    for row in rows:
        drv, lo, hi = ROWS[row]
        per = {name: [] for name, _ in engs}; checks = {name: set() for name, _ in engs}
        for _ in range(REPS):  # engines alternate within each repetition
            for name, path in engs:
                a, ca = one(path, drv, lo, cpu); b, cb = one(path, drv, hi, cpu); checks[name].add((ca, cb))
                per[name].append({k: (b[k] - a[k]) / (hi - lo) for k in b if k != 'rc'})
        for name, _ in engs:
            m = {k: statistics.median(p[k] for p in per[name]) for k in per[name][0]}
            m['ipc'] = m['instr'] / m['cycles']
            m['check'] = sorted(checks[name])[0]
            m['cycles_all'] = [round(p['cycles'], 1) for p in per[name]]
            res[f'{name}:{row}'] = m
            print(f"{row:10s} {name:5s} cyc={m['cycles']:.1f} ins={m['instr']:.1f} ipc={m['ipc']:.2f} festall={m['festall']:.1f} stlfbad={m['stlfbad']:.2f} icmiss={m['icmiss']:.2f} | per-kins festall={1000*m['festall']/m['instr']:.1f} stlf={1000*m['stlfbad']/m['instr']:.2f} ic={1000*m['icmiss']/m['instr']:.2f} chk={m['check'][1][:24]} cyc-reps={m['cycles_all']}", flush=True)
        json.dump(res, open(outp, 'w'), indent=1)
main()
