#!/usr/bin/env python3
"""compall2-profrun.py OUT.jsonl [SHARD NSHARDS] : dynamic profile of the resurvey32 rows above 10x.

For each row (survey{2,3,4}.jsonl ratio > 10; driver from the survey's specs, as solo19.py finds it):
  - plain runs at N and N/4 on BIN (default v35): ns/step = (t(N) - t(N/4)) / (N - N/4), BENCH-CHECK result kept;
  - profiled runs (tools/ai/compall2-prof.el) at N and N/4: per function, calls / self usec / direct counted
    callees, differenced and divided by N - N/4, so the load and warm-up calls cancel.
N is chosen so that the plain timed loop takes about PROF_SECS (default 0.1 s) at the resurvey32 ns/step.
One JSON line per row.  CPU from PROF_CPU (default 9).  Rows already in OUT are skipped (resumable).
"""
import sys, os, json, subprocess, re, tempfile
HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.abspath(HERE + '/../..')
RES = os.path.abspath(ROOT + '/../resume-4943-20261024/target')
SV = os.environ.get('PROF_SURVEY', RES + '/resurvey32')
BIN = os.path.realpath(os.environ.get('PROF_BIN', RES + '/integ-1052-build/dynamic/nelisp-dynamic'))
CPU = os.environ.get('PROF_CPU', '9')
SECS = float(os.environ.get('PROF_SECS', '0.1'))
LINE = re.compile(r'BENCH-CHECK n=(\d+) result=(.*) elapsed=([0-9.]+)')


def specs_of(sv):
    d = f'{SV}/survey{sv}'; specs = {}
    for f in ('specs.json', 'specs_bc.json', 'specs-bc.json', 'specs-bc2.json'):
        if os.path.exists(f'{d}/{f}'):
            for s in json.load(open(f'{d}/{f}')):
                k = 'bc' if (f.startswith('specs_bc') or f.startswith('specs-bc') or s.get('kind') == 'bc') else 'interp'
                if sv == 2: k = s['kind']
                specs[(s['id'], k)] = s
    return specs


def rows10(minratio=10.0):
    out = []
    for sv in (2, 3, 4):
        sp = specs_of(sv)
        for l in open(f'{SV}/survey{sv}.jsonl'):
            r = json.loads(l)
            if r['ratio'] <= minratio: continue
            id = r['row'][:-3] if (r['flavour'] == 'bc' and r['row'].endswith('-bc') and (r['row'][:-3], 'bc') in sp and sv == 4) else r['row']
            if (id, r['flavour']) not in sp and sv == 2: id = r['row']
            s = sp.get((id, r['flavour']))
            if not s: continue
            out.append(dict(key=f"{r['row']}/{r['flavour']}/{sv}", survey=sv, row=r['row'], flavour=r['flavour'],
                            area=r.get('area'), ratio32=r['ratio'], nl32=r['nl_ns'], gnu32=r['gnu_ns'],
                            drv=f'{SV}/survey{sv}/' + s['driver']))
    return out


def run(drv, n, prof=None):
    env = dict(os.environ, BENCH_N=str(n))
    cmd = ['taskset', '-c', CPU, 'timeout', '300', BIN, '--load']
    if prof:
        env.update(PROF_DRV=drv, PROF_OUT=prof, PROF_REPO=ROOT)
        cmd.append(HERE + '/compall2-prof.el')
    else:
        cmd.append(drv)
    p = subprocess.run(['bash', '-c', 'ulimit -v 8388608; exec "$@"', 'x'] + cmd, env=env, capture_output=True,
                       stdin=subprocess.DEVNULL, cwd=os.path.dirname(drv))
    m = LINE.search(p.stdout.decode('utf8', 'replace'))
    return (float(m[3]), m[2]) if m else (None, p.stdout.decode('utf8', 'replace')[-300:] + p.stderr.decode('utf8', 'replace')[-300:])


def readprof(path):
    f = {}; wall = None
    if not os.path.exists(path): return None, None
    for l in open(path):
        w = l.split()
        if w[0] == 'P': f[w[1]] = (w[2], int(w[3]), int(w[4]), int(w[5]))
        elif w[0] == 'PROF-WALL': wall = int(w[1])
    return f, wall


def one(row):
    n = max(8, int(SECS * 1e9 / max(row['nl32'], 1.0)))
    lo = max(2, n // 4)
    res = dict(row, n=n, lo=lo)
    a, ra = run(row['drv'], n); b, rb = run(row['drv'], lo)
    if a is None or b is None:
        res['error'] = 'plain: ' + str(ra if a is None else rb)[-200:]; return res
    res['ns_v35'] = (a - b) / (n - lo) * 1e9; res['result'] = ra[:200]
    with tempfile.TemporaryDirectory() as td:
        pa, pb = td + '/a', td + '/b'
        x, rx = run(row['drv'], n, pa); y, ry = run(row['drv'], lo, pb)
        if x is None or y is None:
            res['error'] = 'prof: ' + str(rx if x is None else ry)[-200:]; return res
        res['prof_same_result'] = (rx == ra and ry == rb)
        fa, wa = readprof(pa); fb, wb = readprof(pb)
        if fa is None or fb is None:
            res['error'] = 'prof: no dump'; return res
        res['prof_ns_step'] = (x - y) / (n - lo) * 1e9
        d = n - lo; funcs = {}
        for name, (kind, c, s, k) in fa.items():
            cb, sb, kb = (fb[name][1], fb[name][2], fb[name][3]) if name in fb else (0, 0, 0)
            dc = (c - cb) / d
            if dc <= 0: continue
            funcs[name] = [kind, round(dc, 4), round((s - sb) / d * 1000, 1), round((k - kb) / d, 4)]
        res['funcs'] = funcs
    return res


def main():
    out = sys.argv[1]
    sh, nsh = (int(sys.argv[2]), int(sys.argv[3])) if len(sys.argv) > 3 else (0, 1)
    done = set()
    if os.path.exists(out):
        for l in open(out):
            try: done.add(json.loads(l)['key'])
            except Exception: pass
    rows = [r for i, r in enumerate(rows10()) if i % nsh == sh and r['key'] not in done]
    for r in rows:
        res = one(r)
        with open(out, 'a') as f: f.write(json.dumps(res) + '\n')
        print(r['key'], res.get('error') or f"ns={res['ns_v35']:.0f} prof={res['prof_ns_step']:.0f} funcs={len(res['funcs'])} same={res['prof_same_result']}", flush=True)
    print('PROFRUN done', flush=True)


if __name__ == '__main__':
    main()
