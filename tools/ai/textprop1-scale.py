#!/usr/bin/env python3
"""Scaling probes for text properties: run each probe at M and 4M in fresh processes.

usage: textprop1-scale.py --bin BINARY [--overlay FILE] [--gnu] [--json OUT] [--expect green|red]
Per-op probes must scale by <= PER_OP_MAX when the interval count is x4; walk probes (linear in the
interval count) by <= WALK_MAX.  --expect red succeeds when at least RED_MIN probes fail (baseline).
"""
import argparse, json, os, subprocess, sys, time
HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.abspath(os.path.join(HERE, '..', '..'))
# probe -> (kind, M)   kind: perop (fixed work, growing tree), linear (work proportional to M)
PROBES = {
    'a1-loop': ('linear', 800), 'build': ('linear', 250),
    'put-existing': ('perop', 250), 'get': ('perop', 250), 'props-at': ('perop', 250),
    'insert-mid': ('perop', 250), 'delete-mid': ('perop', 250),
    'any': ('linear', 250), 'not-all': ('linear', 250), 'walk': ('linear', 250), 'bufstr': ('linear', 250),
}
PER_OP_MAX = 2.5
WALK_MAX = 6.0
RED_MIN = 5

def run(cmd, env, tmo=300):
    t = time.time()
    r = subprocess.run(cmd, env=env, stdin=subprocess.DEVNULL, capture_output=True, text=True, timeout=tmo)
    out = [l for l in r.stdout.splitlines() if l.startswith('RESULT')]
    if not out:
        return None, (r.stdout + r.stderr)[-300:]
    p = out[-1].split()
    return float(p[3]), p[4]

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--bin'); ap.add_argument('--overlay'); ap.add_argument('--gnu', action='store_true')
    ap.add_argument('--json'); ap.add_argument('--expect', default='green'); ap.add_argument('--only')
    a = ap.parse_args()
    el = os.path.join(HERE, 'textprop1-scale.el')
    rows = {}
    fails = 0
    for name, (kind, m) in PROBES.items():
        if a.only and name != a.only: continue
        ts = []
        for mm in (m, 4 * m):
            env = dict(os.environ, PROBE=name, M=str(mm))
            if a.gnu:
                cmd = ['/usr/local/bin/emacs', '-Q', '--batch', '-l', el]
            else:
                cmd = [a.bin] + (['--load', a.overlay] if a.overlay else []) + ['--load', el, '--']
            ts.append(run(cmd, env))
        if ts[0][0] is None or ts[1][0] is None:
            rows[name] = {'kind': kind, 'error': str(ts)}; fails += 1; print(f'{name:14s} ERROR {ts}'); continue
        ratio = ts[1][0] / max(ts[0][0], 1e-9)
        limit = PER_OP_MAX if kind == 'perop' else WALK_MAX
        ok = ratio <= limit
        fails += 0 if ok else 1
        rows[name] = {'kind': kind, 'M': m, 't1': ts[0][0], 't4': ts[1][0], 'ratio': round(ratio, 2), 'limit': limit, 'ok': ok}
        print(f'{name:14s} {kind:7s} M={m:4d} t={ts[0][0]:9.4f} t4={ts[1][0]:9.4f} ratio={ratio:7.2f} limit={limit} {"ok" if ok else "FAIL"}')
    if a.json:
        json.dump(rows, open(a.json, 'w'), indent=1)
    if a.expect == 'red':
        sys.exit(0 if fails >= RED_MIN else 1)
    sys.exit(0 if fails == 0 else 1)
main()
