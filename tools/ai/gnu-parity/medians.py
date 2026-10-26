#!/usr/bin/env python3
"""Speed-survey median / geomean checker (usable as a ledger cmd).

  medians.py RESULTS_DIR SURVEY [--max-median 1.0] [--max-geomean X] [--flavour all|interp|bc]
      Reads RESULTS_DIR/survey<N>.jsonl (SURVEY = 2, 3, 4 or survey2/3/4).  One JSON object per
      measured row: survey, area, row, flavour (interp|bc), key_fn, nl_ns, gnu_ns, ratio, notes.
      Rows excluded from timing (parity mismatch / error) live in survey<N>-excluded.jsonl and are
      NOT part of the statistics (the original surveys excluded them too).
      Default flavour: interp for survey 4 (its published median is over interp rows), all otherwise.
      Prints the median ratio and exits 0 iff median <= --max-median (default 1.0).
      --max-geomean X additionally requires geomean <= X.
  medians.py --matrix MATRIX_DIR [--max-geomean 1.0] [--engine LABEL]
      Geomean over MATRIX_DIR/out/*/report.json of candidate/gnu ns-per-step medians.  The candidate is
      the second non-gnu engine label in MATRIX_DIR/runall.py ENG (or the only one); --engine overrides.
      Exits 0 iff geomean <= --max-geomean.
Exit 2 = usage/data error."""
import argparse, glob, json, math, os, re, statistics, sys

def load(rd, survey, flavour):
    n = re.sub(r'\D', '', survey)
    p = os.path.join(rd, f'survey{n}.jsonl')
    if not os.path.exists(p):
        print(f'missing {p}', file=sys.stderr); sys.exit(2)
    rows = [json.loads(l) for l in open(p) if l.strip()]
    if flavour is None:
        flavour = 'interp' if n == '4' else 'all'
    if flavour != 'all':
        rows = [r for r in rows if r.get('flavour') == flavour]
    ex = os.path.join(rd, f'survey{n}-excluded.jsonl')
    nex = sum(1 for l in open(ex) if l.strip()) if os.path.exists(ex) else 0
    return [r for r in rows if r.get('ratio') and r['ratio'] > 0], flavour, nex

def stats(vals):
    return dict(n=len(vals), median=statistics.median(vals), geomean=math.exp(sum(map(math.log, vals)) / len(vals)),
                gt1=sum(v > 1 for v in vals), gt10=sum(v > 10 for v in vals), gt100=sum(v > 100 for v in vals),
                gt1000=sum(v > 1000 for v in vals), le1=sum(v <= 1 for v in vals))

def matrix(md, engine):
    cand = engine
    if not cand:
        labs = re.findall(r"'(\w+)=", open(os.path.join(md, 'runall.py')).read().split('def ')[0])
        labs = [l for l in labs if l != 'gnu']
        if not labs: print('no engines in runall.py', file=sys.stderr); sys.exit(2)
        cand = labs[1] if len(labs) > 1 else labs[0]
    rs = []
    for p in sorted(glob.glob(os.path.join(md, 'out', '*', 'report.json'))):
        e = json.load(open(p))['engines']
        if cand not in e or 'gnu' not in e: continue
        rs.append(e[cand]['inner_ns_per_step']['median'] / e['gnu']['inner_ns_per_step']['median'])
    if not rs: print('no rows', file=sys.stderr); sys.exit(2)
    return cand, rs

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('args', nargs='*')
    ap.add_argument('--max-median', type=float, default=None)
    ap.add_argument('--max-geomean', type=float, default=None)
    ap.add_argument('--flavour', choices=['all', 'interp', 'bc'])
    ap.add_argument('--matrix')
    ap.add_argument('--engine')
    a = ap.parse_args()
    if a.matrix:
        cand, rs = matrix(a.matrix, a.engine); s = stats(rs)
        lim = a.max_geomean if a.max_geomean is not None else 1.0
        print(f"matrix engine={cand} rows={s['n']} geomean={s['geomean']:.3f} median={s['median']:.3f} le1={s['le1']} (limit geomean<={lim})")
        sys.exit(0 if s['geomean'] <= lim else 1)
    if len(a.args) != 2: ap.error('need RESULTS_DIR SURVEY or --matrix DIR')
    rows, fl, nex = load(*a.args, a.flavour)
    if not rows: print('no rows', file=sys.stderr); sys.exit(2)
    s = stats([r['ratio'] for r in rows])
    lim = a.max_median if a.max_median is not None else 1.0
    print(f"survey={a.args[1]} flavour={fl} rows={s['n']} excluded={nex} median={s['median']:.3f} geomean={s['geomean']:.3f} "
          f"le1={s['le1']} gt10={s['gt10']} gt100={s['gt100']} gt1000={s['gt1000']} (limit median<={lim})")
    ok = s['median'] <= lim and (a.max_geomean is None or s['geomean'] <= a.max_geomean)
    sys.exit(0 if ok else 1)

main()
