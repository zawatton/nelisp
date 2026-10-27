#!/usr/bin/env python3
"""matrix-layout-check.py INTEG_DIR [--threshold 1.03] [--seconds 0.25] [--jobs 2] [--only ROW ...]

Layout-versus-work check for a speed matrix (target/integ-NNNN/matrix).

A matrix row whose new/old time ratio is >= THRESHOLD may be a real slowdown
(more instructions) or only code/data layout (same instructions, other cache
and branch behaviour).  For each such row of INTEG_DIR/matrix/REPORT.md this
runs callgrind on both engines (the first and the last non-gnu engine of
INTEG_DIR/matrix/runall.py's ENG list, i.e. old and new), with the row's
driver INTEG_DIR/matrix/drv/ROW.el (the files of specs.json are loaded by it)
at two trip counts, BENCH_N=HI and HI/4.  Per engine the instruction count of
the work is Ir(HI) - Ir(HI/4), which cancels startup and image-load cost.
HI is chosen from the row's measured ns per trip so that one native run takes
about --seconds (capped by the matrix's own calibrated count); callgrind runs
about 40-60 times slower, so a run stays well under 3 minutes.  The runs go
through /home/madblack-21/Cowork/Notes/bin/heavy-run.sh (override with the
HEAVY_RUN environment variable) under `taskset -c 0-7' and `ulimit -v 8388608'.

The BENCH-CHECK lines (n and result) of old and new must agree at both counts,
else the row is reported CHECK-MISMATCH (counts as failure).

Per row prints:  row  ratio  Ir_old  Ir_new  delta%  verdict
  LAYOUT  |delta| < 2 %   (the slowdown is not more instructions)
  WORK    otherwise      (the new engine executes different work)
Exit 0 iff no WORK / CHECK-MISMATCH / ERROR row.
"""
import argparse, ast, concurrent.futures as cf, json, os, re, subprocess, sys, tempfile

HEAVY = os.environ.get('HEAVY_RUN', '/home/madblack-21/Cowork/Notes/bin/heavy-run.sh')
LINE = re.compile(r'BENCH-CHECK n=(\d+) result=(.*) elapsed=([0-9.]+)')


def engines(runall):
    tree = ast.parse(open(runall).read())
    env = {}
    for node in tree.body:
        if isinstance(node, ast.Assign) and len(node.targets) == 1 and isinstance(node.targets[0], ast.Name):
            if node.targets[0].id in ('B', 'ENG'):
                env[node.targets[0].id] = eval(compile(ast.Expression(node.value), runall, 'eval'), dict(env))
    eng = [tuple(e.split('=', 1)) for e in env['ENG']]
    eng = [e for e in eng if not os.path.basename(e[1]).startswith('emacs')]
    if len(eng) < 2:
        sys.exit(f'{runall}: need two non-gnu engines in ENG, got {eng}')
    return eng[0], eng[-1]


def report_rows(report, old, new, threshold):
    ratio_name = f'{new}/{old}'
    rows, col, names = [], None, None
    for l in open(report):
        if not l.startswith('|'):
            continue
        c = [x.strip() for x in l.strip().strip('|').split('|')]
        if c[0] == 'workload':
            names = c
            if ratio_name not in c:
                sys.exit(f'{report}: no column {ratio_name} in header {c}')
            col = c.index(ratio_name)
            ncol = c.index(new)
            continue
        if col is None or c[0].startswith('---') or c[0].startswith(':'):
            continue
        try:
            ratio = float(c[col]); ns = float(c[ncol])
        except ValueError:
            continue
        if ratio >= threshold:
            rows.append((c[0], ratio, ns))
    if col is None:
        sys.exit(f'{report}: no matrix table')
    return rows


def callgrind(binary, driver, n, cpus, out):
    env = dict(os.environ, BENCH_N=str(n))
    cmd = ['bash', '-c',
           'ulimit -v 8388608; exec "$0" "$@"', HEAVY, 'timeout', '900', 'taskset', '-c', cpus,
           'valgrind', '--tool=callgrind', f'--callgrind-out-file={out}', '--log-file=' + out + '.log',
           binary, '--load', driver]
    r = subprocess.run(cmd, env=env, stdin=subprocess.DEVNULL, capture_output=True, text=True)
    log = open(out + '.log').read() if os.path.exists(out + '.log') else ''
    m = re.search(r'Collected\s*:\s*(\d+)', log)
    c = LINE.search(r.stdout)
    if r.returncode != 0 or not m or not c or int(c[1]) != n:
        raise RuntimeError(f'{binary} n={n} rc={r.returncode} ir={bool(m)} check={bool(c)}: {(r.stdout + r.stderr)[-200:]!r}')
    return int(m[1]), c[2]


def main():
    ap = argparse.ArgumentParser(description=__doc__.split('\n')[0])
    ap.add_argument('integ_dir')
    ap.add_argument('--threshold', type=float, default=1.03)
    ap.add_argument('--seconds', type=float, default=0.25, help='native seconds per HI-count run')
    ap.add_argument('--jobs', type=int, default=2)
    ap.add_argument('--cpus', default='0-7')
    ap.add_argument('--only', nargs='*', default=[])
    a = ap.parse_args()
    M = os.path.join(os.path.abspath(a.integ_dir), 'matrix')
    (olab, obin), (nlab, nbin) = engines(os.path.join(M, 'runall.py'))
    for b in (obin, nbin):
        if not os.access(b, os.X_OK):
            sys.exit(f'engine binary missing: {b}')
    calib = json.load(open(os.path.join(M, 'calib.json')))
    rows = [r for r in report_rows(os.path.join(M, 'REPORT.md'), olab, nlab, a.threshold) if not a.only or r[0] in a.only]
    print(f'# matrix-layout-check {a.integ_dir}: old={olab} new={nlab} threshold={a.threshold} rows={len(rows)}', flush=True)
    tmp = tempfile.mkdtemp(prefix='mlc-')

    def one(job):
        row, ratio, ns = job
        drv = os.path.join(M, 'drv', row + '.el')
        cap = int(float(f'{calib[row][3]:.2g}')) if row in calib else 10**7
        hi = max(400, min(cap, int(a.seconds * 1e9 / max(ns, 1.0))))
        lo = max(100, hi // 4)
        res = {}
        try:
            for lab, b in ((olab, obin), (nlab, nbin)):
                for n in (hi, lo):
                    res[lab, n] = callgrind(b, drv, n, a.cpus, os.path.join(tmp, f'{row}.{lab}.{n}.cg'))
        except Exception as e:
            return row, ratio, None, f'ERROR {e}'
        bad = [n for n in (hi, lo) if res[olab, n][1] != res[nlab, n][1]]
        io = res[olab, hi][0] - res[olab, lo][0]
        inn = res[nlab, hi][0] - res[nlab, lo][0]
        return row, ratio, (io, inn, hi, lo), ('CHECK-MISMATCH n=%s %r vs %r' % (bad[0], res[olab, bad[0]][1], res[nlab, bad[0]][1])) if bad else None

    bad = 0
    print('row ratio Ir_old Ir_new delta% verdict  (Ir = work between N=HI/4 and N=HI)', flush=True)
    with cf.ThreadPoolExecutor(a.jobs) as ex:
        for row, ratio, ir, err in ex.map(one, rows):
            if ir is None or err:
                bad += 1
                print(f'{row} {ratio:.2f} - - - {err}', flush=True)
                continue
            io, inn, hi, lo = ir
            d = 100.0 * (inn - io) / io if io else float('inf')
            v = 'LAYOUT' if abs(d) < 2.0 else 'WORK'
            bad += v == 'WORK'
            print(f'{row} {ratio:.2f} {io} {inn} {d:+.2f}% {v}  (n={lo}..{hi})', flush=True)
    print(f'# matrix-layout-check: rows={len(rows)} failing={bad}')
    sys.exit(1 if bad else 0)


if __name__ == '__main__':
    main()
