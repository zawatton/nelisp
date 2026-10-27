#!/usr/bin/env python3
"""hot-order.py -- generate the hot-function link order (scripts/nelisp-link-hot-order.txt).

The list plays the role of the profile that marks GCC's `.text.hot' functions: the
linker (`nelisp-link-layout-units') places the listed defuns first, in list order,
each at an address congruent to its residue modulo MODULUS.

  hot-order.py extract --nm NM --out PROFILE CG...   per-trip hot code lines from callgrind
                                                     --dump-instr runs; each row needs two
                                                     files ROW-N.cg (two trip counts) and
                                                     the nm of the profiled binary
  hot-order.py slim PROFILE                          rewrite an unfiltered profile in the
                                                     filtered form extract writes
  hot-order.py gen [--profile P] [--out F]           write the list from PROFILE
  hot-order.py --check                               regenerate from the committed profile
                                                     and compare byte for byte (exit 1 if
                                                     different)

Selection: every row is normalised to 1 (its Ir per trip); a function's weight is the
sum of its shares.  Functions are taken in weight order until CUM of the total weight
is covered (and at least MIN_SHARE of some row).  Residues: a 64-set, 8-way model of
an op cache indexed by line address bits 6..11 (Zen 3: 64 sets x 8 ways; an entry
holds up to 8 ops, so a line of k executed instructions takes ceil(k/8) ways).  Going
down the list, each function gets the residue (in 64-byte steps) that adds the least
over-subscription summed over all rows; ties go to the residue that needs the least
padding after the previous function (sizes from the profile), so without conflicts
the list is simply packed.  Deterministic: sorted inputs, integer arithmetic.
"""
import argparse, bisect, collections, os, re, sys

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))
PROFILE = os.path.join(ROOT, 'tools/ai/layout/hot-profile.tsv')
OUT = os.path.join(ROOT, 'scripts/nelisp-link-hot-order.txt')
MODULUS = 4096
LINE = 64
WAYS = 8
OPS = 8
CUM = 0.995
MIN_SHARE = 0.002
KEEP_SHARE = 0.0005


def parse_cg(path):
    ir = collections.Counter()
    pos = 0; skip = False
    with open(path, errors='replace') as f:
        for l in f:
            c = l[:1]
            if c.isdigit() or c in ('+', '-', '*'):
                a = l.split(None, 1)[0]
                if a.startswith('0x'):
                    pos = int(a, 16)
                elif a[0] == '+':
                    pos += int(a[1:], 0)
                elif a[0] == '-':
                    pos -= int(a[1:], 0)
                elif a != '*':
                    pos = int(a, 0)
                if skip:
                    skip = False; continue
                p = l.split()
                if len(p) >= 3:
                    ir[pos] += int(p[2])
            elif l.startswith('calls='):
                skip = True
    return ir


def load_nm(path):
    s = []
    for l in open(path):
        f = l.split()
        if len(f) >= 3 and f[1] in 'Tt':
            s.append((int(f[0], 16), f[2]))
    s.sort()
    return [a for a, _ in s], [n for _, n in s]


def extract(a):
    addrs, names = load_nm(a.nm)
    rows = collections.defaultdict(list)
    for p in a.cg:
        m = re.match(r'(?:nl-)?(.+)-(\d+)\.cg$', os.path.basename(p))
        rows[m.group(1)].append((int(m.group(2)), p))
    out = []
    for row in sorted(rows):
        fs = sorted(rows[row])
        if len(fs) != 2:
            sys.exit(f'{row}: need two trip counts, got {fs}')
        (nlo, plo), (nhi, phi) = fs
        lo, hi = parse_cg(plo), parse_cg(phi)
        lines = collections.defaultdict(lambda: [0, 0])
        for ad, v in hi.items():
            per = (v - lo.get(ad, 0)) / (nhi - nlo)
            if per < 0.5 or ad >= addrs[-1]:
                continue
            i = bisect.bisect_right(addrs, ad) - 1
            if i < 0:
                continue
            fn = names[i]; off = (ad - addrs[i]) // LINE * LINE
            size = addrs[i + 1] - addrs[i]
            e = lines[(fn, size, off)]
            e[0] += 1; e[1] += per
        for (fn, size, off), (k, ex) in sorted(lines.items()):
            out.append((row, fn, size, off, k, round(ex, 1)))
    write_profile(out, a.out, os.path.basename(a.nm))


def write_profile(recs, path, source):
    """Write RECS (row, fn, size, off, ninsn, exec) as the profile: one TOTAL line per row
    (its exec per trip over all functions), then only the lines of functions whose share
    of some row reaches KEEP_SHARE (the rest cannot enter the list)."""
    tot = collections.Counter(); per = collections.Counter()
    for row, fn, _, _, _, ex in recs:
        tot[row] += ex; per[(row, fn)] += ex
    keep = {fn for (row, fn), ex in per.items() if ex >= KEEP_SHARE * tot[row]}
    with open(path, 'w') as f:
        f.write('# row\tfunction\tsize\tline_off\tninsn\texec_per_trip (hot-order.py extract; nm '
                + source + ')\n')
        for row in sorted(tot):
            f.write(f'{row}\t*TOTAL*\t0\t0\t0\t{tot[row]:.1f}\n')
        for r in sorted(recs):
            if r[1] in keep:
                f.write('%s\t%s\t%d\t%d\t%d\t%.1f\n' % r)


def read_profile(path):
    recs = []
    for l in open(path):
        if l.startswith('#'):
            continue
        row, fn, sz, off, k, ex = l.rstrip('\n').split('\t')
        recs.append((row, fn, int(sz), int(off), int(k), float(ex)))
    return recs


def gen(profile):
    rows = collections.defaultdict(list); size = {}; total = {}
    for row, fn, sz, off, k, ex in read_profile(profile):
        if fn == '*TOTAL*':
            total[row] = ex; continue
        rows[row].append((fn, off, k, ex))
        size[fn] = sz
    weight = collections.Counter(); peak = collections.Counter()
    for row, ls in rows.items():
        tot = total.get(row) or sum(x[3] for x in ls)
        per = collections.Counter()
        for fn, _, _, ex in ls:
            per[fn] += ex / tot
        for fn, s in per.items():
            weight[fn] += s; peak[fn] = max(peak[fn], s)
    total = float(len(rows))
    order = sorted(weight, key=lambda f: (-round(weight[f], 9), f))
    chosen = []; cum = 0.0
    for fn in order:
        if cum >= CUM * total:
            break
        cum += weight[fn]
        if peak[fn] >= MIN_SHARE:
            chosen.append(fn)
    nsets = MODULUS // LINE
    occ = {row: [0] * nsets for row in rows}
    flines = collections.defaultdict(list)  # fn -> [(row, line index, entries)]
    for row, ls in sorted(rows.items()):
        for fn, off, k, _ in ls:
            flines[fn].append((row, off // LINE, -(-k // OPS)))
    over = lambda e: e - WAYS if e > WAYS else 0
    res = {}; cur = 0
    for fn in chosen:
        best = None
        for c in range(nsets):
            d = 0
            add = collections.Counter()
            for row, li, e in flines[fn]:
                add[(row, (c + li) % nsets)] += e
            for (row, s), e in add.items():
                o = occ[row][s]
                d += over(o + e) - over(o)
            pad = (c * LINE - cur) % MODULUS
            key = (d, pad)
            if best is None or key < best[0]:
                best = (key, c)
        c = best[1]
        for row, li, e in flines[fn]:
            occ[row][(c + li) % nsets] += e
        res[fn] = c * LINE
        cur = (cur + best[0][1] + size[fn] + LINE - 1) // LINE * LINE
    lines = ['# nelisp hot-function link order: NAME RESIDUE (bytes mod modulus).',
             '# Generated by tools/ai/layout/hot-order.py from tools/ai/layout/hot-profile.tsv; do not edit.',
             f'# {len(chosen)} functions, {cum / total * 100:.2f}% of the profiled Ir over {len(rows)} rows.',
             f'modulus {MODULUS}']
    lines += [f'{fn} {res[fn]}' for fn in chosen]
    return '\n'.join(lines) + '\n'


def main():
    if sys.argv[1:] == ['--check']:
        want = gen(PROFILE)
        have = open(OUT).read() if os.path.exists(OUT) else ''
        if want != have:
            print('hot-order: committed list differs from the regenerated one'); sys.exit(1)
        print(f'hot-order: OK ({len(want.splitlines()) - 4} functions)'); return
    ap = argparse.ArgumentParser()
    sub = ap.add_subparsers(dest='cmd', required=True)
    e = sub.add_parser('extract'); e.add_argument('--nm', required=True); e.add_argument('--out', required=True)
    e.add_argument('cg', nargs='+')
    sl = sub.add_parser('slim'); sl.add_argument('profile')
    g = sub.add_parser('gen'); g.add_argument('--profile', default=PROFILE); g.add_argument('--out', default=OUT)
    a = ap.parse_args()
    if a.cmd == 'extract':
        extract(a)
    elif a.cmd == 'slim':
        hdr = open(a.profile).readline().split('nm ')[-1].rstrip(')\n')
        write_profile(read_profile(a.profile), a.profile, hdr)
    else:
        open(a.out, 'w').write(gen(a.profile))


if __name__ == '__main__':
    main()
