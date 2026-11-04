#!/usr/bin/env python3
"""compall2-rank.py PROFILE.jsonl [OUT.txt] [SELECT.txt] : rank prelude functions by executed calls x per-call cost.

PROFILE.jsonl: tools/ai/compall2-profrun.py output (per >10x row: v35 ns/step, profiled ns/step, and per function
[KIND, calls/step, self ns/step, direct counted callees/step]).

Cost model.  A wrapped activation costs the profiler BIAS_IN ns inside the timed interval (attributed to the
function's own self time) and OVERHEAD ns outside it (attributed to its caller), both measured by the profiler's
self-test (target/compall2/logs/prof-selftest.txt: PROF-LEAF-NS minus the leaf's real work, PROF-OVERHEAD-NS).
  self_corr(F) = max(0, self(F) - calls(F) * BIAS_IN - kids(F) * OVERHEAD)
  share(F, row) = self_corr(F) / sum over the row of self_corr      (the row's time split among its functions;
                  time in no wrapped function -- the row's own code, subrs called from it -- is the remainder
                  prof_corr - sum, counted as not compilable)
  prof_corr(row) = prof ns/step - sum over F of calls(F) * (BIAS_IN + OVERHEAD)
A function is compilable when its KIND is fx (an fx record on the word VM), bytecode (adopted byte code), or
interp (a closure with no record yet), and the generator can give it a row (RANK_EXCLUDE: its skip list).
native: already compiled.

Per row: compilable fraction C = sum of share over compilable functions, scaled by sum/prof_corr.  With native
code K times faster than the VM on that work, the row speeds up 1 / (1 - C + C/K); a row can reach 2x only if
C >= 2 (K-1)... i.e. C >= K/(2(K-1)) (K = 4: C >= 0.67).

Ranking: the functions a row needs to reach the 2x line (its compilable functions by share, largest first, until
the predicted speedup reaches 2x), rows with the cheapest needed sets first (greedy set cover by new functions),
then every other compilable function by summed share over all rows.
Prints FEASIBLE N (rows that can reach 2x with every compilable function native), the per-row table (ROW ...),
the ranked function table (RANK ...) and writes the ranked names to SELECT.txt.
--check-profile PROFILE.jsonl : exit 0 iff at least 270 rows profiled without error and every area appears.
"""
import sys, json, re, os, collections, math

K = float(os.environ.get('RANK_K', '4'))
HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.abspath(HERE + '/../..')


def calib():
    p = ROOT + '/target/compall2/logs/prof-selftest.txt'
    s = open(p).read()
    leaf = int(re.search(r'PROF-LEAF-NS (\d+)', s).group(1))
    oh = int(re.search(r'PROF-OVERHEAD-NS (\d+)', s).group(1))
    # The leaf's own work, (+ x 1) on the VM: about 200 ns (bare `funcall' of it, probe5).
    return max(0, leaf - 200), oh


def load(path):
    return [json.loads(l) for l in open(path) if l.strip()]


def speed(c):
    return 1.0 / (1.0 - c + c / K)


def main():
    if sys.argv[1] == '--check-profile':
        rows = load(sys.argv[2])
        ok = [r for r in rows if 'error' not in r and 'funcs' in r]
        areas = {r['area'] for r in rows}
        okareas = {r['area'] for r in ok}
        print(f'profile rows={len(rows)} ok={len(ok)} areas={len(okareas)}/{len(areas)}')
        sys.exit(0 if len(ok) >= 270 and okareas == areas else 1)
    bias, oh = calib()
    # RANK_EXCLUDE: names the generator cannot give a row ("NAME REASON" lines, from its
    # skip lines): they count as not compilable.
    excl = {}
    if os.environ.get('RANK_EXCLUDE'):
        for l in open(os.environ['RANK_EXCLUDE']):
            w = l.split()
            if w: excl[w[0]] = ' '.join(w[1:])
    rows = [r for r in load(sys.argv[1]) if 'error' not in r and 'funcs' in r]
    out = open(sys.argv[2], 'w') if len(sys.argv) > 2 else sys.stdout
    selp = sys.argv[3] if len(sys.argv) > 3 else None
    print(f'CALIB bias_in={bias} overhead={oh} K={K}', file=out)
    kind = {}
    tot = collections.Counter(); nrows = collections.Counter(); calls = collections.Counter()
    need = []
    feas = 0; table = []
    for r in rows:
        f = r['funcs']
        corr = {n: max(0.0, v[2] - v[1] * bias - v[3] * oh) for n, v in f.items()}
        for n, v in f.items(): kind[n] = v[0]
        pc = r['prof_ns_step'] - sum(v[1] * (bias + oh) for v in f.values())
        s = sum(corr.values())
        denom = max(pc, s, 1e-9)
        share = {n: c / denom for n, c in corr.items()}
        comp = {n: x for n, x in share.items() if f[n][0] in ('fx', 'bytecode', 'interp') and n not in excl}
        C = sum(comp.values())
        for n, x in comp.items():
            tot[n] += x; nrows[n] += 1; calls[n] += f[n][1]
        # Needed set for 2x.
        acc = 0.0; nd = []
        for n, x in sorted(comp.items(), key=lambda kv: -kv[1]):
            if speed(acc) >= 2.0: break
            acc += x; nd.append(n)
        ok2 = speed(acc) >= 2.0
        if ok2: feas += 1; need.append((r['key'], nd))
        top = ' '.join(f'{n}:{f[n][0]}:{share[n]:.2f}' for n in sorted(share, key=lambda k: -share[k])[:5])
        table.append((r['key'], r['ratio32'], r['ns_v35'], pc, C, speed(C), ok2, len(nd) if ok2 else -1, top))
    print(f'FEASIBLE {feas} of {len(rows)} rows can reach 2x (K={K}) with every compilable function native', file=out)
    print(f'NEAR {sum(1 for t in table if speed(t[4]) >= 1.6)} rows predicted >= 1.6x', file=out)
    for t in sorted(table, key=lambda t: -t[4]):
        print(f'ROW {t[0]} ratio32={t[1]:.1f} ns_v35={t[2]:.0f} prof_corr={t[3]:.0f} comp={t[4]:.2f} '
              f'pred={t[5]:.2f} 2x={"yes" if t[6] else "no"} need={t[7]} top: {t[8]}', file=out)
    # Greedy set cover: rows whose needed sets add the fewest new functions first.
    chosen = []; cs = set(); pend = list(need)
    while pend:
        pend.sort(key=lambda kn: (len([n for n in kn[1] if n not in cs]), kn[0]))
        k, nd = pend.pop(0)
        new = [n for n in nd if n not in cs]
        for n in sorted(new, key=lambda n: -tot[n]):
            cs.add(n); chosen.append((n, k))
    rest = sorted((n for n in tot if n not in cs), key=lambda n: -tot[n])
    order = [n for n, _ in chosen] + rest
    why = dict(chosen)
    for i, n in enumerate(order):
        print(f'RANK {i + 1} {n} {kind[n]} share_sum={tot[n]:.3f} rows={nrows[n]} calls/step_sum={calls[n]:.1f} '
              f'for={why.get(n, "-")}', file=out)
    if selp:
        with open(selp, 'w') as g:
            g.write('# GENERATED by tools/ai/compall2-rank.py from the compall2 dynamic profile: one name per line,\n'
                    '# ranked (functions that bring a >10x row to the 2x line first, then by summed time share).\n')
            for n in order: g.write(n + '\n')
    print(f'SELECTABLE {len(order)} chosen-for-2x {len(cs)}', file=out)


if __name__ == '__main__':
    main()
