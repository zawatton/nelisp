#!/usr/bin/env python3
"""Wall time of three explicit collections against GNU 31.1 (gc-port lane, ledger S2.8).

  gc-port-gc3.py check --dyn BIN [--gnu EMACS] [--rounds N]

The collector's wall time, not its instruction count, is what the churn rows pay: a steady-state collection is
memory-bound here (about 4 instructions per nanosecond against GNU's 6), so Ir is a diagnostic only.
test/gc-port-gc3-bench.el warms the Doc 220 churn up and times three explicit collections of the cons, string and
vector variants with float-time.  Each of N alternating rounds (default 11, order swapped every round, both pinned
to CPU 8, ulimit -v 8 GiB) runs GNU Emacs and the candidate once; the verdict is the median over rounds of the
candidate/GNU ratio, per kind.  PASS when every kind's median ratio is at most 1.0.  A negative control checks
that the verdict function fails on a ratio of 2.0.  Run it through bin/heavy-run.sh on a quiet machine: another
lane's load moves both sides, and the ratio less than the absolute times.  Exit 0 only on PASS."""
import argparse
import re
import statistics
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
BENCH = ROOT / 'test/gc-port-gc3-bench.el'
KINDS = ('cons', 'string', 'vector')


def verdict(ratio):
    return ratio <= 1.0


def run(argv):
    # GCP_CPU overrides the lane's CPU 8 (the integration's measurement CPU is 11).
    cmd = ['bash', '-c', 'ulimit -v 8388608; exec taskset -c "${GCP_CPU:-8}" "$@"', 'gc3', *argv]
    out = subprocess.run(cmd, cwd=ROOT, capture_output=True, text=True).stdout
    got = {m.group(1): float(m.group(2)) for m in re.finditer(r'^GC3 (\w+) ([0-9.]+)', out, re.M)}
    if set(got) != set(KINDS):
        raise SystemExit(f'FAIL no GC3 lines from {argv[0]}: {out[-200:]!r}')
    return got


def main(argv):
    ap = argparse.ArgumentParser()
    ap.add_argument('command', choices=['check'])
    ap.add_argument('--dyn', required=True, type=Path)
    ap.add_argument('--gnu', type=Path, default=Path('/usr/local/bin/emacs'))
    ap.add_argument('--rounds', type=int, default=11)
    a = ap.parse_args(argv)
    if verdict(2.0):
        print('FAIL negative control passed a ratio of 2.0')
        return 1
    cand_cmd = [str(a.dyn.resolve()), '--load', str(BENCH)]
    gnu_cmd = [str(a.gnu), '-Q', '--batch', '--load', str(BENCH)]
    ratios = {k: [] for k in KINDS}
    cand_t = {k: [] for k in KINDS}
    gnu_t = {k: [] for k in KINDS}
    for i in range(a.rounds):
        order = (('gnu', gnu_cmd), ('cand', cand_cmd)) if i % 2 == 0 else (('cand', cand_cmd), ('gnu', gnu_cmd))
        res = {name: run(cmd) for name, cmd in order}
        for k in KINDS:
            ratios[k].append(res['cand'][k] / res['gnu'][k])
            cand_t[k].append(res['cand'][k])
            gnu_t[k].append(res['gnu'][k])
    ok = True
    for k in KINDS:
        r = statistics.median(ratios[k])
        good = verdict(r)
        ok &= good
        print(f"{'PASS' if good else 'FAIL'} gc3 {k}: NeLisp {statistics.median(cand_t[k]) * 1000:.1f} ms, GNU "
              f"{statistics.median(gnu_t[k]) * 1000:.1f} ms, median ratio x{r:.2f} ({a.rounds} paired rounds)")
    print('GC-PORT-GC3 ' + ('PASS' if ok else 'FAIL'))
    return 0 if ok else 1


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
