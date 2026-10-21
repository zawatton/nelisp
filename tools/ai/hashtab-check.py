#!/usr/bin/env python3
"""Small acceptance checker for the GNU hash storage ledger."""
import argparse
import hashlib
import json
from pathlib import Path
import subprocess
import sys
import statistics

ROOT = Path(__file__).resolve().parents[2]
OUT = ROOT / 'target/hashtab'


def check(ok, message):
    if not ok:
        raise ValueError(message)


def plan():
    doc = (ROOT / 'docs/design/228-gnu-hash-table-port.org').read_text()
    for mechanism in ('key_and_value', 'hash_find_with_hash', 'hash_put',
                      'hash_remove_from_table', 'maybe_resize_hash_table', 'sweep_weak_table'):
        check(mechanism in doc, 'missing GNU mechanism: ' + mechanism)
    check('src/fns.c:' in doc and 'src/lisp.h:' in doc, 'missing GNU source mapping')
    print('HASHTAB plan PASS: six GNU storage/algorithm mappings')


def recorded(kind):
    p = OUT / (kind + '.json')
    check(p.exists(), 'missing evidence: ' + str(p.relative_to(ROOT)))
    data = json.loads(p.read_text())
    for item in data['inputs']:
        path = ROOT / item['path']
        check(path.is_file() and hashlib.sha256(path.read_bytes()).hexdigest() == item['sha256'],
              'changed input: ' + item['path'])
    check(data['pass'], data.get('reason', kind + ' did not pass'))
    print('HASHTAB ' + kind + ' PASS')


def speed():
    failures = []
    for variant in ('dynamic', 'static'):
        path = OUT / ('wall-candidate-' + variant + '.json')
        check(path.exists(), 'missing candidate timing: ' + variant)
        rows = json.loads(path.read_text())
        for row in rows:
            item = row['binary']
            check(hashlib.sha256(Path(item['path']).read_bytes()).hexdigest() == item['sha256'],
                  'changed timing binary')
            check(row['cpu'] == 15 and row['frequency_before'] and row['frequency_after'],
                  'missing CPU 15 frequency observations')
            check(row['probe_sha256'] == hashlib.sha256((ROOT / 'tools/ai/eqhash-wall.el').read_bytes()).hexdigest(),
                  'changed wall workload')
        for layout in (0, 700, 1400, 2100):
            for test in ('eq', 'equal'):
                for op in ('hit', 'miss', 'put'):
                    ratios = []
                    for sample in range(7):
                        costs = []
                        for runtime in ('candidate', 'gnu'):
                            selected = [r for r in rows if (r['layout'], r['sample'], r['runtime']) == (layout, sample, runtime)]
                            check(len(selected) == 1, 'incomplete paired wall rounds')
                            v = selected[0]['values']
                            cost = v[f'{test}/{op}/0'] - v[f'{test}/{op}/1']
                            check(cost > 0, 'nonpositive own cost; measurement unresolved')
                            costs.append(cost)
                        ratios.append(costs[0] / costs[1])
                    ratio = statistics.median(ratios)
                    if ratio > 1:
                        failures.append(f'{variant}/pad{layout}/{test}/{op}={ratio:.3f}x GNU')
    check(not failures, '; '.join(failures))
    print('HASHTAB speed PASS')


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('criterion', choices=['plan', 'layout', 'semantics', 'speed', 'final', 'notes65'])
    args = p.parse_args()
    try:
        (plan() if args.criterion == 'plan' else
         speed() if args.criterion == 'speed' else recorded(args.criterion))
    except (ValueError, KeyError, OSError) as e:
        print('HASHTAB FAIL: ' + str(e), file=sys.stderr)
        return 1
    return 0


if __name__ == '__main__':
    sys.exit(main())
