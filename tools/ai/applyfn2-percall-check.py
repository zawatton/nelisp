#!/usr/bin/env python3
"""Check applyfn2 instruction budgets and identical per-call driver results."""
from pathlib import Path
import json
import re
import sys

root = Path(__file__).resolve().parents[2]
folder = root / 'target/applyfn2/pc'


def read(tag):
    text = (folder / f'{tag}-percall.txt').read_text()
    assert text.rstrip().endswith('DONE'), f'{tag}: incomplete'
    rows = {}
    for line in text.splitlines():
        match = re.fullmatch(r'PERCALL \S+ (src|bc) (\S+) loop=[\d.]+ op=([\d.]+) PC-CHECK mode=\S+ n=8000 result=(.*)', line)
        if match:
            flavor, mode, count, result = match.groups()
            rows[flavor, mode] = float(count), result
    assert len(rows) == 26, f'{tag}: expected 26 rows, got {len(rows)}'
    return rows


candidate, old, older = read('A'), read('v35'), read('v34')
checks = []
if sys.argv[1] == 's41':
    checks = [(mode, older, 1.0) for mode in ('applymax', 'applyformat', 'applyappend', 'applyconcat')]
    checks += [(mode, old, 1.0) for mode in ('max', 'format', 'append')]
elif sys.argv[1] == 's42':
    checks = [(mode, old, .7) for mode in ('concat', 'mapcar', 'assoc', 'alistget')]
else:
    raise SystemExit('expected s41 or s42')
bad = []
for mode, reference, limit in checks:
    current, expected = candidate['src', mode], reference['src', mode]
    passed = current[1] == expected[1] and current[0] <= expected[0] * limit
    print(f'{mode}: {expected[0]:.1f} -> {current[0]:.1f} Ir, ratio={current[0]/expected[0]:.3f}, {"PASS" if passed else "FAIL"}')
    if not passed:
        bad.append(mode)
if sys.argv[1] == 's42':
    # A passing per-call microbenchmark alone cannot establish the separate
    # attribution-workload requirement.  Missing measurements remain red.
    candidate_rows = json.loads((root / 'target/applyfn2/cg/rows-A.json').read_text())
    baseline_rows = json.loads((root / 'target/applyfn2/cg/rows-v35.json').read_text())
    plan = json.loads((root.parent / 'resume-4943-20261024/target/attrib34/plan.json').read_text())
    # The original attribution harness excludes this row because it has no
    # list-route calls and costs346M Ir per trip.  Preserve its existing scope.
    expected_rows = set(plan) - {'ms-aref-loop-130k'}
    assert set(candidate_rows) == set(baseline_rows) == expected_rows
    for name, row in candidate_rows.items():
        baseline = baseline_rows[name]
        passed = (row['same'] is True and baseline['same'] is True
                  and row['lane'] is not None and baseline['lane'] is not None
                  and row['lane'] <= baseline['lane'])
        if not passed:
            bad.append(name)
    print(f'ATTRIB rows={len(candidate_rows)} failures={len(bad)}')
raise SystemExit(bool(bad))
