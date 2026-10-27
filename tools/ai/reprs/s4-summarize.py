#!/usr/bin/env python3
"""Collect the S4 measurements (callgrind Ir per trip, paired CPU 12 timings) into evidence/s4-results.json.
Inputs: target/reprs/s4/cg-<row>-<engine>.txt and target/reprs/s4/paired-<row>/report.json (not tracked)."""
import json, pathlib, re, sys
root = pathlib.Path(__file__).resolve().parents[3]
s4 = root / 'target/reprs/s4'
out = {'note': 'v25 = integ-1042-build; new = lane build 2 (greek: plus tools/ai/reprs/case-overlay.el); gnu = emacs 31.1',
       'ir_per_trip': {}, 'paired_ns_per_step': {}}
for row in ('put', 'aset', 'greek', 'bvu'):
    out['ir_per_trip'][row] = {}
    for eng in ('v25', 'new', 'gnu'):
        m = re.search(r'IR_PER_TRIP ([0-9.]+)', (s4 / f'cg-{row}-{eng}.txt').read_text())
        out['ir_per_trip'][row][eng] = float(m.group(1))
for row in ('regsum', 'r3mix', 'put', 'aset', 'bvu', 'greek'):
    f = s4 / f'paired-{row}/report.json'
    if not f.exists(): continue
    r = json.load(open(f))
    out['paired_ns_per_step'][row] = {e: {'median': v['inner_ns_per_step']['median'], 'min': v['inner_ns_per_step']['min'],
                                          'max': v['inner_ns_per_step']['max']}
                                      for e, v in r['engines'].items()}
    out['paired_ns_per_step'][row]['cpu'] = r['cpu']
    out['paired_ns_per_step'][row]['sibling_util_before'] = r.get('sibling_util_before')
dst = root / 'tools/ai/reprs/evidence/s4-results.json'
dst.write_text(json.dumps(out, indent=1, sort_keys=True) + '\n')
print(dst)
