#!/usr/bin/env python3
"""Check native numeric DSL against GNU, then prove a broken significand fails."""
import math
import os
from pathlib import Path
import struct
import subprocess

ROOT = Path(__file__).resolve().parents[2]
OUT = ROOT / 'target/prim-sort/number-host'
OUT.mkdir(parents=True, exist_ok=True)
values = [0., -0., 1.5, -1.5, float('inf'), -float('inf'), float('nan')]
for exponent in (-1074, -1022, -1, 0, 52, 60, 80, 1023):
    value = math.ldexp(1., exponent)
    values.extend([value, -value, math.nextafter(value, math.inf), math.nextafter(value, 0.)])
rows = []
for value in values:
    bits = struct.unpack('<Q', struct.pack('<d', value))[0]
    literal = ('0.0e+NaN' if math.isnan(value) else
               ('-1.0e+INF' if value < 0 else '1.0e+INF') if math.isinf(value) else repr(value))
    rows.append(f'({bits} {literal})')
path = OUT / 'float-rows.el'
path.write_text('(' + '\n'.join(rows) + ')\n')
command = [os.environ.get('GNU_EMACS', 'emacs'), '-Q', '--batch', '-L', 'lisp',
           '--load', 'tools/ai/prim-sort-number-host.el']
for label, mutation in [('broken', True), ('valid', False)]:
    env = dict(os.environ, PRIM_SORT_FLOAT_ROWS=str(path))
    env.pop('PRIM_SORT_MUTATE', None)
    if mutation:
        env['PRIM_SORT_MUTATE'] = '1'
    result = subprocess.run(command, cwd=ROOT, env=env, capture_output=True, timeout=40)
    (OUT / f'{label}.out').write_bytes(result.stdout)
    (OUT / f'{label}.err').write_bytes(result.stderr)
    if result.stderr:
        raise SystemExit(f'{label}: unexpected stderr')
    expected = (result.returncode == 1 and b'NUMERIC-HOST FAIL case ' in result.stdout) if mutation else (
        result.returncode == 0 and b'NUMERIC-HOST checked=6241 findings=0\n' == result.stdout)
    if not expected:
        raise SystemExit(f'{label}: unexpected exit={result.returncode}; see {OUT}')
print('NUMERIC-HOST PASS: 6241 pairs; broken significand rejected')
