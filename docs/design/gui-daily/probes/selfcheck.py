#!/usr/bin/env python3
"""Negative controls for screenshot assertions, against the same callable check."""
import json
from pathlib import Path
import subprocess
import pixels

ROOT = Path(__file__).resolve().parent/'results'
cases = [('blank', ['-size','800x500','xc:white']),
         ('two-tone', [str(ROOT/'fonts.png'),'-threshold','50%']),
         ('wrong-size', ['-size','200x200','xc:white'])]
results = {}
for name,args in cases:
    path = ROOT/f'negative-{name}.png'
    subprocess.run(['convert',*args,str(path)],check=True)
    try:
        pixels.inspect(path)
    except AssertionError as e:
        results[name] = dict(rejected=True, reason=str(e))
    else:
        raise AssertionError(f'checker accepted {name}')
for name in ['fonts','pango']:
    results[name] = dict(accepted=True, measurements=pixels.inspect(ROOT/f'{name}.png'))
(ROOT/'pixel-selfcheck.json').write_text(json.dumps(results,indent=2)+'\n')
print('PIXEL-SELFCHECK-PASS checked=',len(results))
