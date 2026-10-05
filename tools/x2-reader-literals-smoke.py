#!/usr/bin/env python3
"""Compare actual literal source loading, which parity-driver staging canonicalizes."""
import os
from pathlib import Path
import subprocess
import sys
root = Path(__file__).resolve().parent.parent
binary = Path(os.environ['NELISP_BIN']).resolve()
image = subprocess.check_output(['bash', 'tools/c-core-image.sh', 'path'], cwd=root, env=os.environ, text=True).strip()
source = root / 'tools/x2-reader-literals.el'
output = root / 'build/x2-reader-literals'
output.mkdir(parents=True, exist_ok=True)
expected = sum(line.startswith('(princ (format "L|') for line in source.read_text().splitlines())
results = []
for label, argv in [('gnu', [os.environ.get('EMACS', 'emacs'), '-Q', '--batch', '-l', str(source)]),
                    ('native', [str(binary), '--cold-load-from', image, '--load', str(source)])]:
    p = subprocess.run(argv, cwd=root, env=os.environ, capture_output=True, timeout=120)
    (output / (label + '.out')).write_bytes(p.stdout)
    (output / (label + '.err')).write_bytes(p.stderr)
    if p.returncode or p.stderr:
        sys.exit(f'FAIL {label}: rc={p.returncode}; see {output}')
    lines = p.stdout.splitlines()
    if len(lines) != expected + 1 or lines[-1:] != [b'L-DONE']:
        sys.exit(f'FAIL {label}: incomplete or extra output; see {output}')
    if any(not line.startswith(f'L|{i}|'.encode()) for i, line in enumerate(lines[:-1])):
        sys.exit(f'FAIL {label}: missing or out-of-order records')
    results.append(lines)
if results[0] != results[1]:
    for g, n in zip(*results):
        if g != n:
            print(f'GNU {g!r}\nNATIVE {n!r}')
    sys.exit('FAIL actual source literals differ')
print(f'x2-reader-literals: PASS {expected} GNU/native source literals')
