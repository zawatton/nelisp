#!/usr/bin/env python3
"""Compare the activation API corpus with GNU Emacs, including small frames."""
import os
from pathlib import Path
import subprocess
import sys
import tempfile

ROOT = Path(__file__).resolve().parents[1]
parent = ROOT / 'target/abi-n4-reference'
parent.mkdir(parents=True, exist_ok=True)
directory = Path(tempfile.mkdtemp(dir=parent))
env = dict(os.environ, TMPDIR=str(directory), XDG_CACHE_HOME=str(directory / 'cache'))
corpus = directory / 'driver.el'
corpus.write_text(';;; -*- lexical-binding: t; -*-\n' + ''.join('(load ' + __import__('json').dumps(str(ROOT / path)) + ' nil t)\n'
                          for path in ('packages/nelisp-emacs-foundation/src/emacs-cc-thread-1.el',
                                       'packages/nelisp-emacs-foundation/src/emacs-cc-thread-predicates-1.el',
                                       'test/nelisp-n4b-reference.el')) + 'nil\n')
commands = {
    'gnu': [os.environ.get('EMACS', 'emacs'), '-Q', '--batch', '-l', str(corpus)],
    'candidate': [str(Path(sys.argv[1]).resolve()), '--load', str(corpus)],
}
outputs = {}
for who, command in commands.items():
    result = subprocess.run(command, cwd=ROOT, env=env, capture_output=True, timeout=120)
    (directory / f'{who}.out').write_bytes(result.stdout)
    (directory / f'{who}.err').write_bytes(result.stderr)
    if result.returncode or result.stderr:
        print(f'N4-REFERENCE {who} FAIL exit={result.returncode}: {directory}')
        sys.exit(1)
    lines = result.stdout.decode().rstrip().splitlines()
    if lines and lines[-1] == 'nil':
        lines.pop()  # standalone --load echoes its final value
    if not lines or lines[-1] != 'N4-BACKTRACE-REFERENCE-DONE':
        sys.exit(f'N4-REFERENCE {who} missing completion: {directory}')
    outputs[who] = lines
if outputs['gnu'] != outputs['candidate']:
    import difflib
    print('\n'.join(difflib.unified_diff(outputs['gnu'], outputs['candidate'], lineterm=''))[:3000])
    sys.exit(f'N4-REFERENCE output differs: {directory}')
print(f'N4-REFERENCE checks={len(outputs["gnu"]) - 1} PASS {directory}')
