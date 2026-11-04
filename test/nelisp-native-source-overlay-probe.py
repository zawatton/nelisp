#!/usr/bin/env python3
"""Run focused host ERT against explicit source overlays, recording identities."""
import argparse
import hashlib
import json
import re
from pathlib import Path
import subprocess
import time

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--base', type=Path, required=True)
parser.add_argument('--overlay', type=Path, required=True)
parser.add_argument('--source', action='append', required=True)
parser.add_argument('--test', type=Path, required=True)
parser.add_argument('--output', type=Path, required=True)
args = parser.parse_args()
base = args.base.resolve()
overlay = args.overlay.resolve()
sources = [(overlay / name).resolve() for name in args.source]
if any(not path.is_relative_to(overlay) for path in sources):
    parser.error('Sources must remain within the explicit overlay')
command = ['emacs', '-Q', '--batch', '-L', str(base / 'lisp')]
for source in sources:
    command += ['-l', str(source)]
command += ['-l', str(args.test.resolve()), '-f', 'ert-run-tests-batch-and-exit']
args.output.parent.mkdir(parents=True, exist_ok=True)
started = time.monotonic()
with args.output.with_suffix('.stdout').open('wb') as out, \
        args.output.with_suffix('.stderr').open('wb') as err:
    try:
        result = subprocess.run(command, stdout=out, stderr=err, timeout=90)
        exit_code = result.returncode
    except subprocess.TimeoutExpired:
        exit_code = 124
log = args.output.with_suffix('.stderr').read_text(errors='replace')
counts = re.search(r'Ran (\d+) tests, (\d+) results as expected, (\d+) unexpected', log)
case_count = int(counts.group(1)) if counts else 0
if exit_code == 0 and (case_count == 0 or int(counts.group(3)) != 0):
    exit_code = 2
report = {'command': command, 'exit_code': exit_code, 'case_count': case_count,
          'elapsed_seconds': round(time.monotonic() - started, 6),
          'source_sha256': {str(path.relative_to(overlay)):
                            hashlib.sha256(path.read_bytes()).hexdigest()
                            for path in sources},
          'test_sha256': hashlib.sha256(args.test.read_bytes()).hexdigest(),
          'route': 'host-source-explicit; no native execution claim'}
args.output.with_suffix('.json').write_text(json.dumps(report, indent=2) + '\n')
print(json.dumps({'exit_code': exit_code, 'case_count': case_count,
                  'elapsed_seconds': report['elapsed_seconds']}))
raise SystemExit(exit_code)
