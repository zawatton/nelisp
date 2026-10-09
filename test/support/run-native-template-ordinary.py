#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
"""Median-of-three P2.3 template compilation/load/call receipts and runtime controls."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import statistics
import subprocess
import tempfile
import time

ROOT = Path(__file__).resolve().parents[2]
NAMES = ('compiler-r3-cons', 'cadr', 'syntax-class', 'string-greaterp')


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('binary', type=Path)
    parser.add_argument('--nice', type=int, default=0, help='Explicit measured scheduler niceness, never load normalization.')
    args = parser.parse_args()
    binary = args.binary.resolve(strict=True)
    cold = Path(str(binary) + '.cold')
    work = Path(tempfile.mkdtemp(prefix='p23-timing-', dir=ROOT / 'target'))
    source = work / 'cons.el'
    source.write_text(';;; -*- lexical-binding: t; -*-\n(defun compiler-r3-cons (a b) (cons a b))\n')
    env = dict(os.environ, P23_SOURCE=str(source))
    subprocess.run([env.get('EMACS', 'emacs'), '-Q', '--batch', '--eval',
                    '(unless (byte-compile-file (getenv "P23_SOURCE")) (error "GNU fixture failed"))'],
                   cwd=ROOT, env=env, check=True, capture_output=True)
    rows = []
    identities = dict(binary_sha256=digest(binary), cold_sha256=digest(cold), fixture_sha256=digest(source.with_suffix('.elc')),
                      f3_sha256=digest(ROOT / 'test/support/native-real-corpus-fixtures.el'),
                      driver_sha256=digest(ROOT / 'test/standalone-native-template-ordinary-driver.el'))
    for name in NAMES:
        for sample in range(3):
            directory = work / f'{name}-{sample}'; directory.mkdir(mode=0o700)
            cache = directory / 'cache'; cache.mkdir(mode=0o700)
            controls = name == NAMES[0] and sample == 0
            local = dict(env, TEMPLATE_NAME=name, TEMPLATE_FIXTURE=str(source.with_suffix('.elc')),
                         TEMPLATE_CONTROLS='1' if controls else '0', NELISP_NATIVE_CACHE=str(cache))
            command = ['nice', '-n', str(args.nice), 'timeout', '-k', '5', '290', str(binary), '--cold-load-from', str(cold),
                       '-L', 'lisp', '-L', 'src', '-L', 'scripts', '-L', 'packages/nl-ffi/src',
                       '--load', 'test/standalone-native-template-ordinary-driver.el']
            load_before = os.getloadavg(); start = time.monotonic()
            with (directory / 'stdout').open('w') as out, (directory / 'stderr').open('w') as err:
                result = subprocess.run(command, cwd=ROOT, env=local, stdout=out, stderr=err)
            seconds = time.monotonic() - start
            output = (directory / 'stdout').read_text(); errors = (directory / 'stderr').read_text()
            records = re.findall(r'^P23-TIMING name=(\S+) compile=([\d.]+) load=([\d.]+) call=([\d.]+) total=([\d.]+) validations=1 maps=1 entries=1$', output, re.M)
            passed = result.returncode == 0 and seconds < 300 and not errors and len(records) == 1 and records[0][0] == name
            if controls:
                passed &= output.splitlines().count('P23-SWITCH-PASS valid=3 invalid=4') == 1
                passed &= output.splitlines().count('P23-CYCLE-PASS callbacks=3 quit=1 gc=3') == 1
            row = dict(name=name, sample=sample, nice=args.nice, rc=result.returncode, process_seconds=seconds,
                       load_before=load_before, load_after=os.getloadavg(), passed=passed)
            if records:
                row.update(zip(('compile', 'load', 'call', 'total'), map(float, records[0][1:])))
            rows.append(row)
            report = dict(identities=identities, rows=rows,
                          medians={n: statistics.median(r['total'] for r in rows if r['name'] == n and 'total' in r)
                                   for n in NAMES if any(r['name'] == n and 'total' in r for r in rows)})
            (work / 'receipt.json').write_text(json.dumps(report, indent=2) + '\n')
            print(output[-1500:], end='', flush=True)
            if not passed:
                print(errors[-2500:]); print(f'P23-EVIDENCE={work}'); return 1
    print(json.dumps(report['medians'])); print(f'P23-EVIDENCE={work}'); return 0


if __name__ == '__main__':
    raise SystemExit(main())
