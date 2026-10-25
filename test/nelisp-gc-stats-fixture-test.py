#!/usr/bin/env python3
"""Exercise the statistics fixture and adversarial controls on an existing binary.

Run through bin/heavy-run.sh with --binary BIN (repeatable). No build is required.
"""
import argparse
import hashlib
import os
from pathlib import Path
import re
import subprocess
import sys
import time

ROOT = Path(__file__).resolve().parents[1]
SOURCE = ROOT / 'tools/ai/gc-port-gc-stats.el'


def replace_once(source, old, new):
    if source.count(old) != 1:
        raise ValueError(f'Expected exactly one mutation site: {old}')
    return source.replace(old, new, 1)


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument('--binary', required=True, type=Path, action='append')
    ap.add_argument('--work', type=Path, default=ROOT / 'target/gc-stats-fixture-controls')
    a = ap.parse_args()
    a.work.mkdir(parents=True, exist_ok=True)
    source = SOURCE.read_text()
    cases = [
        ('normal', source, None),
        ('retain-population', replace_once(
            source, '(aset holder 0 nil) (aset holder 1 nil) (aset holder 2 nil) (aset holder 3 nil)',
            'nil'), 'dropped objects still counted'),
        ('counter-disagreement', replace_once(
            source, '(setq comparison (nelisp--debug-switch 53))',
            "(setq comparison (if (> n 0) '(1) (nelisp--debug-switch 53)))"), 'exact: counters/census'),
        ('growing-control', replace_once(
            source, '(setq kept (gcs-delta a (gcs-stats)))',
            '(setq gcs-keep (cons (vector n) gcs-keep))\n    (setq kept (gcs-delta a (gcs-stats)))'),
         'zero-population control'),
    ]
    ok = True
    start = time.monotonic()
    for index, binary in enumerate(a.binary):
        binary = binary.resolve()
        print(f'BINARY {binary.name} sha256={hashlib.sha256(binary.read_bytes()).hexdigest()}')
        for name, program, failure in cases:
            stem = a.work / f'{index}-{binary.name}-{name}'
            script = stem.with_suffix('.el')
            script.write_text(program)
            result = subprocess.run(
                ['bash', '-c', 'ulimit -v 8388608; exec timeout 120 "$@"', 'gcs-control',
                 str(binary), '--load', str(script.resolve())],
                cwd=ROOT, env=dict(os.environ, GCS_MODE='run'), capture_output=True, text=True,
                timeout=130)
            stem.with_suffix('.stdout').write_text(result.stdout)
            stem.with_suffix('.stderr').write_text(result.stderr)
            summaries = re.findall(r'^GCS run checks=(\d+) failures=(\d+) (PASS|FAIL)$', result.stdout, re.M)
            valid = result.returncode == 0 and not result.stderr and len(summaries) == 1
            if valid:
                checks, failures, verdict = summaries[0]
                valid = int(checks) >= 17
                valid &= (int(failures) == 0 and verdict == 'PASS') if failure is None else (
                    int(failures) > 0 and verdict == 'FAIL' and f'GCS FAIL {failure}' in result.stdout)
            ok &= valid
            print(f'{"PASS" if valid else "FAIL"} {binary.name} {name}: rc={result.returncode} {summaries}')
            for line in result.stdout.splitlines():
                if line.startswith('GCS FAIL'):
                    print(line)
    print(f'GC-STATS-FIXTURE-CONTROLS {"PASS" if ok else "FAIL"} elapsed={time.monotonic() - start:.3f}s')
    return 0 if ok else 1


if __name__ == '__main__':
    sys.exit(main())
