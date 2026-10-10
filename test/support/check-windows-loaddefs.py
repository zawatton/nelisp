#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
"""Reproduce Windows host autoload data on Linux without rebuilding a reader."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import subprocess
import tempfile
import time
import zipfile

ROOT = Path(__file__).resolve().parents[2]
MEMBER = 'share/emacs/31.1/lisp/loaddefs.elc'


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('zip', type=Path, help='Official GNU Emacs 31.1 Windows zip')
    args = parser.parse_args()
    start = time.monotonic()
    with zipfile.ZipFile(args.zip) as archive:
        content = archive.read(MEMBER)
    (ROOT / 'target').mkdir(parents=True, exist_ok=True)
    work = Path(tempfile.mkdtemp(prefix='windows-loaddefs-', dir=ROOT / 'target'))
    file = work / 'loaddefs.elc'
    file.write_bytes(content)
    env = dict(os.environ, NELISP_TEST_WINDOWS_LOADDEFS=str(file))
    command = [env.get('EMACS', 'emacs'), '-Q', '--batch', '-L', 'scripts',
               '-l', 'test/nelisp-prelude-bytecode-autoload-test.el',
               '-f', 'ert-run-tests-batch-and-exit']
    result = subprocess.run(command, cwd=ROOT, env=env, capture_output=True, timeout=60)
    (work / 'test.out').write_bytes(result.stdout)
    (work / 'test.err').write_bytes(result.stderr)
    passed = (result.returncode == 0 and
              b'3 results as expected, 0 unexpected' in result.stderr and
              b'SKIPPED' not in result.stderr)
    receipt = dict(status='PASS' if passed else 'FAIL', tests=3,
                   loaddefs_sha256=hashlib.sha256(content).hexdigest(),
                   seconds=time.monotonic() - start)
    (work / 'receipt.json').write_text(json.dumps(receipt, indent=2) + '\n')
    print('WINDOWS-LOADDEFS-' + receipt['status'] + ' ' + str(work))
    return 0 if passed else 1


if __name__ == '__main__':
    raise SystemExit(main())
