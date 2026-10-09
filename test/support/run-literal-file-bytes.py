#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
"""Check arbitrary byte windows directly, without compiler/corpus preparation."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import subprocess
import tempfile
import time

ROOT = Path(__file__).resolve().parents[2]


def bridge_probe(source):
    """Require the actual bridge to replace the reader, not merely load."""
    return ("(let ((before (symbol-function 'nelisp--syscall-read-file)))\n"
            + source + "\n"
            "  (when (eq before (symbol-function 'nelisp--syscall-read-file))\n"
            "    (error \"File-read bridge was not installed\"))\n"
            "  (princ \"BRIDGE-INSTALLED cases=1\\n\"))\n")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('binary', help='Standalone reader path, or GNU Emacs with --gnu')
    parser.add_argument('--gnu', action='store_true')
    parser.add_argument('--bridge', action='store_true', help='Load the actual standalone file-read bridge')
    parser.add_argument('--work', type=Path)
    args = parser.parse_args()
    if args.gnu and args.bridge:
        parser.error('--bridge requires a standalone reader')
    work = args.work or Path(tempfile.mkdtemp(prefix='literal-bytes-', dir=ROOT / 'target'))
    work = work.resolve()
    work.mkdir(parents=True, exist_ok=True)
    fixture = work / 'bytes.bin'
    data = b'x' * 4095 + bytes([192, 193]) + bytes(range(256)) * 2 + bytes(
        [13, 10, 0, 192, 128, 193, 191, 194, 128, 255, 192])
    fixture.write_bytes(data)
    binary = args.binary if args.gnu else str(Path(args.binary).resolve(strict=True))
    command = [binary] + (['-Q', '--batch'] if args.gnu else [])
    bridge_source = ROOT / 'packages/nelisp-emacs-io/src/files-standalone-buffer.el'
    if args.bridge:
        source = bridge_source.read_text()
        start = source.index("(when (and (fboundp 'rdf) (fboundp 'nelisp--write-stderr-line))")
        stop = source.index("\n(provide 'files-standalone-buffer)", start)
        bridge = work / 'bridge.el'
        bridge.write_text(bridge_probe(source[start:stop]))
        command += ['--load', str(bridge)]
    if not args.gnu:
        command += ['--load', str(ROOT / 'test/standalone-file-read-hook-arity.el')]
    command += ['--load', str(ROOT / 'test/standalone-literal-file-bytes.el')]
    start = time.monotonic()
    result = subprocess.run(command, cwd=ROOT, env=dict(os.environ, NELISP_LITERAL_FIXTURE=str(fixture)),
                            capture_output=True, timeout=60)
    (work / 'stdout').write_bytes(result.stdout)
    (work / 'stderr').write_bytes(result.stderr)
    marker = b'LITERAL-BYTES-PASS cases=16 bytes=4620\n'
    passed = result.returncode == 0 and not result.stderr and marker in result.stdout.splitlines(keepends=True)
    if not args.gnu:
        passed = passed and b'FILE-READ-HOOK-PASS cases=1\n' in result.stdout.splitlines(keepends=True)
    if args.bridge:
        passed = passed and b'BRIDGE-INSTALLED cases=1\n' in result.stdout.splitlines(keepends=True)
    receipt = dict(rc=result.returncode, passed=passed, seconds=time.monotonic() - start,
                   fixture_sha256=hashlib.sha256(data).hexdigest(), command=command)
    receipt['legacy_hook_checked'] = not args.gnu
    if args.bridge:
        receipt['bridge_source_sha256'] = hashlib.sha256(bridge_source.read_bytes()).hexdigest()
    if not args.gnu:
        receipt['binary_sha256'] = hashlib.sha256(Path(binary).read_bytes()).hexdigest()
    (work / 'receipt.json').write_text(json.dumps(receipt, indent=2) + '\n')
    print(json.dumps(receipt))
    return 0 if passed else 1


if __name__ == '__main__':
    raise SystemExit(main())
