#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
"""Check the real reader's rodata identity and iterative 1 MiB SHA paths.
This Linux check needs no Wine, proof decoder, bundled rebuild or cold image.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import subprocess
import tempfile
import time

ROOT = Path(__file__).resolve().parents[2]
MARKER = b'\x7fNELISP-BLDDGST\x01'


def checked(command, env, marker, timeout=60):
    start = time.monotonic()
    result = subprocess.run(command, cwd=ROOT, env=env, capture_output=True,
                            text=True, timeout=timeout)
    if result.returncode or result.stderr or result.stdout.splitlines().count(marker) != 1:
        raise RuntimeError(f'Reader check failed rc={result.returncode}: '
                           + result.stdout[-1000:] + result.stderr[-1000:])
    return time.monotonic() - start


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('binary', nargs='?', type=Path, default=Path('target/nelisp'))
    args = parser.parse_args()
    binary = args.binary.resolve(strict=True)
    image = binary.read_bytes()
    if image.count(MARKER) != 1:
        raise ValueError('Build marker must be unique')
    offset = image.index(MARKER) + len(MARKER)
    raw = image[offset:offset + 32]
    zeroed = image[:offset] + bytes(32) + image[offset + 32:]
    expected = hashlib.sha256(zeroed).hexdigest()
    if raw.hex() != expected:
        raise ValueError('Linked stamp differs from independent zero-field SHA-256')
    env = dict(os.environ, NELISP_EXPECTED_BUILD_DIGEST=expected,
               NELISP_EXPECTED_FILE_DIGEST=hashlib.sha256(image).hexdigest())
    command = [str(binary), '-L', 'lisp', '-L', 'src', '-L', 'scripts']
    with tempfile.TemporaryDirectory(prefix='build-digest-vector-', dir=ROOT / 'target') as scratch:
        high_bytes = Path(scratch) / 'high-bytes.bin'
        high_bytes.write_bytes(b'\xff' * 1048576)
        env['NELISP_HIGH_BYTES'] = str(high_bytes)
        seconds = checked(command + ['--load', 'test/standalone-build-digest-driver.el'],
                          env, 'BUILD-DIGEST-SHA-PASS')
    # The exact accessor output is also independently computed in host Lisp.
    expression = ('(progn '
                  '(unless (equal (nelisp-build-digest-reference '
                  '(apply (quote unibyte-string) (quote ' + '(' + ' '.join(map(str, raw)) + ')'
                  + '))) "' + expected + '") (error "Reference mismatch")))')
    subprocess.run([env.get('EMACS', 'emacs'), '-Q', '--batch',
                    '-l', str(ROOT / 'test/nelisp-build-digest-test.el'), '--eval', expression],
                   cwd=ROOT, env=env, check=True, capture_output=True)
    # Unstamped readers must return nil rather than minting an all-zero identity.
    with tempfile.TemporaryDirectory(prefix='build-digest-', dir=ROOT / 'target') as scratch:
        unstamped = Path(scratch) / 'unstamped-reader'
        unstamped.write_bytes(zeroed)
        unstamped.chmod(0o700)
        checked([str(unstamped), '--eval',
                 '(if (nelisp--build-digest) (error "Unstamped identity accepted") '
                 '(princ "UNSTAMPED-REFUSED\\n"))'], env, 'UNSTAMPED-REFUSED')
    print(json.dumps(dict(binary_sha256=hashlib.sha256(image).hexdigest(),
                          build_digest=expected, seconds=seconds,
                          checks=['stamp', 'fresh-string', 'unstamped', 'lisp-reference', 'Linux-whole-file',
                                  '1MiB-string', '1MiB-bytes', '1MiB-high-bytes', 'Windows-buffer'])))


if __name__ == '__main__':
    main()
