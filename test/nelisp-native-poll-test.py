#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
"""Check the real allocator's debt/growth re-arming without a reader rebuild."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import subprocess
import tempfile
import time

ROOT = Path(__file__).resolve().parents[1]

def sha(path):
    with path.open('rb') as stream:
        return hashlib.file_digest(stream, 'sha256').hexdigest()

def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--binary', type=Path, default=Path('target/nelisp-static'))
    parser.add_argument('--source', type=Path, default=Path('lisp/nelisp-native-poll.el'))
    parser.add_argument('--work', type=Path)
    args = parser.parse_args()
    binary = (ROOT / args.binary).resolve(strict=True)
    source = (ROOT / args.source).resolve(strict=True)
    cold = Path(str(binary) + '.cold')
    # The fixed virtual address is valid only for this unstripped Linux ELF.
    with binary.open('rb') as stream:
        header = stream.read(20)
    if header[:5] != b'\x7fELF\x02' or int.from_bytes(header[16:18], 'little') != 2:
        raise ValueError('Allocator control requires an ELF64 ET_EXEC reader')
    symbols = subprocess.check_output(['nm', str(binary)], text=True)
    addresses = [line.split()[0] for line in symbols.splitlines()
                 if line.endswith(' B nl_gc_stats')]
    if len(addresses) != 1 or int(addresses[0], 16) < 4096:
        raise ValueError('Reader does not expose one nl_gc_stats ELF symbol')
    if args.work:
        work = ROOT / args.work
        work.mkdir(parents=True, mode=0o700)
    else:
        work = Path(tempfile.mkdtemp(prefix='native-poll-', dir=ROOT / 'target'))
    driver = ROOT / 'test/standalone-native-poll-driver.el'
    identities = dict(binary=sha(binary), image=sha(cold), source=sha(source), driver=sha(driver))
    cmd = ['timeout', '-k', '5', '120', str(binary), '--cold-load-from', str(cold),
           '-L', 'lisp', '-L', 'src', '-L', 'scripts', '--load', str(driver)]
    start = time.monotonic()
    with (work / 'stdout').open('w') as stdout, (work / 'stderr').open('w') as stderr:
        result = subprocess.run(cmd, cwd=ROOT, stdout=stdout, stderr=stderr,
                                env=dict(os.environ, POLL_GC_STATS=addresses[0],
                                         POLL_CONTROL_SOURCE=str(source)))
    output = (work / 'stdout').read_text()
    passed = (result.returncode == 0 and not (work / 'stderr').stat().st_size
              and output.count('POLL-ALLOCATOR-PASS debt=1 quiet-polls=10 roots=retained\n') == 1
              and output.count('POLL-ALLOCATOR-PASS growth=1 quiet-polls=10 roots=retained\n') == 1
              and identities == dict(binary=sha(binary), image=sha(cold), source=sha(source), driver=sha(driver)))
    row = dict(passed=passed, rc=result.returncode, seconds=time.monotonic()-start,
               command=cmd, sha256=identities, stats_address=addresses[0])
    (work / 'receipt.json').write_text(json.dumps(row, indent=2)+'\n')
    print(f'NATIVE-POLL passed={passed} seconds={row["seconds"]:.3f} evidence={work}')
    return 0 if passed else 1

if __name__ == '__main__':
    raise SystemExit(main())
