#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
"""Capture per-source and per-form evaluation timing from an opt-in trace reader.
Build a dedicated output with -l scripts/nelisp-startup-trace.el after loading
nelisp-standalone-build. Pass that binary and a new JSON receipt path here.
Receipt timestamps are pipe arrival times, useful for attribution, not benchmarks.
"""
import argparse
import collections
import hashlib
import json
from pathlib import Path
import struct
import subprocess
import time


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('binary', type=Path)
    parser.add_argument('receipt', type=Path)
    args = parser.parse_args()
    binary = args.binary.resolve(strict=True)
    if args.receipt.exists():
        parser.error('receipt exists')
    begin = time.monotonic()
    process = subprocess.Popen([str(binary), '--eval', '(+ 1 2)'],
                               stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stack, rows, counts = [], [], collections.Counter()
    diagnostics = bytearray()
    while True:
        header = process.stderr.read(40)
        if not header:
            break
        if len(header) != 40 or struct.unpack('<Q', header[:8])[0] != 5641975213432120625:
            diagnostics.extend(header + process.stderr.read())
            break
        _, kind, source, end, length = struct.unpack('<5Q', header)
        label = process.stderr.read(length).decode('utf-8', errors='replace').strip()
        now = time.monotonic()
        if kind == 0:
            counts[source] += 1
            stack.append(dict(source=source, end_offset=end, form=counts[source],
                              label=label, start=now, children=0.0))
        else:
            item = stack.pop()
            assert (item['source'], item['end_offset']) == (source, end)
            elapsed = now - item.pop('start')
            item['seconds'] = elapsed
            item['exclusive_seconds'] = elapsed - item.pop('children')
            if stack:
                stack[-1]['children'] += elapsed
            rows.append(item)
    output = process.stdout.read().decode(errors='replace')
    status = process.wait()
    sources = {}
    for row in rows:
        entry = sources.setdefault(row['source'], dict(label=row['label'], forms=0, exclusive_seconds=0))
        entry['forms'] += 1
        entry['exclusive_seconds'] += row['exclusive_seconds']
    report = dict(binary_sha256=hashlib.sha256(binary.read_bytes()).hexdigest(),
                  rc=status, output=output, diagnostics=diagnostics.decode(errors='replace'),
                  seconds=time.monotonic()-begin, sources=list(sources.values()), forms=rows)
    args.receipt.write_text(json.dumps(report, indent=2)+'\n')
    return 0 if status == 0 and output == '3\n' and not diagnostics and not stack and rows else 1


if __name__ == '__main__':
    raise SystemExit(main())
