#!/usr/bin/env python3
"""S5.1: check linked entry/loop placement and the paired floors evidence.

check [BINARY] reads nm/objdump and target/codegen/alignment/placement.json.
No timing claim is accepted from instruction counts. Artifacts stay in target/.
"""
import hashlib
import json
import os
from pathlib import Path
import re
import statistics
import subprocess
import sys

ROOT = Path(__file__).resolve().parents[2]
OUT = ROOT / 'target/codegen/alignment'
HOT = ('nl_root_frame', 'nl_alloc_bytes', 'nl_gc_sweep_merge_range',
       'wf_bytecode_execute_frame', 'wf_bytecode_w_fast_run')


def sha(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def inspect(binary):
    rows = subprocess.run(['nm', '-n', str(binary)], capture_output=True,
                          text=True, check=True).stdout.splitlines()
    funcs = [r.split() for r in rows if len(r.split()) == 3 and r.split()[1] in ('t', 'T')]
    if not funcs:
        raise ValueError('nm found no text symbols')
    failures = sum(int(r[0], 16) % 16 != 0 for r in funcs)
    loops = []
    present = {r[2] for r in funcs}
    for name in HOT:
        if name not in present:
            continue
        asm = subprocess.run(['objdump', '-d', '--no-show-raw-insn',
                              '--disassemble=' + name, str(binary)],
                             capture_output=True, text=True, check=True).stdout
        targets = set()
        for line in asm.splitlines():
            m = re.match(r'\s*([0-9a-f]+):\s+j\w*\s+([0-9a-f]+)\b', line)
            if m and int(m[2], 16) < int(m[1], 16):
                targets.add(int(m[2], 16))
        loops.extend(targets)
    return {'functions': len(funcs), 'unaligned_functions': failures,
            'hot_backward_targets': len(loops),
            'unaligned_hot_targets': sum(x % 16 != 0 for x in loops)}


def main():
    binary = Path(sys.argv[2] if len(sys.argv) > 2 else os.environ.get(
        'CODEGEN_ALIGNMENT_BIN', str(ROOT / 'target/codegen/lv-src/target/final/nelisp-align')))
    try:
        counts = inspect(binary)
        print(json.dumps(counts, sort_keys=True))
        if counts['unaligned_functions'] or counts['unaligned_hot_targets'] or not counts['hot_backward_targets']:
            raise ValueError('linked function/loop alignment failed')
        record = json.loads((OUT / 'placement.json').read_text())
        if record['binary_sha256'] != sha(binary):
            raise ValueError('placement evidence names a different binary')
        for name, digest in record['sources'].items():
            if sha(ROOT / name) != digest:
                raise ValueError('placement source evidence is stale: ' + name)
        if set(record['workloads']) != {'builtin', 'funcall', 'let', 'while'}:
            raise ValueError('four floors workloads are required')
        for name, rows in record['workloads'].items():
            if len(rows) != 7 or any(not r.get('candidate_khz') or not r.get('base_khz') for r in rows):
                raise ValueError('need seven clock-recorded paired rounds: ' + name)
            ratios = [r['candidate_seconds'] / r['base_seconds'] for r in rows]
            median = statistics.median(ratios)
            spread = max(ratios) - min(ratios)
            print(f'{name}: paired candidate/base={median:.5f}, range width={spread:.5f}')
            if median > 1.03 or spread >= 0.06:
                raise ValueError('placement regression/stability budget exceeded: ' + name)
        print('PASS S5.1 linked alignment and paired floors stability')
        return 0
    except (OSError, ValueError, KeyError, subprocess.CalledProcessError) as error:
        print('FAIL S5.1: ' + str(error), file=sys.stderr)
        return 1


if __name__ == '__main__':
    sys.exit(main())
