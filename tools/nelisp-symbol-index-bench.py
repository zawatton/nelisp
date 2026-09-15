#!/usr/bin/env python3
"""Compare generated-symbol table work, including process startup and setup."""
import argparse
import hashlib
import json
from pathlib import Path
import statistics
import subprocess
import time


def positive(value):
    number = int(value)
    if number < 1:
        raise argparse.ArgumentTypeError('must be positive')
    return number


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--before', type=Path, required=True)
    parser.add_argument('--after', type=Path, required=True)
    parser.add_argument('--keys', type=positive, default=100)
    parser.add_argument('--repeat', type=positive, default=3)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    binaries = {'before': args.before.resolve(), 'after': args.after.resolve()}
    hashes = {name: digest(path) for name, path in binaries.items()}
    if hashes['before'] == hashes['after']:
        parser.error('before and after must be different executable contents')
    source = f'''(setq bench-table (make-hash-table :test 'eq) bench-keys nil bench-i 0 bench-sum 0)
(while (< bench-i {args.keys}) (let ((s (make-symbol "bench"))) (puthash s bench-i bench-table) (setq bench-keys (cons s bench-keys))) (setq bench-i (+ bench-i 1)))
(dolist (s bench-keys) (setq bench-sum (+ bench-sum (gethash s bench-table))))
(prin1 (list (hash-table-count bench-table) bench-sum))
(terpri)
(exit 0)
'''
    expected = f'({args.keys} {args.keys * (args.keys - 1) // 2})\n'
    report = {'binaries': {k: str(v) for k, v in binaries.items()},
              'sha256': hashes, 'keys': args.keys, 'source': source,
              'expected': expected, 'pairs_ms': [], 'passed': False,
              'scope': 'Wall time includes startup, setup, insertion and lookup; '
                       'run only after other builds/tests finish.'}
    args.output.parent.mkdir(parents=True, exist_ok=True)
    try:
        for trial in range(args.repeat):
            pair = {}
            order = ['before', 'after'] if trial % 2 == 0 else ['after', 'before']
            for name in order:
                start = time.monotonic()
                result = subprocess.run(
                    [str(binaries[name]), '--repl', '--no-prompt', '--no-print'],
                    input=source, capture_output=True, text=True, timeout=120)
                pair[name] = (time.monotonic() - start) * 1000
                if (result.returncode, result.stderr, result.stdout) != (0, '', expected):
                    report['failure'] = {'side': name, 'exit_code': result.returncode,
                                         'stdout': result.stdout, 'stderr': result.stderr}
                    raise RuntimeError(f'{name}: benchmark output contract failed')
            report['pairs_ms'].append(pair)
        if any(digest(path) != hashes[name] for name, path in binaries.items()):
            raise RuntimeError('executable changed during measurement')
        report['median_ms'] = {
            name: statistics.median(pair[name] for pair in report['pairs_ms'])
            for name in binaries}
        report['passed'] = True
    except Exception as error:
        report['error'] = str(error)
        raise
    finally:
        args.output.write_text(json.dumps(report, indent=2) + '\n', encoding='utf-8')
    print(json.dumps(report['median_ms']))


if __name__ == '__main__':
    main()
