#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
"""W1.4: on Windows the 1e6 hot loop runs natively in at most 1/5 of the
same reader's interpreted time.  Exit 0 and print WINDOWS-HOT-LOOP-PASS
only when compile, both timings and the value check succeed."""
import argparse
import importlib.util
import json
import os
from pathlib import Path
import re
import subprocess
import sys
import time

ROOT = Path(__file__).resolve().parents[2]
_spec = importlib.util.spec_from_file_location(
    'f1', Path(__file__).with_name('run-windows-native-f1.py'))
f1 = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(f1)
FIXTURE = ROOT / 'test/windows-native-hot-loop-fixture.el'
DRIVER = 'test/windows-native-hot-loop-driver.el'
TIME = re.compile(r'HOT-TIME mode=(\w+) backend=\S+ n=(\d+) seconds=([0-9.]+)')


def run(command, env, deadline, log):
    started = time.monotonic()
    try:
        result = subprocess.run(command, env=env, capture_output=True, timeout=deadline)
        rc, out, err = result.returncode, result.stdout, result.stderr
    except subprocess.TimeoutExpired as exc:
        rc, out, err = 'timeout', exc.stdout or b'', exc.stderr or b''
    log.write_bytes(out + b'\n--- stderr ---\n' + err)
    return rc, out.decode('utf-8', 'replace'), time.monotonic() - started


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('binary', type=Path)
    parser.add_argument('--work', type=Path, required=True)
    parser.add_argument('--n', type=int, default=1000000)
    parser.add_argument('--samples', type=int, default=3)
    parser.add_argument('--wine', action='store_true')
    args = parser.parse_args()
    if os.name != 'nt' and not args.wine:
        parser.error('Windows execution required')
    if args.wine:
        os.environ.setdefault('WINEPREFIX', str(Path.home() / '.cache/wine-nelisp'))
        os.environ['WINEDEBUG'] = '-all'
    binary = args.binary.resolve(strict=True)
    cold = Path(str(binary) + '.cold').resolve(strict=True)
    work = args.work.resolve()
    work.mkdir(parents=True, exist_ok=False)
    source = work / 'fixture.el'
    source.write_bytes(FIXTURE.read_bytes())
    deadline = f1.DEFAULT_DEADLINE
    host = subprocess.run(['emacs', '-Q', '--batch', '-L', str(ROOT / 'lisp'),
                           '--eval', f1.fixture_expression('F1_SOURCE')],
                          env=dict(os.environ, F1_SOURCE=str(source)),
                          capture_output=True, timeout=600)
    fixture = source.with_suffix('.elc')
    if host.returncode != 0 or not fixture.is_file():
        sys.stderr.write(host.stderr.decode('utf-8', 'replace')[-2000:])
        print('WINDOWS-HOT-LOOP-FAIL fixture')
        return 1
    # The reader creates the cache itself with its protected DACL.
    env = dict(os.environ, F1_SOURCE=str(source), F1_FIXTURE=str(fixture),
               NELISP_NATIVE_CACHE=str(work / 'cache'), HOT_BACKEND='in-house',
               HOT_N=str(args.n))
    env = f1.reader_environment(env, args.wine)
    env['HOT_SOURCE'], env['HOT_FIXTURE'] = env.pop('F1_SOURCE'), env.pop('F1_FIXTURE')
    command = f1.reader_command(binary, cold, DRIVER, args.wine)
    report = dict(execution='wine' if args.wine else 'windows', n=args.n,
                  binary_sha256=f1.digest(binary), cold_sha256=f1.digest(cold),
                  fixture_sha256=f1.digest(fixture), driver_sha256=f1.digest(ROOT / DRIVER))
    rc, out, seconds = run(command, dict(env, HOT_PHASE='compile'), deadline, work / 'compile.log')
    report['compile'] = dict(rc=rc, seconds=seconds, passed=rc == 0 and 'HOT-COMPILE-PASS' in out)
    times = {'native': [], 'interpreted': []}
    if report['compile']['passed']:
        for index in range(args.samples):
            for mode in ('native', 'interpreted'):
                rc, out, _ = run(command, dict(env, HOT_PHASE='time', HOT_MODE=mode),
                                 deadline, work / f'{mode}-{index}.log')
                match = TIME.search(out)
                if rc != 0 or not match or match.group(1) != mode or int(match.group(2)) != args.n:
                    times[mode].append(None)
                else:
                    times[mode].append(float(match.group(3)))
    valid = all(times[m] and None not in times[m] for m in times)
    native = min(times['native']) if valid else None
    interpreted = min(times['interpreted']) if valid else None
    report.update(times=times, native=native, interpreted=interpreted,
                  ratio=(native / interpreted) if valid and interpreted else None)
    report['passed'] = bool(valid and interpreted and native * 5 <= interpreted)
    (work / 'receipt.json').write_text(json.dumps(report, indent=2) + '\n', encoding='utf-8')
    verdict = 'PASS' if report['passed'] else 'FAIL'
    print(f'WINDOWS-HOT-LOOP-{verdict} native={native} interpreted={interpreted} '
          f'ratio={report["ratio"]} evidence={work}')
    return 0 if report['passed'] else 1


if __name__ == '__main__':
    sys.exit(main())
