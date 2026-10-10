#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
"""Native-path Windows F1 acceptance probe; unsupported runtime is a failure."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import signal
import subprocess
import tempfile
import time

ROOT = Path(__file__).resolve().parents[2]
DIGEST = '52c26b63098a83d62c044908b231afd5cce3a85da3b559147e6dc4810eeac2ef'


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def run(command, env, work, phase, deadline=290):
    """Save output and terminate the process tree on a deadline, including on POSIX."""
    start = time.monotonic()
    options = ({'creationflags': subprocess.CREATE_NEW_PROCESS_GROUP}
               if os.name == 'nt' else {'start_new_session': True})
    with (work / (phase + '.out')).open('wb') as output, (work / (phase + '.err')).open('wb') as errors:
        process = subprocess.Popen(command, env=env, cwd=ROOT, stdout=output, stderr=errors, **options)
        try:
            rc = process.wait(timeout=deadline)
        except subprocess.TimeoutExpired:
            try:
                if os.name == 'nt':
                    cleanup = subprocess.run(['taskkill', '/PID', str(process.pid), '/T', '/F'],
                                             stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, timeout=10)
                    if cleanup.returncode:
                        errors.write(b'Windows process-tree termination failed\n')
                else:
                    os.killpg(process.pid, signal.SIGKILL)
            except (OSError, subprocess.TimeoutExpired):
                errors.write(b'Process-tree termination unavailable\n')
            finally:
                if process.poll() is None:
                    process.kill()
                process.wait(timeout=10)
            rc = 124
    return dict(rc=rc, seconds=time.monotonic() - start)


def phase_passed(phase, receipt, output, errors):
    """Require native backend, exact completion, validation count and load digest."""
    if receipt['rc'] != 0 or receipt['seconds'] >= 300 or errors:
        return False
    marker = 'F1-COMPILE-PASS' if phase == 'compile' else 'F1-CACHE-PASS'
    lines = [line for line in output.splitlines() if line.startswith(marker)]
    validations = 1 if phase == 'compile' else 0
    if len(lines) != 1 or not re.match(r'^' + marker + r' backend=in-house ', lines[0]):
        return False
    if re.findall(r'\bvalidations=(\d+)(?:\s|$)', lines[0]) != [str(validations)]:
        return False
    if phase == 'load':
        if output.splitlines().count('F1-CORPUS-DIGEST=' + DIGEST) != 1:
            return False
        if output.splitlines().count('F1-FORCED-GC-PASS backend=in-house') != 1:
            return False
        if not re.search(r'\bcorpus=5\b', lines[0]):
            return False
    return True


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('binary', type=Path)
    parser.add_argument('--work', type=Path, help='New receipt directory; existing directory refuses.')
    parser.add_argument('--cold', action='store_true', help='Require the matching .cold image.')
    args = parser.parse_args()
    if os.name != 'nt':
        parser.error('Windows execution required; Linux emitter tests cannot qualify F1')
    binary = args.binary.resolve(strict=True)
    cold = Path(str(binary) + '.cold') if args.cold else None
    if cold is not None:
        cold.resolve(strict=True)
    work = args.work.resolve() if args.work else Path(tempfile.mkdtemp(prefix='windows-f1-', dir=ROOT / 'target'))
    if args.work:
        work.mkdir(parents=True, exist_ok=False)
    cache = work / 'cache'
    # The reader creates the cache with its protected TokenUser DACL.
    source = work / 'fixture.el'
    source.write_text(';;; -*- lexical-binding: t; -*-\n'
                      '(defun f1-fixture (x) (f1-user (cons (car x) (cdr x))))\n', encoding='utf-8')
    env = dict(os.environ, F1_SOURCE=str(source), F1_FIXTURE=str(source.with_suffix('.elc')),
               F1_BACKEND='in-house', F1_FORCE_GC='1', NELISP_NATIVE_CACHE=str(cache))
    rows = []
    report = dict(binary_sha256=digest(binary), cold_sha256=digest(cold) if cold else None,
                  driver_sha256=digest(ROOT / 'test/standalone-bytecode-native-funcall-driver.el'), rows=rows)
    host = run([env.get('EMACS', 'emacs'), '-Q', '--batch', '--eval',
                '(progn (require (quote bytecomp)) (unless (byte-compile-file (getenv "F1_SOURCE")) (error "GNU fixture failed")))'],
               env, work, 'host', deadline=60)
    if host['rc'] != 0 or not source.with_suffix('.elc').is_file():
        report['fixture_compile'] = host
        (work / 'receipt.json').write_text(json.dumps(report, indent=2) + '\n')
        print('WINDOWS-F1-FAIL fixture compilation; evidence=' + str(work))
        return 1
    report['fixture_sha256'] = digest(source.with_suffix('.elc'))
    for phase in ('compile', 'load'):
        command = [str(binary)]
        if cold:
            command += ['--cold-load-from', str(cold)]
        for path in ('lisp', 'src', 'scripts', 'packages/nl-ffi/src', 'packages/nl-prelude/src'):
            command += ['-L', str(ROOT / path)]
        command += ['--load', str(ROOT / 'test/standalone-bytecode-native-funcall-driver.el')]
        receipt = run(command, dict(env, F1_PHASE=phase), work, phase)
        output = (work / (phase + '.out')).read_text(encoding='utf-8', errors='replace')
        errors = (work / (phase + '.err')).read_text(encoding='utf-8', errors='replace')
        receipt.update(phase=phase, passed=phase_passed(phase, receipt, output, errors))
        rows.append(receipt)
        (work / 'receipt.json').write_text(json.dumps(report, indent=2) + '\n')
        print(output, end='', flush=True)
        if not receipt['passed']:
            print(errors[-4000:])
            print('WINDOWS-F1-FAIL evidence=' + str(work))
            return 1
    # Independent raw six-word bridge and preserved-register probes.
    def execute(label, driver, extra, markers):
        command = [str(binary)]
        if cold:
            command += ['--cold-load-from', str(cold)]
        for path in ('lisp', 'src', 'scripts', 'packages/nl-ffi/src', 'packages/nl-prelude/src'):
            command += ['-L', str(ROOT / path)]
        command += ['--load', str(ROOT / driver)]
        receipt = run(command, dict(env, **extra), work, label)
        output = (work / (label + '.out')).read_text(encoding='utf-8', errors='replace')
        errors = (work / (label + '.err')).read_text(encoding='utf-8', errors='replace')
        receipt.update(phase=label, passed=receipt['rc'] == 0 and not errors and
                       all(output.splitlines().count(marker) == 1 for marker in markers),
                       driver_sha256=digest(ROOT / driver))
        rows.append(receipt)
        (work / 'receipt.json').write_text(json.dumps(report, indent=2) + '\n')
        print(output, end='', flush=True)
        if not receipt['passed']:
            print(errors[-4000:])
        return receipt['passed']
    if not execute('abi', 'test/standalone-windows-native-abi-driver.el', {},
                   ['F1-ROOTS-PASS cases=6', 'WINDOWS-REGISTER-SENTINEL-PASS N=0 N=6 GP=8 XMM=10']):
        return 1
    exits = work / 'exits.el'
    exits.write_text(';;; -*- lexical-binding: t; -*-\n'
                     '(defun f1b-one (f x) (f1b-tick) (funcall f x))\n'
                     '(defun f1b-zero (f) (funcall f))\n'
                     '(defun f1b-six (f x) (funcall f x x x x x x))\n', encoding='utf-8')
    host = run([env.get('EMACS', 'emacs'), '-Q', '--batch', '--eval',
                '(progn (require (quote bytecomp)) (unless (byte-compile-file (getenv "F1B_SOURCE")) (error "GNU exit fixture failed")))'],
               dict(env, F1B_SOURCE=str(exits)), work, 'exit-host', deadline=60)
    if host['rc'] != 0 or not exits.with_suffix('.elc').is_file():
        return 1
    extra = dict(F1B_FIXTURE=str(exits.with_suffix('.elc')), F1B_BACKEND='in-house',
                 F1B_COLD='1' if cold else '0', NELISP_NATIVE_CACHE=str(work / 'exit-cache'))
    for unit in ('f1b-one', 'f1b-zero', 'f1b-six'):
        if not execute('exit-compile-' + unit, 'test/standalone-native-funcall-v2-exits-driver.el',
                       dict(extra, F1B_PHASE='compile', F1B_COMPILE_UNIT=unit),
                       ['F1B-COMPILE-PASS units=1 unit=' + unit]):
            return 1
    if not execute('exit-load', 'test/standalone-native-funcall-v2-exits-driver.el',
                   dict(extra, F1B_PHASE='load'),
                   ['F1B-CORPUS-DIGEST=ebe5cd1249025f15a6245e00f493cf7a5a9f1765bb4e07a5295836ad5a5b67d0',
                    'F1B-LOAD-PASS backend=in-house cleanup=24']):
        return 1
    if not execute('ancestor-pin', 'test/standalone-windows-native-trust-driver.el',
                   dict(WINDOWS_TRUST_ROOT=str(work), WINDOWS_TRUST_MODE='pin'),
                   ['WINDOWS-ANCESTOR-PIN-PASS unpinned=1 pinned=0 sharing=32']):
        return 1
    junction = work / 'cache-junction'
    control = subprocess.run(['cmd', '/c', 'mklink', '/J', str(junction), str(cache)],
                             capture_output=True, timeout=30)
    (work / 'junction-create.out').write_bytes(control.stdout)
    (work / 'junction-create.err').write_bytes(control.stderr)
    if control.returncode != 0 or not junction.is_dir():
        print('WINDOWS-F1-FAIL junction negative-control setup')
        return 1
    if not execute('reparse', 'test/standalone-windows-native-trust-driver.el',
                   dict(WINDOWS_TRUST_ROOT=str(junction), WINDOWS_TRUST_MODE='reparse'),
                   ['WINDOWS-REPARSE-REFUSED maps=0']):
        return 1
    print('WINDOWS-F1-PASS evidence=' + str(work))
    return 0


if __name__ == '__main__':
    raise SystemExit(main())
