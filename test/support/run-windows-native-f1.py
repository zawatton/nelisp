#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
"""Native-path Windows F1 acceptance probe; unsupported runtime is a failure."""
import argparse
from functools import lru_cache
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
PATH_VARIABLES = ('F1_SOURCE', 'F1_FIXTURE', 'F1B_SOURCE', 'F1B_FIXTURE',
                  'NELISP_NATIVE_CACHE', 'WINDOWS_TRUST_ROOT')


@lru_cache(maxsize=64)
def wine_path(path):
    """Use the selected Wine prefix's drive mappings, once per distinct path."""
    result = subprocess.run(['winepath', '-w', str(path)], check=True,
                            capture_output=True, text=True, timeout=30)
    converted = result.stdout.strip()
    if result.stderr or not re.fullmatch(r'[A-Za-z]:[\\/].+', converted):
        raise ValueError('Wine path conversion refused: ' + str(path))
    return converted


def reader_command(binary, cold, driver, wine=False):
    path = wine_path if wine else str
    command = ['wine', str(binary)] if wine else [str(binary)]
    if cold:
        command += ['--cold-load-from', path(cold)]
    for directory in ('lisp', 'src', 'scripts', 'packages/nl-ffi/src', 'packages/nl-prelude/src'):
        command += ['-L', path(ROOT / directory)]
    return command + ['--load', path(ROOT / driver)]


def reader_environment(env, wine=False):
    """Keep GNU Emacs host paths separate from Windows reader paths."""
    result = dict(env)
    if wine:
        for name in PATH_VARIABLES:
            if name in result:
                result[name] = wine_path(result[name])
    return result


def fixture_expression(variable):
    """Refuse an unpinned host before producing platform-independent bytecode."""
    return ('(progn (require (quote nelisp-bytecode-compiler-input-dialect)) '
            '(unless (eq (plist-get (nelisp-bytecode-compiler-input-dialect) :status) (quote pinned)) '
            '(error "GNU fixture requires pinned Emacs 31.1 dialect")) '
            '(require (quote bytecomp)) (unless (byte-compile-file (getenv "' + variable + '")) '
            '(error "GNU fixture failed")))')


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
    parser.add_argument('--wine', action='store_true',
                        help='Run the PE under Wine on POSIX; cannot qualify real Windows acceptance.')
    args = parser.parse_args()
    if args.wine and os.name == 'nt':
        parser.error('--wine requires a POSIX host')
    if os.name != 'nt' and not args.wine:
        parser.error('Windows execution required; Linux emitter tests cannot qualify F1')
    if args.wine:
        os.environ.setdefault('WINEPREFIX', str(Path.home() / '.cache/wine-nelisp'))
        os.environ['WINEDEBUG'] = '-all'
    verdict = 'WINE-F1' if args.wine else 'WINDOWS-F1'
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
    report = dict(execution='wine' if args.wine else 'windows',
                  binary_sha256=digest(binary), cold_sha256=digest(cold) if cold else None,
                  startup_sha256=digest(Path(str(binary) + '.native-startup.el')),
                  driver_sha256=digest(ROOT / 'test/standalone-bytecode-native-funcall-driver.el'), rows=rows)
    host = run([env.get('EMACS', 'emacs'), '-Q', '--batch', '-L', str(ROOT / 'lisp'), '--eval',
                fixture_expression('F1_SOURCE')],
               env, work, 'host', deadline=60)
    report['fixture_compile'] = host
    if host['rc'] != 0 or not source.with_suffix('.elc').is_file():
        (work / 'receipt.json').write_text(json.dumps(report, indent=2) + '\n')
        print(verdict + '-FAIL fixture compilation; evidence=' + str(work))
        return 1
    report['fixture_sha256'] = digest(source.with_suffix('.elc'))
    for phase in ('compile', 'load'):
        try:
            command = reader_command(binary, cold, 'test/standalone-bytecode-native-funcall-driver.el', args.wine)
            current = reader_environment(dict(env, F1_PHASE=phase), args.wine)
        except (OSError, ValueError, subprocess.SubprocessError) as error:
            report['wine_setup_error'] = str(error)
            report['wine_setup_stderr'] = getattr(error, 'stderr', None)
            (work / 'receipt.json').write_text(json.dumps(report, indent=2) + '\n')
            print(verdict + '-FAIL ' + str(error) + '; evidence=' + str(work))
            return 1
        receipt = run(command, current, work, phase)
        output = (work / (phase + '.out')).read_text(encoding='utf-8', errors='replace')
        errors = (work / (phase + '.err')).read_text(encoding='utf-8', errors='replace')
        receipt.update(phase=phase, passed=phase_passed(phase, receipt, output, errors))
        rows.append(receipt)
        (work / 'receipt.json').write_text(json.dumps(report, indent=2) + '\n')
        print(output, end='', flush=True)
        if not receipt['passed']:
            print(errors[-4000:])
            print(verdict + '-FAIL evidence=' + str(work))
            return 1
    # Independent raw six-word bridge and preserved-register probes.
    def execute(label, driver, extra, markers):
        command = reader_command(binary, cold, driver, args.wine)
        receipt = run(command, reader_environment(dict(env, **extra), args.wine), work, label)
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
    host = run([env.get('EMACS', 'emacs'), '-Q', '--batch', '-L', str(ROOT / 'lisp'), '--eval',
                fixture_expression('F1B_SOURCE')],
               dict(env, F1B_SOURCE=str(exits)), work, 'exit-host', deadline=60)
    report['exit_fixture_compile'] = host
    (work / 'receipt.json').write_text(json.dumps(report, indent=2) + '\n')
    if host['rc'] != 0 or not exits.with_suffix('.elc').is_file():
        return 1
    report['exit_fixture_sha256'] = digest(exits.with_suffix('.elc'))
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
    control_command = (['wine', 'cmd', '/c', 'mklink', '/J', wine_path(junction), wine_path(cache)]
                       if args.wine else ['cmd', '/c', 'mklink', '/J', str(junction), str(cache)])
    control = subprocess.run(control_command,
                             capture_output=True, timeout=30)
    (work / 'junction-create.out').write_bytes(control.stdout)
    (work / 'junction-create.err').write_bytes(control.stderr)
    if control.returncode != 0 or not junction.is_dir():
        print(verdict + '-FAIL junction negative-control setup')
        return 1
    if not execute('reparse', 'test/standalone-windows-native-trust-driver.el',
                   dict(WINDOWS_TRUST_ROOT=str(junction), WINDOWS_TRUST_MODE='reparse'),
                   ['WINDOWS-REPARSE-REFUSED maps=0']):
        return 1
    print(verdict + '-PASS evidence=' + str(work))
    return 0


if __name__ == '__main__':
    raise SystemExit(main())
