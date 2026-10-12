#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
"""Run genuine GNU F1 on Linux ARM64, natively or with explicit qemu-user."""
import argparse
import importlib.util
import json
import os
from pathlib import Path
import platform
import re
import subprocess
import tempfile
import time
import unittest
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[2]
spec = importlib.util.spec_from_file_location('f1_common', Path(__file__).with_name('run-windows-native-f1.py'))
common = importlib.util.module_from_spec(spec)
spec.loader.exec_module(common)
DIGEST = common.DIGEST
DEADLINE = int(os.environ.get('NELISP_ARM64_NATIVE_DEADLINE', '1800'))
common.DEFAULT_DEADLINE = DEADLINE


def command(binary, cold, driver, qemu=None):
    result = ([str(qemu)] if qemu else []) + [str(binary)]
    if cold:
        result += ['--cold-load-from', str(cold)]
    for directory in ('lisp', 'src', 'scripts', 'test/support', 'packages/nl-ffi/src', 'packages/nl-prelude/src'):
        result += ['-L', str(ROOT / directory)]
    return result + ['-l', str(ROOT / driver)]


def phase_passed(phase, row, output, errors):
    entries = re.findall(r'^ARM64-NATIVE-ENTRIES=(\d+)$', output, re.M)
    pages = re.findall(r'^ARM64-PAGE-BYTES=(\d+)$', output, re.M)
    return (common.phase_passed(phase, row, output, errors) and len(entries) == 1 and int(entries[0]) > 0
            and len(pages) == 1 and int(pages[0]) in (4096, 16384, 65536))


class Controls(unittest.TestCase):
    def test_receipt_refuses_missing_or_duplicate_native_entries(self):
        row = dict(rc=0, seconds=1)
        base = 'ARM64-PAGE-BYTES=4096\nF1-COMPILE-PASS backend=in-house seconds=1 validations=1 file="fixture"\n'
        self.assertTrue(phase_passed('compile', row, base + 'ARM64-NATIVE-ENTRIES=1\n', ''))
        for suffix in ('', 'ARM64-NATIVE-ENTRIES=0\n', 'ARM64-NATIVE-ENTRIES=1\n' * 2):
            self.assertFalse(phase_passed('compile', row, base + suffix, ''))
        self.assertFalse(phase_passed('compile', row, base + 'ARM64-NATIVE-ENTRIES=1\n', 'error'))
    def test_qemu_and_cold_are_both_in_exact_command(self):
        result = command(Path('/reader'), Path('/reader.cold'), 'fixture.el', Path('/qemu'))
        self.assertEqual(result[:4], ['/qemu', '/reader', '--cold-load-from', '/reader.cold'])
        self.assertEqual(result[-2:], ['-l', str(ROOT / 'fixture.el')])
    def test_deadline_cleanup(self):
        import sys
        with tempfile.TemporaryDirectory() as directory:
            result = common.run([sys.executable, '-c', 'import time; time.sleep(10)'], dict(os.environ), Path(directory), 'timeout', deadline=.1)
            self.assertEqual(result['rc'], 124)
            self.assertLess(result['seconds'], 5)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('binary', nargs='?', type=Path)
    parser.add_argument('--qemu', type=Path, help='Explicit qemu-aarch64 executable; emulation does not qualify hardware.')
    parser.add_argument('--cold', action='store_true')
    parser.add_argument('--both', action='store_true', help='Require cold and source paths in independent caches.')
    parser.add_argument('--work', type=Path, help='Fresh receipt directory, never reuse an earlier cache.')
    parser.add_argument('--cache-parent', type=Path, help='Optional private cache ancestor; receipts remain under --work.')
    parser.add_argument('--self-test', action='store_true')
    args = parser.parse_args()
    if args.self_test:
        return 0 if unittest.TextTestRunner().run(unittest.defaultTestLoader.loadTestsFromTestCase(Controls)).wasSuccessful() else 1
    if not args.binary: parser.error('binary required')
    binary = args.binary.resolve(strict=True)
    header = binary.open('rb').read(64)
    if header[:6] != b'\x7fELF\x02\x01' or int.from_bytes(header[18:20], 'little') != 183:
        parser.error('reader must be an EM_AARCH64 ELF64 image')
    if not args.qemu and (platform.system() != 'Linux' or platform.machine() not in ('aarch64', 'arm64')):
        parser.error('Linux ARM64 execution or --qemu required')
    qemu = args.qemu.resolve(strict=True) if args.qemu else None
    work = args.work.resolve() if args.work else Path(tempfile.mkdtemp(prefix='arm64-f1-', dir=ROOT / 'target/arm64-native'))
    if args.work: work.mkdir(parents=True, mode=0o700, exist_ok=False)
    common.RUN_DEADLINE = time.monotonic() + int(os.environ.get('NELISP_ARM64_NATIVE_RUN_BUDGET', '10800'))
    report = dict(domain='linux-arm64-f1-v1', execution='qemu-user' if qemu else 'linux-arm64-hardware',
                  hardware_qualified=False, uname=platform.uname()._asdict(), host_page_size=os.sysconf('SC_PAGESIZE'),
                  binary_sha256=common.digest(binary), startup_sha256=common.digest(Path(str(binary) + '.native-startup.el')),
                  corpus_digest=DIGEST, paths={}, passed=False)
    report['cache_coherence_qualified'] = False
    report['probe_sources'] = {name: common.digest(ROOT / name) for name in
                               ('test/support/native-entry-observer.el', 'test/support/arm64-native-probes.el',
                                'scripts/generate-arm64-native-probes.py')}
    report['cpu'] = Path('/proc/cpuinfo').read_text() if Path('/proc/cpuinfo').exists() else None
    report['source_head'] = subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=ROOT, text=True).strip()
    import hashlib
    report['source_diff_sha256'] = hashlib.sha256(subprocess.check_output(['git', 'diff', '--binary', 'HEAD'], cwd=ROOT)).hexdigest()
    report['source_files'] = {str(p.relative_to(ROOT)): common.digest(p)
                              for directory in ('src', 'lisp', 'scripts')
                              for p in sorted((ROOT / directory).glob('*.el'))}
    def save(): (work / 'receipt.json').write_text(json.dumps(report, indent=2) + '\n')
    if qemu:
        report['qemu'] = dict(path=str(qemu), sha256=common.digest(qemu), version=subprocess.check_output([str(qemu), '--version'], text=True).splitlines()[0])
    save()
    for cold_mode in ([True, False] if args.both else [args.cold]):
        label = 'cold' if cold_mode else 'source'
        directory = work / label; directory.mkdir(mode=0o700)
        cold = Path(str(binary) + '.cold') if cold_mode else None
        if cold: cold.resolve(strict=True)
        source = directory / 'fixture.el'
        source.write_text(';;; -*- lexical-binding: t; -*-\n(defun f1-fixture (x) (f1-user (cons (car x) (cdr x))))\n')
        cache_root = (Path(tempfile.mkdtemp(prefix='arm64-' + label + '-cache-', dir=args.cache_parent.resolve(strict=True)))
                      if args.cache_parent else directory)
        env = dict(os.environ, F1_SOURCE=str(source), F1_FIXTURE=str(source.with_suffix('.elc')),
                   F1_BACKEND='in-house', F1_FORCE_GC='1', NELISP_NATIVE_CACHE=str(cache_root / 'cache'))
        rows = []
        path_report = dict(cache_root=str(cache_root), cold_sha256=common.digest(cold) if cold else None, rows=rows)
        report['paths'][label] = path_report
        if cold:
            path_report['environment_scrubbed'] = common.COLD_ENV_CANARY not in cold.read_bytes()
            if not path_report['environment_scrubbed']:
                save(); return 1
            with patch.object(common, 'reader_command', side_effect=lambda b, c, d, wine=False: command(b, c, d, qemu)):
                path_report['cold_controls'] = common.cold_controls(binary, cold, env, directory)
            save()
            if not all(row['passed'] for row in path_report['cold_controls']): return 1
        host = common.run([env.get('EMACS', 'emacs'), '-Q', '--batch', '-L', str(ROOT / 'lisp'), '--eval', common.fixture_expression('F1_SOURCE')], env, directory, 'host', deadline=60)
        path_report['host_fixture'] = host; save()
        if host['rc'] or not source.with_suffix('.elc').is_file(): return 1
        path_report['fixture_sha256'] = common.digest(source.with_suffix('.elc'))
        def execute(phase, driver, extra, markers=None):
            row = common.run(command(binary, cold, driver, qemu), dict(env, **extra), directory, phase)
            output = (directory / (phase + '.out')).read_text(errors='replace')
            errors = (directory / (phase + '.err')).read_text(errors='replace')
            ok = (row['rc'] == 0 and not errors and all(output.splitlines().count(m) == 1 for m in markers)) if markers is not None else phase_passed(phase, row, output, errors)
            row.update(phase=phase, passed=ok, driver_sha256=common.digest(ROOT / driver))
            if phase in ('compile', 'load'):
                row['target_page_bytes'] = [int(v) for v in re.findall(r'^ARM64-PAGE-BYTES=(\d+)$', output, re.M)]
                row['native_entries'] = [int(v) for v in re.findall(r'^ARM64-NATIVE-ENTRIES=(\d+)$', output, re.M)]
                row['validations'] = [int(v) for v in re.findall(r'^F1-(?:COMPILE|CACHE)-PASS .*?validations=(\d+)', output, re.M)]
            rows.append(row); save()
            print(label + '/' + phase + ': ' + ('PASS' if ok else 'FAIL') + ' %.3fs' % row['seconds'], flush=True)
            if not ok: print((output + errors)[-6000:], flush=True)
            return ok
        for phase in ('compile', 'load'):
            if not execute(phase, 'test/standalone-arm64-native-f1-driver.el', dict(F1_PHASE=phase)): return 1
        producing_files = re.findall(r'^F1-COMPILE-PASS .*? file="([^"\n]+)"$',
                                     (directory / 'compile.out').read_text(), re.M)
        if len(producing_files) != 1: return 1
        path_report['producing_cache_file'] = producing_files[0]
        if not execute('abi', 'test/standalone-arm64-native-abi-driver.el', {}, ['F1-ROOTS-PASS cases=6', 'ARM64-TICKET-REFUSED N=0 N=6 executions=0', 'ARM64-REGISTER-SENTINEL-PASS N=0 N=6 GP=11 D=8 SP=16']): return 1
        if not execute('refusals', 'test/standalone-arm64-native-refusals-driver.el',
                       dict(F1_PRODUCER_FILE=producing_files[0]), ['ARM64-REFUSALS-PASS cases=9 maps=0']): return 1
        exits = directory / 'exits.el'
        exits.write_text(';;; -*- lexical-binding: t; -*-\n(defun f1b-one (f x) (f1b-tick) (funcall f x))\n(defun f1b-zero (f) (funcall f))\n(defun f1b-six (f x) (funcall f x x x x x x))\n')
        host = common.run([env.get('EMACS', 'emacs'), '-Q', '--batch', '-L', str(ROOT / 'lisp'), '--eval', common.fixture_expression('F1B_SOURCE')], dict(env, F1B_SOURCE=str(exits)), directory, 'exit-host', deadline=60)
        path_report['exit_fixture'] = host; save()
        if host['rc']: return 1
        extra = dict(F1B_FIXTURE=str(exits.with_suffix('.elc')), F1B_BACKEND='in-house', F1B_COLD='1' if cold else '0', NELISP_NATIVE_CACHE=str(cache_root / 'exit-cache'))
        for unit in ('f1b-one', 'f1b-zero', 'f1b-six'):
            if not execute('exit-' + unit, 'test/standalone-native-funcall-v2-exits-driver.el', dict(extra, F1B_PHASE='compile', F1B_COMPILE_UNIT=unit), ['F1B-COMPILE-PASS units=1 unit=' + unit]): return 1
        if not execute('exit-load', 'test/standalone-native-funcall-v2-exits-driver.el', dict(extra, F1B_PHASE='load'), ['F1B-CORPUS-DIGEST=ebe5cd1249025f15a6245e00f493cf7a5a9f1765bb4e07a5295836ad5a5b67d0', 'F1B-LOAD-PASS backend=in-house cleanup=24']): return 1
        if args.cache_parent:
            import shutil
            shutil.copytree(cache_root, directory / 'cache-snapshot')
        path_report['passed'] = True; save()
    report['passed'] = True
    report['hardware_qualified'] = qemu is None
    save(); print('ARM64-F1-PASS evidence=' + str(work)); return 0

if __name__ == '__main__': raise SystemExit(main())
