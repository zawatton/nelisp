#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
"""Negative controls for the Windows acceptance receipt and process deadline."""
import importlib.util
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest
from unittest.mock import patch

spec = importlib.util.spec_from_file_location('windows_f1', Path(__file__).with_name('run-windows-native-f1.py'))
runner = importlib.util.module_from_spec(spec)
spec.loader.exec_module(runner)


class WineControls(unittest.TestCase):
    def tearDown(self):
        runner.wine_path.cache_clear()

    def test_drive_paths_are_reader_only_and_cached(self):
        host = dict(F1_SOURCE='/fixture with spaces/source.el', F1_FIXTURE='/fixture with spaces/source.elc',
                    NELISP_NATIVE_CACHE='/isolated/cache', F1_PHASE='compile', F1_FORCE_GC='1')
        original = dict(host)
        def convert(command, **options):
            self.assertEqual(command[:2], ['winepath', '-w'])
            self.assertLessEqual(options['timeout'], 600)
            return subprocess.CompletedProcess(command, 0,
                                               'Q:\\' + command[2].lstrip('/').replace('/', '\\') + '\n', '')
        with patch.object(runner.subprocess, 'run', side_effect=convert) as calls:
            env = runner.reader_environment(host, wine=True)
            command = runner.reader_command(Path('/reader.exe'), Path('/reader.exe.cold'),
                                            'test/standalone-bytecode-native-funcall-driver.el', wine=True)
            count = calls.call_count
            self.assertEqual(runner.reader_environment(host, wine=True), env)
            self.assertEqual(calls.call_count, count)
        self.assertEqual(host, original)
        self.assertEqual(runner.reader_environment(host), original)
        self.assertEqual(env['F1_FIXTURE'], 'Q:\\fixture with spaces\\source.elc')
        self.assertEqual(env['F1_FORCE_GC'], '1')
        self.assertEqual(command[:4], ['wine', '/reader.exe', '--cold-load-from', 'Q:\\reader.exe.cold'])
        self.assertTrue(all(argument.startswith('Q:\\') for argument in command[5::2]))

    def test_path_conversion_refuses_bad_output(self):
        for stdout, stderr in (('/posix/path\n', ''), ('C:\\path\nC:\\extra\n', ''),
                               ('C:\\path\n', 'unexpected error')):
            runner.wine_path.cache_clear()
            with patch.object(runner.subprocess, 'run',
                              return_value=subprocess.CompletedProcess([], 0, stdout, stderr)):
                with self.assertRaises(ValueError):
                    runner.wine_path('/fixture')
        runner.wine_path.cache_clear()
        with patch.object(runner.subprocess, 'run', side_effect=subprocess.CalledProcessError(1, 'winepath')):
            with self.assertRaises(subprocess.CalledProcessError):
                runner.wine_path('/fixture')

    def test_real_host_fixture_refuses_unpinned_dialect(self):
        with tempfile.TemporaryDirectory() as directory:
            source = Path(directory) / 'fixture with spaces.el'
            source.write_text(';;; -*- lexical-binding: t; -*-\n(defun wine-fixture (x) (cons x x))\n')
            env = dict(os.environ, F1_SOURCE=str(source))
            command = [env.get('EMACS', 'emacs'), '-Q', '--batch', '-L', str(runner.ROOT / 'lisp'), '--eval']
            bad = subprocess.run(command + ['(setq emacs-version "30.2")', '--eval', runner.fixture_expression('F1_SOURCE')],
                                 env=env, capture_output=True, timeout=60)
            self.assertNotEqual(bad.returncode, 0)
            self.assertFalse(source.with_suffix('.elc').exists())
            good = subprocess.run(command + [runner.fixture_expression('F1_SOURCE')],
                                  env=env, capture_output=True, timeout=60)
            self.assertEqual(good.returncode, 0, good.stderr.decode(errors='replace'))
            self.assertTrue(source.with_suffix('.elc').is_file())


class CheckoutControls(unittest.TestCase):
    def test_autocrlf_preserves_pinned_inventory(self):
        """Exercise Git's Windows checkout conversion against the real attributes."""
        relative = Path('test/fixtures/native-bytecode/gnu-31.1-opcodes.json')
        expected = (runner.ROOT / relative).read_bytes()
        # A checkout already converted by the old attributes is itself invalid.
        self.assertEqual(runner.digest(runner.ROOT / relative),
                         '147da590c9f5bdcf190b5b410c6af878c793ac89e07eafa4ae9f05a9b7aa7bcb')
        with tempfile.TemporaryDirectory() as directory:
            repository = Path(directory) / 'repository'
            checkout = Path(directory) / 'checkout'
            repository.mkdir()
            checkout.mkdir()
            def git(*args):
                return subprocess.run(['git', '-C', str(repository), *args],
                                      check=True, capture_output=True, timeout=30)
            git('init', '--quiet')
            git('config', 'core.autocrlf', 'true')
            (repository / '.gitattributes').write_bytes((runner.ROOT / '.gitattributes').read_bytes())
            fixture = repository / relative
            fixture.parent.mkdir(parents=True)
            fixture.write_bytes(expected)
            git('add', '.gitattributes', relative.as_posix())
            git('checkout-index', '--all', '--force', '--prefix=' + checkout.as_posix() + '/')
            self.assertEqual((checkout / relative).read_bytes(), expected)


class ReceiptControls(unittest.TestCase):
    def test_completion_and_failures(self):
        compile_output = 'F1-COMPILE-PASS backend=in-house seconds=1.000 validations=1 file="fixture"\n'
        load_output = ('F1-CORPUS-DIGEST=' + runner.DIGEST + '\n'
                       'F1-FORCED-GC-PASS backend=in-house\n'
                       'F1-CACHE-PASS backend=in-house corpus=5 seconds=1.000 validations=0\n')
        good = dict(rc=0, seconds=1)
        for phase, output in (('compile', compile_output), ('load', load_output)):
            self.assertTrue(runner.phase_passed(phase, good, output, ''))
            for bad in (dict(rc=1, seconds=1), dict(rc=0, seconds=300), dict(rc=124, seconds=1)):
                self.assertFalse(runner.phase_passed(phase, bad, output, ''))
            for bad in ('', output + output, output.replace('in-house', 'template'),
                        output.replace('validations=', 'missing='),
                        output.replace('validations=', 'validations=2 validations=')):
                self.assertFalse(runner.phase_passed(phase, good, bad, ''))
            self.assertFalse(runner.phase_passed(phase, good, output, 'unexpected stderr'))
        self.assertFalse(runner.phase_passed('compile', good, compile_output.replace('validations=1', 'validations=0'), ''))
        for bad in (load_output.replace(runner.DIGEST, '0' * 64),
                    load_output.replace('corpus=5', 'corpus=0'),
                    load_output.replace('validations=0', 'validations=1'),
                    load_output.replace('F1-FORCED-GC-PASS', 'GC-SKIPPED')):
            self.assertFalse(runner.phase_passed('load', good, bad, ''))

    def test_deadline_kills_child(self):
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            receipt = runner.run([sys.executable, '-c', 'import time; time.sleep(30)'],
                                 dict(os.environ), work, 'deadline', deadline=0.1)
            self.assertEqual(receipt['rc'], 124)
            self.assertLess(receipt['seconds'], 15)
            self.assertTrue((work / 'deadline.out').is_file())


if __name__ == '__main__':
    unittest.main()
