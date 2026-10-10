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

spec = importlib.util.spec_from_file_location('windows_f1', Path(__file__).with_name('run-windows-native-f1.py'))
runner = importlib.util.module_from_spec(spec)
spec.loader.exec_module(runner)


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
