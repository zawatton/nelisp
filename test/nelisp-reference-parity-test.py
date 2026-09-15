"""Process-level controls for the explicit-reader comparison command."""
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]


@unittest.skipUnless(os.name == 'posix', 'POSIX executable fixtures')
class ReferenceParity(unittest.TestCase):
    def run_comparison(self, native_out='same', native_err='', native_code=0,
                       reference_out='same', version='GNU Emacs 30.1'):
        with tempfile.TemporaryDirectory(prefix='nelisp parity 日本語 ') as directory:
            root = Path(directory)
            reference, native = root / 'reference', root / 'reader'
            reference.write_text(
                '#!/usr/bin/env python3\nimport sys\n'
                f'sys.stdout.write({version!r} if "--version" in sys.argv else {reference_out!r})\n')
            native.write_text(
                '#!/usr/bin/env python3\nimport sys\n'
                f'sys.stdout.write({native_out!r})\n'
                f'sys.stderr.write({native_err!r})\n'
                f'sys.exit({native_code!r})\n')
            for executable in (reference, native):
                executable.chmod(0o755)
            cases = root / 'cases.el'
            cases.write_text('(list 1)\n')
            prefix = root / 'evidence'
            result = subprocess.run(
                [sys.executable, str(ROOT / 'tools/nelisp-reference-parity.py'),
                 '--binary', str(native), '--emacs', str(reference),
                 '--cases', str(cases), '--prefix', str(prefix)],
                capture_output=True, text=True, timeout=15)
            report = json.loads(prefix.with_suffix('.json').read_text())
            return result, report

    def test_complete_match(self):
        result, report = self.run_comparison()
        self.assertEqual((result.returncode, result.stderr), (0, ''))
        self.assertTrue(report['passed'])
        self.assertEqual(report['bytes'], 4)
        self.assertEqual(len(report['binary_sha256']), 64)

    def test_rejects_false_positive_comparisons(self):
        # A prefix-only comparison that suppresses stderr/exit status would
        # accept the first three controls despite unsuccessful execution.
        for options in ({'native_err': 'uncaught error'},
                        {'native_code': 7},
                        {'native_out': 'same trailing garbage'},
                        {'reference_out': '', 'native_out': ''},
                        {'version': 'GNU Emacs 31.1'},
                        {'version': ''}):
            with self.subTest(options=options):
                result, report = self.run_comparison(**options)
                self.assertEqual(result.returncode, 1, result.stderr)
                self.assertFalse(report['passed'])
                self.assertIn('error', report)


if __name__ == '__main__':
    result = unittest.main(exit=False).result
    findings = len(result.failures) + len(result.errors)
    print(f'GATE-COUNT checked={result.testsRun} findings={findings}')
    raise SystemExit(not result.wasSuccessful())
