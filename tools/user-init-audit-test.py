#!/usr/bin/env python3
"""Synthetic safety and continuation checks; never reads the user's init."""
import importlib.util
import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location('audit', ROOT / 'tools/user-init-audit.py')
audit = importlib.util.module_from_spec(spec)
spec.loader.exec_module(audit)


class AuditTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        (ROOT / 'build/user-init').mkdir(parents=True, exist_ok=True)

    def test_cap_is_failure_with_unobserved_forms(self):
        with tempfile.TemporaryDirectory(dir=ROOT / 'build/user-init') as name:
            root = Path(name)
            source = root / 'source'
            source.mkdir()
            (source / 'early-init.el').write_text('(while t)\n')
            (source / 'init.el').write_text('nil\n')
            output = root / 'output'
            result = subprocess.run([os.sys.executable, str(ROOT / 'tools/user-init-audit.py'),
                                     '--source', str(source), '--output', str(output), '--cap', '1'],
                                    capture_output=True, text=True, timeout=15)
            self.assertEqual(result.returncode, 1, result.stdout + result.stderr)
            summary = json.loads((output / 'summary.json').read_text())
            self.assertFalse(summary['complete'])
            self.assertEqual(summary['classifications'], {'unobserved': 2})
            self.assertTrue(summary['metrics']['host']['timeout'])
            self.assertTrue(summary['metrics']['nelisp']['timeout'])

    def test_relative_image_is_loaded_inside_fixture_home(self):
        image = Path(subprocess.check_output(
            ['bash', str(ROOT / 'tools/c-core-image.sh'), 'path'],
            cwd=ROOT, text=True).strip())
        with tempfile.TemporaryDirectory(dir=ROOT / 'build/user-init') as name:
            root = Path(name)
            source = root / 'source'
            source.mkdir()
            (source / 'early-init.el').write_text('nil\n')
            (source / 'init.el').write_text(
                "(when (fboundp 'rdf) (unless (boundp 'c-core-image--identity) "
                '(error "base heap used instead of explicit image")))\n')
            output = root / 'output'
            command = [os.sys.executable, str(ROOT / 'tools/user-init-audit.py'),
                       '--source', str(source), '--output', str(output), '--cap', '30',
                       '--image', os.path.relpath(image, ROOT)]
            result = subprocess.run(command, cwd=ROOT, capture_output=True,
                                    text=True, timeout=70)
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            summary = json.loads((output / 'summary.json').read_text())
            self.assertEqual(summary['classifications'], {'pass': 2})

    def test_filesystem_and_network_are_enforced_by_os(self):
        with tempfile.TemporaryDirectory(dir=ROOT / 'build/user-init') as name:
            root = Path(name)
            source = root / 'source'
            source.mkdir()
            for file in ('early-init.el', 'init.el'):
                (source / file).write_text('nil\n')
            home = root / 'home'
            audit.fixture(home, source)
            env = audit.environment(home, source)
            code = '''import os,socket
from pathlib import Path
blocked=[]
try: Path(os.environ['USER_INIT_SOURCE']+'/init.el').write_text('mutated')
except OSError: blocked.append('source')
try: socket.socket()
except OSError: blocked.append('network')
Path(os.environ['HOME']+'/state/allowed').write_text('fixture')
print(','.join(blocked))
'''
            result = subprocess.run(audit.sandbox([os.sys.executable, '-c', code], home, env),
                                    env=env, capture_output=True, text=True, timeout=10)
            self.assertEqual(result.returncode, 0)
            self.assertEqual(result.stdout.strip(), 'source,network')
            self.assertEqual((source / 'init.el').read_text(), 'nil\n')
            self.assertEqual((home / 'state/allowed').read_text(), 'fixture')

    def test_errors_continue_and_private_strings_are_redacted(self):
        with tempfile.TemporaryDirectory(dir=ROOT / 'build/user-init') as name:
            root = Path(name)
            source = root / 'source'
            source.mkdir()
            (source / 'early-init.el').write_text('(setq s13-audit-test-state 7)\n')
            secret = 'S13-SYNTHETIC-PRIVATE-STRING'
            (source / 'init.el').write_text(
                '; Unicode offset check: 日本語\n'
                '(if (fboundp \'rdf) (s13-audit-nelisp-only) t)\n'
                '(s13-audit-missing-function)\n'
                '(error "' + secret + '")\n'
                '(package-refresh-contents)\n'
                '(if (fboundp \'rdf) (error "native condition") (s13-audit-reference-only))\n'
                '(unless (= s13-audit-test-state 7) (error "lost state"))\n'
                '42\n')
            output = root / 'output'
            result = subprocess.run([os.sys.executable, str(ROOT / 'tools/user-init-audit.py'),
                                     '--source', str(source), '--output', str(output), '--cap', '30'],
                                    capture_output=True, text=True, timeout=70)
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            summary = json.loads((output / 'summary.json').read_text())
            self.assertEqual(summary['forms_total'], 8)
            self.assertEqual(summary['classifications'],
                             {'pass': 3, 'nelisp-only': 1, 'gnu-failure-excluded': 4})
            for path in output.iterdir():
                if path.is_file():
                    self.assertNotIn(secret, path.read_text())


if __name__ == '__main__':
    unittest.main()
