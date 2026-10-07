#!/usr/bin/env python3
"""Negative controls for the S5.1 oracle evidence comparison."""
import copy
import importlib.util
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location('scenario', ROOT/'scripts/gui-daily-scenario.py')
scenario = importlib.util.module_from_spec(spec); spec.loader.exec_module(scenario)


class EvidenceTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.output = tempfile.TemporaryDirectory(prefix='s51-oracle-test-')
        proc = subprocess.run(['xvfb-run', '-a', '-s',
                        '-screen 0 1600x1000x24 -dpi 96 -nolisten tcp -extension GLX',
                        sys.executable, str(ROOT/'scripts/gui-daily-gate.py'), 'S5.1',
                        '--init=-Q', '--fixture=daily', '--compare=gnu', '--one-display',
                        '--engine=gnu', '--out', cls.output.name],
                       env=dict(os.environ, EMACS=os.environ.get('EMACS','emacs')),
                       timeout=60, capture_output=True, text=True)
        assert proc.returncode == 0, proc.stdout + proc.stderr
        cls.oracle = json.loads((Path(cls.output.name)/'result.json').read_text())

    @classmethod
    def tearDownClass(cls):
        cls.output.cleanup()

    def setUp(self):
        # Real GNU observation, rather than a mirror of comparison code.
        oracle = copy.deepcopy(self.oracle)
        self.cases = {'gnu':oracle, 'nelisp':copy.deepcopy(oracle)}
        self.cases['nelisp']['sessions'][0].update(
            display=':251', command=[str(ROOT/'bin/nemacs-xcb'), '--init=-Q'])

    def test_equal_complete_evidence(self):
        scenario.compare(self.cases)

    def test_reject_corrupt_evidence(self):
        changes = [
            lambda c:c['saved_bytes'].append(0),
            lambda c:c['milestones']['drag'].update(point=1),
            lambda c:c['milestones']['drag'].update(mark=1),
            lambda c:c['milestones']['drag'].update(active=0),
            lambda c:c['milestones']['drag'].update(region='wrong'),
            lambda c:c['milestones']['open'].update(buffer='wrong'),
            lambda c:c['milestones']['other-window'].update(selected=0),
            lambda c:c['milestones']['split']['windows'].pop(),
            lambda c:c['milestones']['copy'].update(clipboard='wrong'),
            lambda c:c['milestones'].pop('save'),
            lambda c:c['pixels'].pop('region'),
            lambda c:c['sessions'][0].update(rc=1),
            lambda c:c.update(production_quit=False),
            lambda c:c['sessions'][0]['command'].append('--fixture=render'),
            lambda c:c.update(clipboard_import='wrong'),
        ]
        for change in changes:
            with self.subTest(change=change):
                cases=copy.deepcopy(self.cases); change(cases['nelisp'])
                with self.assertRaises(AssertionError): scenario.compare(cases)

    def test_reject_blank_and_missing_text(self):
        class Screen:
            label = 'nelisp'
            def shot(self, name): return name
        width, height = 960, 700
        def check(raw):
            def command(argv):
                return b'960 700' if argv[0]=='identify' else raw
            with self.assertRaises(AssertionError):
                scenario.pixels(Screen(), self.oracle['milestones']['open'], 'negative',
                                {'command':command}, {}, scenario.FIXTURE)
        check(bytes([24,32,40]) * width * height)
        # A colorful strip outside text cells must not satisfy the ink checks.
        raw = bytearray(bytes([24,32,40]) * width * height)
        for x in range(256):
            i=((height-1)*width+x)*3; raw[i:i+3]=bytes([x,0,0])
        # Semantic milestones deliberately omit geometry; use actual oracle
        # observer geometry for this screenshot-specific negative control.
        rows=[json.loads(line) for line in (Path(self.output.name)/'state.jsonl').read_text().splitlines()]
        opened=next(row for row in rows if row['buffer']=='daily.txt' and row['text']==scenario.FIXTURE)
        def command(argv): return b'960 700' if argv[0]=='identify' else raw
        with self.assertRaises(AssertionError):
            scenario.pixels(Screen(), opened, 'negative', {'command':command}, {}, scenario.FIXTURE)


if __name__ == '__main__': unittest.main()
