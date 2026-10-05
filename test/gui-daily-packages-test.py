#!/usr/bin/env python3
"""Negative controls for S5.2 evidence, independent of a live runtime."""
import importlib.util
from pathlib import Path
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location('packages_gate', ROOT/'scripts/gui-daily-packages.py')
gate = importlib.util.module_from_spec(spec)
spec.loader.exec_module(gate)


class Evidence(unittest.TestCase):
    def test_wrong_mode_name_text_and_lisp_errors(self):
        good = dict(mode='dired-mode', buffer='tree', window_buffer='tree',
                    text='alpha.txt\nbeta.txt\n', messages='')
        gate.validate(good, 'dired-mode', ['alpha.txt','beta.txt'])
        for patch in [dict(mode='fundamental-mode'), dict(window_buffer='*scratch*'),
                      dict(buffer=''),dict(text=''),dict(text='alpha.txt'),
                      dict(messages='Lisp error: (void-function foo)'),
                      dict(messages='Symbol’s function definition is void: forward-word-strictly'),
                      dict(messages="Symbol's value is void: missing-variable"),
                      dict(messages='Wrong type argument: stringp, [24]'),
                      dict(messages='Key sequence g d starts with non-prefix key g')]:
            with self.subTest(patch=patch), self.assertRaises(AssertionError):
                gate.validate(dict(good, **patch),'dired-mode',['alpha.txt','beta.txt'])

    def test_blank_and_two_tone_screenshots(self):
        api = dict(command=lambda argv: subprocess.check_output(argv), sha=lambda path: 'unused')
        with tempfile.TemporaryDirectory() as directory:
            for variant in ('blank','two-tone'):
                path = Path(directory)/(variant+'.png')
                argv=['convert','-size','960x672','xc:#182028']
                if variant=='two-tone':
                    argv += ['-fill','#e8e8e8','-draw','rectangle 0,0 100,100']
                subprocess.run(argv+[str(path)],check=True)
                with self.subTest(variant=variant), self.assertRaises(AssertionError):
                    gate.screenshot(path,api)


if __name__ == '__main__':
    unittest.main()
