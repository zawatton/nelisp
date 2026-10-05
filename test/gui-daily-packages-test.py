#!/usr/bin/env python3
"""Negative controls for S5.2 evidence, independent of a live runtime."""
import importlib.util
import gzip
import os
import json
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
                      dict(messages='Args out of range: #("m" 0 1 (dired-filename t)), 2, nil'),
                      dict(messages='Key sequence g d starts with non-prefix key g')]:
            with self.subTest(patch=patch), self.assertRaises(AssertionError):
                gate.validate(dict(good, **patch),'dired-mode',['alpha.txt','beta.txt'])

    def test_exact_gnu_preloads_and_missing_source_control(self):
        gnu = Path(subprocess.check_output(
            ['emacs','-Q','--batch','--eval','(princ lisp-directory)'], text=True).strip())
        source = gnu/'international/mule-cmds.el'
        raw = source.read_bytes() if source.exists() else gzip.decompress(
            source.with_suffix('.el.gz').read_bytes())
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root/'international').mkdir()
            copy = root/'international/mule-cmds.el'
            copy.write_bytes(raw)
            output = root/'preloaded.el'
            evidence = gate.prepare_preloads(root,output,dict(os.environ))
            self.assertIn('coding-system-change-eol-conversion',evidence['names'])
            # Loading the extracted file must install the stock variable and
            # real GNU function. Unrelated mule-cmds initialization stays out.
            output.write_text(output.read_text().replace('etags-program-name','s52c-test-etags-program-name'))
            expression = '(progn (fmakunbound (quote coding-system-change-eol-conversion)) (load '+json.dumps(str(output))+' nil t t) (prin1 (list s52c-test-etags-program-name (coding-system-change-eol-conversion (quote utf-8) (quote unix)))))'
            result = subprocess.check_output(['emacs','-Q','--batch','--eval',expression],text=True)
            self.assertEqual(result,'("etags" utf-8-unix)')
            copy.write_text('(error "No requested GNU definition")')
            with self.assertRaises(subprocess.CalledProcessError):
                gate.prepare_preloads(root,output,dict(os.environ))

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

    def test_shorthands_match_gnu_load_and_preserve_plain_sources(self):
        source = ''';;; Reader fixture. -*- lexical-binding: t; -*-
;; Copyright: retained source header.
(defmacro long--and$ (value body) `(let ((it ,value)) ,body))
(defun long--reader-test ()
  (and$ 7 (list it 'short-value '#_short-value "short-value"
                '[short-value "short-value"])))
;; Local Variables:
;; read-symbol-shorthands: (("and$" . "long--and$") ("short-" . "long--"))
;; End:
'''
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            vendor = root/'vendor'
            vendor.mkdir()
            original = root/'original.el'
            expanded = vendor/'expanded.el'
            plain = vendor/'plain.el'
            original.write_text(source)
            expanded.write_text(source)
            plain.write_text('(defvar plain "read-symbol-shorthands: only string data")\n')
            before = plain.read_bytes()
            def observe(path):
                expression = '(progn (load '+json.dumps(str(path))+' nil t t) (prin1 (long--reader-test)))'
                return subprocess.check_output(['emacs','-Q','--batch','--eval',expression],text=True)
            expected = observe(original)
            receipt = gate.prepare_shorthands(vendor,dict(os.environ))
            self.assertEqual(receipt['files'],1)
            self.assertEqual(observe(expanded),expected)
            self.assertEqual(expected,'(7 long--value short-value "short-value" [long--value "short-value"])')
            self.assertEqual(plain.read_bytes(),before)
            self.assertIn('Copyright: retained',expanded.read_text())
            provenance = json.loads(Path(receipt['manifest']).read_text())[0]
            self.assertNotEqual(provenance['original_sha256'],provenance['expanded_sha256'])
            # A malformed form must fail before publishing an expanded copy.
            expanded.write_text(source.replace('(defmacro','(\n(defmacro',1))
            broken = expanded.read_bytes()
            with self.assertRaises(subprocess.CalledProcessError):
                gate.prepare_shorthands(vendor,dict(os.environ))
            self.assertEqual(expanded.read_bytes(),broken)

    def test_header_shorthand_is_applied_once(self):
        source = ''';;; -*- lexical-binding: t; read-symbol-shorthands: (("s-" . "s-long-")); -*-
(defun s-probe () 's-value)
'''
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            vendor = root/'vendor'
            vendor.mkdir()
            path = vendor/'header.el'
            path.write_text(source)
            receipt = gate.prepare_shorthands(vendor,dict(os.environ))
            self.assertEqual(receipt['files'],1)
            expression = '(progn (load '+json.dumps(str(path))+' nil t t) (prin1 (list (s-long-probe) (fboundp (quote s-long-long-probe)))))'
            result = subprocess.check_output(['emacs','-Q','--batch','--eval',expression],text=True)
            self.assertEqual(result,'(s-long-value nil)')


if __name__ == '__main__':
    unittest.main()
