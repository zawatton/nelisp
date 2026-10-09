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
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location('packages_gate', ROOT/'scripts/gui-daily-packages.py')
gate = importlib.util.module_from_spec(spec)
spec.loader.exec_module(gate)


class Evidence(unittest.TestCase):
    def test_reused_fixture_never_invokes_git(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)/'fixture'
            for relative in ('load-path.json', 'sources.json', 'vendor/gnu-preloaded.el',
                             'agenda.org', 'tree/alpha.txt', 'repo/.git/HEAD'):
                path = root/relative
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_text('fixture')
            (root/'load-path.json').write_text(json.dumps([str(root/'vendor')]))
            env = dict(os.environ, NELISP_GUI_PACKAGES_REUSE_FIXTURE=str(root))
            with patch.object(gate.subprocess, 'check_output', side_effect=AssertionError('Git forbidden')):
                actual, prepared, receipt = gate.prepare(Path(directory)/'out', env)
            self.assertEqual(actual, root.resolve())
            self.assertFalse(receipt['git_writes'])
            self.assertEqual(prepared['GIT_OPTIONAL_LOCKS'], '0')
            self.assertEqual(prepared['GIT_CEILING_DIRECTORIES'], str(root.resolve()))
            (root/'repo/.git/HEAD').unlink()
            with self.assertRaises(RuntimeError):
                gate.prepare(Path(directory)/'out', env)

    def test_production_quit_and_negative_controls(self):
        good = dict(rc=0, test_exit_group=False, fault=None,
                    events=[['n'], ['ctrl+x', 'ctrl+c']])
        keys = ('GUI-KEY|code=53|event=24|group=0|\n'
                'GUI-KEY|code=54|event=3|group=0|\n')
        # Ordinary production exit has no GUI-CLOSED test teardown marker.
        gate.validate_production_quit(good, keys)
        for changed, log in [
                (dict(rc=1), keys), (dict(rc=None), keys),
                (dict(test_exit_group=True), keys),
                (dict(fault='quit'), keys), (dict(events=[['f12']]), keys),
                ({}, keys.replace('|event=24|', '|event=25|')),
                ({}, keys.replace('|event=3|', '|event=4|')),
                ({}, keys+'GUI-ERROR|condition=(void-function missing)\n'),
                ({}, keys+'Lisp error: (void-variable missing)\n')]:
            with self.subTest(changed=changed, log=log), self.assertRaises(AssertionError):
                gate.validate_production_quit(dict(good, **changed), log)

    def test_wrong_mode_name_text_and_lisp_errors(self):
        good = dict(mode='dired-mode', buffer='tree', window_buffer='tree',
                    text='alpha.txt\nbeta.txt\n',
                    messages='nemacs 0.1.0-mvp ready (Layer 2 / Doc 51)\n')
        gate.validate(good, 'dired-mode', ['alpha.txt','beta.txt'])
        for patch in [dict(mode='fundamental-mode'), dict(window_buffer='*scratch*'),
                      dict(buffer=''),dict(text=''),dict(text='alpha.txt'),
                      dict(messages='Lisp error: (void-function foo)'),
                      dict(messages='Symbol’s function definition is void: forward-word-strictly'),
                      dict(messages="Symbol's value is void: missing-variable"),
                      dict(messages="Symbol’s value as variable is void: auto-window-vscroll"),
                      dict(messages='Wrong type argument: stringp, [24]'),
                      dict(messages='Args out of range: #("m" 0 1 (dired-filename t)), 2, nil'),
                      dict(messages=good['messages']+'Args out of range: #("o" 0 1 (dired-filename t)), 2, nil\n'),
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
            tab = gnu/'tab-bar.el'
            (root/'tab-bar.el').write_bytes(tab.read_bytes() if tab.exists() else
                                          gzip.decompress(tab.with_suffix('.el.gz').read_bytes()))
            window = gnu/'window.el'
            (root/'window.el').write_bytes(window.read_bytes() if window.exists() else
                                          gzip.decompress(window.with_suffix('.el.gz').read_bytes()))
            bindings = gnu/'emacs-lisp/cl-macs.el'
            (root/'emacs-lisp').mkdir()
            (root/'emacs-lisp/cl-macs.el').write_bytes(
                bindings.read_bytes() if bindings.exists() else
                gzip.decompress(bindings.with_suffix('.el.gz').read_bytes()))
            output = root/'preloaded.el'
            evidence = gate.prepare_preloads(root,output,dict(os.environ))
            self.assertIn('coding-system-change-eol-conversion',evidence['names'])
            # Loading the extracted file must install the stock variable and
            # real GNU function. Unrelated mule-cmds initialization stays out.
            output.write_text(output.read_text().replace('etags-program-name','s52c-test-etags-program-name'))
            expression = '(progn (mapc (quote fmakunbound) (quote (coding-system-change-eol-conversion window-full-width-p window-full-height-p window-normalize-window))) (load '+json.dumps(str(output))+' nil t t) (prin1 (list s52c-test-etags-program-name (coding-system-change-eol-conversion (quote utf-8) (quote unix)) (window-full-width-p) (window-full-height-p))))'
            result = subprocess.check_output(['emacs','-Q','--batch','--eval',expression],text=True)
            self.assertEqual(result,'("etags" utf-8-unix t t)')
            # A core defvar alone does not supply Custom's standard expression.
            # Tramp must see GNU's expression, following the live environment
            # while leaving configured values and existing metadata intact.
            expression = '''(let ((stock (get 'temporary-file-directory 'standard-value))
                                  (temporary-file-directory "/configured-current/"))
              (put 'temporary-file-directory 'standard-value nil)
              (load OUTPUT nil t t)
              (unless (equal stock (get 'temporary-file-directory 'standard-value))
                (error "GNU standard expression was not restored exactly"))
              (unless (equal temporary-file-directory "/configured-current/")
                (error "Configured current directory changed"))
              (dolist (directory '("/live-default-a/" "/live-default-b/"))
                (setenv "TMPDIR" directory)
                (unless (equal directory (eval (car (get 'temporary-file-directory 'standard-value)) t))
                  (error "Standard expression froze the environment")))
              (put 'temporary-file-directory 'standard-value '("/existing-standard/"))
              (load OUTPUT nil t t)
              (unless (equal (get 'temporary-file-directory 'standard-value) '("/existing-standard/"))
                (error "Existing standard metadata was replaced"))
              (princ "metadata PASS"))'''.replace('OUTPUT', json.dumps(str(output)))
            result = subprocess.check_output(
                ['emacs','-Q','--batch','--eval',expression], text=True)
            self.assertEqual(result, 'metadata PASS')
            copy.write_text('(error "No requested GNU definition")')
            with self.assertRaises(subprocess.CalledProcessError):
                gate.prepare_preloads(root,output,dict(os.environ))
            copy.write_bytes(raw)
            (root/'window.el').write_text('(error "Missing genuine window functions")')
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
