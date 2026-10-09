#!/usr/bin/env python3
"""GUI image bundle construction, independent of a live runtime."""
import importlib.util
from pathlib import Path
import subprocess
import tempfile
import unittest
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location('gui_daily_build', ROOT/'scripts/gui-daily-build.py')
build = importlib.util.module_from_spec(spec)
spec.loader.exec_module(build)

BUNDLE = b''';;; generated bundle -*- lexical-binding: t; -*-
;;; >>> first.el
(defvar gdb-test-table)
(progn (defvar gdb-test-result))
(defun gdb-test-read () (list gdb-test-table gdb-test-result))
(defun gdb-test-bind ()
  (let ((gdb-test-table 42) (gdb-test-result 17)) (gdb-test-read)))
;;; <<< first.el
;;; >>> second.el
(defun gdb-test-capture ()
  (let ((gdb-test-table 'lexical)) (lambda () gdb-test-table)))
;;; <<< second.el
'''


class IsolateLexicalForms(unittest.TestCase):
    def test_file_local_defvar_scope_matches_gnu_load(self):
        """A member's `(defvar SYM)' reaches its later forms, never the next member."""
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root/'build').mkdir()
            with patch.object(build, 'ROOT', root):
                build.isolate_lexical_forms(BUNDLE)
            target = root/'build/gui-daily-lexical.el'
            result = subprocess.check_output(
                ['emacs', '-Q', '--batch', '--load', str(target), '--eval',
                 '(prin1 (list (gdb-test-bind) (funcall (gdb-test-capture))'
                 ' (boundp (quote gdb-test-table))))'], text=True)
            self.assertEqual(result, '((42 17) lexical nil)')


if __name__ == '__main__':
    unittest.main()
