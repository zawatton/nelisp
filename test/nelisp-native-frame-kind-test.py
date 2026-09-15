"""Exercise actual native frame primitives in the private reader fixture.

Build with test/nelisp-native-frame-kind-build.el first.  Reuse that executable
through NELISP_FRAME_TEST_BIN for iteration without rebuilding the reader.
"""
import os
from pathlib import Path
import subprocess
import unittest

ROOT = Path(__file__).resolve().parents[1]
SETUP = '''
(let ((cell (frame-test-cell 7))) (unless cell (error "fixture cell missing")) (unless (= (frame-test-value cell) 7) (error "fixture cell ABI")))
(load "lisp/nelisp-stdlib-fast-hash.el")
(load "lisp/nelisp-lexframe.el")
(setq fixture-stack (nelisp-lexframe-stack-make))
(setq fixture-outer (nelisp--make-record 'nelisp-lexframe (nelisp--fast-hash-make 16)))
(setq fixture-inner (nelisp-lexframe-make))
(nelisp-lexframe-bind fixture-outer "x" (frame-test-cell 7))
(nelisp-lexframe-bind fixture-outer "z" (frame-test-cell 5))
(nelisp-lexframe-bind fixture-outer "nil-value" (frame-test-cell nil))
(nelisp-lexframe-bind fixture-inner "x" (frame-test-cell 9) t)
(nelisp-lexframe-bind fixture-inner "dynamic-only" (frame-test-cell 11) t)
(nelisp-lexframe-stack-push! fixture-stack fixture-outer)
(nelisp-lexframe-stack-push! fixture-stack fixture-inner)
'''


class FrameKinds(unittest.TestCase):
    def run_cases(self, cases):
        binary = Path(os.environ.get('NELISP_FRAME_TEST_BIN',
                                      str(ROOT/'target/nelisp-native-frame-kinds'))).resolve()
        self.assertTrue(binary.is_file(), 'Build the private native frame fixture first')
        source = SETUP + '\n'.join(f'(progn (prin1 {form}) (terpri))' for form, _ in cases)
        run = subprocess.run([str(binary), '--repl', '--no-prompt', '--no-print'],
                             input=source+'\n(exit 0)\n', cwd=ROOT,
                             capture_output=True, text=True, timeout=20)
        self.assertEqual((run.returncode, run.stderr), (0, ''))
        self.assertEqual(run.stdout.splitlines(), [expected for _, expected in cases])
        print(f'NATIVE-FRAME checked={len(cases)} findings=0')

    def test_capture(self):
        self.run_cases([
            ('(let ((cap (frame-test-capture (vector fixture-stack 2 nil nil 0)))) '
             '(list (length cap) (frame-test-value (cdr (assoc "x" cap))) '
             '(frame-test-value (cdr (assoc "z" cap))) (null (assoc "dynamic-only" cap)) '
             '(null (frame-test-value (cdr (assoc "nil-value" cap))))))', '(3 7 5 t t)'),
            ("(let ((cap (frame-test-capture (vector fixture-stack 2 nil '(x dynamic-only missing) 1)))) "
             "(list (length cap) (frame-test-value (cdr (assq 'x cap)))))", '(1 7)'),
            ('(let ((cap (frame-test-capture (vector fixture-stack 1 nil nil 0)))) '
             '(list (length cap) (frame-test-value (cdr (assoc "x" cap)))) )', '(3 7)'),
            ('(frame-test-capture (vector fixture-stack 0 nil nil 0))', 'nil'),
        ])

    def test_kind_search_and_unwind(self):
        self.run_cases([
            ('(frame-test-find fixture-stack "x" 0)', '7'),
            ('(frame-test-find fixture-stack "x" 1)', '9'),
            ('(frame-test-find fixture-stack "dynamic-only" 0)', 'nil'),
            ('(frame-test-find fixture-stack "dynamic-only" 1)', '11'),
            ('(frame-test-find fixture-stack "missing" 0)', 'nil'),
            ('(frame-test-find fixture-stack 1 0)', 'nil'),
            ('(progn (garbage-collect) (frame-test-find fixture-stack "x" 1))', '9'),
            ('(progn (condition-case nil (unwind-protect (error "unwind") '
             '(nelisp-lexframe-stack-pop! fixture-stack)) (error nil)) '
             '(list (frame-test-find fixture-stack "x" 0) '
             '(frame-test-find fixture-stack "x" 1)))', '(7 nil)'),
        ])

    def test_call_scope_boundary(self):
        self.run_cases([
            ('(progn (nelisp-lexframe-mark-scope! fixture-inner) '
             '(list (frame-test-find fixture-stack "x" 0) '
             '(frame-test-find fixture-stack "x" 1)))', '(nil 9)'),
            ('(frame-test-capture (vector fixture-stack 2 nil nil 0))', 'nil'),
            ("(frame-test-capture (vector fixture-stack 2 nil '(x z) 1))", 'nil'),
            ('(progn (nelisp-lexframe-bind fixture-inner "own" (frame-test-cell 3)) '
             '(garbage-collect) '
             '(frame-test-value (cdr (assoc "own" '
             '(frame-test-capture (vector fixture-stack 2 nil nil 0))))))', '3'),
            ('(progn (nelisp-lexframe-stack-pop! fixture-stack) '
             '(frame-test-find fixture-stack "x" 0))', '7'),
        ])

    def test_local_declaration_metadata(self):
        self.run_cases([
            ('(progn (setq declaration-stack (nelisp-lexframe-stack-make)) '
             '(setq declaration-root (nelisp-lexframe-make)) '
             '(nelisp-lexframe-mark-scope! declaration-root) '
             '(nelisp-lexframe-stack-push! declaration-stack declaration-root) '
             '(nelisp-lexframe-stack-push! declaration-stack (nelisp-lexframe-make)) '
             "(frame-test-declare declaration-stack 'declared))", '0'),
            ('(nelisp-lexframe-local-declarations declaration-root)', '("declared")'),
            ("(frame-test-local-special declaration-stack 'declared)", 't'),
            ('(frame-test-capture (vector declaration-stack 2 nil nil 0))', '(declared)'),
            ("(frame-test-capture (vector declaration-stack 2 nil '(unrelated) 1))", '(declared)'),
            ('(progn (garbage-collect) '
             '(frame-test-find declaration-stack "declared" 1))', 'nil'),
            ('(progn (nelisp-lexframe-stack-pop! declaration-stack) '
             '(setq declaration-callee (nelisp-lexframe-make)) '
             '(nelisp-lexframe-mark-scope! declaration-callee) '
             '(nelisp-lexframe-stack-push! declaration-stack declaration-callee) '
             "(frame-test-local-special declaration-stack 'declared))", 'nil'),
        ])


if __name__ == '__main__':
    unittest.main()
