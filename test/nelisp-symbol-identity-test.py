"""Public symbol identity contracts, including uninterned colon names.

Run with NELISP_BIN set to an isolated candidate and EMACS to the reference.
All cases are independent, so batching preserves the per-case contract.
"""
import os
from pathlib import Path
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
CASES = [
    ('captured-local-declaration',
     '(let* ((s (make-symbol "capture-local")) '
     '(f (eval `(let ((scope 1)) (defvar ,s) '
     '(lambda () (let ((,s 7)) (list ,s (boundp (quote ,s)))))) t))) '
     '(garbage-collect) '
     '(equal (list (funcall f) (special-variable-p s) (boundp s)) (quote ((7 t) nil nil))))'),
    ('public-name', '(equal (symbol-name (make-symbol "probe")) "probe")'),
    ('colon-name', '(equal (symbol-name (make-symbol ":probe")) ":probe")'),
    ('empty-name', '(equal (symbol-name (make-symbol "")) "")'),
    ('nil-name', '(let ((s (make-symbol "nil"))) (and (not (eq s nil)) (equal (symbol-name s) "nil")))'),
    ('not-keyword', '(not (keywordp (make-symbol ":probe")))'),
    ('not-bound', '(not (boundp (make-symbol ":probe")))'),
    ('void-value', '(let ((s (make-symbol ":probe"))) (condition-case nil (progn (symbol-value s) nil) (void-variable t)))'),
    ('not-special', '(not (special-variable-p (make-symbol ":probe")))'),
    ('can-unbind', '(let ((s (make-symbol ":probe"))) (condition-case nil (eq (makunbound s) s) (setting-constant nil)))'),
    ('soft-lookup', '(not (intern-soft (make-symbol "probe")))'),
    ('intern-distinct', '(let ((s (make-symbol "probe"))) (not (eq s (intern (symbol-name s)))))'),
    ('two-identities', '(not (eq (make-symbol "probe") (make-symbol "probe")))'),
    ('printed-name', '(let ((print-gensym nil)) (equal (prin1-to-string (make-symbol "probe")) "probe"))'),
    ('value-isolation', '(progn (set (intern "symbol-identity-value") 7) (let ((s (make-symbol "symbol-identity-value"))) (set s 9) (equal (list (symbol-value s) (symbol-value (intern "symbol-identity-value"))) (quote (9 7)))))'),
    ('lexical-identities', '(let ((a (make-symbol "probe")) (b (make-symbol "probe"))) (equal (eval (list (quote let) (list (list a 1) (list b 2)) (list (quote list) a b)) t) (quote (1 2))))'),
    ('eq-table-identities', '(let ((a (make-symbol "probe")) (b (make-symbol "probe")) (table (make-hash-table :test (quote eq)))) (puthash a 1 table) (puthash b 2 table) (equal (list (gethash a table) (gethash b table)) (quote (1 2))))'),
    ('equal-table-identities', '(let ((a (make-symbol "probe")) (b (make-symbol "probe")) (table (make-hash-table :test (quote equal)))) (puthash a 1 table) (puthash b 2 table) (equal (list (gethash a table) (gethash b table)) (quote (1 2))))'),
    ('closure-identities', '(let ((a (make-symbol "probe")) (b (make-symbol "probe"))) (let ((f (eval (list (quote let) (list (list a 1) (list b 2)) (list (quote function) (list (quote lambda) nil (list (quote list) a b)))) t))) (garbage-collect) (equal (funcall f) (quote (1 2)))))'),
    ('input-type', '(condition-case e (progn (make-symbol 1) nil) (wrong-type-argument (equal e (quote (wrong-type-argument stringp 1)))))'),
    ('unibyte-name', '(equal (symbol-name (make-symbol (unibyte-string 97 98))) "ab")'),
    ('function-cell', '(let ((s (make-symbol "fn"))) (fset s (lambda (x) (+ x 1))) (equal (list (fboundp s) (funcall s 2) (apply s (quote (3))) (eval (list s 4) t)) (quote (t 3 4 5))))'),
    ('property-isolation', '(let ((a (make-symbol "prop")) (b (make-symbol "prop"))) (put a (quote k) 7) (equal (list (get a (quote k)) (get b (quote k)) (symbol-plist a)) (quote (7 nil (k 7)))))'),
]


def source(cases, reference=False):
    forms = []
    for label, expression in cases:
        form = f'(list (quote {label}) {expression})'
        if reference:
            form = f'(eval (quote {form}) t)'
        forms.append(f'(progn (prin1 {form}) (terpri))')
    return '\n'.join(forms)


class SymbolIdentity(unittest.TestCase):
    def assert_contract(self, result):
        self.assertEqual((result.returncode, result.stderr), (0, ''), result.stderr)
        lines = result.stdout.splitlines()
        self.assertEqual(len(lines), len(CASES), result.stdout)
        for (label, _), line in zip(CASES, lines):
            with self.subTest(contract=label):
                self.assertEqual(line, f'({label} t)')

    def test_reference(self):
        result = subprocess.run(
            [os.environ.get('EMACS', 'emacs'), '-Q', '--batch', '--eval',
             '(progn ' + source(CASES, reference=True) + ')'],
            capture_output=True, text=True, timeout=20)
        self.assert_contract(result)

    def test_native(self):
        binary = Path(os.environ.get('NELISP_BIN', str(ROOT / 'target/nelisp'))).resolve()
        result = subprocess.run([str(binary), '--repl', '--no-prompt', '--no-print'],
                                input=source(CASES) + '\n(exit 0)\n', cwd=ROOT,
                                capture_output=True, text=True, timeout=20)
        self.assert_contract(result)

    def test_runtime_image_identity(self):
        binary = Path(os.environ.get('NELISP_BIN', str(ROOT / 'target/nelisp'))).resolve()
        with tempfile.TemporaryDirectory(prefix='nelisp symbol image ') as directory:
            image = Path(directory) / 'symbols.nlri'
            setup = ('(progn (setq image-symbol (make-symbol ":probe")) '
                     '(setq image-symbol-alias image-symbol) '
                     '(setq image-symbol-table (make-hash-table :test (quote eq))) '
                     '(puthash image-symbol 77 image-symbol-table) '
                     '(setq image-declaration (make-symbol "image-local-declaration")) '
                     '(setq image-declaration-reader '
                     '(eval `(let ((scope 1)) (defvar ,image-declaration) '
                     '(lambda () (let ((,image-declaration 7)) '
                     '(boundp (quote ,image-declaration))))) t)) '
                     '(garbage-collect))')
            dump = subprocess.run([str(binary), 'dump-runtime-image', str(image), setup],
                                  cwd=ROOT, capture_output=True, text=True, timeout=30)
            self.assertEqual((dump.returncode, dump.stderr), (0, ''))
            expression = ('(list (equal (symbol-name image-symbol) ":probe") '
                          '(not (keywordp image-symbol)) (not (boundp image-symbol)) '
                          '(eq image-symbol image-symbol-alias) '
                          '(not (intern-soft image-symbol)) '
                          '(not (eq image-symbol (make-symbol ":probe"))) '
                          '(gethash image-symbol-alias image-symbol-table) '
                          '(funcall image-declaration-reader) '
                          '(boundp image-declaration) '
                          '(special-variable-p image-declaration))')
            restored = subprocess.run(
                [str(binary), 'eval-runtime-image', str(image), expression],
                cwd=ROOT, capture_output=True, text=True, timeout=30)
            self.assertEqual((restored.returncode, restored.stderr, restored.stdout),
                             (0, '', '(t t t t t t 77 t nil nil)\n'))

    def test_hash_index_after_growth_and_gc(self):
        binary = Path(os.environ.get('NELISP_BIN', str(ROOT / 'target/nelisp'))).resolve()
        for mode in ['eq', 'equal']:
            with self.subTest(mode=mode):
                expression = f'''(let ((table (make-hash-table :test '{mode}))
 (keys (make-vector 100 nil)) (i 0))
 (while (< i 100)
   (aset keys i (make-symbol "index-key"))
   (puthash (aref keys i) i table)
   (setq i (+ i 1)))
 (setq i 0)
 (while (< i 25) (remhash (aref keys i) table) (setq i (+ i 1)))
 (puthash (aref keys 99) 999 table)
 (garbage-collect)
 (list (hash-table-count table) (gethash (aref keys 50) table)
       (gethash (aref keys 0) table) (gethash (aref keys 99) table)))'''
                result = subprocess.run([str(binary), '--eval', expression],
                                        cwd=ROOT, capture_output=True, text=True, timeout=20)
                self.assertEqual((result.returncode, result.stderr, result.stdout),
                                 (0, '', '(75 50 nil 999)\n'))

    def test_cli_symbol_name(self):
        binary = Path(os.environ.get('NELISP_BIN', str(ROOT / 'target/nelisp'))).resolve()
        with tempfile.TemporaryDirectory(prefix='nelisp symbol cli ') as directory:
            source_file = Path(directory) / 'symbol.el'
            source_file.write_text('(make-symbol "probe")\n', encoding='utf-8')
            for arguments in [('--eval', '(make-symbol "probe")'),
                              ('--load', str(source_file))]:
                with self.subTest(mode=arguments[0]):
                    result = subprocess.run([str(binary), *arguments], cwd=ROOT,
                                            capture_output=True, text=True, timeout=20)
                    self.assertEqual((result.returncode, result.stderr, result.stdout),
                                     (0, '', 'probe\n'))


if __name__ == '__main__':
    result = unittest.main(exit=False).result
    findings = len(result.failures) + len(result.errors)
    print(f'GATE-COUNT checked={result.testsRun} findings={findings}')
    raise SystemExit(not result.wasSuccessful())
