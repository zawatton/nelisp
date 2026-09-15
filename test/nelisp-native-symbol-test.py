"""Qualification of the internal tag-16 representation, not public symbol APIs."""
import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]


class NativeSymbol(unittest.TestCase):
    def setUp(self):
        self.binary = Path(os.environ['NELISP_SYMBOL_TEST_BIN']).resolve()

    def repl(self, source):
        with tempfile.TemporaryDirectory(prefix='nelisp symbol source ') as directory:
            script = Path(directory) / 'probe.el'
            script.write_text(source, encoding='utf-8')
            result = subprocess.run(
                [str(self.binary), '--repl', '--no-prompt', '--no-print'],
                input=f'(load {json.dumps(str(script))})\n(exit 0)\n',
                capture_output=True, text=True, cwd=ROOT, timeout=30)
        self.assertEqual((result.returncode, result.stderr), (0, ''))
        return result.stdout

    def test_names_identity_and_clones(self):
        self.assertEqual(self.repl('''
(setq symbol-a (symbol-test-make ":試験" 268500000))
(setq symbol-b (symbol-test-make ":試験" 268500001))
(setq symbol-copy (car (list symbol-a)))
(prin1 (list (symbol-test-id symbol-a) (symbol-test-name symbol-a)
 (symbol-test-same symbol-a symbol-copy)
 (symbol-test-same symbol-a symbol-b)
 (symbol-test-same symbol-a :試験)
 (symbol-test-name (symbol-test-make "" 3))
 (symbol-test-name (symbol-test-make "nil" 4))))
(terpri)
'''), '(268500000 ":試験" t nil nil "" "nil")\n')

    def test_precise_mark_and_collection(self):
        # The private probe saves/restores the mark bit and calls the actual
        # precise marker. Conservative stack retention cannot mask a missing
        # tag edge, as it can in an ordinary collect-and-read smoke.
        self.assertEqual(self.repl('''
(setq symbol-root (vector (symbol-test-make ":retained" 71)))
(prin1 (symbol-test-mark (aref symbol-root 0)))
(terpri)
(garbage-collect)
(prin1 (list (symbol-test-id (aref symbol-root 0))
 (symbol-test-name (aref symbol-root 0))
 (symbol-test-mark (aref symbol-root 0))))
(terpri)
'''), '1\n(71 ":retained" 1)\n')

    def test_public_symbol_inspection(self):
        source = '''
(setq inspect-a (symbol-test-make ":inspect" 801))
(setq inspect-b (symbol-test-make ":inspect" 802))
(setq inspect-copy (car (list inspect-a)))
(prin1 (list (symbolp inspect-a) (type-of inspect-a)
 (symbol-name inspect-a) (keywordp inspect-a) (intern-soft inspect-a)
 (eq inspect-a inspect-copy) (eq inspect-a inspect-b)
 (equal inspect-a inspect-copy) (equal inspect-a inspect-b)
 (eq inspect-a :inspect)
 (let ((print-gensym nil)) (prin1-to-string inspect-a))))
(terpri)
'''
        expected = '(t symbol ":inspect" nil nil t nil t nil nil ":inspect")\n'
        # Use actual Emacs symbols for the same observations; the native
        # fixture differs only in how it constructs the two fresh objects.
        reference_source = source.replace(
            '(symbol-test-make ":inspect" 801)', '(make-symbol ":inspect")').replace(
            '(symbol-test-make ":inspect" 802)', '(make-symbol ":inspect")')
        reference = subprocess.run(
            [os.environ.get('EMACS', 'emacs'), '-Q', '--batch', '--eval',
             '(progn ' + reference_source + ')'],
            capture_output=True, text=True, timeout=30)
        self.assertEqual((reference.returncode, reference.stderr, reference.stdout),
                         (0, '', expected))
        self.assertEqual(self.repl(source), expected)

    def test_runtime_image_preserves_scalar_identity(self):
        with tempfile.TemporaryDirectory(prefix='nelisp native symbol ') as directory:
            image = Path(directory) / 'symbols.nlri'
            setup = ('(progn (setq image-symbol (symbol-test-make ":image" 268500000)) '
                     '(setq image-alias image-symbol) '
                     '(symbol-test-global-set image-symbol 77))')
            dump = subprocess.run([str(self.binary), 'dump-runtime-image', str(image), setup],
                                  cwd=ROOT, capture_output=True, text=True, timeout=30)
            self.assertEqual((dump.returncode, dump.stderr), (0, ''))
            expression = ('(list (symbol-test-id image-symbol) '
                          '(symbol-test-name image-symbol) '
                          '(symbol-test-same image-symbol image-alias) '
                          '(symbol-test-mark image-symbol) '
                          '(symbol-test-global-get image-symbol))')
            restored = subprocess.run(
                [str(self.binary), 'eval-runtime-image', str(image), expression],
                cwd=ROOT, capture_output=True, text=True, timeout=30)
            self.assertEqual((restored.returncode, restored.stderr, restored.stdout),
                             (0, '', '(268500000 ":image" t 1 77)\n'))

    def test_native_frame_identity_keys(self):
        source = '''
(load "lisp/nelisp-stdlib-fast-hash.el")
(load "lisp/nelisp-lexframe.el")
(setq key-a (symbol-test-make "same" 901))
(setq key-b (symbol-test-make "same" 902))
(setq key-stack (nelisp-lexframe-stack-make))
(setq key-frame (nelisp--make-record 'nelisp-lexframe (nelisp--fast-hash-make 1)))
(nelisp-lexframe-stack-push! key-stack key-frame)
(symbol-test-bind key-stack key-a (symbol-test-cell 1))
(symbol-test-bind key-stack key-b (symbol-test-cell 2))
(symbol-test-bind key-stack "same" (symbol-test-cell 3))
(symbol-test-bind key-stack 'same (symbol-test-cell 4))
(symbol-test-bind key-stack key-a (symbol-test-cell 9))
(garbage-collect)
(setq key-cap (symbol-test-capture (vector key-stack 1 nil nil 0)))
(setq key-filtered (symbol-test-capture (vector key-stack 1 nil (list key-a) 1)))
(prin1 (list (symbol-test-find key-stack key-a)
 (symbol-test-find key-stack key-b) (symbol-test-find key-stack "same")
 (nelisp--fast-hash-count (nelisp--record-ref key-frame 0))
 (length key-cap) (length key-filtered)
 (eq (car (car key-filtered)) key-a)))
(terpri)
'''
        for name, buckets in [('same', 1), ('試験', 16)]:
            with self.subTest(name=name, buckets=buckets):
                probe = source.replace('same', name).replace(
                    '(nelisp--fast-hash-make 1)', f'(nelisp--fast-hash-make {buckets})')
                self.assertEqual(self.repl(probe), '(9 2 4 3 3 1 t)\n')

    def test_native_global_identity_keys(self):
        self.assertEqual(self.repl('''
(setq global-a (symbol-test-make "private-global-key" 951))
(setq global-b (symbol-test-make "private-global-key" 952))
(symbol-test-global-set global-a 1)
(symbol-test-global-set global-b 2)
(symbol-test-global-set 'private-global-key 3)
(symbol-test-global-set global-a 9)
(garbage-collect)
(prin1 (list (symbol-test-global-get global-a)
 (symbol-test-global-get global-b) (symbol-test-global-get 'private-global-key)
 (symbol-test-global-get (symbol-test-make "private-global-key" 953))))
(terpri)
'''), '(9 2 3 nil)\n')

    def test_source_native_frame_hash_interoperation(self):
        names = ['ascii', 'é', '試験', '🙂']
        setup = ('(load "lisp/nelisp-stdlib-fast-hash.el")\n'
                 '(load "lisp/nelisp-lexframe.el")\n')
        case = '''
(setq mix-name NAME mix-a (make-symbol mix-name) mix-b (make-symbol mix-name))
(setq mix-stack (nelisp-lexframe-stack-make) mix-frame (nelisp-lexframe-make 64))
(nelisp-lexframe-stack-push! mix-stack mix-frame)
(setq mix-cell-a (symbol-test-cell 7) mix-cell-b (symbol-test-cell 9))
(nelisp-lexframe-bind mix-frame mix-a mix-cell-a)
(symbol-test-bind mix-stack mix-b mix-cell-b)
(nelisp-lexframe-bind mix-frame mix-name (symbol-test-cell 11))
(garbage-collect)
(prin1 (list (symbol-test-find mix-stack mix-a)
 (eq (nelisp-lexframe-lookup mix-frame mix-b) mix-cell-b)
 (symbol-test-find mix-stack mix-name)
 (nelisp--fast-hash-count (nelisp--record-ref mix-frame 0))))
(terpri)
'''
        lines = self.repl(setup + ''.join(
            case.replace('NAME', json.dumps(name, ensure_ascii=False))
            for name in names)).splitlines()
        self.assertEqual(len(lines), len(names))
        for name, line in zip(names, lines):
            with self.subTest(name=name):
                self.assertEqual(line, '(7 t 11 3)')

    def test_jit_and_reader_share_identity_issuance(self):
        self.assertEqual(self.repl('''
(setq jit-table (make-hash-table :test 'eq) jit-i 0)
(while (< jit-i 100)
 (puthash (if (= (mod jit-i 2) 0) (symbol-test-jit-make ":same")
           (make-symbol ":same")) jit-i jit-table)
 (setq jit-i (+ jit-i 1)))
(setq jit-a (symbol-test-jit-make ":same"))
(setq jit-b (make-symbol ":same"))
(setq jit-mutable (symbol-test-boxed-string "a🙂c"))
(aset jit-mutable 0 98)
(setq jit-unibyte (symbol-test-boxed-string (unibyte-string 97 98)))
(aset jit-unibyte 1 99)
(garbage-collect)
(prin1 (list (hash-table-count jit-table) (symbolp jit-a)
 (symbol-name jit-a) (eq jit-a jit-b) (intern-soft jit-a) (keywordp jit-a)
 (> (symbol-test-id jit-a) 0)
 (symbol-name (symbol-test-jit-make ""))
 (symbol-name (symbol-test-jit-make (unibyte-string 97 98)))
 (symbol-name (symbol-test-jit-make "試験"))
 (symbol-name (symbol-test-jit-make jit-mutable))
 (symbol-name (symbol-test-jit-make jit-unibyte))))
(terpri)
'''), '(100 t ":same" nil nil nil t "" "ab" "試験" "b🙂c" "ac")\n')

    def test_source_rehash_preserves_live_native_frame(self):
        self.assertEqual(self.repl('''
(load "lisp/nelisp-stdlib-fast-hash.el")
(load "lisp/nelisp-lexframe.el")
(setq rehash-original (symbol-function 'nelisp--fast-hash--hash))
(fset 'nelisp--fast-hash--hash (lambda (name buckets) 0))
(setq rehash-key (make-symbol "試験") rehash-cell (symbol-test-cell 7))
(setq rehash-stack (nelisp-lexframe-stack-make) rehash-frame (nelisp-lexframe-make 64))
(nelisp-lexframe-stack-push! rehash-stack rehash-frame)
(nelisp-lexframe-bind rehash-frame rehash-key rehash-cell)
(fset 'nelisp--fast-hash--hash rehash-original)
(setq rehash-before (symbol-test-find rehash-stack rehash-key))
(nelisp--fast-hash-rehash! (nelisp--record-ref rehash-frame 0))
(garbage-collect)
(prin1 (list rehash-before (symbol-test-find rehash-stack rehash-key)
 (eq (nelisp-lexframe-lookup rehash-frame rehash-key) rehash-cell)))
(terpri)
'''), '(nil 7 t)\n')

    def test_jit_rejects_non_strings_without_writing_output(self):
        self.assertEqual(self.repl('''
(prin1 (list (symbol-test-jit-error nil) (symbol-test-jit-error t)
 (symbol-test-jit-error 1) (symbol-test-jit-error 'name)
 (symbol-test-jit-error []) (symbol-test-jit-error '(1))))
(terpri)
'''), '(t t t t t t)\n')

    def test_captured_declaration_name_representations(self):
        self.assertEqual(self.repl('''
(load "lisp/nelisp-stdlib-fast-hash.el")
(load "lisp/nelisp-lexframe.el")
(setq decl-stack (nelisp-lexframe-stack-make))
(nelisp-lexframe-stack-push! decl-stack (nelisp-lexframe-make))
(setq decl-symbol (make-symbol "private-declaration"))
(setq decl-names (list 'interned-declaration "試験"
 (unibyte-string 97) (symbol-test-boxed-string "字")
 (symbol-test-boxed-string (unibyte-string 98)) decl-symbol))
(symbol-test-restore-declarations decl-stack decl-names)
(garbage-collect)
(prin1 (append (mapcar (lambda (name)
 (symbol-test-local-special decl-stack name)) decl-names)
 (list (symbol-test-local-special decl-stack (make-symbol "private-declaration"))
       (symbol-test-local-special decl-stack 'missing-declaration))))
(terpri)
'''), '(1 1 1 1 1 1 0 0)\n')


if __name__ == '__main__':
    unittest.main()
