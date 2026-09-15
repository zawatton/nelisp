"""Compare declarations, variable cells, and binding behavior with Emacs."""
import os
import json
from pathlib import Path
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
CASES = [
    ("(progn (defvar const-dynamic 3) (list (let ((const-dynamic 7)) (defconst const-dynamic 9) const-dynamic) const-dynamic))", '(9 3)'),
    ("(list (let ((const-dynamic 7)) (eval (macroexpand '(defconst const-dynamic 11))) (funcall (lambda () const-dynamic))) const-dynamic)", '(11 3)'),
    ("(let ((const-lexical 7)) (defconst const-lexical 9) (list const-lexical (symbol-value 'const-lexical)))", '(7 9)'),
    ("(progn (defconst const-init-order (special-variable-p 'const-init-order)) (list const-init-order (special-variable-p 'const-init-order)))", '(nil t)'),
    ("(progn (condition-case nil (defconst const-failed (error \"init\")) (error nil)) (list (special-variable-p 'const-failed) (boundp 'const-failed)))", '(nil nil)'),
    ("(list (let ((const-dynamic 7)) (defconst const-dynamic (progn (garbage-collect) 13)) const-dynamic) const-dynamic)", '(13 3)'),
    ("(progn (defconst const-writable 7) (set 'const-writable 11) const-writable)", '11'),
    ("(progn (setq const-init-side 0) (list (condition-case e (defconst 1 (setq const-init-side 1)) (wrong-type-argument e)) const-init-side))", '((wrong-type-argument symbolp 1) 0)'),
    ("(progn (setq const-init-side 0) (list (condition-case e (defconst nil (setq const-init-side 1)) (setting-constant e)) const-init-side (condition-case e (defconst t 1) (setting-constant e)) (condition-case e (defconst :const-key 1) (setting-constant e)) (defconst :const-self :const-self)))", '((setting-constant nil) 1 (setting-constant t) (setting-constant :const-key) :const-self)'),
    ("(list (macroexpand-1 '(defvar expand-kept)) (macroexpand '(defvar expand-kept 7)) (macroexpand '(defconst expand-kept 9)))", '((defvar expand-kept) (defvar expand-kept 7) (defconst expand-kept 9))'),
    ("(progn (condition-case nil (defvar expand-default (error \"declare\")) (error nil)) (list (let ((expand-default 7)) (eval (macroexpand '(defvar expand-default 9))) expand-default) expand-default))", '(7 9)'),
    ("(macroexpand '(defvar expand-env 1) (list (cons 'defvar (lambda (&rest ignored) '(quote overridden)))))", "'overridden"),
    ("(progn (defmacro expand-user-declaration (&rest ignored) '(quote overridden)) (let ((original (symbol-function 'defvar))) (unwind-protect (progn (fset 'defvar (symbol-function 'expand-user-declaration)) (macroexpand '(defvar expand-user 1))) (fset 'defvar original))))", "'overridden"),
    ("(list (makunbound 'unbind-new) (boundp 'unbind-new))", '(unbind-new nil)'),
    ("(progn (setq unbind-interned 7) (let ((symbol (make-symbol \"unbind-interned\"))) (set symbol 9) (makunbound symbol) (list (boundp symbol) unbind-interned)))", '(nil 7)'),
    ("(progn (defvar unbind-global 7) (makunbound 'unbind-global) (list (special-variable-p 'unbind-global) (boundp 'unbind-global) (condition-case e unbind-global (void-variable e))))", '(t nil (void-variable unbind-global))'),
    ("(progn (defvar unbind-global 9) (list unbind-global (special-variable-p 'unbind-global)))", '(9 t)'),
    ("(progn (setq unbind-lexical 3) (list (let ((unbind-lexical 7)) (makunbound 'unbind-lexical) (list unbind-lexical (boundp 'unbind-lexical))) (boundp 'unbind-lexical)))", '((7 nil) nil)'),
    ("(progn (defvar unbind-dynamic 3) (list (let ((unbind-dynamic 7)) (setq unbind-reader (lambda () unbind-dynamic)) (makunbound 'unbind-dynamic) (list (boundp 'unbind-dynamic) (condition-case nil (symbol-value 'unbind-dynamic) (void-variable 'void)) (condition-case nil (funcall unbind-reader) (void-variable 'void)))) unbind-dynamic (funcall unbind-reader)))", '((nil void void) 3 3)'),
    ("(let ((unbind-dynamic 7)) (makunbound 'unbind-dynamic) (set 'unbind-dynamic 11) (list unbind-dynamic (boundp 'unbind-dynamic)))", '(11 t)'),
    ("(progn (defvar unbind-default 3) (list (let ((unbind-default 7)) (makunbound 'unbind-default) (condition-case nil (defvar unbind-default (error \"initializer\")) (error 'initializer))) unbind-default))", '(initializer 3)'),
    ("(list (let ((unbind-default 7)) (makunbound 'unbind-default) (defvar unbind-default 9) (list (boundp 'unbind-default) (symbol-value 'unbind-default))) unbind-default)", '((t 9) 3)'),
    ("(list (let ((unbind-default 7)) (makunbound 'unbind-default) (defvar unbind-default (progn (garbage-collect) 11)) (funcall (lambda () unbind-default))) unbind-default)", '(11 3)'),
    ("(progn (defun unbind-both () 5) (put 'unbind-both 'note 9) (setq unbind-both 7) (makunbound 'unbind-both) (list (fboundp 'unbind-both) (get 'unbind-both 'note) (unbind-both) (boundp 'unbind-both)))", '(t 9 5 nil)'),
    ("(let ((unbind-scope 1)) (defvar unbind-local) (let ((unbind-local 7)) (makunbound 'unbind-local) (list (special-variable-p 'unbind-local) (boundp 'unbind-local) (let ((unbind-local 9)) (boundp 'unbind-local)))))", '(nil nil t)'),
    ("(mapcar (lambda (s) (condition-case e (makunbound s) (setting-constant e))) '(nil t :key))", '((setting-constant nil) (setting-constant t) (setting-constant :key))'),
    ("(condition-case e (makunbound 1) (wrong-type-argument e))", '(wrong-type-argument symbolp 1)'),
    ("(progn (defconst unbind-constant 7) (makunbound 'unbind-constant) (list (special-variable-p 'unbind-constant) (boundp 'unbind-constant)))", '(t nil)'),
    ("(progn (defvar unbind-unwind 3) (condition-case nil (let ((unbind-unwind 7)) (makunbound 'unbind-unwind) (error \"unwind\")) (error nil)) unbind-unwind)", '3'),
    ("(let () (defvar local-forward-a) (list (special-variable-p 'local-forward-a) (let ((local-forward-a 7)) (list local-forward-a (boundp 'local-forward-a) (symbol-value 'local-forward-a))) (boundp 'local-forward-a)))", '(nil (7 t 7) nil)'),
    ("(progn (let () (defvar local-forward-b)) (let ((local-forward-b 9)) (boundp 'local-forward-b)))", 't'),
    ("(progn (let ((local-forward-scope 1)) (defvar local-forward-exit)) (let ((local-forward-exit 9)) (boundp 'local-forward-exit)))", 'nil'),
    ("(progn (setq local-forward-f (let () (defvar local-forward-c) (lambda () (let ((local-forward-c 7)) (list local-forward-c (boundp 'local-forward-c)))))) (list (funcall local-forward-f) (special-variable-p 'local-forward-c)))", '((7 t) nil)'),
    ("(progn (defun local-forward-call () (let ((local-forward-d 7)) (boundp 'local-forward-d))) (let () (defvar local-forward-d) (local-forward-call)))", 'nil'),
    ("(let ((local-forward-e 1)) (defvar local-forward-e) (let ((local-forward-e 2)) (list local-forward-e (symbol-value 'local-forward-e))))", '(1 2)'),
    ("(progn (defvar local-forward-g) (let ((local-forward-g 7)) (boundp 'local-forward-g)))", 't'),
    ("(let () (setq local-forward-before (lambda () (let ((local-forward-h 7)) (boundp 'local-forward-h)))) (defvar local-forward-h) (list (funcall local-forward-before) (funcall (lambda () (let ((local-forward-h 8)) (boundp 'local-forward-h))))))", '(nil t)'),
    ("(let* ((scope-star-x 1) (reader (lambda () scope-star-x)) (scope-star-x 2)) (list scope-star-x (funcall reader)))", '(2 1)'),
    ("(let* ((scope-star-write 1) (reader (lambda () scope-star-write)) (writer (lambda (v) (setq scope-star-write v))) (scope-star-write 2)) (funcall writer 7) (list scope-star-write (funcall reader)))", '(2 7)'),
    ("(let* ((scope-star-mixed 1) (ignored (defvar scope-star-mixed 0)) (scope-star-mixed 2)) (list scope-star-mixed (symbol-value 'scope-star-mixed)))", '(1 2)'),
    ("(progn (defvar scope-star-dynamic 0) (list (let* ((scope-star-dynamic 1) (reader (lambda () scope-star-dynamic)) (scope-star-dynamic 2)) (list scope-star-dynamic (funcall reader))) scope-star-dynamic))", '((2 2) 0)'),
    ("(let ((scope-star-restore 9)) (condition-case nil (let* ((scope-star-restore 1) (scope-star-restore (error \"init\"))) scope-star-restore) (error nil)) scope-star-restore)", '9'),
    ("(let ((scope-star-unwind 9)) (condition-case nil (let* ((scope-star-unwind 1) (scope-star-unwind 2)) (error \"body\")) (error nil)) scope-star-unwind)", '9'),
    ("(let* ((scope-star-gc (list 1)) (reader (lambda () scope-star-gc)) (scope-star-gc (progn (garbage-collect) (list 2)))) (garbage-collect) (list scope-star-gc (funcall reader)))", '((2) (1))'),
    ("(progn 42 (let* ()))", 'nil'),
    ("(progn (setq scope-call-value 1) (defun scope-call-read () scope-call-value) (let ((scope-call-value 2)) (scope-call-read)))", '1'),
    ("(progn (defun scope-call-write () (setq scope-call-value 3)) (list (let ((scope-call-value 2)) (scope-call-write) scope-call-value) scope-call-value))", '(2 3)'),
    ("(progn (defun scope-call-make () (lambda () scope-call-value)) (let ((scope-call-value 9)) (setq scope-call-closure (scope-call-make))) (funcall scope-call-closure))", '3'),
    ("(progn (setq scope-call-captured (let ((scope-call-private 4)) (lambda () (list scope-call-private scope-call-value)))) (let ((scope-call-value 9) (scope-call-private 8)) (funcall scope-call-captured)))", '(4 3)'),
    ("(progn (defvar scope-call-dynamic 1) (defun scope-call-dynamic-read () scope-call-dynamic) (let ((scope-call-dynamic 7)) (scope-call-dynamic-read)))", '7'),
    ("(progn (defun scope-call-error () (error \"callee\")) (let ((scope-call-value 9)) (condition-case nil (scope-call-error) (error nil)) scope-call-value))", '9'),
    ("(progn (defun scope-call-gc () (garbage-collect) scope-call-value) (let ((scope-call-value 9)) (scope-call-gc)))", '3'),
    ("(progn (defun scope-call-required (required) required) (let ((scope-call-value 9)) (condition-case nil (scope-call-required) (wrong-number-of-arguments nil)) scope-call-value))", '9'),
    ("(progn (setq scope-call-required-closure (let ((scope-call-held 4)) (lambda (required) (list scope-call-held required)))) (let ((scope-call-value 9)) (condition-case nil (funcall scope-call-required-closure) (wrong-number-of-arguments nil)) (list scope-call-value (funcall scope-call-required-closure 5))))", '(9 (4 5))'),
    ("(let ((special-late-scope 1)) (defvar special-late-scope 0) (let ((special-late-scope 2)) (list special-late-scope (symbol-value 'special-late-scope) (funcall (lambda () special-late-scope)))))", '(1 2 1)'),
    ("(let ((special-late-write 1)) (defvar special-late-write 0) (let ((special-late-write 2)) (setq special-late-write 3) (set 'special-late-write 4) (list special-late-write (symbol-value 'special-late-write))))", '(3 4)'),
    ('(progn (condition-case nil (defvar special-local-init (error "init")) (error nil)) '
     '(list (let ((special-local-init 7)) (defvar special-local-init 2) special-local-init) '
     "(boundp 'special-local-init) special-local-init))", '(7 t 2)'),
    ("(let ((special-lexical-only 7)) (list (boundp 'special-lexical-only) (condition-case nil (symbol-value 'special-lexical-only) (void-variable 'void-variable))))", '(nil void-variable)'),
    ('(progn (condition-case nil (defvar special-dynamic-unbound (error "init")) (error nil)) '
     "(list (special-variable-p 'special-dynamic-unbound) (let ((special-dynamic-unbound 7)) (list (boundp 'special-dynamic-unbound) (symbol-value 'special-dynamic-unbound))) (boundp 'special-dynamic-unbound)))", '(t (t 7) nil)'),
    ("(progn (defvar special-dynamic-capture 0) (let ((special-dynamic-capture 9)) (setq special-dynamic-reader (lambda () special-dynamic-capture)) (garbage-collect)) (list (funcall special-dynamic-reader) special-dynamic-capture))", '(0 0)'),
    ("(let ((special-dynamic-capture 11)) (funcall special-dynamic-reader))", '11'),
    ("(progn (setq special-dynamic-capture 13) (funcall special-dynamic-reader))", '13'),
    ("(progn (setq special-lexical-param 0) (defun special-lexical-call (special-lexical-param) (list special-lexical-param (symbol-value 'special-lexical-param) (boundp 'special-lexical-param))) (list (special-lexical-call 4) special-lexical-param))", '((4 0 t) 0)'),
    ("(progn (setq special-lexical-opt nil special-lexical-rest nil) (defun special-lexical-args (&optional special-lexical-opt &rest special-lexical-rest) (list special-lexical-opt special-lexical-rest (symbol-value 'special-lexical-opt) (symbol-value 'special-lexical-rest))) (special-lexical-args 2 3 4))", '(2 (3 4) nil nil)'),
    ("(progn (setq special-set-lexical 3) (list (let ((special-set-lexical 7)) (set 'special-set-lexical 9) (list special-set-lexical (symbol-value 'special-set-lexical))) special-set-lexical))", '((7 9) 9)'),
    ("(progn (defvar special-set-dynamic 3) (list (let ((special-set-dynamic 7)) (set 'special-set-dynamic 11) (list special-set-dynamic (symbol-value 'special-set-dynamic))) special-set-dynamic))", '((11 11) 3)'),
    ('(progn (defvar special-unwind-dynamic 3) (condition-case nil (let ((special-unwind-dynamic 7)) (error "unwind")) (error nil)) special-unwind-dynamic)', '3'),
    ("(progn (defvar special-sequential-one 0) (defvar special-sequential-two 0) (let* ((special-sequential-one 7) (special-sequential-two (1+ special-sequential-one))) (list special-sequential-one special-sequential-two (symbol-value 'special-sequential-one) (symbol-value 'special-sequential-two))))", '(7 8 7 8)'),
    ("(progn (defun special-test-only-function () 1) (condition-case e (symbol-value 'special-test-only-function) (void-variable e)))", '(void-variable special-test-only-function)'),
    ("(condition-case e special-test-only-function (void-variable e))", '(void-variable special-test-only-function)'),
    ("(list (boundp 'special-test-only-function) (fboundp 'special-test-only-function) (special-test-only-function))", '(nil t 1)'),
    ("(let ((special-test-only-function 8)) special-test-only-function)", '8'),
    ("(progn (setq special-test-only-function nil) (list (boundp 'special-test-only-function) (symbol-value 'special-test-only-function)))", '(t nil)'),
    ("(progn (defvar special-test-init 7) (list (special-variable-p 'special-test-init) special-test-init))", '(t 7)'),
    ("(progn (defvar special-test-nil nil) (list (special-variable-p 'special-test-nil) (boundp 'special-test-nil)))", '(t t)'),
    ("(progn (defvar special-test-forward) (list (special-variable-p 'special-test-forward) (boundp 'special-test-forward)))", '(nil nil)'),
    ("(progn (defvar special-test-forward 3) (list (special-variable-p 'special-test-forward) special-test-forward))", '(t 3)'),
    ('(progn (setq special-test-existing 99) (defvar special-test-existing (error "must not run")) '
     "(list (special-variable-p 'special-test-existing) special-test-existing))", '(t 99)'),
    ('(progn (condition-case nil (defvar special-test-failed (error "init")) (error nil)) '
     "(list (special-variable-p 'special-test-failed) (boundp 'special-test-failed)))", '(t nil)'),
    ("(progn (defvar special-test-during (special-variable-p 'special-test-during)) special-test-during)", 't'),
    ("(progn (defconst special-test-constant 11) (special-variable-p 'special-test-constant))", 't'),
    ("(progn (eval (macroexpand '(defvar special-test-expanded 5))) (list (special-variable-p 'special-test-expanded) special-test-expanded))", '(t 5)'),
    ("(let ((special-test-local 7)) (list (special-variable-p 'special-test-local) (boundp 'special-test-local)))", '(nil nil)'),
    ("(progn (defun special-test-function () 1) (special-variable-p 'special-test-function))", 'nil'),
    ('(list (special-variable-p nil) (special-variable-p t) (special-variable-p :key))', '(t t t)'),
    ("(condition-case e (special-variable-p 1) (wrong-type-argument e))", '(wrong-type-argument symbolp 1)'),
    ("(condition-case e (boundp 1) (wrong-type-argument e))", '(wrong-type-argument symbolp 1)'),
    ('(let ((special-test-late 7)) (condition-case nil (defvar special-test-late (error "init")) (error nil)) '
     "(list (special-variable-p 'special-test-late) (boundp 'special-test-late)))", '(t nil)'),
]


class SpecialVariables(unittest.TestCase):
    def test_local_declaration_runtime_image(self):
        binary = Path(os.environ.get('NELISP_BIN', str(ROOT/'target/nelisp'))).resolve()
        with tempfile.TemporaryDirectory(prefix='nelisp local image ') as directory:
            image = Path(directory) / 'local.nlri'
            form = ('(setq image-local-reader (let ((image-local-cell 1)) '
                    '(defvar image-local-special) '
                    '(lambda () (let ((image-local-special 7)) '
                    "(list image-local-cell (boundp 'image-local-special) "
                    "(symbol-value 'image-local-special))))))")
            dump = subprocess.run([str(binary), 'dump-runtime-image', str(image), form],
                                  cwd=ROOT, capture_output=True, text=True, timeout=30)
            self.assertEqual((dump.returncode, dump.stderr), (0, ''))
            run = subprocess.run([str(binary), 'eval-runtime-image', str(image),
                                  '(funcall image-local-reader)'], cwd=ROOT,
                                 capture_output=True, text=True, timeout=30)
            self.assertEqual((run.returncode, run.stdout, run.stderr), (0, '(1 t 7)\n', ''))
        print('LOCAL-IMAGE checked=1')

    def test_file_local_declarations_and_nested_load(self):
        with tempfile.TemporaryDirectory(prefix='nelisp local declarations ') as directory:
            parent = Path(directory) / 'parent.el'
            child = Path(directory) / 'child.el'
            child.write_text(';;; -*- lexical-binding: t; -*-\n'
                             '(setq local-file-child (let ((local-file-var 3)) '
                             "(boundp 'local-file-var)))\n")
            parent.write_text(';;; -*- lexical-binding: t; -*-\n'
                              '(defvar local-file-var)\n'
                              '(setq local-file-first (let ((local-file-var 7)) '
                              "(symbol-value 'local-file-var)))\n"
                              f'(load {json.dumps(child.as_posix())} nil t)\n'
                              '(setq local-file-reader (lambda () (let ((local-file-var 9)) '
                              "(list (boundp 'local-file-var) (symbol-value 'local-file-var)))))\n")
            expression = (f'(progn (load {json.dumps(parent.as_posix())} nil t) '
                          '(garbage-collect) '
                          '(list local-file-first local-file-child (funcall local-file-reader) '
                          "(list (special-variable-p 'local-file-var) "
                          "(let ((local-file-var 4)) (boundp 'local-file-var)))))")
            binary = Path(os.environ.get('NELISP_BIN', str(ROOT/'target/nelisp'))).resolve()
            commands = [
                ('native', [str(binary), '--eval', expression]),
                ('reference', [os.environ.get('EMACS', 'emacs'), '-Q', '--batch',
                               '--eval', f"(prin1 (eval '{expression} t))"]),
            ]
            for label, command in commands:
                with self.subTest(substrate=label):
                    run = subprocess.run(command, cwd=ROOT, capture_output=True,
                                         text=True, timeout=20)
                    self.assertEqual((run.returncode, run.stderr), (0, ''))
                    self.assertEqual(run.stdout.strip(), '(7 nil (t 9) (nil nil))')
        print('LOCAL-FILE checked=4 reference=4')

    def test_explicit_frame_kinds_on_both_substrates(self):
        expression = '''
(let* ((stack (nelisp-lexframe-stack-make))
       (outer (nelisp-lexframe-make)) (inner (nelisp-lexframe-make))
       (lexical (list 7)) (dynamic (list 9)))
  (nelisp-lexframe-bind outer "x" lexical)
  (nelisp-lexframe-bind inner "x" dynamic t)
  (nelisp-lexframe-stack-push! stack outer)
  (nelisp-lexframe-stack-push! stack inner)
  (let ((kinds (list (nelisp-lexframe-dynamic-p outer "x")
                     (nelisp-lexframe-dynamic-p inner "x")))
        (cells (list (nelisp-lexframe-stack-find-lexical stack "x")
                     (nelisp-lexframe-stack-find-dynamic stack "x")))
        (captured (nelisp-lexframe-stack-capture-to-depth stack 2)))
    (condition-case nil
        (unwind-protect (error "unwind frame")
          (nelisp-lexframe-stack-pop! stack))
      (error nil))
    (garbage-collect)
    (list kinds cells captured
          (list (nelisp-lexframe-stack-find-lexical stack "x")
                (eq (nelisp-lexframe-stack-find-dynamic stack "x")
                    nelisp--unbound-marker)))))'''
        # The standalone REPL consumes one complete form per input line.
        expression = ' '.join(expression.splitlines())
        expected = '((nil t) ((7) (9)) (("x" 7)) ((7) t))\n'
        binary = Path(os.environ.get('NELISP_BIN', str(ROOT/'target/nelisp'))).resolve()
        native_setup = '(load "lisp/nelisp-stdlib-fast-hash.el") (load "lisp/nelisp-lexframe.el")'
        native = subprocess.run([str(binary), '--repl', '--no-prompt', '--no-print'],
                                input=f'(progn {native_setup} (prin1 {expression}) (terpri))\n(exit 0)\n',
                                cwd=ROOT, capture_output=True, text=True, timeout=15)
        # The host fixture supplies record primitives and loads the canonical
        # library.  Merely loading it defines ERT cases without running them.
        reference = subprocess.run([os.environ.get('EMACS', 'emacs'), '--batch', '-Q',
                                    '-l', 'test/nelisp-lexframe-test.el', '--eval',
                                    f'(progn (prin1 {expression}) (terpri))'],
                                   cwd=ROOT, capture_output=True, text=True, timeout=15)
        for label, run in [('native', native), ('reference', reference)]:
            with self.subTest(substrate=label):
                self.assertEqual((run.returncode, run.stderr, run.stdout), (0, '', expected))
        print('FRAME-KIND checked=4 reference=4')

    def test_emacs_reference(self):
        # Check the expected contract in one lexical Emacs context, preserving
        # declarations between cases just as the native REPL does.  This avoids
        # a separate host launch and repeated setup for each observation.
        source = '(progn ' + ' '.join(
            f"(prin1 (eval '{form} t)) (terpri)" for form, _ in CASES) + ')'
        run = subprocess.run([os.environ.get('EMACS', 'emacs'), '--batch', '-Q',
                              '--eval', source], capture_output=True, text=True,
                             timeout=15)
        self.assertEqual((run.returncode, run.stderr), (0, ''))
        self.assertEqual(run.stdout.splitlines(), [value for _, value in CASES])

    def test_declarations_and_initializer_order(self):
        binary = Path(os.environ.get('NELISP_BIN', str(ROOT/'target/nelisp'))).resolve()
        source = '\n'.join(f'(progn (prin1 {form}) (terpri))' for form, _ in CASES)+'\n(exit 0)\n'
        run = subprocess.run([str(binary), '--repl', '--no-prompt', '--no-print'],
                             input=source, capture_output=True, text=True, timeout=15)
        self.assertEqual((run.returncode, run.stderr), (0, ''))
        self.assertEqual(run.stdout.splitlines(), [value for _, value in CASES])
        raw = subprocess.run([str(binary), '--eval',
                              "(list (funcall '(builtin boundp) nil) (funcall '(builtin boundp) t) (funcall '(builtin boundp) :key))"],
                             capture_output=True, text=True, timeout=15)
        self.assertEqual((raw.returncode, raw.stderr, raw.stdout), (0, '', '(t t t)\n'))
        print(f'GATE-COUNT checked={len(CASES) + 3} findings=0')

    def test_declarations_after_canonical_source_reload(self):
        binary = Path(os.environ.get('NELISP_BIN', str(ROOT/'target/nelisp'))).resolve()
        source = '(load "lisp/nelisp-stdlib-eval-special.el")\n'
        source += '\n'.join(f'(progn (prin1 {form}) (terpri))' for form, _ in CASES)
        # Only the canonical definitions retain native dispatch. A deliberate
        # user replacement must still expand, and restoring the function cell
        # must reattach the canonical identity without another registration.
        source += '''
(setq reload-original-defvar (symbol-function 'defvar))
(defmacro defvar (&rest args) 23)
(prin1 (macroexpand '(defvar reload-user-macro 9)))
(terpri)
(fset 'defvar reload-original-defvar)
(prin1 (equal (macroexpand '(defvar reload-restored 9)) '(defvar reload-restored 9)))
(terpri)
(exit 0)
'''
        run = subprocess.run([str(binary), '--repl', '--no-prompt', '--no-print'],
                             input=source, cwd=ROOT, capture_output=True,
                             text=True, timeout=20)
        self.assertEqual((run.returncode, run.stderr), (0, ''))
        self.assertEqual(run.stdout.splitlines(), [value for _, value in CASES] + ['23', 't'])
        print(f'SOURCE-RELOAD checked={len(CASES) + 2}')


if __name__ == '__main__':
    unittest.main()
