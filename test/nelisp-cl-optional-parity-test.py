"""GNU optional defaults, presence and ordered initialization regression."""
import importlib.util
from pathlib import Path

spec = importlib.util.spec_from_file_location(
    'focused', Path(__file__).with_name('nelisp-focused-parity.py'))
focused = importlib.util.module_from_spec(spec)
spec.loader.exec_module(focused)
SETUP = '''
(cl-defun nelisp-test-presence (&optional (value 'default supplied))
  (list value supplied))
(cl-defun nelisp-test-ordered (head &optional (a head ap) (b (list a ap) bp) &rest tail)
  (list head a ap b bp tail))
(cl-defun nelisp-test-keys (&optional (a 'fallback ap) &key (k 'key-default))
  (list a ap k))
(cl-defun nelisp-test-plain (a &optional b) (list a b))
(cl-defun nelisp-test-count (a &optional (b 9 bp)) (list a b bp))
(defvar nelisp-test-default-count 0)
(cl-defun nelisp-test-effects (&optional (a (progn (setq nelisp-test-default-count
  (1+ nelisp-test-default-count)) 7) ap)) (list a ap nelisp-test-default-count))
'''
CASES = [
    '(nelisp-test-presence)', '(nelisp-test-presence nil)', '(nelisp-test-presence 3)',
    '(nelisp-test-ordered 4)', '(nelisp-test-ordered 4 nil)',
    '(nelisp-test-ordered 4 5 nil 7 8)',
    '(nelisp-test-keys)', '(nelisp-test-keys nil :k 3)',
    '(nelisp-test-plain 1)', '(nelisp-test-plain 1 nil)',
    '(condition-case e (nelisp-test-count 1 2 3) (error (list (car e) (car (last e)))))',
    '(nelisp-test-effects)', '(nelisp-test-effects nil)', '(nelisp-test-effects 8)',
    '(funcall (symbol-function (quote nelisp-test-presence)) nil)',
    '(apply (quote nelisp-test-presence) (quote (nil)))',
]
DEFECT = '0c5fc3694fdd874c3f07e2ea8b217149d1bc479f00fa6ff083408fe3f776676a'
if __name__ == '__main__':
    raise SystemExit(focused.main(CASES, SETUP, DEFECT))
