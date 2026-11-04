;;; generator-test.el --- Tests for the vendored generator  -*- lexical-binding: t; -*-

;;; Commentary:

;; Doc 16 breadth round 23.  src/generator.el is vendored verbatim from GNU
;; Emacs 30.1, so these tests confirm the vendored copy is intact and
;; correct on the batch host.  (Iterator EXECUTION on the NeLisp runtime is
;; blocked by reader-core gap Doc 22 A12 -- macroexpand-all ignoring its
;; environment argument -- which is out of scope for a host test.)

;;; Code:

(require 'ert)
(require 'generator)

(defun generator-test--drain (it)
  "Collect every value produced by iterator IT into a list."
  (let ((acc nil))
    (condition-case nil
        (while t (push (iter-next it) acc))
      (iter-end-of-sequence nil))
    (nreverse acc)))

(iter-defun generator-test--straight ()
  (iter-yield 1)
  (iter-yield 2)
  (iter-yield 3))

(iter-defun generator-test--count (n)
  (let ((i 0))
    (while (< i n)
      (iter-yield i)
      (setq i (1+ i)))))

(ert-deftest generator-test/doc16-round23-straight-line ()
  (should (equal '(1 2 3) (generator-test--drain (generator-test--straight)))))

(ert-deftest generator-test/doc16-round23-loop ()
  (should (equal '(0 1 2 3) (generator-test--drain (generator-test--count 4)))))

(ert-deftest generator-test/doc16-round23-iter-lambda ()
  (let ((g (funcall (iter-lambda () (iter-yield 'a) (iter-yield 'b)))))
    (should (equal '(a b) (generator-test--drain g)))))

(provide 'generator-test)

;;; generator-test.el ends here
