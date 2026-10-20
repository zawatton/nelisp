;;; nelisp-bytecode-noop-unwind-test.el --- Empty unwind boundary -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'ert)
(require 'cl-lib)
(defvar nelisp-bytecode-noop-unwind-test--function nil)

(defun nelisp-bytecode-noop-unwind-test--host (form)
  (if (not (consp form)) form
    (cons (if (eq (car form) 'seq) 'progn (car form))
          (mapcar #'nelisp-bytecode-noop-unwind-test--host (cdr form)))))

(with-temp-buffer
  (insert-file-contents
   (or (getenv "NELISP_UNWIND_TEST_SOURCE")
       (expand-file-name "../scripts/nelisp-standalone-build.el"
                         (file-name-directory (or load-file-name buffer-file-name)))))
  (search-forward "(defun wf_bytecode_unwind_step")
  (goto-char (match-beginning 0))
  (setq nelisp-bytecode-noop-unwind-test--function
        (eval (cons 'lambda
                    (cddr (nelisp-bytecode-noop-unwind-test--host
                           (read (current-buffer))))) t)))

(ert-deftest nelisp-bytecode-noop-unwind-needs-no-scratch-roots ()
  (let (written)
    (cl-letf (((symbol-function 'wf_bytecode_w_eqw)
               (lambda (a b) (if (and (= a 100) (= b 3)) 1 0)))
              ((symbol-function 'nl_root_mark)
               (lambda (&rest _) (error "No-op unwind requested scratch roots")))
              ((symbol-function 'wf_write_int)
               (lambda (slot value) (setq written (list slot value)))))
      (should (= (funcall nelisp-bytecode-noop-unwind-test--function 1 100 3 7 42 0 0) 0))
      (should (equal written '(42 7))))))

(ert-deftest nelisp-bytecode-unmatched-unwind-retains-rooted-error-path ()
  (let ((roots 0) released copied written)
    (cl-letf (((symbol-function 'wf_bytecode_w_eqw) (lambda (&rest _) 0))
              ((symbol-function 'bf_eq2) (lambda (&rest _) 0))
              ((symbol-function 'nl_root_mark) (lambda (_) 9))
              ((symbol-function 'nl_root_reserve) (lambda (_) (cl-incf roots)))
              ((symbol-function 'ptr-read-u64) (lambda (&rest _) 0))
              ((symbol-function 'wf_bytecode_copy) (lambda (a b) (setq copied (list a b))))
              ((symbol-function 'wf_write_int) (lambda (a b) (setq written (list a b))))
              ((symbol-function 'nl_root_release) (lambda (a b) (setq released (list a b)))))
      (should (= (funcall nelisp-bytecode-noop-unwind-test--function 1 200 3 7 42 0 0) 1))
      (should (= roots 3))
      (should (equal copied '(200 3)))
      (should (equal written '(42 7)))
      (should (equal released '(1 9))))))
