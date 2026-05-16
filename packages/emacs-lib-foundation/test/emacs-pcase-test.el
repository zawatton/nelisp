;;; emacs-pcase-test.el --- Tests for emacs-pcase  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the minimal `pcase' port split out of `emacs-stub.el'.
;; Under batch host Emacs the vendor `pcase' family wins by default, so
;; a few tests temporarily unbind the relevant symbols and reload the
;; module to exercise the local helpers and stub macroexpansions.

;;; Code:

(require 'ert)
(require 'emacs-pcase)

(defconst emacs-pcase-test--module-file
  (expand-file-name "../src/emacs-pcase.el"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defun emacs-pcase-test--with-reloaded-module (symbols thunk)
  "Reload `emacs-pcase' with SYMBOLS temporarily unbound, then call THUNK."
  (let ((saved nil))
    (unwind-protect
        (progn
          (dolist (sym symbols)
            (push (cons sym (and (fboundp sym) (symbol-function sym))) saved)
            (fmakunbound sym))
          (load emacs-pcase-test--module-file t t)
          (funcall thunk))
      (dolist (cell saved)
        (if (cdr cell)
            (fset (car cell) (cdr cell))
          (fmakunbound (car cell)))))))

;;;; Load / feature contract

(ert-deftest emacs-pcase-test/require-loads-cleanly ()
  (should (featurep 'emacs-pcase))
  (should (featurep 'pcase))
  (should (fboundp 'pcase))
  (should (fboundp 'pcase-let))
  (should (fboundp 'pcase-let*))
  (should (fboundp 'pcase-dolist)))

;;;; Helper coverage

(ert-deftest emacs-pcase-test/test-helper-covers-basic-patterns ()
  (emacs-pcase-test--with-reloaded-module
   '(pcase)
   (lambda ()
     (should (equal (emacs-pcase--test '_ 'v) '(t)))
     (should (equal (emacs-pcase--test 'sym 'v) '(t (sym v))))
     (should (equal (emacs-pcase--test 7 'v) '((equal v 7))))
     (should (equal (emacs-pcase--test "x" 'v) '((equal v "x")))))))

(ert-deftest emacs-pcase-test/test-helper-covers-quote-pred-and-bare-cons ()
  (emacs-pcase-test--with-reloaded-module
   '(pcase)
   (lambda ()
     (should (equal (emacs-pcase--test '(quote q) 'v) '((eq v 'q))))
     (should (equal (emacs-pcase--test '(pred symbolp) 'v) '((funcall #'symbolp v))))
     (should (equal (emacs-pcase--test '(foo . bar) 'v) '(t))))))

(ert-deftest emacs-pcase-test/test-helper-covers-and-and-or ()
  (emacs-pcase-test--with-reloaded-module
   '(pcase)
   (lambda ()
     (should (equal (emacs-pcase--and '(sym (quote :a)) 'v)
                    '((and t (eq v ':a)) (sym v))))
     (should (equal (emacs-pcase--or '((quote :a) (quote :b)) 'v)
                    '((or (eq v ':a) (eq v ':b))))))))

(ert-deftest emacs-pcase-test/test-helper-covers-backquote-comma-and-comma-at ()
  (emacs-pcase-test--with-reloaded-module
   '(pcase)
   (lambda ()
     (should (equal (emacs-pcase--backquote '(comma x) 'v) '(t (x v))))
     (should (equal (emacs-pcase--backquote '(comma-at rest) 'v) '(t (rest v)))))))

(ert-deftest emacs-pcase-test/test-helper-covers-backquote-nested-cons-and-tail ()
  (emacs-pcase-test--with-reloaded-module
   '(pcase)
   (lambda ()
     (should (equal (emacs-pcase--backquote 'foo 'v) '((equal v 'foo))))
     (should (equal (emacs-pcase--backquote '(a (comma x) (comma-at rest) nil) 'v)
                    '((and (consp v)
                           (equal (car v) 'a)
                           (and (consp (cdr v))
                                t
                                (and (consp (cdr (cdr v)))
                                     t
                                     (and (consp (cdr (cdr (cdr v))))
                                          (null (car (cdr (cdr (cdr v)))))
                                          (null (cdr (cdr (cdr (cdr v)))))))))
                      (x (car (cdr v)))
                      (rest (car (cdr (cdr v))))))))))

(ert-deftest emacs-pcase-test/stubs-macroexpand-to-simple-forms ()
  (emacs-pcase-test--with-reloaded-module
   '(pcase pcase-let pcase-let* pcase-dolist)
   (lambda ()
     (should (equal (macroexpand-1 '(pcase-let ((x 1)) x))
                    '(let ((x 1)) x)))
     (should (equal (macroexpand-1 '(pcase-let* ((x 1)) x))
                    '(let* ((x 1)) x)))
     (should (equal (macroexpand-1 '(pcase-dolist (x '(1 2)) x))
                    '(dolist (x '(1 2)) x))))))

(ert-deftest emacs-pcase-test/pcase-expands-and-evaluates ()
  (emacs-pcase-test--with-reloaded-module
   '(pcase)
   (lambda ()
     (let ((expanded (macroexpand '(pcase x ((quote :a) 1) ('b 2) (_ 3)))))
       (should (eq 'let (car expanded)))
       (should (memq 'cond (flatten-tree expanded)))
       (should (equal (let ((x :a))
                        (pcase x ((quote :a) 1) ('b 2) (_ 3)))
                      1))
       (should (equal (let ((x 'b))
                        (pcase x ((quote :a) 1) ('b 2) (_ 3)))
                      2))
       (should (equal (let ((x 99))
                        (pcase x ((quote :a) 1) ('b 2) (_ 3)))
                      3))))))

(provide 'emacs-pcase-test)

;;; emacs-pcase-test.el ends here
