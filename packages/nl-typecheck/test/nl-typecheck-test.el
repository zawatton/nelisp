;;; nl-typecheck-test.el --- ERT tests for the Doc 236 checker -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nl-typecheck)

(defun nl-typecheck-test--check (source &optional suffix)
  "Check SOURCE written to a temp file with SUFFIX; return diagnostics."
  (let ((file (make-temp-file "nl-tc-" nil (or suffix ".el"))))
    (unwind-protect
        (progn
          (with-temp-file file (insert source))
          (nl-typecheck-file file))
      (delete-file file))))

(defun nl-typecheck-test--levels (source)
  "Return the list of diagnostic levels for SOURCE."
  (mapcar (lambda (d) (plist-get d :level)
            ) (nl-typecheck-test--check source)))

(ert-deftest nl-typecheck-literal-clash-is-error ()
  (let ((d (nl-typecheck-test--check "(defun f () (car 5))\n")))
    (should (= (length d) 1))
    (should (eq (plist-get (car d) :level) 'error))))

(ert-deftest nl-typecheck-declared-clash-and-format ()
  (let* ((d (nl-typecheck-test--check
             "(defun g (n) (declare (ftype (function (integer) integer))) n)\n\n(defun h () (g \"s\"))\n"))
         (line (nl-typecheck-format-diagnostic (car d))))
    (should (= (length d) 1))
    (should (string-match "\\`.*:3:[0-9]+: error: " line))))

(ert-deftest nl-typecheck-inferred-clash-is-warning ()
  (should (equal (nl-typecheck-test--levels
                  "(defun f () (let ((x \"a\")) (car x)))\n")
                 '(warning))))

(ert-deftest nl-typecheck-prim-result-propagates-as-warning ()
  (should (equal (nl-typecheck-test--levels
                  "(defun f (l) (let ((n (length l))) (car n)))\n")
                 '(warning))))

(ert-deftest nl-typecheck-gradual-t-never-errors ()
  (should-not (nl-typecheck-test--check
               "(defun f (x) (car x) (+ x 1) (concat x \"a\"))\n(defun k () (unknown-fn 1 2) (funcall (car (list 1)) 3))\n")))

(ert-deftest nl-typecheck-note-only-under-strict ()
  (let ((src "(defun g (n) (declare (ftype (function (integer) integer))) n)\n(defun h (v) (g v))\n"))
    (should-not (nl-typecheck-test--check src))
    (let ((nelisp-typecheck-strict t))
      (should (equal (mapcar (lambda (d) (plist-get d :level))
                             (nl-typecheck-test--check src))
                     '(note))))))

(ert-deftest nl-typecheck-arity-error ()
  (let ((d (nl-typecheck-test--check
            "(defun g (a) (declare (ftype (function (integer) integer))) a)\n(defun h () (g 1 2) (g))\n")))
    (should (= (length d) 2))
    (should (cl-every (lambda (x) (eq (plist-get x :level) 'error)) d))))

(ert-deftest nl-typecheck-result-clash-is-warning ()
  (should (equal (nl-typecheck-test--levels
                  "(defun g (a) (declare (ftype (function (integer) string))) (+ a 1))\n")
                 '(warning))))

(ert-deftest nl-typecheck-narrowing-predicates ()
  ;; Each guarded use is clean; the same use unguarded on a typed param errors.
  (dolist (case '(("consp" "(car x)") ("stringp" "(length x)") ("integerp" "(1+ x)")
                  ("symbolp" "(symbol-name x)") ("listp" "(car x)")
                  ("vectorp" "(aref x 0)") ("numberp" "(1+ x)")
                  ("fixnump" "(1+ x)")))
    (let ((src (format "(defun f (x) (if (%s x) %s nil))\n" (car case) (cadr case))))
      (should-not (nl-typecheck-test--check src))))
  ;; Narrowing produces an inferred type, so a clash is a warning.
  (should (equal (nl-typecheck-test--levels
                  "(defun f (x) (if (stringp x) (car x) nil))\n")
                 '(warning)))
  (should (equal (nl-typecheck-test--levels
                  "(defun f (x) (cond ((integerp x) (car x)) (t nil)))\n")
                 '(warning)))
  (should (equal (nl-typecheck-test--levels
                  "(defun f (x) (when (and (consp x) (stringp (car x))) (1+ x)))\n")
                 '(warning)))
  (should (equal (nl-typecheck-test--levels
                  "(defun f (x) (unless (null x) (if (eq x 'a) (car x) nil)))\n")
                 '(warning)))
  (should (equal (nl-typecheck-test--levels
                  "(defun f (x) (if (cl-typep x 'string) (car x) nil))\n")
                 '(warning))))

(ert-deftest nl-typecheck-setq-widens ()
  (should-not (nl-typecheck-test--check
               "(defun f () (let ((x 1)) (setq x \"a\") (length x)))\n")))

(ert-deftest nl-typecheck-cl-the ()
  (should (equal (nl-typecheck-test--levels
                  "(defun f () (cl-the integer \"s\"))\n")
                 '(error)))
  (should-not (nl-typecheck-test--check
               "(defun f (x) (1+ (cl-the integer x)))\n")))

(ert-deftest nl-typecheck-nl-typed-and-declare-type ()
  (should (equal (nl-typecheck-test--levels
                  "(nl-typed defun f ((n integer)) n)\n(defun h () (f \"s\"))\n")
                 '(error)))
  (should (equal (nl-typecheck-test--levels
                  "(nelisp-declare-type 'ext (function (string) t))\n(defun h () (ext 5))\n")
                 '(error))))

(ert-deftest nl-typecheck-side-file ()
  (let* ((dir (make-temp-file "nl-tc-dir" t))
         (src (expand-file-name "lib.el" dir))
         (side (expand-file-name "lib-types.el" dir)))
    (unwind-protect
        (progn
          (with-temp-file side
            (insert "(nelisp-declare-type 'lib-fn (function (string) t))\n"))
          (with-temp-file src (insert "(defun user () (lib-fn 5))\n"))
          (should (equal (mapcar (lambda (d) (plist-get d :level))
                                 (nl-typecheck-file src))
                         '(error))))
      (delete-directory dir t))))

(ert-deftest nl-typecheck-reads-el-gz ()
  (let* ((dir (make-temp-file "nl-tc-gz" t))
         (plain (expand-file-name "z.el" dir))
         (gz (concat plain ".gz")))
    (unwind-protect
        (progn
          (with-temp-file plain (insert "(defun f () (car 5))\n"))
          (if (not (executable-find "gzip"))
              (ert-skip "gzip not available")
            (should (eq 0 (call-process "gzip" nil nil nil plain)))
            (should (equal (mapcar (lambda (d) (plist-get d :level))
                                   (nl-typecheck-file gz))
                           '(error)))))
      (delete-directory dir t))))

(ert-deftest nl-typecheck-unexpandable-form-is-skipped-silently ()
  (should-not (nl-typecheck-test--check
               "(defun f () (cl-loop for))\n(defvar v (cl-loop for))\n"))
  (should (>= (plist-get nl-typecheck-last-stats :skipped) 1)))

(ert-deftest nl-typecheck-untyped-plain-elisp-is-clean ()
  (should-not
   (nl-typecheck-test--check
    "(defun a (l) (let ((n 0)) (dolist (x l) (setq n (+ n (length x)))) n))
(defun b (s) (if (stringp s) (concat s \"x\") (format \"%S\" s)))
(defun c (v) (let ((r nil)) (dotimes (i (length v)) (push (aref v i) r)) (nreverse r)))
(defun d (h k) (or (gethash k h) (puthash k (list k) h)))
(defun e (x) (cond ((null x) 0) ((consp x) (length x)) ((stringp x) (string-to-number x)) (t 1)))
")))

(ert-deftest nl-typecheck-fixtures ()
  (let ((dir (expand-file-name "fixtures" (file-name-directory (or load-file-name buffer-file-name)))))
    (should (cl-some (lambda (d) (eq (plist-get d :level) 'error))
                     (nl-typecheck-file (expand-file-name "clash.el" dir))))
    (should-not (nl-typecheck-file (expand-file-name "clash-fixed.el" dir)))
    (should (equal (mapcar (lambda (d) (plist-get d :level))
                           (nl-typecheck-file (expand-file-name "typed-warning.el" dir)))
                   '(warning)))))

(provide 'nl-typecheck-test)
;;; nl-typecheck-test.el ends here
