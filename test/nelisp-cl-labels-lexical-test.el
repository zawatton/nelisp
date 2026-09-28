;;; nelisp-cl-labels-lexical-test.el --- `cl-labels' lexical function binding gate  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The prelude's `cl-labels' used to emulate local functions by
;; `defalias'-ing NAME globally and restoring (or `fmakunbound'-ing) the
;; global cell on exit.  GNU `named-let' (lisp/emacs-lisp/subr-x.el)
;; expands to `(funcall (cl-labels ((NAME ...)) #'NAME) ARGS...)': the
;; function escapes the `cl-labels' form before it is called, so under
;; the global-cell emulation every `named-let' loop signalled
;; `(void-function NAME)'.  Loading GNU bytecomp.el hit this in
;; `byte-compile--first-symbol-with-pos' (`named-let loop'), aborting the
;; load-time self-compile bootstrap with `(void-function loop)'.
;;
;; Every case runs on the built standalone binary from a lexical-binding
;; file, and every expected value was checked against GNU Emacs.

;;; Code:

(require 'ert)

(defun nelisp-cl-labels-lexical--run (forms)
  "Load FORMS from a lexical-binding file on the standalone; return output."
  (let ((binary (expand-file-name (or (getenv "NELISP_BIN") "target/nelisp")
                                  default-directory))
        (file (make-temp-file "nelisp-cl-labels-" nil ".el")))
    (unless (file-executable-p binary)
      (ert-skip "standalone binary is not built; standalone-reader gate owns it"))
    (unwind-protect
        (progn
          (with-temp-file file
            ;; `kill-emacs' keeps --load from echoing the last value.
            (insert ";;; -*- lexical-binding: t; -*-\n" forms
                    "\n(kill-emacs 0)\n"))
          (with-temp-buffer
            (let ((rc (call-process binary nil t nil "--load" file)))
              (unless (= rc 0)
                (ert-fail (format "standalone load failed: rc=%S output=%S"
                                  rc (buffer-string))))
              (string-trim (buffer-string)))))
      (delete-file file))))

(defmacro nelisp-cl-labels-lexical--deftest (name forms expected)
  "Define ERT test NAME: FORMS must print EXPECTED on the standalone."
  `(ert-deftest ,name ()
     (should (equal (nelisp-cl-labels-lexical--run ,forms) ,expected))))

;; The exact expansion GNU `named-let' produces: the function escapes.
(nelisp-cl-labels-lexical--deftest
 nelisp-cl-labels-lexical-escaping-function
 "(princ (funcall (cl-labels ((loop (x n) (if x (loop (cdr x) (1+ n)) n))) #'loop) '(a b c) 0))"
 "3")

;; The `byte-compile--first-symbol-with-pos' shape, via GNU's macro body.
(nelisp-cl-labels-lexical--deftest
 nelisp-cl-labels-lexical-gnu-named-let-shape
 "(defmacro nl-test-named-let (name bindings &rest body)
    `(funcall (cl-labels ((,name ,(mapcar #'car bindings) ,@body)) #',name)
              ,@(mapcar #'cadr bindings)))
  (defun nl-test-depth (form)
    (nl-test-named-let loop ((form form) (depth 0))
      (if (consp form)
          (max (loop (car form) (1+ depth)) (loop (cdr form) depth))
        depth)))
  (princ (list (nl-test-depth '(a (b (c)))) (fboundp 'loop)))"
 "(3 nil)")

(nelisp-cl-labels-lexical--deftest
 nelisp-cl-labels-lexical-returned-closure
 "(let ((g (cl-labels ((h (x) (* 2 x))) #'h))) (princ (funcall g 21)))"
 "42")

(nelisp-cl-labels-lexical--deftest
 nelisp-cl-labels-lexical-mutual-recursion
 "(princ (cl-labels ((ev (n) (if (= n 0) t (od (1- n))))
                    (od (n) (if (= n 0) nil (ev (1- n)))))
          (list (ev 10) (od 7))))"
 "(t t)")

;; Inner binding shadows outer; quoted data is not rewritten.
(nelisp-cl-labels-lexical--deftest
 nelisp-cl-labels-lexical-shadowing-and-quote
 "(princ (cl-labels ((f (x) (list 'outer x)))
          (list (f 1) (cl-labels ((f (x) (list 'inner x))) (f 2)) '(f 3))))"
 "((outer 1) (inner 2) (f 3))")

;; `#'NAME' passed to a mapping function, and no global cell is touched.
(nelisp-cl-labels-lexical--deftest
 nelisp-cl-labels-lexical-sharp-quote-arg-no-global
 "(defun nl-test-w () 'global)
  (princ (list (cl-labels ((nl-test-w (x) (if (consp x) (apply #'+ (mapcar #'nl-test-w x)) 1)))
                 (nl-test-w '(a (b c) ((d)))))
               (nl-test-w)))"
 "(4 global)")

;; Local calls inside nested special forms and macros GNU expands through:
;; `pcase' patterns, `cl-loop', `condition-case' handlers, backquote,
;; `#'NAME' in a nested lambda, a forward reference to a later sibling,
;; and variables sharing a local function's name (separate namespaces).
(nelisp-cl-labels-lexical--deftest
 nelisp-cl-labels-lexical-nested-forms
 "(princ (list
    (cl-labels ((f (x) (pcase x (`(,a . ,b) (+ (f a) (f b))) ((pred numberp) x) (_ 0))))
      (f '(1 (2 . 3) 4)))
    (cl-labels ((sq (x) (* x x))) (cl-loop for i in '(1 2 3) collect (sq i)))
    (cl-labels ((g (x) (1+ x))) (condition-case nil (/ 1 0) (arith-error (g 41))))
    (cl-labels ((g (x) (list 'g x))) `(a ,(g 1) ,@(mapcar #'g '(2 3))))
    (cl-labels ((g (x) (* 3 x))) (let ((h (lambda (y) (funcall #'g (g y))))) (funcall h 2)))
    (cl-labels ((a (n) (if (> n 0) (b (1- n)) 'done)) (b (n) (a n))) (a 3))
    (let ((g 1)) (cl-labels ((g (x) (+ x 100))) (cond (g (g g)))))
    (cl-labels ((g (x) (+ x 1))) (cl-flet ((h (y) (g y))) (h 1)))))"
 "(10 (1 4 9) 42 (a (g 1) (g 2) (g 3)) 18 done 101 2)")

;; nelisp-emacs-lib's `emacs-parity-macroexpand.el' replaces the global
;; `macroexpand-all' with a walker that leaves `(function ...)' opaque.
;; `cl-labels' used to expand each local function body through that global
;; name, so every call inside a body stayed a plain call and signalled
;; `(void-function fail-key)' in `nelisp-rx--match-from'.  The prelude
;; walker must be immune to such a replacement.  (GNU `cl-labels' itself
;; relies on its `macroexpand-all'; the expected value is GNU's result
;; without the replacement.)
(nelisp-cl-labels-lexical--deftest
 nelisp-cl-labels-lexical-survives-shallow-macroexpand-all
 "(defun macroexpand-all (form &optional environment)
    (cond ((not (consp form)) form)
          ((memq (car form) '(quote function)) form)
          (t (let ((e (macroexpand-1 form environment)))
               (if (eq e form)
                   (mapcar (lambda (x) (macroexpand-all x environment)) form)
                 (macroexpand-all e environment))))))
  (defun nl-test-memo (n)
    (let ((failed (make-hash-table :test 'equal)))
      (cl-labels ((fail-key (i) (* 2 i))
                  (walk (i) (if (gethash (fail-key i) failed)
                                nil
                              (mapcar (lambda (x) (fail-key x)) (list i (fail-key i))))))
        (walk n))))
  (princ (list (nl-test-memo 3) (fboundp 'fail-key)))"
 "((6 12) nil)")

(provide 'nelisp-cl-labels-lexical-test)

;;; nelisp-cl-labels-lexical-test.el ends here
