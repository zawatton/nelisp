;;; nelisp-eval-closure-standalone-smoke.el --- requiring nelisp-eval keeps closures intact -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Regression smoke for a name collision: src/nelisp-eval.el once defined
;; `nelisp--closure-p', which is also the native closure-kind builtin the
;; prelude relies on.  After the require, every interpreted closure
;; reported `(type-of c)' = cons with `closurep' and `functionp' nil.  The
;; interpreter's own predicate is now `nelisp-eval-closure-p'.
;;
;; Run from the repository root:
;;
;;   ./target/nelisp --load test/nelisp-eval-closure-standalone-smoke.el
;;
;; Prints `nelisp-eval-closure-standalone-smoke: PASS'; any failure signals
;; an error so the process exits non-zero.

;;; Code:

(setq load-path (append (list (expand-file-name "src") (expand-file-name "lisp"))
                        load-path))
(require 'nelisp-eval)

(defun nelisp-eval-closure-smoke--check (label ok)
  (unless ok
    (error "nelisp-eval-closure-standalone-smoke: FAIL %s" label)))

(let ((fresh (eval '(lambda (a) a) t)))
  (nelisp-eval-closure-smoke--check "closurep fresh lambda" (closurep fresh))
  (nelisp-eval-closure-smoke--check "functionp fresh lambda" (functionp fresh))
  (nelisp-eval-closure-smoke--check "type-of fresh lambda"
                                    (eq (type-of fresh) 'interpreted-function)))
(defun nelisp-eval-closure-smoke--fn (a) a)
(nelisp-eval-closure-smoke--check
 "closurep defun"
 (closurep (symbol-function 'nelisp-eval-closure-smoke--fn)))

(princ "nelisp-eval-closure-standalone-smoke: PASS\n")

;;; nelisp-eval-closure-standalone-smoke.el ends here
