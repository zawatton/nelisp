;;; hello-utils.el --- Toy helper for NeLisp Phase 5-A.5 E2E -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Minimal helper package loaded by `hello.el' via NeLisp `require'.
;; Demonstrates the deeper level of a 2-level dependency tree
;; (hello -> hello-utils) under `nelisp-require' with the host
;; `load-path' disabled.  No host Elisp is referenced directly —
;; every symbol used here is part of NeLisp's primitive set (`concat',
;; `list', `car', `cdr', etc.), so cycle-tests that walk this source
;; through NeLisp's own evaluator succeed without any extra glue.

;;; Code:

(defun hello-utils-format-greeting (salutation name)
  "Compose a greeting from SALUTATION and NAME.
Returns the string \"SALUTATION, NAME!\"."
  (concat salutation ", " name "!"))

(defun hello-utils-excited (text)
  "Return TEXT with a trailing emphasis marker."
  (concat text " !!!"))

(defun hello-utils-join-words (words)
  "Join a list of WORDS with single spaces.
Returns the empty string when WORDS is nil."
  (cond
   ((null words) "")
   ((null (cdr words)) (car words))
   (t (concat (car words) " " (hello-utils-join-words (cdr words))))))

(provide 'hello-utils)

;;; hello-utils.el ends here
