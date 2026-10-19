;;; nelisp-cc-eval-is-truthy.el --- nl_eval_is_truthy AOT swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The public truth predicate returns 0 for Lisp nil, 1 for every other
;; value, and -1 for evaluation failure. Doc 215 consumes the evaluator's
;; WORD directly. No temporary result slot may outlive a collecting form.

;;; Code:

(defconst nelisp-cc-eval-is-truthy--source
  '(seq
    ;; Doc 215: consume the evaluator's WORD directly. A private physical
    ;; FORM is a canonical input WORD. A scratch can be swept while it
    ;; collects and then reused as an arena
    ;; block header; writing a legacy result back into it corrupts the heap.
    ;; No safepoint follows the returned value before this immediate test.
    ;; 0 is evaluation failure (including flagless abort), 3 is Lisp nil.
    (defun nl_eval_is_truthy (form env)
      (let* ((word (extern-call nelisp_eval_call_w form env 0)))
        (if (= word 0) -1 (if (= word 3) 0 1)))))
  "AOT truth predicate over the evaluator's WORD result.")

(provide 'nelisp-cc-eval-is-truthy)
;;; nelisp-cc-eval-is-truthy.el ends here
