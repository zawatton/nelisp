;;; restart-resume.el --- nl-condition demo: a restart that resumes -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Distinctive feature of `nl-condition' (docs/design/169-nl-prelude.org
;; Phase 4): `nl-handler-bind' runs its handler BEFORE the stack
;; unwinds, so the handler can decide a replacement value and hand it
;; to a restart established close to the fault -- and execution
;; resumes from there instead of aborting the whole computation.
;; Elisp's plain `condition-case' cannot do this: by the time its
;; handler runs, everything between the signal and the handler has
;; already unwound, so there is nowhere left to resume.
;;
;; This walks a list summing numbers.  One entry is not a number.
;; `nl-demo-coerce' offers a `use-value' restart around each element;
;; an outer handler, bound once around the whole walk, decides the
;; substitute value at the exact point the fault happened, and the sum
;; continues -- every element after the bad one still contributes.
;;
;; Runs unchanged on host Emacs and on the standalone binary (measured
;; in packages/nl-condition/README.org's Testing section); see
;; packages/nl-condition/test/nl-condition-example-test.el for the ERT
;; coverage that keeps this working, on both substrates.

;;; Code:

(require 'nl-condition)

(define-error 'nl-demo-not-a-number "value is not a number")

(defun nl-demo-coerce (x)
  "Return X if it is a number.
Otherwise signal `nl-demo-not-a-number', offering a `use-value'
restart that a handler further out can invoke with a replacement."
  (nl-restart-case
      (if (numberp x)
          x
        (nl-signal 'nl-demo-not-a-number (list x)))
    (use-value (replacement) replacement)))

(defun nl-demo-sum-resuming (items &optional fallback)
  "Sum ITEMS, substituting FALLBACK (default 0) for non-numbers.
The substitution happens via `nl-invoke-restart', so the walk resumes
exactly where `nl-demo-coerce' faulted -- nothing after the bad
element is skipped or lost."
  (nl-handler-bind ((nl-demo-not-a-number
                      (lambda (_c)
                        (nl-invoke-restart 'use-value (or fallback 0)))))
    (let ((total 0))
      (dolist (item items total)
        (setq total (+ total (nl-demo-coerce item)))))))

(provide 'nl-condition-restart-resume-demo)

;;; restart-resume.el ends here
