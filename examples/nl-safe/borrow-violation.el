;;; borrow-violation.el --- nl-safe demo: violation caught at the moment it happens -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Distinctive feature of `nl-safe' (docs/design/170-nl-safe.org
;; Stage 1): "sharing XOR mutation" is enforced with runtime borrow
;; counters, so a conflicting borrow raises `nl-borrow-error' at the
;; exact call that violates the rule -- not on the next access, and
;; not as silently corrupted state.
;;
;; This demo opens a shared borrow on a cell, then attempts an
;; exclusive borrow of the SAME cell while the shared one is still
;; open.  The violation is caught right there by a `condition-case'
;; wrapped tightly around the offending form: the write inside it
;; never runs, the cell's value is unchanged, and the event lands in
;; `nl-safe--violation-log' with the full context (which cell, what
;; was already held, what was requested).
;;
;; Runs unchanged on host Emacs and on the standalone binary (measured
;; in packages/nl-safe/README.org's Testing section); see
;; packages/nl-safe/test/nl-safe-example-test.el for the ERT coverage
;; that keeps this working, on both substrates.

;;; Code:

(require 'nl-safe)

(nl-defcell nl-demo-counter 10)

(defun nl-demo-borrow-violation ()
  "Open a shared borrow, then try an exclusive one on the same cell.
Returns a plist describing what happened at the moment of the clash:

  :caught       non-nil iff `nl-borrow-error' was signaled
  :value-during the cell's value seen through the open shared borrow
  :value-after  the cell's value once both borrows have exited
  :log-entry    the violation-log record for the rejected attempt,
                or nil"
  (let ((nl-safe-log-violations t)
        (nl-safe--violation-log nil)
        caught
        value-during)
    (nl-with-borrow (shared nl-demo-counter)
      (setq value-during shared)
      (condition-case err
          (nl-with-borrow-mut (excl nl-demo-counter)
            ;; Never reached: `nl-with-borrow-mut' signals before this
            ;; body runs, because the shared borrow above is still open.
            (nl-cell-set nl-demo-counter (1+ excl)))
        (nl-borrow-error (setq caught err))))
    (list :caught (and caught t)
          :value-during value-during
          :value-after (nl-with-borrow (v nl-demo-counter) v)
          :log-entry (car nl-safe--violation-log))))

(provide 'nl-safe-borrow-violation-demo)

;;; borrow-violation.el ends here
