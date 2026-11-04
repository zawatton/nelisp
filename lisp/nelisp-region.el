;;; nelisp-region.el --- Scoped no-GC ownership regions (Doc 237) -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; `nelisp-with-region' is the supported path to the `nelisp--region-*'
;; primitives: it opens a region, runs BODY inside `unwind-protect', and
;; closes the region on every exit (normal, `throw', `signal', quit) unless
;; BODY already closed it or moved it to a new owner (then the old handle is
;; no longer live and the cleanup does nothing).

;;; Code:

(defmacro nelisp-with-region (spec &rest body)
  "Run BODY with VAR bound to a fresh region of BYTES bytes.
SPEC is (VAR BYTES).  BYTES is evaluated once, before the region is opened.
The region is closed on every exit unless `nelisp--region-live-p' says VAR's
handle is no longer live (closed, or moved away by BODY)."
  (declare (indent 1))
  (unless (and (consp spec) (symbolp (car spec)) (consp (cdr spec)) (null (cddr spec)))
    (signal 'wrong-number-of-arguments (list 'nelisp-with-region spec)))
  (let ((var (car spec)) (bytes (cadr spec)))
    `(let ((,var (nelisp--region-open ,bytes)))
       (unwind-protect
           (progn ,@body)
         (when (nelisp--region-live-p ,var)
           (nelisp--region-close ,var))))))

(provide 'nelisp-region)
;;; nelisp-region.el ends here
