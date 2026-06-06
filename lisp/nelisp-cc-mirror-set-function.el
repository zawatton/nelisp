;;; nelisp-cc-mirror-set-function.el --- Doc 111 §111.E #8 mirror_set_function  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 111 §111.E Group B helper #8 — `mirror_set_function'.  AOT
;; reimplementation of the env-mirror entry function-slot writer (=
;; slot 1 instead of slot 0).  Otherwise identical structure to helper
;; #7 `mirror_set_value' (see that file for the rationale + ABI deps).
;;
;; Source-level: replace `0' with `1' as the slot index passed to
;; `record-slot-set'.  Compiles to a tiny separate `.o' so the link
;; line tracks one symbol per helper (= ar archive level granularity).

;;; Code:

(defconst nelisp-cc-mirror-set-function--source
  '(seq
    (defun nelisp_mirror_set_function_apply (entry-ptr val-ptr)
      ;; Inner: dispatch on entry-ptr.  Slot 1 = function cell.
      ;; `record-slot-set' returns rax=1 (Doc 111 §111.E enabler) so
      ;; the `(and X 1)' chain reliably returns 1 after the write.
      (if (= entry-ptr 0)
          0
        (and (record-slot-set entry-ptr 1 val-ptr) 1)))
    (defun nelisp_mirror_set_function (mirror-ptr sym-ptr val-ptr)
      ;; mirror-ptr: *const Sexp pointing at the env-mirror Record.
      ;; sym-ptr:    *const Sexp pointing at the Sexp::Symbol / Str.
      ;; val-ptr:    *const Sexp pointing at the new function value.
      ;;
      ;; Returns: i64.  1 on hit (slot 1 overwritten in place), 0 on
      ;; miss (entry not found — caller falls back to install).
      (nelisp_mirror_set_function_apply
       (extern-call nelisp_mirror_lookup_entry mirror-ptr sym-ptr)
       val-ptr)))
  "AOT source for Doc 111 §111.E #8 `mirror_set_function'.

Compose-on-7 with slot index 1 (= the function-cell slot, vs. the
value-cell slot 0).  Returns i64 1 on hit / 0 on miss.")

(provide 'nelisp-cc-mirror-set-function)

;;; nelisp-cc-mirror-set-function.el ends here
