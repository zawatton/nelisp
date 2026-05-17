;;; nelisp-cc-cons-construct.el --- Doc 101 §101.D cons constructor swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 101 §101.D moves `(cons A B)' construction into a
;; Phase-47-compiled elisp object.  The body allocates a fresh
;; `NlConsBox', copies the caller-owned car/cdr Sexp slots into the
;; box, and writes `Sexp::Cons(box)' into the caller-owned result
;; slot.

;;; Code:

(defconst nelisp-cc-cons-construct--source
  '(defun nelisp_cons_construct (arg0 arg1 result-slot)
     ;; arg0: *const Sexp pointing at the car value (= caller-owned,
     ;; will be cloned into the box).
     ;; arg1: *const Sexp pointing at the cdr value.
     ;; result-slot: *mut Sexp 32-byte slot — will hold the new
     ;; Sexp::Cons(_).
     ;;
     ;; MVP refcount note: this constructor assumes the caller passes
     ;; owned Sexp values whose inner boxed payloads already carry the
     ;; needed refcount bumps on the Rust side.  The raw 32-byte copies
     ;; here do not call `nl_rc_inc'; a future §101.D.2 stage can make
     ;; the copies refcount-aware if profiling justifies the extra cost.
     (cons-make arg0 arg1 result-slot))
  "Phase 47 source for the Doc 101 §101.D cons constructor swap.")

(provide 'nelisp-cc-cons-construct)

;;; nelisp-cc-cons-construct.el ends here
