;;; nelisp-cc-jit-elt.el --- Doc 120 §120.D elt swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 120 §120.D — Phase-47-compiled replacement for the Rust
;; `nl_jit_access_elt' trampoline in `build-tool/src/jit/access.rs'.
;; Same `(*const Sexp, i64, *mut Sexp) -> i64' contract:
;;
;;   1. idx < 0:    TRAMPOLINE_ERR (= 1).
;;   2. Vector arm: bounds-check + refcount-aware `vector-ref' (=
;;      same as `aref' Vector arm).
;;   3. Cons arm:   recursive cons-list walk via
;;      `cons-cdr-raw-from-box'.  `sexp-payload-ptr' seeds the
;;      walker with `NlConsBox*' for Cons, 0 for everything else.
;;      The walker stops when (a) box ptr hits 0 (= proper-list
;;      end or non-Cons mid-chain) → ERR, or (b) idx countdown
;;      hits 0 on a live box → refcount-aware `nl_sexp_clone_into'
;;      of `box.car' into `*out' → OK.
;;
;; `NlConsBox.car' lives at offset 0 (see
;; `nelisp-nlconsbox--offset-car'), so the `NlConsBox*' raw
;; pointer cast to `*const Sexp' lands directly on the car field
;; — same trick `cons-cdr-raw-from-box' uses on the cdr edge.
;;
;; Tag-byte constant: 8 = `nelisp-sexp--tag-vector'.

;;; Code:

(defconst nelisp-cc-jit-elt--source
  '(seq
    (defun nelisp_jit_elt_walk (cur-box idx out)
      ;; cur-box: *const NlConsBox (raw payload pointer) or 0.
      ;; idx: i64 countdown.  out: *mut Sexp.  Returns: i64 = 0 OK,
      ;; 1 ERR (= walk fell off the proper-list end).
      (if (= cur-box 0)
          1
        (if (= idx 0)
            ;; NlConsBox.car at offset 0 — cur-box (NlConsBox*) cast
            ;; as *const Sexp points at the car field directly.
            (and (extern-call nl_sexp_clone_into cur-box out) 0)
          (nelisp_jit_elt_walk
           (cons-cdr-raw-from-box cur-box)
           (- idx 1)
           out))))
    (defun nelisp_jit_elt (arg idx out)
      ;; arg: *const Sexp (Vector or Cons or other).  idx: i64.
      ;; out: *mut Sexp.  Returns: i64 = 0 OK, 1 ERR.
      (if (< idx 0)
          1
        (if (= (sexp-tag arg) 8)
            (if (< idx (vector-len arg))
                (and (vector-ref arg idx out) 0)
              1)
          (nelisp_jit_elt_walk
           (sexp-payload-ptr arg)
           idx
           out)))))
  "Phase 47 source for the §120.D `nl_jit_access_elt' swap.

Vector arm shares `aref's bounds-checked `vector-ref' shape.
Cons arm uses a recursive walker that consumes the cons list one
step per call, terminating either on (a) a non-Cons cdr (= box
pointer hits 0 → ERR) or (b) idx countdown reaching 0 (=
refcount-aware copy of `box.car' into `*out' → OK).

The walker reuses the §101.B `cons-cdr-raw-from-box' /
`sexp-payload-ptr' grammar pieces; the only access-specific bit
is the car copy at the OK arm, threaded through
`nl_sexp_clone_into' via `extern-call' (= same refcount helper
`vector-ref' uses internally).  Non-Vector / non-Cons inputs
fall into the walker with `cur-box = 0' and return ERR
immediately — strategy.el's `condition-case' dispatcher then
routes by `type-of'.")

(provide 'nelisp-cc-jit-elt)

;;; nelisp-cc-jit-elt.el ends here
