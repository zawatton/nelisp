;;; nelisp-cc-jit-concat-ints.el --- AOT body for nl_jit_concat_ints  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 86 §86.1.e.2 (2026-05-19): AOT elisp migration of
;; `nl_jit_concat_ints' from `build-tool/src/jit/strings.rs'.
;;
;; Trampoline signature: `(*const Sexp, *mut Sexp) -> i64'
;; (OK=0 / ERR=1), reached via `nl-jit-call-out-1' from
;; `nelisp-jit-strategy.el'.
;;
;; Algorithm:
;;   1. Call `mut-str-make-empty out 0' to allocate a fresh MutStr
;;      builder in the `*out' slot.
;;   2. Walk the cons list (arg: *const Sexp) recursively via the
;;      helper `nl_jit_concat_ints_walk':
;;      a. Nil base case → return 0 (OK sentinel).
;;      b. Cons cell → read car tag; if not Int (SEXP_TAG_INT = 2),
;;         return 1 (TRAMPOLINE_ERR).  Read the Int codepoint value
;;         via `sexp-int-unwrap (sexp-payload-ptr arg)'.  Skip
;;         codepoints that are negative, > 0x10FFFF, or in the
;;         surrogate range 0xD800-0xDFFF; push all others via
;;         `mut-str-push-codepoint'.  Advance to the cdr by deriving
;;         `*const Sexp' for the cdr field at `(+ (sexp-payload-ptr
;;         arg) 32)' (= NlConsBox.cdr at offset 32), then recurse.
;;      c. Any other tag → return 1 (TRAMPOLINE_ERR).
;;   3. If the walk returned 0 (OK), call `mut-str-finalize out out'
;;      to clone the inner String into a fresh `Sexp::Str(s)' and
;;      write it back to `*out', then return 0 (TRAMPOLINE_OK).
;;   4. If the walk returned 1 (ERR), return 1 (TRAMPOLINE_ERR).
;;
;; Codepoint validity matches Rust's `char::from_u32' semantics:
;; codepoints outside 0x0000-0xD7FF U 0xE000-0x10FFFF are silently
;; skipped (not pushed as U+FFFD), preserving the Rust contract
;; "Lossy on invalid codepoints; ERR on shape" (= `strings.rs' doc).
;;
;; Grammar ops consumed (all existing — no new opcode needed):
;;   `sexp-tag'               — read tag byte at offset 0.
;;   `sexp-payload-ptr'       — read NlConsBox* (payload ptr at offset 8).
;;   `sexp-int-unwrap'        — read i64 at Int-payload offset 8.
;;   `mut-str-make-empty'     — §122.B: alloc fresh MutStr builder.
;;   `mut-str-push-codepoint' — §122.B: UTF-8 encode + append.
;;   `mut-str-finalize'       — §122.B: clone MutStr→Str, write to slot.
;;   AOT arith: `+', `<', `>', `=', `>=', `<=', `and', `if', `or'
;;
;; Tag constants (pinned by §62.5 ABI assert tests in
;; `tests/sexp_repr.rs'):
;;   SEXP_TAG_NIL  = 0
;;   SEXP_TAG_INT  = 2
;;   SEXP_TAG_CONS = 7
;;
;; Build wiring:
;;   `scripts/compile-elisp-objects.el' manifest: entry →
;;   `nl_jit_concat_ints.o' (Linux x86_64/aarch64 + macOS aarch64).
;;   `build-tool/build.rs' manifest_sources: entry for
;;   `nelisp-cc-jit-concat-ints.el'.
;;   `build-tool/src/jit/bridge.rs': `strings_link' module with
;;   `extern "C" fn nl_jit_concat_ints' + dispatch update.
;;   `build-tool/src/jit/strings.rs': Rust body deleted.

;;; Code:

(defconst nelisp-cc-jit-concat-ints--source
  '(seq
    ;; ---- list walker -------------------------------------------------------
    ;;
    ;; Tail-recursive: walk the cons list `arg' (*const Sexp), pushing
    ;; valid codepoints from each car (must be Sexp::Int) into the
    ;; MutStr builder at `*out'.
    ;;
    ;; Returns:
    ;;   0 — success (Nil base case or full list consumed).
    ;;   1 — shape error (non-Nil/non-Cons mid-list, or car not Int).
    ;;
    ;; Invalid codepoints (negative, > 0x10FFFF, or in the surrogate
    ;; range 55296-57343) are silently skipped — `if' returns 1
    ;; (truthy `and' sentinel) without calling `mut-str-push-codepoint',
    ;; matching `char::from_u32' None semantics in the Rust trampoline.
    ;;
    ;; Cdr access: `(+ (sexp-payload-ptr arg) 32)' gives `*const Sexp'
    ;; for the cdr field at `NlConsBox' offset 32 (= sizeof::<Sexp>()).
    (defun nl_jit_concat_ints_walk (arg out)
      (if (= (sexp-tag arg) 0)
          ;; Nil → base case, list fully consumed.
          0
        (if (= (sexp-tag arg) 7)
            ;; Cons → check that car is Int (tag = 2).
            (if (= (sexp-tag (sexp-payload-ptr arg)) 2)
                (and
                 ;; Push codepoint or skip if invalid.  Both branches
                 ;; return 1 (truthy) so `and' continues to the
                 ;; recursive call.
                 (if (or (< (sexp-int-unwrap (sexp-payload-ptr arg)) 0)
                         (> (sexp-int-unwrap (sexp-payload-ptr arg)) 1114111)
                         (and (>= (sexp-int-unwrap (sexp-payload-ptr arg)) 55296)
                              (<= (sexp-int-unwrap (sexp-payload-ptr arg)) 57343)))
                     1
                   (mut-str-push-codepoint out
                                           (sexp-int-unwrap
                                            (sexp-payload-ptr arg))))
                 ;; Advance to cdr and recurse.
                 (nl_jit_concat_ints_walk (+ (sexp-payload-ptr arg) 32)
                                          out))
              ;; car not Int → ERR.
              1)
          ;; Not Nil / not Cons → ERR.
          1)))

    ;; ---- public trampoline -------------------------------------------------
    ;;
    ;; Signature: (arg: *const Sexp, out: *mut Sexp) -> i64
    ;; Returns 0 (TRAMPOLINE_OK) or 1 (TRAMPOLINE_ERR).
    ;;
    ;; Steps:
    ;;   1. mut-str-make-empty: write Sexp::MutStr(_) to *out.
    ;;   2. Walk the list; if walk returns 0 (OK):
    ;;      a. mut-str-finalize: clone MutStr → Str, overwrite *out.
    ;;      b. Return 0 (TRAMPOLINE_OK).
    ;;   3. Walk returned 1 (ERR) → return 1 (TRAMPOLINE_ERR).
    (defun nl_jit_concat_ints (arg out)
      (and
       (mut-str-make-empty out 0)
       (if (= (nl_jit_concat_ints_walk arg out) 0)
           (and (mut-str-finalize out out) 0)
         1))))
  "AOT source for the `nl_jit_concat_ints' trampoline.

Two-entry `(seq DEFUN ...)' manifest:
  `nl_jit_concat_ints_walk (arg out)' — recursive cons-list walker.
  `nl_jit_concat_ints (arg out)'      — public trampoline.

Algorithm: build Sexp::Str from a proper list of Int codepoints.
Invalid codepoints (negative / surrogate / > U+10FFFF) are silently
skipped, matching the Rust `char::from_u32' None path in the
former `strings.rs' body.  Shape errors (non-Cons mid-list, car
not Int) return TRAMPOLINE_ERR = 1.

Tag constants: SEXP_TAG_NIL=0, SEXP_TAG_INT=2, SEXP_TAG_CONS=7.
Cdr field offset inside NlConsBox = 32 (= sizeof::<Sexp>()).")

(provide 'nelisp-cc-jit-concat-ints)

;;; nelisp-cc-jit-concat-ints.el ends here
