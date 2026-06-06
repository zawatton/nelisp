;;; nelisp-cc-jit-downcase.el --- AOT body for nl_jit_downcase  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; AOT elisp migration of `nl_jit_downcase' from
;; `build-tool/src/jit/strings.rs'.
;;
;; Trampoline signature: `(*const Sexp, *mut Sexp) -> i64'
;; (OK=0 / ERR=1), reached via `nl-jit-call-out-1' from
;; `nelisp-jit-strategy.el'.
;;
;; Algorithm:
;;   1. Read the input arg tag via `sexp-tag'.  Accept Str (5),
;;      MutStr (6), and Symbol (4).  Also accept Nil (0) → "nil" and
;;      T (1) → "t" (matching the former `read_text' helper).  Any
;;      other tag → return 1 (TRAMPOLINE_ERR).
;;   2. Walk the bytes of the source string via `str-bytes-ptr' +
;;      `str-len' + `ptr-read-u8'.  For each byte b:
;;        if 65 <= b <= 90 (ASCII A-Z): push (b + 32) (= lowercase).
;;        else: push b as-is (non-ASCII bytes pass through unchanged).
;;   3. mut-str-finalize out out → write Sexp::Str to *out.
;;   4. Return 0 (TRAMPOLINE_OK).
;;
;; Non-ASCII semantics: bytes >= 0x80 are passed through unchanged
;; (ASCII-only fold, matching the task spec "non-ASCII passthrough").
;; This differs from Rust's Unicode-aware `to_lowercase' (e.g. ß→ss)
;; but is acceptable per the task design decision.
;;
;; Tag constants (pinned by §62.5 ABI assert tests):
;;   SEXP_TAG_NIL    = 0
;;   SEXP_TAG_T      = 1
;;   SEXP_TAG_SYMBOL = 4
;;   SEXP_TAG_STR    = 5
;;   SEXP_TAG_MUT_STR = 6
;;
;; Build wiring:
;;   `scripts/compile-elisp-objects.el' manifest: entry →
;;   `nl_jit_downcase.o' (Linux x86_64 only).
;;   `build-tool/build.rs' manifest_sources: entry for
;;   `nelisp-cc-jit-downcase.el'.
;;   `build-tool/src/jit/bridge.rs': `extern "C" fn nl_jit_downcase'
;;   + `_ELISP_ARCHIVE_ANCHOR' entry.
;;   `build-tool/src/jit/strings.rs': Rust body deleted.

;;; Code:

(defconst nelisp-cc-jit-downcase--source
  '(seq
    ;; ---- nil/t literal bytes helper ----------------------------------------
    ;;
    ;; Given out and a char-by-char constant, write Sexp::Str literal.
    ;; For Nil→"nil" and T→"t" we build directly from constants.
    ;;
    ;; Helper: walk bytes of src (a str/mut-str/symbol arg), lowercase
    ;; ASCII A-Z (65-90 → +32), pass all other bytes as-is, push into out.
    (defun nl_jit_downcase_walk (src_ptr src_len out idx)
      (if (= idx src_len)
          1
        (and
         (if (and (>= (ptr-read-u8 src_ptr idx) 65)
                  (<= (ptr-read-u8 src_ptr idx) 90))
             (mut-str-push-byte out (+ (ptr-read-u8 src_ptr idx) 32))
           (mut-str-push-byte out (ptr-read-u8 src_ptr idx)))
         (nl_jit_downcase_walk src_ptr src_len out (+ idx 1)))))

    ;; ---- public trampoline --------------------------------------------------
    ;;
    ;; Signature: (arg: *const Sexp, out: *mut Sexp) -> i64
    ;; Returns 0 (TRAMPOLINE_OK) or 1 (TRAMPOLINE_ERR).
    (defun nl_jit_downcase (arg out)
      (if (= (sexp-tag arg) 0)
          ;; Nil → "nil" (3 bytes: 110 105 108)
          (and
           (mut-str-make-empty out 3)
           (mut-str-push-byte out 110)
           (mut-str-push-byte out 105)
           (mut-str-push-byte out 108)
           (mut-str-finalize out out)
           0)
        (if (= (sexp-tag arg) 1)
            ;; T → "t" (1 byte: 116)
            (and
             (mut-str-make-empty out 1)
             (mut-str-push-byte out 116)
             (mut-str-finalize out out)
             0)
          (if (or (= (sexp-tag arg) 4)
                  (= (sexp-tag arg) 5)
                  (= (sexp-tag arg) 6))
              ;; Str / MutStr / Symbol → walk bytes with ASCII fold
              (and
               (mut-str-make-empty out 0)
               (nl_jit_downcase_walk (str-bytes-ptr arg)
                                     (str-len arg)
                                     out
                                     0)
               (mut-str-finalize out out)
               0)
            ;; Any other tag → ERR
            1)))))
  "AOT source for the `nl_jit_downcase' trampoline.

Two-entry `(seq DEFUN ...)' manifest:
  `nl_jit_downcase_walk (src_ptr src_len out idx)' — byte walker.
  `nl_jit_downcase (arg out)'                      — public trampoline.

Algorithm: ASCII A-Z fold (bytes 65-90 → +32); non-ASCII bytes pass
through unchanged.  Nil→\"nil\", T→\"t\" via constant byte push.
Tag constants: SEXP_TAG_NIL=0, SEXP_TAG_T=1, SEXP_TAG_SYMBOL=4,
SEXP_TAG_STR=5, SEXP_TAG_MUT_STR=6.")

(provide 'nelisp-cc-jit-downcase)

;;; nelisp-cc-jit-downcase.el ends here
