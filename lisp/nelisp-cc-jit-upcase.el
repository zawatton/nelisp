;;; nelisp-cc-jit-upcase.el --- AOT body for nl_jit_upcase  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; AOT elisp migration of `nl_jit_upcase' from
;; `build-tool/src/jit/strings.rs'.
;;
;; Trampoline signature: `(*const Sexp, *mut Sexp) -> i64'
;; (OK=0 / ERR=1), reached via `nl-jit-call-out-1' from
;; `nelisp-jit-strategy.el'.
;;
;; Algorithm:
;;   1. Read the input arg tag via `sexp-tag'.  Accept Str (5),
;;      MutStr (6), and Symbol (4).  Also accept Nil (0) → "NIL" and
;;      T (1) → "T" (matching the former `read_text' helper +
;;      to_uppercase semantics).  Any other tag → return 1 (ERR).
;;   2. Walk the bytes of the source string via `str-bytes-ptr' +
;;      `str-len' + `ptr-read-u8'.  For each byte b:
;;        if 97 <= b <= 122 (ASCII a-z): push (b - 32) (= uppercase).
;;        else: push b as-is (non-ASCII bytes pass through unchanged).
;;   3. mut-str-finalize out out → write Sexp::Str to *out.
;;   4. Return 0 (TRAMPOLINE_OK).
;;
;; Non-ASCII semantics: bytes >= 0x80 pass through unchanged
;; (ASCII-only fold).  This differs from Rust's `to_uppercase'
;; (e.g. ß→SS) but matches the task spec "non-ASCII passthrough".
;;
;; Note on Nil/T: Rust's `read_text' returns "nil"/"t" (lowercase),
;; and `to_uppercase' → "NIL"/"T".  We match that here.
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
;;   `nl_jit_upcase.o' (Linux x86_64 only).
;;   `build-tool/build.rs' manifest_sources: entry for
;;   `nelisp-cc-jit-upcase.el'.
;;   `build-tool/src/jit/bridge.rs': `extern "C" fn nl_jit_upcase'
;;   + `_ELISP_ARCHIVE_ANCHOR' entry.
;;   `build-tool/src/jit/strings.rs': Rust body deleted.

;;; Code:

(defconst nelisp-cc-jit-upcase--source
  '(seq
    ;; ---- byte walker -------------------------------------------------------
    ;;
    ;; Walk bytes of src, uppercase ASCII a-z (97-122 → -32), pass
    ;; all other bytes (including non-ASCII >= 0x80) as-is.
    (defun nl_jit_upcase_walk (src_ptr src_len out idx)
      (if (= idx src_len)
          1
        (and
         (if (and (>= (ptr-read-u8 src_ptr idx) 97)
                  (<= (ptr-read-u8 src_ptr idx) 122))
             (mut-str-push-byte out (- (ptr-read-u8 src_ptr idx) 32))
           (mut-str-push-byte out (ptr-read-u8 src_ptr idx)))
         (nl_jit_upcase_walk src_ptr src_len out (+ idx 1)))))

    ;; ---- public trampoline -------------------------------------------------
    ;;
    ;; Signature: (arg: *const Sexp, out: *mut Sexp) -> i64
    ;; Returns 0 (TRAMPOLINE_OK) or 1 (TRAMPOLINE_ERR).
    (defun nl_jit_upcase (arg out)
      (if (= (sexp-tag arg) 0)
          ;; Nil → "NIL" (3 bytes: 78 73 76)
          (and
           (mut-str-make-empty out 3)
           (mut-str-push-byte out 78)
           (mut-str-push-byte out 73)
           (mut-str-push-byte out 76)
           (mut-str-finalize out out)
           0)
        (if (= (sexp-tag arg) 1)
            ;; T → "T" (1 byte: 84)
            (and
             (mut-str-make-empty out 1)
             (mut-str-push-byte out 84)
             (mut-str-finalize out out)
             0)
          (if (or (= (sexp-tag arg) 4)
                  (= (sexp-tag arg) 5)
                  (= (sexp-tag arg) 6))
              ;; Str / MutStr / Symbol → walk bytes with ASCII fold
              (and
               (mut-str-make-empty out 0)
               (nl_jit_upcase_walk (str-bytes-ptr arg)
                                   (str-len arg)
                                   out
                                   0)
               (mut-str-finalize out out)
               0)
            ;; Any other tag → ERR
            1)))))
  "AOT source for the `nl_jit_upcase' trampoline.

Two-entry `(seq DEFUN ...)' manifest:
  `nl_jit_upcase_walk (src_ptr src_len out idx)' — byte walker.
  `nl_jit_upcase (arg out)'                      — public trampoline.

Algorithm: ASCII a-z fold (bytes 97-122 → -32); non-ASCII bytes pass
through unchanged.  Nil→\"NIL\", T→\"T\" via constant byte push.
Tag constants: SEXP_TAG_NIL=0, SEXP_TAG_T=1, SEXP_TAG_SYMBOL=4,
SEXP_TAG_STR=5, SEXP_TAG_MUT_STR=6.")

(provide 'nelisp-cc-jit-upcase)

;;; nelisp-cc-jit-upcase.el ends here
