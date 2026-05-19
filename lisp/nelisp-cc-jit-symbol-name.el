;;; nelisp-cc-jit-symbol-name.el --- Phase 47 body for nl_jit_symbol_name  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 122 §122.A — Phase 47 elisp migration of `nl_jit_symbol_name'
;; from `build-tool/src/jit/strings.rs'.
;;
;; Trampoline signature: `(*const Sexp, *mut Sexp) -> i64'
;; (OK=0 / ERR=1), reached via `nl-jit-call-out-1' from
;; `nelisp-jit-strategy.el::symbol-name'.
;;
;; Contract (mirrors Rust `read_sexp_str' + `nl_jit_symbol_name'):
;;   Symbol (tag 4) → copy the symbol's string content into *out as Str.
;;   Str    (tag 5) → copy the string content into *out as Str.
;;   Nil    (tag 0) → write literal "nil" into *out as Str.
;;   T      (tag 1) → write literal "t" into *out as Str.
;;   Other          → return 1 (TRAMPOLINE_ERR).
;;
;; MutStr (tag 6) is intentionally not handled: it is an internal
;; type never passed by `symbol-name' callers (per strings.rs comment).
;;
;; Algorithm for Symbol/Str arms:
;;   - `str-bytes-ptr arg' returns a `*const u8' over all three string-y
;;     variants (Str/Symbol/MutStr) via `nl_str_bytes_ptr'.
;;   - `str-len arg' reads the byte count from the inline String length
;;     field (offset 24 from Sexp start); valid for tags 4 and 5.
;;   - `sexp-write-str out ptr len' allocates a fresh Sexp::Str(String)
;;     and writes it into *out.
;;
;; Algorithm for Nil/T literals:
;;   Two-level fill+write helper pair (= Phase 47 `let'-is-compile-time
;;   constraint forces runtime values through function parameters):
;;     nl_sym_name_fill_nil (out buf)   — write 3 bytes "nil", then free.
;;     nl_sym_name_write_nil (out)      — alloc 3 bytes, thread to fill.
;;     nl_sym_name_fill_t   (out buf)   — write 1 byte "t", then free.
;;     nl_sym_name_write_t  (out)       — alloc 1 byte, thread to fill.
;;
;; Grammar ops consumed (all existing — no new opcode needed):
;;   `sexp-tag'         — read tag byte at offset 0.
;;   `str-len'          — read String byte count from inline field.
;;   `str-bytes-ptr'    — §122.H: dispatch-safe bytes pointer.
;;   `sexp-write-str'   — §122.A: allocate Sexp::Str and write to slot.
;;   `alloc-bytes'      — §125.A: heap allocate raw byte buffer.
;;   `ptr-write-u8'     — §125.A: write one byte at offset.
;;   `dealloc-bytes'    — §125.A: free raw byte buffer.
;;   Phase 47 arith: `=', `or', `if', `and'
;;
;; Tag constants (§62.5 ABI assert tests in `tests/sexp_repr.rs'):
;;   SEXP_TAG_NIL    = 0
;;   SEXP_TAG_T      = 1
;;   SEXP_TAG_SYMBOL = 4
;;   SEXP_TAG_STR    = 5
;;
;; Build wiring:
;;   `scripts/compile-elisp-objects.el' manifest: entry →
;;   `nl_jit_symbol_name.o' (Linux x86_64).
;;   `build-tool/build.rs' manifest_sources: entry for
;;   `nelisp-cc-jit-symbol-name.el'.
;;   `build-tool/src/jit/bridge.rs': extern "C" fn nl_jit_symbol_name +
;;   _ELISP_ARCHIVE_ANCHOR entry + count bump (62 → 63 if replacing
;;   last Rust body; actual count depends on HEAD state).
;;   `build-tool/src/jit/strings.rs': nl_jit_symbol_name Rust body deleted.

;;; Code:

(defconst nelisp-cc-jit-symbol-name--source
  '(seq
    ;; ---- Nil → "nil" literal writer ------------------------------------
    ;;
    ;; Two-level pattern (required by Phase 47's compile-time-only `let'):
    ;;   fill: receives the pre-allocated 3-byte buffer, fills "nil", writes
    ;;         to *out via sexp-write-str, frees the buffer, returns 0.
    ;;   write: allocates the buffer and threads it into fill.
    ;;
    ;; ASCII: n=110, i=105, l=108
    (defun nl_sym_name_fill_nil (out buf)
      (and (ptr-write-u8 buf 0 110)
           (ptr-write-u8 buf 1 105)
           (ptr-write-u8 buf 2 108)
           (sexp-write-str out buf 3)
           (dealloc-bytes buf 3 1)
           0))
    (defun nl_sym_name_write_nil (out)
      (nl_sym_name_fill_nil out (alloc-bytes 3 1)))

    ;; ---- T → "t" literal writer ----------------------------------------
    ;;
    ;; Same two-level pattern with a 1-byte buffer.
    ;; ASCII: t=116
    (defun nl_sym_name_fill_t (out buf)
      (and (ptr-write-u8 buf 0 116)
           (sexp-write-str out buf 1)
           (dealloc-bytes buf 1 1)
           0))
    (defun nl_sym_name_write_t (out)
      (nl_sym_name_fill_t out (alloc-bytes 1 1)))

    ;; ---- public trampoline -----------------------------------------------
    ;;
    ;; Signature: (arg: *const Sexp, out: *mut Sexp) -> i64
    ;; Returns 0 (TRAMPOLINE_OK) on success, 1 (TRAMPOLINE_ERR) on
    ;; unsupported tag.
    ;;
    ;; Tags 4 (Symbol) and 5 (Str): copy via str-bytes-ptr + str-len.
    ;; Tag 0 (Nil): write literal "nil".
    ;; Tag 1 (T):   write literal "t".
    ;; Anything else: return 1 (TRAMPOLINE_ERR).
    (defun nl_jit_symbol_name (arg out)
      (if (or (= (sexp-tag arg) 4) (= (sexp-tag arg) 5))
          (and (sexp-write-str out (str-bytes-ptr arg) (str-len arg)) 0)
        (if (= (sexp-tag arg) 0)
            (nl_sym_name_write_nil out)
          (if (= (sexp-tag arg) 1)
              (nl_sym_name_write_t out)
            1)))))
  "Phase 47 source for the `nl_jit_symbol_name' trampoline.

Five-entry `(seq DEFUN ...)' manifest:
  `nl_sym_name_fill_nil (out buf)' — fill 3-byte buffer with \"nil\" + write.
  `nl_sym_name_write_nil (out)'    — alloc 3 bytes, thread to fill_nil.
  `nl_sym_name_fill_t   (out buf)' — fill 1-byte buffer with \"t\" + write.
  `nl_sym_name_write_t  (out)'     — alloc 1 byte, thread to fill_t.
  `nl_jit_symbol_name   (arg out)' — public trampoline.

Contract: Symbol(4)/Str(5) → copy content as Sexp::Str; Nil(0) → \\\"nil\\\";
T(1) → \\\"t\\\"; anything else → TRAMPOLINE_ERR(1).  MutStr(6) unsupported
(internal type, never passed by `symbol-name' callers).

Replaces the 9-LOC Rust body in `build-tool/src/jit/strings.rs'.")

(provide 'nelisp-cc-jit-symbol-name)

;;; nelisp-cc-jit-symbol-name.el ends here
