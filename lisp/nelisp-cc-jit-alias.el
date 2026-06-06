;;; nelisp-cc-jit-alias.el --- AOT nl_jit_alias: JIT name→canonical map  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 133 §133.A — AOT migration of the `alias' helper from
;; `build-tool/src/jit/bridge.rs'.  Also migrates `as_name' (inline
;; type-guard) into the same object, removing both from Rust.
;;
;; Function contract:
;;   nl_jit_alias (name: *const Sexp, out: *mut Sexp) -> i64
;;     Accepts a `Sexp::Symbol' (tag 4) OR `Sexp::Str' (tag 5) holding
;;     the caller-facing JIT function name.  Writes the canonical (dlsym-
;;     resolvable) name as `Sexp::Str' into *out.
;;     Returns 0 (TRAMPOLINE_OK) on success.
;;     Returns 1 (TRAMPOLINE_ERR) if name is not Symbol or Str.
;;
;; Alias table (mirrors bridge.rs::alias verbatim):
;;   nelisp_jit_car              → nelisp_jit_cons_car
;;   nelisp_jit_cdr              → nelisp_jit_cons_cdr
;;   nelisp_jit_setcar           → nelisp_jit_cons_setcar
;;   nelisp_jit_setcdr           → nelisp_jit_cons_setcdr
;;   nelisp_jit_eq_inline        → nelisp_jit_predicate_eq
;;   nelisp_jit_cons             → nelisp_jit_cons_make
;;   nelisp_jit_type_of          → nl_jit_type_of
;;   nelisp_jit_sxhash           → nl_jit_sxhash
;;   nelisp_jit_intern           → nl_jit_intern
;;   nelisp_jit_symbol_name      → nl_jit_symbol_name
;;   nelisp_jit_make_symbol      → nl_jit_make_symbol
;;   nelisp_jit_syscall          → nl_jit_syscall_call
;;   nelisp_jit_syscall_supported_p → nl_jit_syscall_supported_p
;;   (other)                     → identity (copy input name)
;;
;; AOT ops consumed (all existing):
;;   `sexp-name-eq'     — Doc 133 §133.B new op: Symbol(4)/Str(5) tag
;;                        + length + byte-loop compare.  Replaces the
;;                        Rust `alias(name: &str) -> &str' match logic.
;;   `sexp-tag'         — tag byte at offset 0.
;;   `str-bytes-ptr'    — §122.H: bytes pointer for copy (identity case).
;;   `str-len'          — String::len byte count.
;;   `sexp-write-str'   — §122.A: allocate Sexp::Str and write to slot.
;;   `alloc-bytes'      — §125.A: heap-allocate raw byte buffer.
;;   `ptr-write-u8'     — §125.A: byte write at offset.
;;   `dealloc-bytes'    — §125.A: free raw byte buffer.
;;
;; Shared buffer discipline:
;;   All alias target strings are ≤ 26 bytes (longest:
;;   "nl_jit_syscall_supported_p").  Each write helper allocates a
;;   26-byte buffer (alloc-bytes 26 1), fills the needed N bytes,
;;   writes via `sexp-write-str' with N, frees with (dealloc-bytes 26 1).
;;   The identity-copy case passes the input sexp's bytes pointer and
;;   length directly to `sexp-write-str' (no extra allocation needed).
;;
;; Build wiring:
;;   `scripts/compile-elisp-objects.el' manifest: entry →
;;   `nl_jit_alias.o' (Linux x86_64).
;;   `build-tool/build.rs' manifest_sources: entry for
;;   `nelisp-cc-jit-alias.el'.
;;   `build-tool/src/jit/bridge.rs':
;;     extern "C" fn nl_jit_alias(name: *const Sexp, out: *mut Sexp) -> i64
;;     _ELISP_ARCHIVE_ANCHOR entry + count bump (67 → 68).
;;     Rust functions removed: `as_name' + `alias'.
;;     `unified_fn_ptr' signature changed to accept `*const Sexp',
;;     body changed to call `nl_jit_alias', extract Sexp::Str,
;;     then CString::new + dlsym.

;;; Code:

;; Byte constants (ASCII) used by the alias write helpers below.
;; Organised into groups matching the shared prefix of each target name
;; for readability.  All values are compile-time i64 immediates.
;;
;; Common prefix "nelisp_jit_cons_" bytes (16):
;;   n=110 e=101 l=108 i=105 s=115 p=112 _=95 j=106 i=105 t=116 _=95
;;   c=99 o=111 n=110 s=115 _=95
;; Common prefix "nl_jit_" bytes (7):
;;   n=110 l=108 _=95 j=106 i=105 t=116 _=95

(defconst nelisp-cc-jit-alias--source
  '(seq

    ;; ---- Write helper: allocate a 26-byte buffer ----------------------------
    ;;
    ;; Returns *mut u8 buffer pointer (= i64).
    ;; Caller is responsible for dealloc-bytes buf 26 1 after sexp-write-str.
    (defun nl_jit_alias_buf ()
      (alloc-bytes 26 1))

    ;; ---- Alias write helper: "nelisp_jit_cons_car" (19 bytes) ---------------
    ;;
    ;; ASCII: n e l i s p _ j i t _ c o n s _ c a r
    ;;       110 101 108 105 115 112 95 106 105 116 95 99 111 110 115 95 99 97 114
    (defun nl_jit_alias_write_cons_car (out buf)
      (and (ptr-write-u8 buf  0 110) (ptr-write-u8 buf  1 101) (ptr-write-u8 buf  2 108)
           (ptr-write-u8 buf  3 105) (ptr-write-u8 buf  4 115) (ptr-write-u8 buf  5 112)
           (ptr-write-u8 buf  6  95) (ptr-write-u8 buf  7 106) (ptr-write-u8 buf  8 105)
           (ptr-write-u8 buf  9 116) (ptr-write-u8 buf 10  95) (ptr-write-u8 buf 11  99)
           (ptr-write-u8 buf 12 111) (ptr-write-u8 buf 13 110) (ptr-write-u8 buf 14 115)
           (ptr-write-u8 buf 15  95) (ptr-write-u8 buf 16  99) (ptr-write-u8 buf 17  97)
           (ptr-write-u8 buf 18 114)
           (sexp-write-str out buf 19)
           (dealloc-bytes buf 26 1)
           0))

    ;; ---- Alias write helper: "nelisp_jit_cons_cdr" (19 bytes) ---------------
    ;;
    ;; ASCII: n e l i s p _ j i t _ c o n s _ c d r
    ;;       110 101 108 105 115 112 95 106 105 116 95 99 111 110 115 95 99 100 114
    (defun nl_jit_alias_write_cons_cdr (out buf)
      (and (ptr-write-u8 buf  0 110) (ptr-write-u8 buf  1 101) (ptr-write-u8 buf  2 108)
           (ptr-write-u8 buf  3 105) (ptr-write-u8 buf  4 115) (ptr-write-u8 buf  5 112)
           (ptr-write-u8 buf  6  95) (ptr-write-u8 buf  7 106) (ptr-write-u8 buf  8 105)
           (ptr-write-u8 buf  9 116) (ptr-write-u8 buf 10  95) (ptr-write-u8 buf 11  99)
           (ptr-write-u8 buf 12 111) (ptr-write-u8 buf 13 110) (ptr-write-u8 buf 14 115)
           (ptr-write-u8 buf 15  95) (ptr-write-u8 buf 16  99) (ptr-write-u8 buf 17 100)
           (ptr-write-u8 buf 18 114)
           (sexp-write-str out buf 19)
           (dealloc-bytes buf 26 1)
           0))

    ;; ---- Alias write helper: "nelisp_jit_cons_setcar" (22 bytes) ------------
    ;;
    ;; ASCII: n e l i s p _ j i t _ c o n s _ s e t c a r
    ;;       110 101 108 105 115 112 95 106 105 116 95 99 111 110 115 95 115 101 116 99 97 114
    (defun nl_jit_alias_write_cons_setcar (out buf)
      (and (ptr-write-u8 buf  0 110) (ptr-write-u8 buf  1 101) (ptr-write-u8 buf  2 108)
           (ptr-write-u8 buf  3 105) (ptr-write-u8 buf  4 115) (ptr-write-u8 buf  5 112)
           (ptr-write-u8 buf  6  95) (ptr-write-u8 buf  7 106) (ptr-write-u8 buf  8 105)
           (ptr-write-u8 buf  9 116) (ptr-write-u8 buf 10  95) (ptr-write-u8 buf 11  99)
           (ptr-write-u8 buf 12 111) (ptr-write-u8 buf 13 110) (ptr-write-u8 buf 14 115)
           (ptr-write-u8 buf 15  95) (ptr-write-u8 buf 16 115) (ptr-write-u8 buf 17 101)
           (ptr-write-u8 buf 18 116) (ptr-write-u8 buf 19  99) (ptr-write-u8 buf 20  97)
           (ptr-write-u8 buf 21 114)
           (sexp-write-str out buf 22)
           (dealloc-bytes buf 26 1)
           0))

    ;; ---- Alias write helper: "nelisp_jit_cons_setcdr" (22 bytes) ------------
    ;;
    ;; ASCII: n e l i s p _ j i t _ c o n s _ s e t c d r
    ;;       110 101 108 105 115 112 95 106 105 116 95 99 111 110 115 95 115 101 116 99 100 114
    (defun nl_jit_alias_write_cons_setcdr (out buf)
      (and (ptr-write-u8 buf  0 110) (ptr-write-u8 buf  1 101) (ptr-write-u8 buf  2 108)
           (ptr-write-u8 buf  3 105) (ptr-write-u8 buf  4 115) (ptr-write-u8 buf  5 112)
           (ptr-write-u8 buf  6  95) (ptr-write-u8 buf  7 106) (ptr-write-u8 buf  8 105)
           (ptr-write-u8 buf  9 116) (ptr-write-u8 buf 10  95) (ptr-write-u8 buf 11  99)
           (ptr-write-u8 buf 12 111) (ptr-write-u8 buf 13 110) (ptr-write-u8 buf 14 115)
           (ptr-write-u8 buf 15  95) (ptr-write-u8 buf 16 115) (ptr-write-u8 buf 17 101)
           (ptr-write-u8 buf 18 116) (ptr-write-u8 buf 19  99) (ptr-write-u8 buf 20 100)
           (ptr-write-u8 buf 21 114)
           (sexp-write-str out buf 22)
           (dealloc-bytes buf 26 1)
           0))

    ;; ---- Alias write helper: "nelisp_jit_predicate_eq" (23 bytes) -----------
    ;;
    ;; ASCII: n e l i s p _ j i t _ p r e d i c a t e _ e q
    ;;       110 101 108 105 115 112 95 106 105 116 95 112 114 101 100 105 99 97 116 101 95 101 113
    (defun nl_jit_alias_write_predicate_eq (out buf)
      (and (ptr-write-u8 buf  0 110) (ptr-write-u8 buf  1 101) (ptr-write-u8 buf  2 108)
           (ptr-write-u8 buf  3 105) (ptr-write-u8 buf  4 115) (ptr-write-u8 buf  5 112)
           (ptr-write-u8 buf  6  95) (ptr-write-u8 buf  7 106) (ptr-write-u8 buf  8 105)
           (ptr-write-u8 buf  9 116) (ptr-write-u8 buf 10  95) (ptr-write-u8 buf 11 112)
           (ptr-write-u8 buf 12 114) (ptr-write-u8 buf 13 101) (ptr-write-u8 buf 14 100)
           (ptr-write-u8 buf 15 105) (ptr-write-u8 buf 16  99) (ptr-write-u8 buf 17  97)
           (ptr-write-u8 buf 18 116) (ptr-write-u8 buf 19 101) (ptr-write-u8 buf 20  95)
           (ptr-write-u8 buf 21 101) (ptr-write-u8 buf 22 113)
           (sexp-write-str out buf 23)
           (dealloc-bytes buf 26 1)
           0))

    ;; ---- Alias write helper: "nelisp_jit_cons_make" (20 bytes) --------------
    ;;
    ;; ASCII: n e l i s p _ j i t _ c o n s _ m a k e
    ;;       110 101 108 105 115 112 95 106 105 116 95 99 111 110 115 95 109 97 107 101
    (defun nl_jit_alias_write_cons_make (out buf)
      (and (ptr-write-u8 buf  0 110) (ptr-write-u8 buf  1 101) (ptr-write-u8 buf  2 108)
           (ptr-write-u8 buf  3 105) (ptr-write-u8 buf  4 115) (ptr-write-u8 buf  5 112)
           (ptr-write-u8 buf  6  95) (ptr-write-u8 buf  7 106) (ptr-write-u8 buf  8 105)
           (ptr-write-u8 buf  9 116) (ptr-write-u8 buf 10  95) (ptr-write-u8 buf 11  99)
           (ptr-write-u8 buf 12 111) (ptr-write-u8 buf 13 110) (ptr-write-u8 buf 14 115)
           (ptr-write-u8 buf 15  95) (ptr-write-u8 buf 16 109) (ptr-write-u8 buf 17  97)
           (ptr-write-u8 buf 18 107) (ptr-write-u8 buf 19 101)
           (sexp-write-str out buf 20)
           (dealloc-bytes buf 26 1)
           0))

    ;; ---- Alias write helper: "nl_jit_type_of" (14 bytes) --------------------
    ;;
    ;; ASCII: n l _ j i t _ t y p e _ o f
    ;;       110 108 95 106 105 116 95 116 121 112 101 95 111 102
    (defun nl_jit_alias_write_type_of (out buf)
      (and (ptr-write-u8 buf  0 110) (ptr-write-u8 buf  1 108) (ptr-write-u8 buf  2  95)
           (ptr-write-u8 buf  3 106) (ptr-write-u8 buf  4 105) (ptr-write-u8 buf  5 116)
           (ptr-write-u8 buf  6  95) (ptr-write-u8 buf  7 116) (ptr-write-u8 buf  8 121)
           (ptr-write-u8 buf  9 112) (ptr-write-u8 buf 10 101) (ptr-write-u8 buf 11  95)
           (ptr-write-u8 buf 12 111) (ptr-write-u8 buf 13 102)
           (sexp-write-str out buf 14)
           (dealloc-bytes buf 26 1)
           0))

    ;; ---- Alias write helper: "nl_jit_sxhash" (13 bytes) ---------------------
    ;;
    ;; ASCII: n l _ j i t _ s x h a s h
    ;;       110 108 95 106 105 116 95 115 120 104 97 115 104
    (defun nl_jit_alias_write_sxhash (out buf)
      (and (ptr-write-u8 buf  0 110) (ptr-write-u8 buf  1 108) (ptr-write-u8 buf  2  95)
           (ptr-write-u8 buf  3 106) (ptr-write-u8 buf  4 105) (ptr-write-u8 buf  5 116)
           (ptr-write-u8 buf  6  95) (ptr-write-u8 buf  7 115) (ptr-write-u8 buf  8 120)
           (ptr-write-u8 buf  9 104) (ptr-write-u8 buf 10  97) (ptr-write-u8 buf 11 115)
           (ptr-write-u8 buf 12 104)
           (sexp-write-str out buf 13)
           (dealloc-bytes buf 26 1)
           0))

    ;; ---- Alias write helper: "nl_jit_intern" (13 bytes) ---------------------
    ;;
    ;; ASCII: n l _ j i t _ i n t e r n
    ;;       110 108 95 106 105 116 95 105 110 116 101 114 110
    (defun nl_jit_alias_write_intern (out buf)
      (and (ptr-write-u8 buf  0 110) (ptr-write-u8 buf  1 108) (ptr-write-u8 buf  2  95)
           (ptr-write-u8 buf  3 106) (ptr-write-u8 buf  4 105) (ptr-write-u8 buf  5 116)
           (ptr-write-u8 buf  6  95) (ptr-write-u8 buf  7 105) (ptr-write-u8 buf  8 110)
           (ptr-write-u8 buf  9 116) (ptr-write-u8 buf 10 101) (ptr-write-u8 buf 11 114)
           (ptr-write-u8 buf 12 110)
           (sexp-write-str out buf 13)
           (dealloc-bytes buf 26 1)
           0))

    ;; ---- Alias write helper: "nl_jit_symbol_name" (18 bytes) ----------------
    ;;
    ;; ASCII: n l _ j i t _ s y m b o l _ n a m e
    ;;       110 108 95 106 105 116 95 115 121 109 98 111 108 95 110 97 109 101
    (defun nl_jit_alias_write_symbol_name (out buf)
      (and (ptr-write-u8 buf  0 110) (ptr-write-u8 buf  1 108) (ptr-write-u8 buf  2  95)
           (ptr-write-u8 buf  3 106) (ptr-write-u8 buf  4 105) (ptr-write-u8 buf  5 116)
           (ptr-write-u8 buf  6  95) (ptr-write-u8 buf  7 115) (ptr-write-u8 buf  8 121)
           (ptr-write-u8 buf  9 109) (ptr-write-u8 buf 10  98) (ptr-write-u8 buf 11 111)
           (ptr-write-u8 buf 12 108) (ptr-write-u8 buf 13  95) (ptr-write-u8 buf 14 110)
           (ptr-write-u8 buf 15  97) (ptr-write-u8 buf 16 109) (ptr-write-u8 buf 17 101)
           (sexp-write-str out buf 18)
           (dealloc-bytes buf 26 1)
           0))

    ;; ---- Alias write helper: "nl_jit_make_symbol" (18 bytes) ----------------
    ;;
    ;; ASCII: n l _ j i t _ m a k e _ s y m b o l
    ;;       110 108 95 106 105 116 95 109 97 107 101 95 115 121 109 98 111 108
    (defun nl_jit_alias_write_make_symbol (out buf)
      (and (ptr-write-u8 buf  0 110) (ptr-write-u8 buf  1 108) (ptr-write-u8 buf  2  95)
           (ptr-write-u8 buf  3 106) (ptr-write-u8 buf  4 105) (ptr-write-u8 buf  5 116)
           (ptr-write-u8 buf  6  95) (ptr-write-u8 buf  7 109) (ptr-write-u8 buf  8  97)
           (ptr-write-u8 buf  9 107) (ptr-write-u8 buf 10 101) (ptr-write-u8 buf 11  95)
           (ptr-write-u8 buf 12 115) (ptr-write-u8 buf 13 121) (ptr-write-u8 buf 14 109)
           (ptr-write-u8 buf 15  98) (ptr-write-u8 buf 16 111) (ptr-write-u8 buf 17 108)
           (sexp-write-str out buf 18)
           (dealloc-bytes buf 26 1)
           0))

    ;; ---- Alias write helper: "nl_jit_syscall_call" (19 bytes) ---------------
    ;;
    ;; ASCII: n l _ j i t _ s y s c a l l _ c a l l
    ;;       110 108 95 106 105 116 95 115 121 115 99 97 108 108 95 99 97 108 108
    (defun nl_jit_alias_write_syscall_call (out buf)
      (and (ptr-write-u8 buf  0 110) (ptr-write-u8 buf  1 108) (ptr-write-u8 buf  2  95)
           (ptr-write-u8 buf  3 106) (ptr-write-u8 buf  4 105) (ptr-write-u8 buf  5 116)
           (ptr-write-u8 buf  6  95) (ptr-write-u8 buf  7 115) (ptr-write-u8 buf  8 121)
           (ptr-write-u8 buf  9 115) (ptr-write-u8 buf 10  99) (ptr-write-u8 buf 11  97)
           (ptr-write-u8 buf 12 108) (ptr-write-u8 buf 13 108) (ptr-write-u8 buf 14  95)
           (ptr-write-u8 buf 15  99) (ptr-write-u8 buf 16  97) (ptr-write-u8 buf 17 108)
           (ptr-write-u8 buf 18 108)
           (sexp-write-str out buf 19)
           (dealloc-bytes buf 26 1)
           0))

    ;; ---- Alias write helper: "nl_jit_syscall_supported_p" (26 bytes) --------
    ;;
    ;; ASCII: n l _ j i t _ s y s c a l l _ s u p p o r t e d _ p
    ;;       110 108 95 106 105 116 95 115 121 115 99 97 108 108 95
    ;;       115 117 112 112 111 114 116 101 100 95 112
    (defun nl_jit_alias_write_syscall_supported_p (out buf)
      (and (ptr-write-u8 buf  0 110) (ptr-write-u8 buf  1 108) (ptr-write-u8 buf  2  95)
           (ptr-write-u8 buf  3 106) (ptr-write-u8 buf  4 105) (ptr-write-u8 buf  5 116)
           (ptr-write-u8 buf  6  95) (ptr-write-u8 buf  7 115) (ptr-write-u8 buf  8 121)
           (ptr-write-u8 buf  9 115) (ptr-write-u8 buf 10  99) (ptr-write-u8 buf 11  97)
           (ptr-write-u8 buf 12 108) (ptr-write-u8 buf 13 108) (ptr-write-u8 buf 14  95)
           (ptr-write-u8 buf 15 115) (ptr-write-u8 buf 16 117) (ptr-write-u8 buf 17 112)
           (ptr-write-u8 buf 18 112) (ptr-write-u8 buf 19 111) (ptr-write-u8 buf 20 114)
           (ptr-write-u8 buf 21 116) (ptr-write-u8 buf 22 101) (ptr-write-u8 buf 23 100)
           (ptr-write-u8 buf 24  95) (ptr-write-u8 buf 25 112)
           (sexp-write-str out buf 26)
           (dealloc-bytes buf 26 1)
           0))

    ;; ---- Public trampoline: nl_jit_alias ------------------------------------
    ;;
    ;; Signature: (name: *const Sexp, out: *mut Sexp) -> i64
    ;; Returns 0 on success (wrote resolved name as Sexp::Str into *out).
    ;; Returns 1 on wrong type (name is not Symbol(4) or Str(5)).
    ;;
    ;; Uses `sexp-name-eq' (Doc 133 §133.B) to match both tag-4 (Symbol)
    ;; and tag-5 (Str) inputs, mirroring the Rust `alias(&str) -> &str'
    ;; match dispatch exactly.
    ;;
    ;; Identity case (no alias match): copies the input name's bytes
    ;; into a fresh Sexp::Str via str-bytes-ptr + str-len + sexp-write-str.
    ;; No extra allocation needed — sexp-write-str allocates internally.
    ;;
    ;; Two-level buf-thread pattern: nl_jit_alias allocates the shared
    ;; 26-byte buffer via nl_jit_alias_buf, then threads it through to the
    ;; matching write helper.  The write helper owns the dealloc call.
    ;; Identity case skips the buffer entirely.
    (defun nl_jit_alias_dispatch (name out buf)
      (if (= (sexp-name-eq name "nelisp_jit_car") 1)
          (nl_jit_alias_write_cons_car out buf)
        (if (= (sexp-name-eq name "nelisp_jit_cdr") 1)
            (nl_jit_alias_write_cons_cdr out buf)
          (if (= (sexp-name-eq name "nelisp_jit_setcar") 1)
              (nl_jit_alias_write_cons_setcar out buf)
            (if (= (sexp-name-eq name "nelisp_jit_setcdr") 1)
                (nl_jit_alias_write_cons_setcdr out buf)
              (if (= (sexp-name-eq name "nelisp_jit_eq_inline") 1)
                  (nl_jit_alias_write_predicate_eq out buf)
                (if (= (sexp-name-eq name "nelisp_jit_cons") 1)
                    (nl_jit_alias_write_cons_make out buf)
                  (if (= (sexp-name-eq name "nelisp_jit_type_of") 1)
                      (nl_jit_alias_write_type_of out buf)
                    (if (= (sexp-name-eq name "nelisp_jit_sxhash") 1)
                        (nl_jit_alias_write_sxhash out buf)
                      (if (= (sexp-name-eq name "nelisp_jit_intern") 1)
                          (nl_jit_alias_write_intern out buf)
                        (if (= (sexp-name-eq name "nelisp_jit_symbol_name") 1)
                            (nl_jit_alias_write_symbol_name out buf)
                          (if (= (sexp-name-eq name "nelisp_jit_make_symbol") 1)
                              (nl_jit_alias_write_make_symbol out buf)
                            (if (= (sexp-name-eq name "nelisp_jit_syscall") 1)
                                (nl_jit_alias_write_syscall_call out buf)
                              (if (= (sexp-name-eq name "nelisp_jit_syscall_supported_p") 1)
                                  (nl_jit_alias_write_syscall_supported_p out buf)
                                ;; Identity: no alias match — free buf, copy original
                                (and (dealloc-bytes buf 26 1)
                                     (sexp-write-str out (str-bytes-ptr name) (str-len name))
                                     0)))))))))))))))

    (defun nl_jit_alias (name out)
      ;; Type guard: accepts Symbol (tag 4) or Str (tag 5).
      ;; Returns 1 for any other input type (= TRAMPOLINE_ERR).
      (if (or (= (sexp-tag name) 4) (= (sexp-tag name) 5))
          (nl_jit_alias_dispatch name out (nl_jit_alias_buf))
        1)))

  "AOT source for the Doc 133 §133.A `nl_jit_alias' trampoline.

Thirteen-alias dispatch table mapping user-facing `nelisp_jit_*' names
to their canonical dlsym-resolvable `nelisp_jit_cons_*' / `nl_jit_*'
equivalents, plus identity fallthrough for unaliased names.

Replaces Rust functions `as_name' (9 LOC) + `alias' (18 LOC) in
`build-tool/src/jit/bridge.rs'.  Net Rust delta from this migration:
-27 LOC (bridge.rs) before anchor/extern additions.")

(provide 'nelisp-cc-jit-alias)

;;; nelisp-cc-jit-alias.el ends here
