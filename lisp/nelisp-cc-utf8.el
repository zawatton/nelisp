;;; nelisp-cc-utf8.el --- Doc 122 §122.D grammar op probes  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 122 §122.D introduces three new AOT grammar ops for the
;; UTF-8 helper cluster (= codepoint-aware string operations the
;; Reader lexer + several Doc 120 trampolines need):
;;
;;   (str-char-count STR)
;;     — Call `nl_str_char_count(str_ptr) -> i64' which delegates to
;;       Rust's `s.chars().count()'.  Used by the elisp `length' shim
;;       String arm (= `(length "藤澤")' = 2, not 6).
;;
;;   (str-codepoint-at STR I CP-SLOT WIDTH-SLOT)
;;     — Call `nl_str_codepoint_at(ptr, idx, cp_slot, width_slot)'
;;       which decodes the codepoint at byte index I, writes it +
;;       its UTF-8 byte width via the caller-owned out-slots, and
;;       returns rax = 1 on success / 0 on invalid I or malformed
;;       UTF-8.  Used to unblock `nl_jit_str_codepoint_at' (Doc 120).
;;
;;   (str-is-alphanumeric-at STR I)
;;     — Call `nl_str_is_alphanumeric_at(ptr, idx) -> i64'.  ASCII
;;       fast path uses a direct `[0-9A-Za-z]' byte test; multi-byte
;;       slow path decodes the codepoint and calls Rust's
;;       `char::is_alphanumeric'.  Used by `nl_jit_split_by_non_alnum'
;;       (Doc 120) + Reader lexer char-class predicates.
;;
;; This file packages each op as a standalone AOT-compiled
;; `defun' so the `tests/elisp_cc_utf8_probe.rs' integration test
;; can probe each round-trip independently.  Pattern mirrors
;; `nelisp-cc-sexp-write-str.el' (§122.A) / `nelisp-cc-mut-str.el'
;; (§122.B) for the sibling allocator + builder op probes.
;;
;; Unlocks Doc 120 SKIPPED trampolines: `nl_jit_mut_str_len' (char
;; count, not byte count), `nl_jit_str_codepoint_at',
;; `nl_jit_downcase' / `nl_jit_upcase' (codepoint-by-codepoint
;; walk), `nl_jit_split_by_non_alnum' (predicate side).  Completes
;; Doc 116.A Reader lexer char-class prereqs.

;;; Code:

(defconst nelisp-cc-utf8--char-count-source
  '(defun nelisp_str_char_count (ptr)
     ;; ptr: *const Sexp — must point at a `Sexp::Str' / `Sexp::Symbol'
     ;;                    / `Sexp::MutStr' (other variants return 0).
     ;;
     ;; Calls `nl_str_char_count(ptr) -> i64' which walks the inner
     ;; `String' via `chars().count()' and returns the codepoint
     ;; count.  Distinct from `str-len' / `nl_mut_str_len' which
     ;; return byte counts.
     (str-char-count ptr))
  "AOT source for the Doc 122 §122.D `str-char-count' op probe.")

(defconst nelisp-cc-utf8--codepoint-at-source
  '(defun nelisp_str_codepoint_at (ptr idx cp-slot width-slot)
     ;; ptr:        *const Sexp — `Sexp::Str' / `Sexp::Symbol' /
     ;;                          `Sexp::MutStr' slot.
     ;; idx:        i64        — byte index into the UTF-8 stream.
     ;; cp-slot:    *mut i64   — caller-owned out-slot for the
     ;;                          decoded codepoint.
     ;; width-slot: *mut i64   — caller-owned out-slot for the
     ;;                          UTF-8 byte width (1..4).
     ;;
     ;; Calls `nl_str_codepoint_at(ptr, idx, cp_slot, width_slot)'
     ;; which decodes the codepoint at IDX, writes it + its byte
     ;; width through the caller-supplied out-slot pointers, and
     ;; returns rax = 1 on success or 0 on invalid IDX / malformed
     ;; UTF-8.  On failure the out-slots are left untouched.
     (str-codepoint-at ptr idx cp-slot width-slot))
  "AOT source for the Doc 122 §122.D `str-codepoint-at' op probe.")

(defconst nelisp-cc-utf8--is-alphanumeric-at-source
  '(defun nelisp_str_is_alphanumeric_at (ptr idx)
     ;; ptr: *const Sexp — `Sexp::Str' / `Sexp::Symbol' / `Sexp::MutStr'
     ;;                   slot.
     ;; idx: i64         — byte index into the UTF-8 stream.
     ;;
     ;; Calls `nl_str_is_alphanumeric_at(ptr, idx) -> i64' which
     ;; takes the ASCII fast path when the byte at IDX is `[0-9A-Za-z]'
     ;; and falls back to a Unicode-aware `char::is_alphanumeric'
     ;; decode otherwise.  Returns rax = 1 if alphanumeric, 0
     ;; otherwise (including out-of-range / mid-codepoint indices).
     (str-is-alphanumeric-at ptr idx))
  "AOT source for the Doc 122 §122.D `str-is-alphanumeric-at' op probe.")

(provide 'nelisp-cc-utf8)

;;; nelisp-cc-utf8.el ends here
