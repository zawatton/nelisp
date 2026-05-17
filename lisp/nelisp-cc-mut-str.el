;;; nelisp-cc-mut-str.el --- Doc 122 §122.B grammar op probes  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 122 §122.B introduces five new Phase 47 grammar ops for the
;; mutable string builder (= incremental byte/codepoint append +
;; finalize-to-immutable):
;;
;;   (mut-str-make-empty SLOT CAP)
;;     — Call `nl_alloc_mut_str(cap, slot)' which allocates a fresh
;;       `NlStrRef::new(String::with_capacity(cap))' and writes
;;       `Sexp::MutStr(rc)' (= tag at offset 0, NlStr* at offset 8)
;;       into the caller-owned SLOT.  Returns SLOT in rax.
;;
;;   (mut-str-push-byte PTR BYTE)
;;     — Append a single byte (low 8 bits of BYTE) to PTR's MutStr.
;;       Returns rax = 1 sentinel for `and'-chain composition.
;;
;;   (mut-str-push-codepoint PTR CP)
;;     — UTF-8 encode CP (1-4 bytes) and append.  Surrogate /
;;       out-of-range codepoints clamp to U+FFFD inside the Rust
;;       extern.  Returns rax = 1 sentinel.
;;
;;   (mut-str-len PTR)
;;     — Current byte length of PTR's MutStr.  Returns i64 in rax.
;;
;;   (mut-str-finalize PTR SLOT)
;;     — Clone PTR's MutStr's inner String into a fresh
;;       `Sexp::Str(s)' and write to SLOT.  The source remains live +
;;       push-able (= clone semantics, not move).  Returns SLOT.
;;
;; This file packages each op as a standalone Phase 47-compiled
;; `defun' so the `tests/elisp_cc_mut_str_probe.rs' integration test
;; can probe each round-trip independently.  Pattern mirrors
;; `nelisp-cc-sexp-write-str.el' (Doc 122 §122.A) for the sibling
;; allocator op probes and `nelisp-cc-cell-ops.el' for the §111.D
;; cell-make / cell-value pattern.
;;
;; Unlocks Doc 120 SKIPPED trampolines: `nl_jit_make_mut_str',
;; `nl_jit_concat_ints', `nl_jit_mut_str_set_codepoint',
;; `nl_jit_record_alloc'.  Completes Doc 116.A lexer prereq (=
;; incremental token building from byte/codepoint streams).

;;; Code:

(defconst nelisp-cc-mut-str--make-empty-source
  '(defun nelisp_mut_str_make_empty (slot cap)
     ;; slot: *mut Sexp — 40-byte slot to receive `Sexp::MutStr(_)'.
     ;; cap:  i64       — reserved byte-capacity for the inner String.
     ;;
     ;; The op evaluates the two args, marshals them to rdi/rsi per
     ;; SysV AMD64, and calls the `nl_alloc_mut_str' extern which
     ;; allocates `NlStrRef::new(String::with_capacity(cap))' and
     ;; writes `Sexp::MutStr(rc)' into `*slot'.  Returns SLOT in rax.
     (mut-str-make-empty slot cap))
  "Phase 47 source for the Doc 122 §122.B `mut-str-make-empty' op probe.")

(defconst nelisp-cc-mut-str--push-byte-source
  '(defun nelisp_mut_str_push_byte (ptr byte)
     ;; ptr:  *mut Sexp — slot carrying `Sexp::MutStr(_)' (caller
     ;;                   responsible for tag invariant).
     ;; byte: i64       — low 8 bits taken as the byte to append.
     ;;
     ;; Calls `nl_mut_str_push_byte(ptr, byte)' which reads the box
     ;; pointer at `*ptr + 8' and pushes the byte into the NlStr's
     ;; inner `String' via `Vec::push'.  Returns rax = 1 sentinel
     ;; (the extern's return is `void').
     (mut-str-push-byte ptr byte))
  "Phase 47 source for the Doc 122 §122.B `mut-str-push-byte' op probe.")

(defconst nelisp-cc-mut-str--push-codepoint-source
  '(defun nelisp_mut_str_push_codepoint (ptr cp)
     ;; ptr: *mut Sexp — `Sexp::MutStr(_)' slot.
     ;; cp:  i64       — Unicode codepoint (clamped to U+FFFD if
     ;;                  out-of-range or surrogate).
     ;;
     ;; Calls `nl_mut_str_push_codepoint(ptr, cp)' which encodes the
     ;; codepoint as 1-4 UTF-8 bytes and appends to the inner
     ;; `String'.  Returns rax = 1 sentinel.
     (mut-str-push-codepoint ptr cp))
  "Phase 47 source for the Doc 122 §122.B `mut-str-push-codepoint' op probe.")

(defconst nelisp-cc-mut-str--len-source
  '(defun nelisp_mut_str_len (ptr)
     ;; ptr: *const Sexp — `Sexp::MutStr(_)' slot.
     ;;
     ;; Calls `nl_mut_str_len(ptr) -> i64' which reads the box
     ;; pointer and returns the inner `String::len' (= byte count).
     (mut-str-len ptr))
  "Phase 47 source for the Doc 122 §122.B `mut-str-len' op probe.")

(defconst nelisp-cc-mut-str--finalize-source
  '(defun nelisp_mut_str_finalize (ptr slot)
     ;; ptr:  *const Sexp — source `Sexp::MutStr(_)' slot.
     ;; slot: *mut Sexp   — destination slot to receive `Sexp::Str(_)'.
     ;;
     ;; Calls `nl_mut_str_finalize(ptr, slot)' which clones the
     ;; MutStr's inner `String' into a fresh `Sexp::Str(s)' and writes
     ;; that into `*slot'.  The source MutStr remains live + push-able
     ;; (= clone semantics, not move) so the Reader lexer can take
     ;; intermediate token snapshots.  Returns SLOT in rax.
     (mut-str-finalize ptr slot))
  "Phase 47 source for the Doc 122 §122.B `mut-str-finalize' op probe.")

(provide 'nelisp-cc-mut-str)

;;; nelisp-cc-mut-str.el ends here
