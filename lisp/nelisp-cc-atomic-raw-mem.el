;;; nelisp-cc-atomic-raw-mem.el --- Doc 122 §122.E grammar op probes  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 122 §122.E introduces six new Phase 47 grammar ops for atomic
;; refcount operations + raw memory access.  These are the substrate
;; gating layer for Doc 123-128 (= refcount elisp化, `nl*.rs::Clone/
;; Drop' elisp化, alloc/dealloc handlers):
;;
;;   (atomic-fetch-add PTR DELTA)
;;     — Atomic fetch-and-add on a `*mut i64' slot with
;;       `Ordering::SeqCst'.  Returns the *previous* (pre-add) value.
;;       Calls Rust extern `nl_atomic_fetch_add(ptr, delta) -> i64'.
;;
;;   (atomic-compare-exchange PTR EXPECTED NEW)
;;     — Atomic compare-and-exchange with `Ordering::SeqCst' on both
;;       success + failure paths.  Returns `1' on success (= slot was
;;       equal to EXPECTED and has been replaced with NEW) or `0' on
;;       mismatch (= slot unchanged).  Calls
;;       `nl_atomic_compare_exchange(ptr, expected, new) -> i64'.
;;
;;   (ptr-read-u64 PTR OFFSET)
;;     — Raw `u64' read at byte address `PTR + OFFSET'.  Returns the
;;       read value re-cast to `i64'.  Calls
;;       `nl_ptr_read_u64(ptr, offset) -> i64'.
;;
;;   (ptr-write-u64 PTR OFFSET VAL)
;;     — Raw `u64' write at byte address `PTR + OFFSET'.  Writes the
;;       low 64 bits of VAL.  Returns rax = 1 sentinel for `and'-chain
;;       composition.  Calls `nl_ptr_write_u64(ptr, offset, val)'.
;;
;;   (ptr-read-u8 PTR OFFSET)
;;     — Raw `u8' read at byte address `PTR + OFFSET'.  Returns the
;;       byte zero-extended to `i64' (= no sign extension).  Calls
;;       `nl_ptr_read_u8(ptr, offset) -> i64'.
;;
;;   (ptr-write-u8 PTR OFFSET VAL)
;;     — Raw `u8' write at byte address `PTR + OFFSET'.  Writes the
;;       low 8 bits of VAL.  Returns rax = 1 sentinel.  Calls
;;       `nl_ptr_write_u8(ptr, offset, val)'.
;;
;; This file packages each op as a standalone Phase 47-compiled
;; `defun' so the `tests/elisp_cc_atomic_raw_mem_probe.rs' integration
;; test can probe each round-trip independently.  Pattern mirrors
;; `nelisp-cc-sexp-write-str.el' (§122.A) and `nelisp-cc-mut-str.el'
;; (§122.B) for the sibling allocator op probes.
;;
;; Unlocks Doc 123 (= refcount elisp化, `nl_rc_inc_strong' /
;; `nl_rc_dec_strong' bodies become atomic-fetch-add expressions),
;; Doc 124 (= `nl*.rs::Clone/Drop' elisp化, requires ptr-read/write
;; for header field walks), Doc 125 (= alloc/dealloc handlers).

;;; Code:

(defconst nelisp-cc-atomic-raw-mem--fetch-add-source
  '(defun nelisp_atomic_fetch_add (ptr delta)
     ;; ptr:   *mut i64 — 8-byte aligned slot.
     ;; delta: i64      — increment (negative permitted).
     ;;
     ;; SeqCst atomic fetch-and-add.  Returns the pre-add value in
     ;; rax.  Caller composes with `cond' on the return for refcount
     ;; transitions (= 0→1 promote, 1→0 finalize signal, etc.).
     (atomic-fetch-add ptr delta))
  "Phase 47 source for the Doc 122 §122.E `atomic-fetch-add' op probe.")

(defconst nelisp-cc-atomic-raw-mem--compare-exchange-source
  '(defun nelisp_atomic_compare_exchange (ptr expected new-val)
     ;; ptr:      *mut i64 — 8-byte aligned slot.
     ;; expected: i64      — value to test against.
     ;; new-val:  i64      — replacement value.
     ;;
     ;; SeqCst CAS on both success + failure orderings.  Returns 1
     ;; in rax on success, 0 on failure.  The Bacon-Rajan cycle
     ;; collector's strong-count promotion uses this directly.
     (atomic-compare-exchange ptr expected new-val))
  "Phase 47 source for the Doc 122 §122.E `atomic-compare-exchange' op probe.")

(defconst nelisp-cc-atomic-raw-mem--read-u64-source
  '(defun nelisp_ptr_read_u64 (ptr offset)
     ;; ptr:    *const u8 — base pointer.
     ;; offset: i64       — byte offset.
     ;;
     ;; Raw `u64' read at byte address `*(u64*)(ptr + offset)'.
     ;; Returns the value re-cast to `i64' in rax (= no sign-extension;
     ;; values above 2^63 wrap silently to negative i64).
     (ptr-read-u64 ptr offset))
  "Phase 47 source for the Doc 122 §122.E `ptr-read-u64' op probe.")

(defconst nelisp-cc-atomic-raw-mem--write-u64-source
  '(defun nelisp_ptr_write_u64 (ptr offset val)
     ;; ptr:    *mut u8 — base pointer.
     ;; offset: i64     — byte offset.
     ;; val:    i64     — low 64 bits written as `u64'.
     ;;
     ;; Raw `u64' store at `*(u64*)(ptr + offset)'.  Returns rax = 1
     ;; sentinel for `and'-chain composition (= the underlying extern
     ;; is `void').
     (ptr-write-u64 ptr offset val))
  "Phase 47 source for the Doc 122 §122.E `ptr-write-u64' op probe.")

(defconst nelisp-cc-atomic-raw-mem--read-u8-source
  '(defun nelisp_ptr_read_u8 (ptr offset)
     ;; ptr:    *const u8 — base pointer.
     ;; offset: i64       — byte offset.
     ;;
     ;; Raw `u8' read at `*(u8*)(ptr + offset)'.  Returns the byte
     ;; zero-extended to `i64' in rax (= 0xFF returns 255, not -1).
     (ptr-read-u8 ptr offset))
  "Phase 47 source for the Doc 122 §122.E `ptr-read-u8' op probe.")

(defconst nelisp-cc-atomic-raw-mem--write-u8-source
  '(defun nelisp_ptr_write_u8 (ptr offset val)
     ;; ptr:    *mut u8 — base pointer.
     ;; offset: i64     — byte offset.
     ;; val:    i64     — low 8 bits written as `u8'.
     ;;
     ;; Raw `u8' store at `*(u8*)(ptr + offset)'.  Returns rax = 1
     ;; sentinel.
     (ptr-write-u8 ptr offset val))
  "Phase 47 source for the Doc 122 §122.E `ptr-write-u8' op probe.")

(provide 'nelisp-cc-atomic-raw-mem)

;;; nelisp-cc-atomic-raw-mem.el ends here
