;;; nelisp-cc-alloc-dealloc.el --- Doc 125 §125.A grammar op probes  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 125 §125.A introduces two new AOT grammar ops for raw
;; byte-level heap allocation + free.  These are the substrate gating
;; layer for Doc 124.G-K (= NlBox Drop kernels' if-zero-refcount free
;; branch) and Doc 126-128 (= bridge GC arena code that frees opaque
;; buffers whose `Layout` is computed at runtime):
;;
;;   (alloc-bytes SIZE ALIGN)
;;     — Generic byte-level allocator wrapping
;;       `std::alloc::alloc(Layout::from_size_align(size, align))'.
;;       Returns the freshly-allocated `*mut u8' re-cast to `i64' in
;;       rax (= 0 on layout error — bad align, overflow — or OOM).
;;       Calls Rust extern `nl_alloc_bytes(size, align) -> *mut u8'.
;;
;;   (dealloc-bytes PTR SIZE ALIGN)
;;     — Generic byte-level deallocator wrapping
;;       `std::alloc::dealloc(ptr, Layout::from_size_align(size,
;;       align))'.  Returns rax = 1 sentinel for `and'-chain
;;       composition.  The `(size, align)' pair MUST match the values
;;       passed to the matching `alloc-bytes' call — `alloc::dealloc'
;;       is UB on layout mismatch.  Calls
;;       `nl_dealloc_bytes(ptr, size, align)'.
;;
;; This file packages each op as a standalone AOT-compiled
;; `defun' so the `tests/elisp_cc_alloc_dealloc_probe.rs' integration
;; test can probe each round-trip independently.  Pattern mirrors
;; `nelisp-cc-atomic-raw-mem.el' (§122.E) — same module classification
;; (= layer 2 / OS primitive), same one-defconst-per-op packaging,
;; same Linux-x86_64 gate (aarch64 emit will land with the rest of
;; the AOT aarch64 sweep).
;;
;; Unlocks Doc 124.G-L (= NlBox Drop elisp化) + Doc 117 Tier C
;; partial (= `bi_*' handlers that today wrap `Box::new(NlT { … })'
;; + matching `Box::from_raw' on the error path).

;;; Code:

(defconst nelisp-cc-alloc-dealloc--alloc-bytes-source
  '(defun nelisp_alloc_bytes (size align)
     ;; size:  i64 — bytes to allocate, must be > 0.
     ;; align: i64 — must be a power of two, > 0.
     ;;
     ;; Generic byte-level allocator.  Returns `*mut u8' re-cast to
     ;; `i64' in rax (= 0 on layout error or OOM).  Substrate gate for
     ;; the Doc 126-128 bridge GC arena allocator.  Doc 124 NlBox
     ;; Drop kernels use the §125.B-F per-type externs instead.
     (alloc-bytes size align))
  "AOT source for the Doc 125 §125.A `alloc-bytes' op probe.")

(defconst nelisp-cc-alloc-dealloc--dealloc-bytes-source
  '(defun nelisp_dealloc_bytes (ptr size align)
     ;; ptr:   *mut u8 — pointer returned by `nl_alloc_bytes' with the
     ;;        same `(size, align)' pair.  null = no-op.
     ;; size:  i64     — must match the alloc-time value.
     ;; align: i64     — must match the alloc-time value.
     ;;
     ;; Generic byte-level deallocator.  Returns rax = 1 sentinel.
     ;; Layout mismatch with the matching `alloc-bytes' call is UB
     ;; per `std::alloc::dealloc' — caller must track `(size, align)'
     ;; per allocation.
     (dealloc-bytes ptr size align))
  "AOT source for the Doc 125 §125.A `dealloc-bytes' op probe.")

(provide 'nelisp-cc-alloc-dealloc)

;;; nelisp-cc-alloc-dealloc.el ends here
