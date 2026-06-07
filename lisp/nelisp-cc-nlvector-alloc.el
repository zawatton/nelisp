;;; nelisp-cc-nlvector-alloc.el --- nl_alloc_vector AOT migration  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Replaces the Rust `nl_alloc_vector' body in
;; `build-tool/src/eval/nlvector.rs' with a AOT-compiled elisp
;; object.  The function allocates a fresh `NlVector' whose data
;; buffer holds `Sexp::Nil' tagged WORDS (Doc 147 Phase 2 shrink).
;;
;; NlVector HEADER layout (UNCHANGED — pinned by `#[repr(C)]'):
;;
;;   value: Vec<Sexp> @ 0  (24 bytes)
;;     Vec.capacity @ 0   (8 bytes — element count)
;;     Vec.data_ptr @ 8   (8 bytes — *mut Sexp-word, data buffer)
;;     Vec.len      @ 16  (8 bytes — element count)
;;   refcount       @ 24  (8 bytes — AtomicUsize)
;;   total = 32 bytes, align = 8
;;
;; Doc 147 Phase 2 — the DATA BUFFER shrinks from `cap * 32B` (one
;; inline `Sexp' per slot) to `cap * 8B` (one tagged WORD per slot).
;; A tagged WORD is 8 bytes: low bit 1 = immediate (Nil = 3, T = 7,
;; Int = (n<<2)|1); low bit 0 = 8-aligned pointer to a 32B child Sexp
;; box (children stay 32B; only the container SLOTS shrink).  The
;; header above is UNCHANGED — only the separately-allocated buffer
;; pointed at by `Vec.data_ptr' gets the 8B-per-slot stride.
;;
;; Vec<Sexp> memory layout: the pinned repo toolchain uses
;; `(capacity, data_ptr, len)' field order — i.e. `nelisp-nlvector--
;; offset-value-capacity = 8' is the data pointer field, not the
;; capacity count.  This is confirmed by the working aref-vector Phase
;; 47 tests (`tests/elisp_cc_aref_vector_probe.rs') and by
;; `--emit-vector-slot-ptr-core' in `nelisp-aot-compiler.el' which
;; adds `[NlVector* + 8]' (= data ptr) to `idx * 32'.
;;
;; Build strategy (three helper defuns + public entry):
;;
;;   nl_alloc_vector_fill (data-ptr i cap) — recursive Nil-fill loop
;;     that writes the 8-byte Nil WORD (= u64 3) into each slot at
;;     `data-ptr + i * 8', counts from i up to cap, then returns
;;     data-ptr.  (Doc 147 Phase 2: stride 8, full 8B WORD store —
;;     was a 32-byte stride + 1-byte Nil tag.)
;;
;;   nl_alloc_vector_build (box-ptr data-ptr cap) — writes the four
;;     words of the NlVector struct:
;;       [box-ptr +  0] = cap      (Vec.capacity count)
;;       [box-ptr +  8] = data-ptr (Vec.data_ptr)
;;       [box-ptr + 16] = cap      (Vec.len = capacity for a full Vec)
;;       [box-ptr + 24] = 1        (NlVector.refcount)
;;     Returns box-ptr.
;;
;;   nl_alloc_vector_with_data (box-ptr data-ptr cap) — chains fill +
;;     build: fills the buffer first, then calls build with the
;;     original data-ptr.
;;
;;   nl_alloc_vector (capacity) — clamps negative capacity to 0,
;;     allocates the 32-byte struct + cap*8-byte data buffer, and
;;     delegates to `nl_alloc_vector_with_data'.
;;
;; Nil-fill via `ptr-write-u64 .. 3': writes the 8-byte Nil immediate
;; WORD (= 3) into each slot, fully initialising the 8B slot.  This is
;; safe because the Nil WORD is a self-contained immediate (low bit 1,
;; no heap payload); `nl_vector_set_slot' (the only way to update a
;; slot after alloc) installs a new tagged WORD via `nl_val_clone_into'
;; over the prior Nil WORD (the old immediate slot needs no drop).
;;
;; Capacity = 0 case: `alloc-bytes(0, 8)' returns null (per `nl_layout'
;; in `raw_mem.rs' which rejects size = 0).  The resulting NlVector has
;; Vec.capacity = 0, Vec.data_ptr = 0 (null), Vec.len = 0.  Rust's Vec
;; drop path checks `capacity > 0' before calling `dealloc', so a null
;; data pointer with capacity 0 is safe.  Rust's `vec![Sexp::Nil; 0]'
;; uses a NonNull::dangling() ptr internally, but production callers of
;; `nl_alloc_vector(0)' never dereference the data ptr when len = 0.
;;
;; Build wiring: `scripts/compile-elisp-objects.el' manifest entry;
;; `build.rs' compiles into `nl_alloc_vector.o' and archives it.
;; `build-tool/src/lib.rs' keeps the `extern "C" fn nl_alloc_vector'
;; declaration (no body, symbol provided by the .o).  The Rust
;; `nlvector.rs' body is deleted entirely — the nlvector integration
;; tests do not call `nl_alloc_vector' as a Rust function (they use
;; `NlVectorRef::new'), so no Rust shim is needed.

;;; Code:

(defconst nelisp-cc-nlvector-alloc--source
  '(seq
    ;; Recursive Nil-fill loop.  Writes the 8-byte Nil immediate WORD
    ;; (= 3) into each slot starting at index i, then loops until
    ;; i == cap.  (Doc 147 Phase 2: stride 8, full 8B WORD store — was
    ;; a 32-byte stride + 1-byte Nil tag.)  Returns data-ptr (= input)
    ;; when done; non-zero when data-ptr is non-null (= cap > 0 case),
    ;; and 0 when cap = 0 (data-ptr = null from alloc-bytes(0, 8)).
    ;; The caller `nl_alloc_vector_with_data' passes the return of this
    ;; function directly to `nl_alloc_vector_build' which handles both
    ;; null and non-null data-ptr correctly.
    (defun nl_alloc_vector_fill (data-ptr i cap)
      (let ((k i))
        (while (< k cap)
          (and (ptr-write-u64 (+ data-ptr (* k 8)) 0 3) (setq k (+ k 1))))
        data-ptr))

    ;; Write the four words of the NlVector struct and return box-ptr.
    ;;
    ;; Vec<Sexp> word order in this toolchain: (capacity, data_ptr, len).
    ;;   [box-ptr +  0] = cap      — Vec.capacity field (element count)
    ;;   [box-ptr +  8] = data-ptr — Vec.data_ptr field (buffer pointer)
    ;;   [box-ptr + 16] = cap      — Vec.len field (= capacity for full Vec)
    ;;   [box-ptr + 24] = 1        — NlVector.refcount (AtomicUsize)
    ;;
    ;; `ptr-write-u64' returns 1 sentinel, so the `and' chain never
    ;; short-circuits here.
    (defun nl_alloc_vector_build (box-ptr data-ptr cap)
      (and (ptr-write-u64 box-ptr 0 cap)
           (ptr-write-u64 box-ptr 8 data-ptr)
           (ptr-write-u64 box-ptr 16 cap)
           (ptr-write-u64 box-ptr 24 1)
           box-ptr))

    ;; Bridge: fill buffer then build struct.  `nl_alloc_vector_fill'
    ;; returns data-ptr (the original buffer pointer, or 0 for cap=0),
    ;; which is passed as the data-ptr argument to `nl_alloc_vector_build'.
    (defun nl_alloc_vector_with_data (box-ptr data-ptr cap)
      (nl_alloc_vector_build
       box-ptr
       (nl_alloc_vector_fill data-ptr 0 cap)
       cap))

    ;; Public entry.  Clamps negative capacity to 0, allocates the
    ;; 32-byte NlVector struct and the cap*32-byte Sexp element buffer,
    ;; then delegates field initialisation to `nl_alloc_vector_with_data'.
    (defun nl_alloc_vector (capacity)
      (nl_alloc_vector_with_data
       (alloc-bytes 32 8)
       (alloc-bytes (* (if (< capacity 0) 0 capacity) 8) 8)
       (if (< capacity 0) 0 capacity))))
  "AOT source for the `nl_alloc_vector' allocator swap.

Four-entry `(seq DEFUN ...)' manifest:
- `nl_alloc_vector_fill (data-ptr i cap) -> data-ptr' — recursive
  Nil-fill loop; writes the 8B Nil WORD (= 3) at data-ptr+i*8
  (Doc 147 Phase 2 stride shrink 32 -> 8).
- `nl_alloc_vector_build (box-ptr data-ptr cap) -> box-ptr' — writes
  the Vec header (capacity@0, data_ptr@8, len@16) and refcount@24.
- `nl_alloc_vector_with_data (box-ptr data-ptr cap) -> box-ptr' —
  chains fill + build, passing fill's return (= data-ptr) into build.
- `nl_alloc_vector (capacity) -> *mut NlVector' — public entry.

AOT ops consumed:
  `alloc-bytes'   — 2-arg `nl_alloc_bytes'; 32 bytes for struct, cap*8
                    bytes for element buffer (Doc 147 Phase 2 shrink).
  `ptr-write-u64'  — Nil-WORD (= 3) fill per slot + 4 Vec header /
                    refcount writes.
  `if' / `<' / `*' / `+' — control flow + arithmetic.

Net Rust delta: deletes the 10-LOC `nl_alloc_vector' body + safety
comment from `build-tool/src/eval/nlvector.rs'.  No Rust shim needed
because `nlvector_integration.rs' uses `NlVectorRef::new', not the raw
`nl_alloc_vector' extern.")

(provide 'nelisp-cc-nlvector-alloc)

;;; nelisp-cc-nlvector-alloc.el ends here
