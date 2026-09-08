;;; nelisp-cc-nlboolvector-alloc.el --- bool-vector (Sexp tag 10) allocator  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; NeLisp's bool-vector had a Rust-only `Sexp::BoolVector' backing before
;; the all-Rust deletion; the elisp-level surface was migrated to plain
;; `Sexp::Vector' of t/nil (`lisp/nelisp-stdlib-plist-str.el' /
;; `scripts/nelisp-stdlib-prelude.el', "Rust-min batch 5b" / "Doc 22"),
;; with `bool-vector-p' hardcoded to always answer nil.  Tag 10 itself
;; stayed reserved: `lisp/nelisp-sexp-layout.el' still names it, and the
;; tracing GC's `nl_gc_mark_slot' (scripts/nelisp-standalone-build.el)
;; already has a tag-10 arm calling `nl_gc_mark_bool_vector_box' and
;; `nl_sexp_clone_into' (lisp/nelisp-cc-sexp-clone-into.el) already
;; dispatches tag 10 to `nelisp_nlboolvector_clone' -- both dormant,
;; never exercised because nothing has produced a genuine tag-10 Sexp
;; since the Rust deletion.  This unit is that producer.
;;
;; Box layout (chosen here; nothing outside this file's own consumers
;; and the two dormant call sites above constrains it beyond the two
;; hard requirements marked below):
;;
;;   [box +  0] = N        -- exact bit count (the vector's `length').
;;   [box +  8] = data-ptr -- REQUIRED at this offset: `nl_gc_mark_bool_
;;                            vector_box' reads it to mark the packed-byte
;;                            buffer live.  Packed low-bit-first, ceil(N/8)
;;                            bytes, unused trailing bits of the last byte
;;                            always 0 (GNU Emacs's own invariant).
;;   [box + 16] = bytelen  -- = ceil(N/8); the buffer's logical byte length.
;;   [box + 24] = 1        -- REQUIRED to be a valid, allocated 8-byte slot:
;;                            `nelisp_nlboolvector_clone' (Doc 124 §124.M,
;;                            already linked via the "boolvec-clone.o"
;;                            manifest entry) does `(atomic-fetch-add
;;                            (+ box 24) 1)' on every clone of a tag-10
;;                            Sexp.  The bumped value is never read back --
;;                            reclamation is the tracing GC's reachability
;;                            sweep, not this counter (see `nl_sci_dispatch's
;;                            own "rc is vestigial, Doc 146 §2" note in
;;                            lisp/nelisp-cc-sexp-clone-into.el) -- so any
;;                            valid in-bounds 8-byte slot here is correct;
;;                            1 just matches the sibling NlVector/NlStr/
;;                            NlRecord box convention (`nl_alloc_vector'
;;                            in lisp/nelisp-cc-nlvector-alloc.el, etc).
;;
;; The Sexp slot itself: tag byte (10) @ +0, box pointer @ +8, matching
;; every other boxed tag (Vector/Record/CharTable/MutStr).
;;
;; Two public entries, both following this file's established "helper-
;; chain" idiom (`alloc-bytes' results are threaded as explicit function
;; parameters, never `let'-bound -- see nelisp-cc-nlstr-direct-ops.el's
;; Commentary for why) and using only `+' `-' `*' `/' `mod' `logand'
;; `logior' arithmetic (no `ash'/`shl'/`sar'/`shr': at least one sibling
;; unit's own Commentary notes `ash' is not linked into every applyfn
;; context, while `*'/`/' always are -- this unit stays in that same safe
;; subset rather than assume its own linkage):
;;
;;   nl_alloc_bool_vector(n, init-bit, result-slot)
;;     -- `make-bool-vector'/`bool-vector' backing.  Fills all N bits with
;;        INIT-BIT (0 or 1), masking the last byte's unused trailing bits
;;        to 0 when N is not a multiple of 8.
;;
;;   nl_alloc_bool_vector_from_bytes(n, src, result-slot)
;;     -- `#&N"BYTES"' reader-literal backing.  Copies the first ceil(N/8)
;;        bytes from SRC, then masks the last used byte's trailing bits to
;;        0 -- matching real Emacs, which both truncates a literal whose
;;        string carries MORE bytes than ceil(N/8) needs and silently
;;        clears any garbage in the unused high bits of the last byte
;;        (measured on host Emacs 31.1: `(read "#&7\"\\377\"")' reads back
;;        as `#&7"\\177"', not `#&7"\\377"').  The caller (the reader
;;        parser) is responsible for first checking the source string
;;        actually HAS at least ceil(N/8) bytes -- a too-short literal is
;;        `invalid-read-syntax' there, not something this allocator
;;        detects.
;;
;; Also exports the bit-level accessors `nl_bv_bit_get'/`nl_bv_bit_set'
;; (used by `bf_aref_checked'/`bf_aset_checked''s tag-10 arms in
;; scripts/nelisp-standalone-build.el) and the equal-by-value comparator
;; `bf_bv_equal' (used by `bf_equal2''s tag-10 arm there).

;;; Code:

(defconst nelisp-cc-nlboolvector-alloc--source
  '(seq
    ;; ---------------------------------------------------------------
    ;; Bit-count <-> byte-count arithmetic.  N is always >= 0 (callers
    ;; normalise/validate before reaching these).
    ;; ---------------------------------------------------------------

    ;; ceil(n/8).
    (defun nl_bv_bytelen (n) (/ (+ n 7) 8))
    ;; floor(n/8) = count of FULLY-populated bytes.
    (defun nl_bv_full_bytes (n) (/ n 8))
    ;; bits used in the partial last byte; 0 exactly when N is a
    ;; multiple of 8 (= no partial byte at all).
    (defun nl_bv_rem_bits (n) (- n (* (nl_bv_full_bytes n) 8)))

    ;; 2^boff for boff in 0..7, via lookup (no shift op -- see Commentary).
    (defun nl_bv_bitval (boff)
      (cond
       ((= boff 0) 1) ((= boff 1) 2) ((= boff 2) 4) ((= boff 3) 8)
       ((= boff 4) 16) ((= boff 5) 32) ((= boff 6) 64) (t 128)))

    ;; (2^bits)-1, for bits in 0..7 -- the mask keeping only the low
    ;; BITS bits of a byte (used to zero a partial last byte's unused
    ;; high bits).
    (defun nl_bv_mask (bits) (- (nl_bv_bitval bits) 1))

    ;; ---------------------------------------------------------------
    ;; Single-bit read/write on a packed byte buffer.  IDX is a bit
    ;; index, 0 <= IDX < N (callers bounds-check against N first).
    ;; ---------------------------------------------------------------

    (defun nl_bv_bit_get (data idx)
      (logand (/ (ptr-read-u8 data (/ idx 8)) (nl_bv_bitval (mod idx 8))) 1))

    (defun nl_bv_bit_set (data idx v)
      (let* ((bidx (/ idx 8))
             (bv (nl_bv_bitval (mod idx 8)))
             (old (ptr-read-u8 data bidx)))
        (if (= v 0)
            (ptr-write-u8 data bidx (logand old (- 255 bv)))
          (ptr-write-u8 data bidx (logior old bv)))))

    ;; ---------------------------------------------------------------
    ;; Fill helpers: zero-fill, one-fill (with trailing-bit masking),
    ;; and byte-copy (for the from-bytes reader-literal path).
    ;; ---------------------------------------------------------------

    (defun nl_bv_fill_zero (data i bytelen)
      (let ((k i))
        (while (< k bytelen)
          (and (ptr-write-u8 data k 0) (setq k (+ k 1))))
        data))

    ;; Fill DATA[I..FULL) with 0xFF, then -- when FULL < BYTELEN -- write
    ;; the partial trailing byte at DATA[FULL] with only its low REM bits
    ;; set, so the unused high bits stay 0 (GNU Emacs's own invariant:
    ;; `(make-bool-vector 7 t)' prints as `#&7"\\177"', not `#&7"\\377"').
    (defun nl_bv_fill_one (data i full bytelen rem)
      (let ((k i))
        (while (< k full)
          (and (ptr-write-u8 data k 255) (setq k (+ k 1))))
        (if (< full bytelen)
            (ptr-write-u8 data full (nl_bv_mask rem))
          1)
        data))

    (defun nl_bv_copy_loop (src dst i bytelen)
      (let ((k i))
        (while (< k bytelen)
          (and (ptr-write-u8 dst k (ptr-read-u8 src k)) (setq k (+ k 1))))
        dst))

    ;; Zero the unused high bits of the last byte after a raw copy (the
    ;; from-bytes path's source data may carry garbage there -- a literal
    ;; like `#&7"\\377"' must read back masked, matching host Emacs).
    (defun nl_bv_mask_last (data full bytelen rem)
      (if (< full bytelen)
          (ptr-write-u8 data full (logand (ptr-read-u8 data full) (nl_bv_mask rem)))
        1))

    ;; ---------------------------------------------------------------
    ;; Box + Sexp-slot writers.  See this file's Commentary for the two
    ;; offsets ([box+8] and [box+24]) that are load-bearing for other,
    ;; already-linked call sites.
    ;; ---------------------------------------------------------------

    (defun nl_bv_write_box (box data bytelen n)
      (and (ptr-write-u64 box 0 n)
           (ptr-write-u64 box 8 data)
           (ptr-write-u64 box 16 bytelen)
           (ptr-write-u64 box 24 1)
           box))

    (defun nl_bv_write_sexp (result-slot box)
      (and (ptr-write-u8 result-slot 0 10)
           (ptr-write-u64 result-slot 8 box)
           result-slot))

    ;; ---------------------------------------------------------------
    ;; `make-bool-vector'/`bool-vector' path: fill-by-INIT-BIT.
    ;; ---------------------------------------------------------------

    (defun nl_alloc_bool_vector_with_data
        (result-slot box data n bytelen full rem init-bit)
      (nl_bv_write_sexp
       result-slot
       (nl_bv_write_box
        box
        (if (= init-bit 0)
            (nl_bv_fill_zero data 0 bytelen)
          (nl_bv_fill_one data 0 full bytelen rem))
        bytelen n)))

    (defun nl_alloc_bool_vector_pos (n init-bit result-slot bytelen full rem)
      (nl_alloc_bool_vector_with_data
       result-slot (alloc-bytes 32 8)
       (alloc-bytes (if (= bytelen 0) 1 bytelen) 1)
       n bytelen full rem init-bit))

    ;; Public entry.  N is a validated non-negative bit count; INIT-BIT is
    ;; 0 or 1 (any non-zero VAL is normalised to 1 by the caller).
    (defun nl_alloc_bool_vector (n init-bit result-slot)
      (nl_alloc_bool_vector_pos
       n init-bit result-slot
       (nl_bv_bytelen n) (nl_bv_full_bytes n) (nl_bv_rem_bits n)))

    ;; ---------------------------------------------------------------
    ;; `#&N"BYTES"' reader-literal path: copy-from-SRC + trailing mask.
    ;; ---------------------------------------------------------------

    (defun nl_alloc_bool_vector_from_bytes_with_data
        (result-slot box data src n bytelen full rem)
      (nl_bv_write_sexp
       result-slot
       (nl_bv_write_box
        box
        (seq (nl_bv_copy_loop src data 0 bytelen)
             (nl_bv_mask_last data full bytelen rem)
             data)
        bytelen n)))

    (defun nl_alloc_bool_vector_from_bytes_pos
        (n src result-slot bytelen full rem)
      (nl_alloc_bool_vector_from_bytes_with_data
       result-slot (alloc-bytes 32 8)
       (alloc-bytes (if (= bytelen 0) 1 bytelen) 1)
       src n bytelen full rem))

    ;; Public entry.  N is a validated non-negative bit count; SRC must
    ;; point at least `(nl_bv_bytelen n)' readable bytes (the reader
    ;; parser checks this before calling).
    (defun nl_alloc_bool_vector_from_bytes (n src result-slot)
      (nl_alloc_bool_vector_from_bytes_pos
       n src result-slot
       (nl_bv_bytelen n) (nl_bv_full_bytes n) (nl_bv_rem_bits n)))

    ;; ---------------------------------------------------------------
    ;; `equal': value comparison (same N, same bytes).  Both operands are
    ;; tag-10 Sexps (the caller, `bf_equal2', already checked `ta = tb =
    ;; 10' before dispatching here).  A plain byte range compare over
    ;; [0, bytelen) is safe without extra masking: every tag-10 object
    ;; this file produces already keeps a partial last byte's unused high
    ;; bits at 0.
    ;; ---------------------------------------------------------------

    (defun nl_bv_bytes_eq (a b i n)
      (if (>= i n) 1
        (if (= (ptr-read-u8 a i) (ptr-read-u8 b i))
            (nl_bv_bytes_eq a b (+ i 1) n)
          0)))

    (defun bf_bv_equal (a b)
      (let* ((boxa (ptr-read-u64 a 8)) (boxb (ptr-read-u64 b 8))
             (na (ptr-read-u64 boxa 0)) (nb (ptr-read-u64 boxb 0)))
        (if (= na nb)
            (nl_bv_bytes_eq (ptr-read-u64 boxa 8) (ptr-read-u64 boxb 8)
                            0 (ptr-read-u64 boxa 16))
          0))))
  "AOT source for the bool-vector (Sexp tag 10) allocator + bit
accessors + equal comparator.  See this file's Commentary for the box
layout and which two fields are load-bearing for the already-linked
GC-mark and clone-refcount call sites.")

(provide 'nelisp-cc-nlboolvector-alloc)

;;; nelisp-cc-nlboolvector-alloc.el ends here
