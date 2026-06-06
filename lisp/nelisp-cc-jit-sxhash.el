;;; nelisp-cc-jit-sxhash.el --- Doc 120 §120.A.3 sxhash trampoline swap -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 120 §120.A.3 — AOT-compiled replacement for the Rust
;; `nl_jit_sxhash' trampoline in `build-tool/src/jit/predicate.rs'.
;; Same `(*const Sexp, *mut Sexp) -> i64' contract:
;;
;;   Output: `*out = Sexp::Int(hash_value)' where hash_value is a
;;   non-negative i64 in range [0, 0x3FFF_FFFF_FFFF_FFFF].
;;
;; Hash algorithm: multiplicative polynomial hash using the FNV-1a 64-bit
;; prime (1099511628211 = 0x00000100000001B3) as the mixing multiplier.
;; Per-variant tag bytes match the Rust `DefaultHasher' variant encoding
;; (= Nil→0, T→1, Int→2, Float→3, Symbol→4, Str→5, MutStr→5, Cons→6,
;; Vector→7, CharTable→9, BoolVector→10, Record→11) so the hash
;; distinguishes all major shape categories.  The absolute values differ
;; from Rust's SipHash-1-3 `DefaultHasher' output — this is permitted per
;; Emacs Lisp's `sxhash' contract (§Hash-Tables: "the hash code may change
;; when Emacs is restarted").
;;
;; Initialisation seed: 14695981039346656037 would sign-extend as i64; we
;; materialise via `(+ (shl 1 63) 5764607523034234880)' = the FNV-1a
;; 64-bit offset basis.  In practice we use a simpler seed (1) because the
;; per-variant tag mixing provides sufficient entropy for disambiguation.
;;
;; String hashing delegates to `nelisp_fnv1a' (Doc 115 §115.7) via
;; `extern-call' — already a AOT-compiled object in the binary.
;;
;; AOT grammar pieces used:
;;   `(sexp-tag PTR)'                   — read tag byte at offset 0.
;;   `(sexp-int-unwrap PTR)'            — read i64 payload at offset 8.
;;   `(ptr-read-u64 PTR OFF)'           — read u64 at PTR+OFF (Float bits).
;;   `(sexp-payload-ptr PTR)'           — read box pointer at offset 8.
;;   `(vector-len PTR)'                 — Vec<Sexp> length.
;;   `(vector-ref-ptr PTR IDX)'         — *const Sexp to element IDX.
;;   `(record-slot-count PTR)'          — slots.len().
;;   `(record-slot-ref-ptr PTR IDX)'    — *const Sexp to slot IDX.
;;   `(extern-call nl_record_type_tag_ptr PTR)' — *const Sexp for type_tag.
;;   `(extern-call nelisp_fnv1a PTR)'   — 32-bit FNV-1a hash of Str/Symbol.
;;   `(logand A B)' / `(logxor A B)' / `(* A B)' — integer mixing ops.
;;
;; Cell unwrapping: `NlCell.value' is at offset 0 of the NlCell heap
;; block, so `sexp-payload-ptr(cell_ptr)' IS a valid `*const Sexp' for
;; the inner value — no heap allocation or clone needed.
;;
;; Cons traversal: `sexp-payload-ptr(cons_ptr)' = pointer to NlConsBox;
;; car is at NlConsBox+0, cdr is at NlConsBox+32.
;;
;; Build wiring: `scripts/compile-elisp-objects.el' manifest entry +
;; `build-tool/build.rs::link_elisp_cc_spike' source list.  The emitted
;; `nl_jit_sxhash' STT_FUNC symbol is resolved by
;; `build-tool/src/jit/bridge.rs::alias' under the `"nelisp_jit_sxhash"'
;; arm.

;;; Code:

(defconst nelisp-cc-jit-sxhash--source
  '(seq
    ;; ------------------------------------------------------------------
    ;; nl_jit_sxhash_mix: accumulate one i64 word into the running hash.
    ;;
    ;; Strategy: `h = (h * PRIME) ^ x', masked to the lower 63 bits so
    ;; the result fits in a positive Sexp::Int.
    ;;
    ;; PRIME = 16777619 (= 0x01000193, FNV-1a 32-bit prime).  Chosen
    ;; because AOT's `MOV r/m64, imm32' encoding only supports
    ;; 32-bit immediate values; the FNV-1a 64-bit prime (1099511628211)
    ;; exceeds this range.  The 32-bit prime still provides good mixing
    ;; properties for the sxhash discrimination requirement.
    ;;
    ;; MASK = 0x3FFF_FFFF_FFFF_FFFF.  Constructed as `(- (shl 1 62) 1)'
    ;; which avoids sign-extension: `(shl 1 62)' emits the clean 64-bit
    ;; value 0x4000_0000_0000_0000 (bit 62 set, all high bits 0); subtracting
    ;; 1 yields exactly 0x3FFF_FFFF_FFFF_FFFF = max positive Sexp::Int.
    ;; ------------------------------------------------------------------
    (defun nl_jit_sxhash_mix (h x)
      ;; h: i64 running hash, x: i64 new data word.
      ;; Returns: updated hash masked to [0, 0x3FFF_FFFF_FFFF_FFFF].
      (logand (logxor (* h 16777619) x)
              (- (shl 1 62) 1)))

    ;; ------------------------------------------------------------------
    ;; nl_jit_sxhash_vec: hash Vector elements [i, n) into h.
    ;; Tail-recursive on index i.
    ;; ------------------------------------------------------------------
    (defun nl_jit_sxhash_vec (vec i n h)
      ;; vec: *const Sexp (Vector), i: i64 index, n: i64 length, h: i64.
      ;; Returns: updated hash.
      (if (< i n)
          (nl_jit_sxhash_vec
           vec (+ i 1) n
           (nl_jit_sxhash_inner (vector-ref-ptr vec i) h))
        h))

    ;; ------------------------------------------------------------------
    ;; nl_jit_sxhash_rec: hash Record slots [i, n) into h.
    ;; Tail-recursive on index i.
    ;; ------------------------------------------------------------------
    (defun nl_jit_sxhash_rec (rec i n h)
      ;; rec: *const Sexp (Record), i: i64, n: i64, h: i64.
      ;; Returns: updated hash.
      (if (< i n)
          (nl_jit_sxhash_rec
           rec (+ i 1) n
           (nl_jit_sxhash_inner (record-slot-ref-ptr rec i) h))
        h))

    ;; ------------------------------------------------------------------
    ;; nl_jit_sxhash_inner: core recursive hash dispatch.
    ;;
    ;; To avoid calling `sexp-tag' twice (once for dispatch, once in the
    ;; branch body), the tag is extracted here and forwarded to
    ;; `nl_jit_sxhash_dispatch'.
    ;; ------------------------------------------------------------------
    (defun nl_jit_sxhash_inner (ptr h)
      ;; ptr: *const Sexp.  h: i64 running hash.
      ;; Returns: updated hash i64.
      (nl_jit_sxhash_dispatch (sexp-tag ptr) ptr h))

    ;; ------------------------------------------------------------------
    ;; nl_jit_sxhash_dispatch: variant dispatch on pre-read tag.
    ;; ------------------------------------------------------------------
    (defun nl_jit_sxhash_dispatch (tag ptr h)
      ;; tag: i64 (= sexp-tag result).
      ;; ptr: *const Sexp (the sexp being hashed).
      ;; h:   i64 running hash.
      ;; Returns: updated hash i64.
      (cond
       ;; Nil (0): mix with tag constant 0.
       ((= tag 0) (nl_jit_sxhash_mix h 0))
       ;; T (1): mix with tag constant 1.
       ((= tag 1) (nl_jit_sxhash_mix h 1))
       ;; Int (2): mix tag byte (2) then integer payload.
       ((= tag 2)
        (nl_jit_sxhash_mix (nl_jit_sxhash_mix h 2) (sexp-int-unwrap ptr)))
       ;; Float (3): mix tag byte (3) then f64 bit-pattern read as u64.
       ;; f64 lives at offset 8, same as Int — read raw bits via ptr-read-u64.
       ((= tag 3)
        (nl_jit_sxhash_mix (nl_jit_sxhash_mix h 3) (ptr-read-u64 ptr 8)))
       ;; Symbol (4): mix tag byte (4) then FNV-1a of the symbol bytes.
       ((= tag 4)
        (nl_jit_sxhash_mix (nl_jit_sxhash_mix h 4)
                            (extern-call nelisp_fnv1a ptr)))
       ;; Str (5): mix tag byte (5) then FNV-1a of string bytes.
       ((= tag 5)
        (nl_jit_sxhash_mix (nl_jit_sxhash_mix h 5)
                            (extern-call nelisp_fnv1a ptr)))
       ;; MutStr (6): same as Str — mix tag byte 5 (matches Rust encoding)
       ;; then FNV-1a.  `nelisp_fnv1a' accepts MutStr via `str-len'/
       ;; `str-byte-at' which cover all three string variants.
       ((= tag 6)
        (nl_jit_sxhash_mix (nl_jit_sxhash_mix h 5)
                            (extern-call nelisp_fnv1a ptr)))
       ;; Cons (7): mix tag byte (6 = Rust encoding), then hash car, then cdr.
       ;; sexp-payload-ptr(cons) = *NlConsBox; car at +0, cdr at +32.
       ((= tag 7)
        (nl_jit_sxhash_inner
         (+ (sexp-payload-ptr ptr) 32)
         (nl_jit_sxhash_inner
          (sexp-payload-ptr ptr)
          (nl_jit_sxhash_mix h 6))))
       ;; Vector (8): mix tag byte (7 = Rust encoding), then hash each element.
       ((= tag 8)
        (nl_jit_sxhash_vec
         ptr 0 (vector-len ptr)
         (nl_jit_sxhash_mix h 7)))
       ;; CharTable (9): mix tag byte only (opaque structure — Rust does same).
       ((= tag 9) (nl_jit_sxhash_mix h 9))
       ;; BoolVector (10): mix tag byte only (bit iteration needs bool-vector
       ;; grammar ops not yet available; Rust tag byte = 10).
       ((= tag 10) (nl_jit_sxhash_mix h 10))
       ;; Cell (11): unwrap via sexp-payload-ptr shortcut (NlCell.value at +0).
       ((= tag 11)
        (nl_jit_sxhash_inner (sexp-payload-ptr ptr) h))
       ;; Record (12): mix tag byte (11 = Rust encoding), hash type_tag,
       ;; then hash each slot.
       ((= tag 12)
        (nl_jit_sxhash_rec
         ptr 0 (record-slot-count ptr)
         (nl_jit_sxhash_inner
          (extern-call nl_record_type_tag_ptr ptr)
          (nl_jit_sxhash_mix h 11))))
       ;; Unknown tag: return h unchanged (defensive, should not occur).
       (t h)))

    ;; ------------------------------------------------------------------
    ;; Public entry point — matches `nl_jit_sxhash' Rust trampoline ABI.
    ;;
    ;; Initialises the running hash to 1 (= non-zero seed, avoids the
    ;; degenerate all-zeros accumulator on empty/trivial inputs) and
    ;; calls `nl_jit_sxhash_inner'.  Masks to 0x3FFF_FFFF_FFFF_FFFF
    ;; via `logand' before writing into *out as Sexp::Int.
    ;; ------------------------------------------------------------------
    (defun nl_jit_sxhash (arg out)
      ;; arg: *const Sexp.  out: *mut Sexp.
      ;; Returns: i64 = 0 always (no error path for valid Sexp input).
      (and
       (sexp-int-make out
                      (logand (nl_jit_sxhash_inner arg 1)
                              (- (shl 1 62) 1)))
       0)))
  "AOT source for the Doc 120 §120.A.3 `nl_jit_sxhash' swap.

Implements a multiplicative polynomial hash (FNV-1a prime mixing) over
the full Sexp variant tree.  Cell unwrapping uses the `sexp-payload-ptr'
zero-copy shortcut; string hashing delegates to `extern-call nelisp_fnv1a'
(Doc 115 §115.7).  The hash values differ from the Rust DefaultHasher
output (SipHash-1-3) but satisfy the Emacs `sxhash' contract (= equal
objects hash equal, value may change between sessions).

Emits `nl_jit_sxhash' STT_FUNC symbol resolved by
`build-tool/src/jit/bridge.rs::alias' under `\"nelisp_jit_sxhash\"'.")

(provide 'nelisp-cc-jit-sxhash)

;;; nelisp-cc-jit-sxhash.el ends here
