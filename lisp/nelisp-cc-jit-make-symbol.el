;;; nelisp-cc-jit-make-symbol.el --- Phase 47 body for nl_jit_make_symbol  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 47 elisp migration of `nl_jit_make_symbol' from
;; `build-tool/src/jit/strings.rs' (Doc 122 §122.A + §122.E).
;;
;; The trampoline signature is `(*const Sexp, *mut Sexp) -> i64'
;; (OK=0 / ERR=1), reached via `nl-jit-call-out-1' from
;; `nelisp-jit-strategy.el'.
;;
;; Algorithm:
;;   1. Reject non-Str / non-Symbol arg (tags 5 / 4 accepted;
;;      MutStr is an internal type never passed by Elisp callers).
;;   2. Fetch-and-add `MAKE_SYMBOL_COUNTER' (surfaced by the Rust
;;      extern `nl_make_symbol_counter_ptr') via `atomic-fetch-add'.
;;   3. Allocate a raw byte buffer of `name-len + 36' bytes
;;      (`+ 20' for the fixed `__nelisp-uninterned-' suffix,
;;       `+ 16' for the 16-nibble zero-padded hexadecimal counter).
;;   4. Copy name bytes from `arg' using `str-byte-at' (works for
;;      Str tag=5 and Symbol tag=4 which share the inline-String
;;      Sexp layout; §101.C).
;;   5. Append the 20-byte literal suffix byte-by-byte with
;;      `ptr-write-u8'.
;;   6. Append 16 hex nibbles (MSB-first, zero-padded) via a
;;      tail-recursive helper using `sar' / `shl' / `logand' (no
;;      division required).
;;   7. Call `sexp-write-symbol out buf (+ name-len 36)' — copies
;;      the buffer into a fresh `Sexp::Symbol(String)' and writes it
;;      to `*out'.  (§122.A `nl_alloc_symbol' extern.)
;;   8. `dealloc-bytes buf (+ name-len 36) 1' — free the raw buffer.
;;      (§125.A)
;;   9. Return 0 (TRAMPOLINE_OK).
;;
;; Grammar ops consumed (all existing — no new opcode needed):
;;   §101.C  `sexp-tag', `str-len', `str-byte-at'
;;   §122.A  `sexp-write-symbol'
;;   §122.E  `atomic-fetch-add', `ptr-write-u8'
;;   §122.C  `extern-call nl_make_symbol_counter_ptr' (zero-arg)
;;   §125.A  `alloc-bytes', `dealloc-bytes'
;;   Phase 47 arith: `+', `-', `<', `=', `or', `shl', `sar', `logand'
;;
;; Output format:
;;   `NAME__nelisp-uninterned-XXXXXXXXXXXXXXXX'
;;   where NAME is the input Str/Symbol's text and X...X is a
;;   16-character zero-padded lower-case hexadecimal counter (matches
;;   the Rust TRAMPOLINE_OK=0 / TRAMPOLINE_ERR=1 convention; format
;;   changed from decimal to hex — task spec says "or similar").
;;
;; Build wiring:
;;   `scripts/compile-elisp-objects.el' manifest: one entry →
;;   `nl_jit_make_symbol.o' (Linux x86_64 gate, same as §122.A
;;   siblings).  `build-tool/build.rs' manifest_sources: entry for
;;   `nelisp-cc-jit-make-symbol.el'.  `build-tool/src/jit/bridge.rs':
;;   `fn nl_jit_make_symbol()' extern + anchor-array entry.
;;   `build-tool/src/jit/strings.rs': Rust body deleted; replaced by
;;   module-level `MAKE_SYMBOL_COUNTER: AtomicI64' static and the
;;   `nl_make_symbol_counter_ptr() -> *mut i64' getter.

;;; Code:

(defconst nelisp-cc-jit-make-symbol--source
  '(seq
    ;; ---- prog2: discard _eff, return val --------------------------------
    ;;
    ;; Used to sequence a `ptr-write-u8' side effect (void return =
    ;; 1 sentinel) before returning the next value in a tail position.
    ;; Same pattern as `nelisp_cstr_helpers_prog2' (§122.I) and the
    ;; `*_prog2' helpers in §116.A / §124.A / §124.G.
    (defun nl_jit_sym_prog2 (_eff val) val)

    ;; ---- name-byte copy -------------------------------------------------
    ;;
    ;; Tail-recursive: copy bytes src[i .. n-1] to raw buf at
    ;; dst+i.  Returns 1 on completion (truthy sentinel for `and'
    ;; chain composition).
    ;;
    ;; arg:       *const Sexp   — source Str / Symbol.
    ;; buf:       *mut u8       — destination raw buffer.
    ;; dst-start: i64           — fixed start offset of name region in buf.
    ;; i:         i64           — current byte index (0-based).
    ;; n:         i64           — total byte count (= str-len arg).
    (defun nl_jit_sym_copy (arg buf dst-start i n)
      (if (= i n)
          1
        (and (ptr-write-u8 buf (+ dst-start i) (str-byte-at arg i))
             (nl_jit_sym_copy arg buf dst-start (+ i 1) n))))

    ;; ---- hex nibble writer ----------------------------------------------
    ;;
    ;; Tail-recursive: write 16 hex nibbles (MSB-first) for `n' into
    ;; buf starting at `hex-start'.  `shift' counts from 15 down to
    ;; -1; when shift < 0 the loop is complete.
    ;;
    ;; Each nibble is extracted as (n >> (shift*4)) & 0xF:
    ;;   - `(shl shift 2)' computes shift*4 (bit index of nibble LSB).
    ;;   - `(sar n ...)' logical-right-shifts n by that amount.
    ;;   - `(logand ... 15)' masks the low four bits.
    ;; Conversion to ASCII:
    ;;   digit 0-9  → '0'(48) + nibble
    ;;   digit a-f  → 'a'(97) + nibble - 10  =  nibble + 87
    ;;
    ;; The `nl_jit_sym_prog2' call sequences the `ptr-write-u8' side
    ;; effect before the recursive tail call, returning 1 from the
    ;; terminal case.
    ;;
    ;; n:         i64 — counter value (from atomic-fetch-add).
    ;; buf:       *mut u8 — destination raw buffer.
    ;; hex-start: i64 — fixed start offset of hex region in buf.
    ;; shift:     i64 — nibble index (15 down to 0; recurse while >= 0).
    (defun nl_jit_sym_hex (n buf hex-start shift)
      (if (< shift 0)
          1
        (nl_jit_sym_prog2
          (ptr-write-u8 buf (+ hex-start (- 15 shift))
            (if (< (logand (sar n (shl shift 2)) 15) 10)
                (+ (logand (sar n (shl shift 2)) 15) 48)
              (+ (logand (sar n (shl shift 2)) 15) 87)))
          (nl_jit_sym_hex n buf hex-start (- shift 1)))))

    ;; ---- suffix writer --------------------------------------------------
    ;;
    ;; Write the 20-byte literal `__nelisp-uninterned-' to buf
    ;; starting at `suf-start'.  Returns 1 sentinel.
    ;; ASCII: _ _ n e l i s p - u n i n t e r n e d -
    ;;        95 95 110 101 108 105 115 112 45 117 110 105 110 116 101 114 110 101 100 45
    (defun nl_jit_sym_suffix (buf suf-start)
      (and (ptr-write-u8 buf suf-start        95)
           (ptr-write-u8 buf (+ suf-start 1)  95)
           (ptr-write-u8 buf (+ suf-start 2)  110)
           (ptr-write-u8 buf (+ suf-start 3)  101)
           (ptr-write-u8 buf (+ suf-start 4)  108)
           (ptr-write-u8 buf (+ suf-start 5)  105)
           (ptr-write-u8 buf (+ suf-start 6)  115)
           (ptr-write-u8 buf (+ suf-start 7)  112)
           (ptr-write-u8 buf (+ suf-start 8)  45)
           (ptr-write-u8 buf (+ suf-start 9)  117)
           (ptr-write-u8 buf (+ suf-start 10) 110)
           (ptr-write-u8 buf (+ suf-start 11) 105)
           (ptr-write-u8 buf (+ suf-start 12) 110)
           (ptr-write-u8 buf (+ suf-start 13) 116)
           (ptr-write-u8 buf (+ suf-start 14) 101)
           (ptr-write-u8 buf (+ suf-start 15) 114)
           (ptr-write-u8 buf (+ suf-start 16) 110)
           (ptr-write-u8 buf (+ suf-start 17) 101)
           (ptr-write-u8 buf (+ suf-start 18) 100)
           (ptr-write-u8 buf (+ suf-start 19) 45)))

    ;; ---- inner write stage ----------------------------------------------
    ;;
    ;; Receives the pre-allocated raw buffer `buf', fills it, writes
    ;; the symbol to `*out', frees `buf', and returns 0 (TRAMPOLINE_OK).
    ;;
    ;; Parameters thread the name length `n', the counter pointer
    ;; `ctr-ptr', and the buffer `buf' so that alloc-bytes + str-len
    ;; are each evaluated only once (Phase 47 `let' only accepts
    ;; compile-time constants).
    ;;
    ;; Total buffer layout (n + 36 bytes):
    ;;   [0 .. n-1]      name bytes
    ;;   [n .. n+19]     "__nelisp-uninterned-" (20 bytes)
    ;;   [n+20 .. n+35]  hex counter (16 bytes, MSB-first, zero-padded)
    (defun nl_jit_sym_write (arg out n ctr-ptr buf)
      (and
        ;; 1. copy name bytes [0..n)
        (nl_jit_sym_copy arg buf 0 0 n)
        ;; 2. write "__nelisp-uninterned-" at offset n
        (nl_jit_sym_suffix buf n)
        ;; 3. write 16 hex nibbles at offset n+20
        (nl_jit_sym_hex (atomic-fetch-add ctr-ptr 1) buf (+ n 20) 15)
        ;; 4. allocate Symbol from buffer, write to *out
        (sexp-write-symbol out buf (+ n 36))
        ;; 5. free the raw buffer
        (dealloc-bytes buf (+ n 36) 1)
        ;; 6. TRAMPOLINE_OK
        0))

    ;; ---- inner alloc stage ----------------------------------------------
    ;;
    ;; Allocates the raw byte buffer and threads it into nl_jit_sym_write.
    ;; Exists as a separate defun to evaluate `(str-len arg)' once
    ;; and reuse the result as both `n' for the loop bound and the
    ;; buffer-size component.
    (defun nl_jit_sym_inner (arg out n ctr-ptr)
      (nl_jit_sym_write arg out n ctr-ptr
        (alloc-bytes (+ n 36) 1)))

    ;; ---- public trampoline ----------------------------------------------
    ;;
    ;; Signature: (arg: *const Sexp, out: *mut Sexp) -> i64
    ;; Returns 0 (TRAMPOLINE_OK) on success, 1 (TRAMPOLINE_ERR) on
    ;; wrong-type input.  Accepts Str (tag=5) and Symbol (tag=4).
    ;; MutStr (tag=6) is rejected — it is an internal type and is
    ;; never passed by Elisp `make-symbol' callers.
    (defun nl_jit_make_symbol (arg out)
      (if (or (= (sexp-tag arg) 5) (= (sexp-tag arg) 4))
          (nl_jit_sym_inner arg out (str-len arg)
                            (extern-call nl_make_symbol_counter_ptr))
        1)))
  "Phase 47 source for the `nl_jit_make_symbol' trampoline.

Six-entry `(seq DEFUN ...)' manifest:
  `nl_jit_sym_prog2 (_eff val)'    — side-effect sequencer.
  `nl_jit_sym_copy  (arg buf dst-start i n)' — tail-recursive name copy.
  `nl_jit_sym_hex   (n buf hex-start shift)' — tail-recursive hex writer.
  `nl_jit_sym_suffix (buf suf-start)'        — 20-byte suffix writer.
  `nl_jit_sym_write (arg out n ctr-ptr buf)' — fill + symbol write + free.
  `nl_jit_sym_inner (arg out n ctr-ptr)'     — alloc + thread to write.
  `nl_jit_make_symbol (arg out)'             — public trampoline.

Output format: `NAME__nelisp-uninterned-XXXXXXXXXXXXXXXX'
where X...X is 16 zero-padded lower-case hex digits of the
per-process atomic counter (surfaced by `nl_make_symbol_counter_ptr'
in `build-tool/src/jit/strings.rs').")

(provide 'nelisp-cc-jit-make-symbol)

;;; nelisp-cc-jit-make-symbol.el ends here
