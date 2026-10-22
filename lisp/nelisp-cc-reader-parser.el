;;; nelisp-cc-reader-parser.el --- Doc 116 §116.B pure-elisp Reader parser  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 116 §116.B — pure-elisp AOT implementation of the
;; recursive-descent s-expression parser in
;; `build-tool/src/reader/parser.rs' (~485 LOC Rust → ~500 LOC elisp).
;; Consumes the Token stream emitted by §116.A's
;; `nelisp_reader_lex_one' and produces final `Sexp' values via the
;; existing AOT grammar primitives (cons-make-with-clone /
;; sexp-write-symbol / sexp-write-str / sexp-int-make + the §122.E
;; raw-mem ops for String header decoding).
;;
;; cons-make-with-clone (Doc 120.E) is used everywhere a Cons cell is
;; written into the result-slot or a depth-level working slot that the
;; caller will own after parse_one returns.  This ensures NlConsBoxRef
;; refcounts are properly bumped so the Rust wrapper can drop the
;; slot-pool normally without double-freeing shared inner boxes.
;;
;; ABI shape:
;;
;;   nelisp_reader_parse_one(STR-PTR, CURSOR-SLOT, RESULT-SLOT,
;;                           SLOT-POOL, DEPTH) -> i64 status
;;
;; Args:
;;   STR-PTR     — `*const Sexp' source (Sexp::Str / Sexp::Symbol
;;                 payload bytes).
;;   CURSOR-SLOT — `*mut Sexp' holding `Sexp::Int(current-cursor)';
;;                 read via `sexp-int-unwrap', written via the lexer's
;;                 cursor-out-slot arg.  Shared between iterations.
;;   RESULT-SLOT — `*mut Sexp', pre-Nil; receives the parsed form.
;;   SLOT-POOL   — `*const Sexp::Vector(N)' of pre-Nil slots.  Layout:
;;                   slot 0:  SCRATCH-MUTSTR (Sexp::MutStr; reset
;;                            between lex calls by the parser).
;;                   slot 1:  PAYLOAD-SLOT  (= lexer's text Sexp::Str
;;                            output; consumed before next lex).
;;                   slot 2:  CONST-NIL    — left Sexp::Nil forever as
;;                            a source for "this cell's cdr = nil".
;;                   slot 3:  read-label entry count (reset per form).
;;                   slots 4 + 4d, 4d+1, 4d+2, 4d+3 (= per-depth
;;                            working slots):
;;                              +0  car / item
;;                              +1  cdr / tail
;;                              +2  head symbol (quote-wrap only)
;;                              +3  reserved / Str-staging
;;   DEPTH       — i64 recursion depth (= caller-provided; the safe
;;                 Rust wrapper passes 0).
;;
;; Returns: i64.  1 = success, 2 = end of input (no form, not an
;; error), anything else (-1, 0) = error.  On
;; error, `*RESULT-SLOT' is `Sexp::Nil'.
;;
;; Read-label metadata/value pairs grow down from the pool tail.  The safe
;; Rust wrapper allocates SLOT-POOL with enough capacity for the deepest
;; expected nesting (= 4 + 4 * MAX_DEPTH slots without labels).  For the
;; bootstrap subset MAX_DEPTH = 32 is a generous ceiling (Doc 44 §3.2
;; pegs the deepest input at ~10).
;;
;; Decoding strategy for lexer payloads:
;;   Int  (kind 20): walk the payload Sexp::Str via `str-byte-at' +
;;                   `str-len' (§101.C) using a digit loop with
;;                   sign handling; write via `sexp-int-make' (§100.B).
;;   Sym  (kind 23): extract bytes-ptr + len from the payload
;;                   Sexp::Str header (offsets 16 / 24 per
;;                   `nelisp-sexp-layout.el') via `ptr-read-u64'
;;                   (§122.E), call `sexp-write-symbol' (§122.A) so
;;                   the new Sexp::Symbol owns its own String (= the
;;                   payload-slot is free for reuse).  Recognise
;;                   "nil" / "t" specially (= produce Sexp::Nil /
;;                   Sexp::T inline).
;;   Str  (kind 22): same as Sym but `sexp-write-str' + no nil/t
;;                   recognition.
;;   Char (kind 24): walk the payload body bytes (= what came after
;;                   `?').  Single ASCII byte -> codepoint.  Leading
;;                   `\\' triggers escape decoding: named escapes
;;                   (n/t/r/s/e/b/d/a/f/v/0), `\\xHH' hex, `\\C-X'
;;                   control, `\\M-X' Meta (= base | 0x8000000).
;;                   Result via `sexp-int-make'.
;;   RadixInt (kind 25): payload first byte = base marker
;;                       (`x'=hex/`o'=oct/`b'=bin); remaining bytes =
;;                       optional sign + digits + underscores.  Loop
;;                       multiplying accumulator by base; result via
;;                       `sexp-int-make'.
;;
;; Quote-family desugar: each prefix `'`, `` ` ``, `,`, `,@`, `#''
;; becomes `(HEAD INNER)' via two cons-make-with-clones (= (cons HEAD
;; (cons INNER nil))).  HEAD symbol bytes are pushed into
;; SCRATCH-MUTSTR via `mut-str-push-byte' then finalised +
;; retag-byte-overwritten to Sexp::Symbol.

;;; Code:

(defconst nelisp-cc-reader-parser--source
  '(seq

    ;; ===========================================================
    ;; `prog2 EFFECT VAL' — side-effect sequencer.
    ;; ===========================================================

    (defun nelisp_reader_p_prog2 (_eff val) val)

    ;; ===========================================================
    ;; Slot-pool indexing helpers (= compute slot index for the
    ;; per-depth working slots).  Slot 3 stores read-label state;
    ;; each depth gets 4 slots starting at offset (4 + d*4).
    ;; ===========================================================

    (defun nelisp_reader_p_car_idx   (d) (+ 4 (* d 4)))
    (defun nelisp_reader_p_cdr_idx   (d) (+ 5 (* d 4)))
    (defun nelisp_reader_p_head_idx  (d) (+ 6 (* d 4)))
    (defun nelisp_reader_p_spare_idx (d) (+ 7 (* d 4)))

    ;; ===========================================================
    ;; Slot address helper (Doc 147 Phase 1.5 Group P).  The slot-pool
    ;; is now a RAW 32B-slot buffer (alloc-bytes (* cap 32) 8) rather
    ;; than a GC-managed Sexp::Vector.  Slot N lives at BASE + N*32.
    ;; This is a pure idempotent address — exactly like the old
    ;; `vector-ref-ptr' on a 32B-stride Vector — so recursive
    ;; write-through composes identically.
    ;; ===========================================================

    (defun nelisp_reader_p_slot (base n) (+ base (* n 32)))

    ;; The token and hash scratch slots remain physical Sexp views. Value
    ;; slots carry WORDs, identified explicitly for the mixed-pool GC walker.
    ;; Clearing a parse slot clears the marker together with its contents.
    (defun nelisp_reader_p_word_slot_set (slot word)
      (seq (ptr-write-u64 slot 24 215)
           (ptr-write-u64 slot 0 word) word))

    ;; ===========================================================
    ;; Reader depth guard.  Mirrors `nelisp_eval_call_stash_excessive_
    ;; lisp_nesting' (scripts/nelisp-standalone-build.el, the eval-side
    ;; `rec_max' guard) for the recursive-descent parser above: every
    ;; nesting level costs 4 slots starting at (4 + d*4) in SLOT-POOL,
    ;; and nothing checked that against the pool's actual allocation
    ;; before this fix.  Past the bound, `nelisp_reader_p_slot' keeps
    ;; computing addresses that walk off the caller's `alloc-bytes'
    ;; region into whatever the flat arena placed next -- silently, the
    ;; same way an unchecked C buffer overrun would.  Measured
    ;; 2026-08-22 on the CLI driver's fixed 32768-slot pool (see
    ;; `driver' in nelisp-standalone-build.el, register @268436448):
    ;; well past the bound (depth 100k-200k) the corruption reads back
    ;; as a plausible-looking value (print truncated to ~8192 opens/
    ;; closes -- the printer is not at fault, see the CLI driver's own
    ;; comment); far enough past it (~2,000,000) the walk finally
    ;; leaves mapped memory and the process SIGSEGVs.  The bound is
    ;; read from the SAME register every parse-loop entry point
    ;; (`bf_load_readable' / `bf_eval_source_string' /
    ;; `bf_read_all_from_string_native' / the CLI driver) already
    ;; writes ITS pool's slot capacity into before running a parse loop
    ;; and restores afterward, so this guard is exact for whichever
    ;; pool is live instead of one fixed number that would be wrong
    ;; for most callers (their pools range from 256 to 4,194,304
    ;; slots).  The -8 margin below covers the widest per-depth slot
    ;; offset (`spare_idx' = 7 + d*4) plus one word of slack.
    ;; ===========================================================

    (defun nelisp_reader_p_pool_cap () (heap-read-u64 268436448 0))
    (defun nelisp_reader_p_label_count (pool)
      (heap-read-u64 (nelisp_reader_p_slot pool 3) 8))
    (defun nelisp_reader_p_max_depth ()
      (/ (- (nelisp_reader_p_pool_cap) 8) 4))
    ;; Arity 2 (even, with a `_pad'), matching `nelisp_eval_call_stash_
    ;; excessive_lisp_nesting''s `(_env rec_max)' shape (scripts/nelisp-
    ;; standalone-build.el) rather than the natural single-argument shape --
    ;; this codebase's other "arity N (even)" comments document a SysV
    ;; AMD64 stack-alignment requirement at static-emit call sites, and
    ;; kept even here for the same reason.  `(not ...)' is the actual
    ;; primitive this DSL does not support (0 hits inside any AOT `(seq
    ;; ...)' blob in the whole tree, vs. host-Emacs generator code where it
    ;; is common) -- an earlier draft calling `(not (nelisp_reader_p_depth_
    ;; ok_p depth))' segfaulted even with `depth_ok_p''s body hardcoded to
    ;; return 1 unconditionally, which is why the call site below compares
    ;; the result to 0 explicitly instead.
    ;; Also records the deepest level this read reached in slot 3 (+16),
    ;; next to the label count, so `nelisp_reader_p_clear_used' knows which
    ;; slots to clear when the read ends.
    (defun nelisp_reader_p_depth_ok_p (pool d)
      (seq
       (if (> d (heap-read-u64 (nelisp_reader_p_slot pool 3) 16))
           (ptr-write-u64 (nelisp_reader_p_slot pool 3) 16 d)
         0)
       (< (nelisp_reader_p_spare_idx d)
          (- (nelisp_reader_p_pool_cap)
             (* 2 (nelisp_reader_p_label_count pool))))))

    ;; Zero slots FROM..TO-1 of POOL.
    (defun nelisp_reader_p_zero_slots (pool from to)
      (let* ((a (nelisp_reader_p_slot pool from))
             (e (nelisp_reader_p_slot pool to)))
        (seq
         (while (< a e)
           (seq (ptr-write-u64 a 0 0) (setq a (+ a 8))))
         0)))

    ;; Clear every slot one read used once it ends, so no stale Sexp is left
    ;; in a pool that outlives the read.  A form-boundary collection does not
    ;; walk pool slots (`nl_gc_collect_form_boundary'), so whatever only a
    ;; stale slot referenced is freed there and its memory reused.  A later
    ;; mid-form collection (`nl_gc_midform_collect') walks every slot of a
    ;; recorded frame's pool, and a stale slot's tag then describes an object
    ;; that is no longer there.  Measured on Windows (F1B, 2026-10-17): a
    ;; tag-6 MutStr left in the token payload slot named memory that held a
    ;; live cons box.  The MutStr arm of `nl_gc_mark_slot' set that box's
    ;; mark bit without tracing its car, the real cons trace then stopped at
    ;; "already marked", and the car block was swept and reused.  The read's
    ;; value is in RESULT-SLOT, outside the pool; label entries are resolved
    ;; by now.  Cleared: scratch, payload and label header (slots 0-3), the
    ;; per-depth slots down to the deepest level reached, the label entries
    ;; and the context slot at PARSE-CAP.
    (defun nelisp_reader_p_clear_used (pool parse-cap)
      (let* ((deepest (heap-read-u64 (nelisp_reader_p_slot pool 3) 16))
             (lcount (nelisp_reader_p_label_count pool))
             (low-end (+ (nelisp_reader_p_spare_idx deepest) 1))
             (high-start (- parse-cap (* 2 lcount))))
        (seq
         (nelisp_reader_p_zero_slots
          pool 0 (if (< low-end parse-cap) low-end parse-cap))
         (if (< high-start 0) 0
           (nelisp_reader_p_zero_slots pool high-start (+ parse-cap 1)))
         0)))
    (defun nelisp_reader_p_stash_excessive_nesting (max-depth _pad)
      (let* ((sig-tag (alloc-bytes 32 8)) (sig-val (alloc-bytes 32 8)) (tag-buf (alloc-bytes 24 1)))
        (seq
         ;; "excessive-lisp-nesting" (22 bytes) -- the same condition
         ;; symbol the eval-side guard raises, so both land in one
         ;; `(condition-case e (...) (error ...))' clause.
         (ptr-write-u64 tag-buf 0 8532477908489566309)
         (ptr-write-u64 (+ tag-buf 8) 0 7939125359116299621)
         (ptr-write-u64 (+ tag-buf 16) 0 113723913302885)
         (nl_alloc_symbol tag-buf 22 sig-tag)
         (ptr-write-u64 sig-val 0 2)
         (ptr-write-u64 (+ sig-val 8) 0 max-depth)
         (ptr-write-u64 (+ sig-val 16) 0 0)
         (ptr-write-u64 (+ sig-val 24) 0 0)
         (nl_exit_signal_w sig-tag sig-val)
         -1)))

    ;; Doc 215 word ABI: when a reader failure left no signal stashed (the
    ;; legacy callers raised `invalid-read-syntax' themselves), stash the same
    ;; condition for SRC; a signal already stashed (nesting, raw byte) is kept.
    (defun nelisp_reader_p_fail_syntax (str-ptr)
      (if (= (heap-read-u64 268435472 0) 1) 0
        (seq (bf_read_syntax_error str-ptr) 0)))

    (defun nelisp_reader_p_stash_raw_byte_unrepresentable ()
      (let* ((sig-tag (alloc-bytes 32 8)) (sig-val (alloc-bytes 32 8)) (tag-buf (alloc-bytes 32 1)))
        (seq
         ;; "nelisp-raw-byte-unrepresentable" (31 bytes), the same condition
         ;; used by concat/format/string-to-multibyte in the runtime.
         (ptr-write-u64 tag-buf 0 8227355735268025710)
         (ptr-write-u64 (+ tag-buf 8) 0 3271148769041545057)
         (ptr-write-u64 (+ tag-buf 16) 0 8315178114073390709)
         (ptr-write-u64 (+ tag-buf 24) 0 28548142445391461)
         (nl_alloc_symbol tag-buf 31 sig-tag)
         (ptr-write-u64 sig-val 0 0)
         (ptr-write-u64 (+ sig-val 8) 0 0)
         (ptr-write-u64 (+ sig-val 16) 0 0)
         (ptr-write-u64 (+ sig-val 24) 0 0)
         (nl_exit_signal_w sig-tag sig-val)
         -1)))

    ;; ===========================================================
    ;; ASCII digit predicate.
    ;; ===========================================================

    (defun nelisp_reader_p_is_digit (b)
      (if (>= b 48) (if (<= b 57) 1 0) 0))

    ;; ===========================================================
    ;; atoi over a Sexp::Str payload — sign + digit loop.
    ;; ===========================================================

    (defun nelisp_reader_p_atoi_step (str-ptr i n acc)
      (if (>= i n)
          acc
        (if (= (nelisp_reader_p_is_digit (str-byte-at str-ptr i)) 1)
            (nelisp_reader_p_atoi_step
             str-ptr (+ i 1) n
             (+ (* acc 10) (- (str-byte-at str-ptr i) 48)))
          acc)))

    (defun nelisp_reader_p_atoi (str-ptr)
      (if (>= (str-len str-ptr) 1)
          (cond
           ((= (str-byte-at str-ptr 0) 45)
            (- 0 (nelisp_reader_p_atoi_step
                  str-ptr 1 (str-len str-ptr) 0)))
           ((= (str-byte-at str-ptr 0) 43)
            (nelisp_reader_p_atoi_step
             str-ptr 1 (str-len str-ptr) 0))
           (t (nelisp_reader_p_atoi_step
               str-ptr 0 (str-len str-ptr) 0)))
        0))

    ;; ===========================================================
    ;; "nil" / "t" name recognition (byte compare).
    ;; ===========================================================

    (defun nelisp_reader_p_is_nil_name (str-ptr)
      (if (= (str-len str-ptr) 3)
          (if (= (str-byte-at str-ptr 0) 110)
              (if (= (str-byte-at str-ptr 1) 105)
                  (if (= (str-byte-at str-ptr 2) 108) 1 0)
                0)
            0)
        0))

    (defun nelisp_reader_p_is_t_name (str-ptr)
      (if (= (str-len str-ptr) 1)
          (if (= (str-byte-at str-ptr 0) 116) 1 0)
        0))

    ;; ===========================================================
    ;; Copy Sexp::Str header members from SRC-STR-SLOT into a
    ;; fresh Sexp::Symbol / Sexp::Str at DEST-SLOT.  See
    ;; nelisp-sexp-layout.el for the offsets 16 / 24.
    ;; ===========================================================

    (defun nelisp_reader_p_symbol_name_kind (source name)
      ;; Symbol names inherit the reader input's representation, including
      ;; multibyte ASCII. Set it before the canonical name is registered.
      (if (or (= (sexp-tag source) 14) (= (sexp-tag source) 15))
          (ptr-write-u8 name 0 14)
        (seq (ptr-write-u8 name 0 5)
             (ptr-write-u64 name 8
               (logior (heap-read-u64 name 8) 4611686018427387904)))))

    (defun nelisp_reader_p_copy_symbol (dest-slot src-str-slot)
      ;; The direct allocator also installs a stable name-Sexp edge for the
      ;; interned symbol; the reader token slot itself is scratch storage.
      (let* ((rc (nl_alloc_symbol_named
                  (heap-read-u64 src-str-slot 16)
                  (heap-read-u64 src-str-slot 24)
                  src-str-slot
                  dest-slot)))
        ;; Doc 226 D9 stage 3.1: the symbol gets its object when it is read.
        (seq (if (= rc 0) 0 (extern-call nl_symobj_note dest-slot)) rc)))

    (defun nelisp_reader_p_make_uninterned_symbol (dest-slot src-str-slot)
      (let* ((identity (nl_next_symbol_identity)))
        (if (= (nl_alloc_uninterned_symbol_named
                (heap-read-u64 src-str-slot 16)
                (heap-read-u64 src-str-slot 24)
                identity src-str-slot dest-slot) 0)
            -1 1)))

    (defun nelisp_reader_p_copy_str (dest-slot src-str-slot)
      (let* ((written
              (sexp-write-str
               dest-slot
               (heap-read-u64 src-str-slot 16)
               (heap-read-u64 src-str-slot 24))))
        (if (= (sexp-tag src-str-slot) 14)
            (nelisp_reader_p_prog2
             (ptr-write-u8 dest-slot 0 14)
             written)
          written)))

    ;; ===========================================================
    ;; Hex-digit decoder.  Returns 0..15 for `0'..`9'/`a'..`f'/`A'..`F',
    ;; -1 otherwise.
    ;; ===========================================================

    (defun nelisp_reader_p_hex_digit (b)
      (cond
       ((>= b 48) (if (<= b 57) (- b 48)
                    (if (>= b 97) (if (<= b 102) (+ (- b 97) 10) -1)
                      (if (>= b 65) (if (<= b 70) (+ (- b 65) 10) -1)
                        -1))))
       (t -1)))

    ;; ===========================================================
    ;; Radix-int decode (kind 25 payload).
    ;;
    ;; Payload first byte = base marker `x'/`o'/`b'.  Remaining bytes
    ;; = optional sign + base-N digits + optional `_' separators.
    ;; Loop multiplying accumulator by BASE; digit < BASE expected.
    ;; ===========================================================

    (defun nelisp_reader_p_radix_step (str-ptr i n base acc)
      (if (>= i n)
          acc
        (cond
         ;; Underscore: skip silently.
         ((= (str-byte-at str-ptr i) 95)
          (nelisp_reader_p_radix_step str-ptr (+ i 1) n base acc))
         (t
          (nelisp_reader_p_radix_dispatch
           str-ptr i n base acc
           (nelisp_reader_p_hex_digit (str-byte-at str-ptr i)))))))

    (defun nelisp_reader_p_radix_dispatch (str-ptr i n base acc digit)
      ;; If DIGIT is negative OR >= BASE, bail (= leave ACC as-is).
      (if (< digit 0)
          acc
        (if (>= digit base)
            acc
          (nelisp_reader_p_radix_step
           str-ptr (+ i 1) n base
           (+ (* acc base) digit)))))

    (defun nelisp_reader_p_decode_radix (payload-slot)
      ;; Payload layout: [base-byte] [opt-sign] [digits...].  We have
      ;; already consumed byte 0 to dispatch on BASE.
      (nelisp_reader_p_decode_radix_dispatch
       payload-slot
       (str-byte-at payload-slot 0)))

    (defun nelisp_reader_p_decode_radix_dispatch (payload-slot base-byte)
      (cond
       ;; `x' -> base 16.
       ((= base-byte 120)
        (nelisp_reader_p_decode_radix_signed payload-slot 16))
       ;; `o' -> base 8.
       ((= base-byte 111)
        (nelisp_reader_p_decode_radix_signed payload-slot 8))
       ;; `b' -> base 2.
       ((= base-byte 98)
        (nelisp_reader_p_decode_radix_signed payload-slot 2))
       (t 0)))

    (defun nelisp_reader_p_decode_radix_signed (payload-slot base)
      ;; Handle optional `+'/`-' sign at byte 1.  Loop starts at byte 2
      ;; if signed, byte 1 otherwise.
      (if (>= (str-len payload-slot) 2)
          (cond
           ((= (str-byte-at payload-slot 1) 45)
            (- 0 (nelisp_reader_p_radix_step
                  payload-slot 2 (str-len payload-slot) base 0)))
           ((= (str-byte-at payload-slot 1) 43)
            (nelisp_reader_p_radix_step
             payload-slot 2 (str-len payload-slot) base 0))
           (t
            (nelisp_reader_p_radix_step
             payload-slot 1 (str-len payload-slot) base 0)))
        0))

    ;; ===========================================================
    ;; Char-literal decode (kind 24 payload).
    ;;
    ;; Payload = the body bytes AFTER the leading `?'.  Two shapes:
    ;;   - One UTF-8 codepoint X      -> codepoint X.
    ;;   - `\\X...' escape sequence   -> named escape / hex / C- / M-.
    ;;
    ;; Mirror `build-tool/src/reader/lexer.rs::read_char_literal'.
    ;; ===========================================================

    (defun nelisp_reader_p_decode_utf8_char (payload-slot)
      (let ((n (str-len payload-slot)))
        (cond
         ((= n 1)
          (str-byte-at payload-slot 0))
         ((= n 2)
          (+ (* (logand (str-byte-at payload-slot 0) 31) 64)
             (logand (str-byte-at payload-slot 1) 63)))
         ((= n 3)
          (+ (* (logand (str-byte-at payload-slot 0) 15) 4096)
             (+ (* (logand (str-byte-at payload-slot 1) 63) 64)
                (logand (str-byte-at payload-slot 2) 63))))
         ((= n 4)
          (+ (* (logand (str-byte-at payload-slot 0) 7) 262144)
             (+ (* (logand (str-byte-at payload-slot 1) 63) 4096)
                (+ (* (logand (str-byte-at payload-slot 2) 63) 64)
                   (logand (str-byte-at payload-slot 3) 63)))))
         (t 0))))

    (defun nelisp_reader_p_decode_char (payload-slot)
      (let* ((n (str-len payload-slot))
             (escaped (if (and (> n 0) (= (str-byte-at payload-slot 0) 92)) 1 0))
             (packed (nelisp_reader_escape_scan payload-slot escaped n escaped 0 0 0)))
        (if (< packed 0) 0 (logand packed 2147483647))))

    (defun nelisp_reader_p_decode_char_escape (payload-slot start esc-byte)
      ;; START points at the escape selector byte (= byte AFTER `\\').
      (cond
       ;; Named single-letter escapes.
       ((= esc-byte 110) 10)        ; \\n -> LF
       ((= esc-byte 116) 9)         ; \\t -> TAB
       ((= esc-byte 114) 13)        ; \\r -> CR
       ((= esc-byte 92) 92)         ; \\\\ -> backslash
       ((= esc-byte 39) 39)         ; \\' -> '
       ((= esc-byte 34) 34)         ; \\" -> "
       ((= esc-byte 115) 32)        ; \\s -> space
       ((= esc-byte 101) 27)        ; \\e -> ESC
       ((= esc-byte 98) 8)          ; \\b -> backspace
       ((= esc-byte 100) 127)       ; \\d -> delete
       ((= esc-byte 97) 7)          ; \\a -> bell
       ((= esc-byte 102) 12)        ; \\f -> form feed
       ((= esc-byte 118) 11)        ; \\v -> vertical tab
       ((= esc-byte 48) 0)          ; \\0 -> NUL
       ;; \\xHH -> 2 hex digits.
       ((= esc-byte 120)
        (nelisp_reader_p_decode_char_hex
         payload-slot (+ start 1)))
       ;; \\C-X -> control modifier.
       ((= esc-byte 67)
        (nelisp_reader_p_decode_char_ctrl
         payload-slot (+ start 1)))
       ;; \\^X -> control modifier (caret synonym for `\\C-').
       ((= esc-byte 94)
        (nelisp_reader_p_decode_char_caret
         payload-slot (+ start 1)))
       ;; \\M-X -> meta modifier (set bit 27 = 0x8000000).
       ((= esc-byte 77)
        (nelisp_reader_p_decode_char_meta
         payload-slot (+ start 1)))
       ;; Doc 51 Phase 3-A''-1 — unknown `\\X' = literal X.
       (t esc-byte)))

    (defun nelisp_reader_p_decode_char_hex (payload-slot start)
      ;; \\xHH — 2 hex digits at START / START+1.
      (if (>= (str-len payload-slot) (+ start 2))
          (+ (* (nelisp_reader_p_hex_digit
                 (str-byte-at payload-slot start)) 16)
             (nelisp_reader_p_hex_digit
              (str-byte-at payload-slot (+ start 1))))
        0))

    (defun nelisp_reader_p_decode_char_ctrl (payload-slot start)
      ;; \\C-X — expect `-' at START, then the control target at START+1.
      ;; Control char = (byte & 0x1f), except `?' denotes DEL (127).
      (if (>= (str-len payload-slot) (+ start 2))
          (if (= (str-byte-at payload-slot start) 45)
              (if (= (str-byte-at payload-slot (+ start 1)) 63)
                  127
                (logand (str-byte-at payload-slot (+ start 1)) 31))
            0)
        0))

    (defun nelisp_reader_p_decode_char_caret (payload-slot start)
      ;; \\^X — START points directly at the control target.
      (if (>= start (str-len payload-slot))
          0
        (if (= (str-byte-at payload-slot start) 63)
            127
          (logand (str-byte-at payload-slot start) 31))))

    (defun nelisp_reader_p_decode_char_meta (payload-slot start)
      ;; \\M-X — expect `-' at START, then the meta target at START+1.
      ;; Meta char = (base | 0x8000000).  Handles single-byte target
      ;; only (= nested \\M-\\C-x is left to the Rust fallback path
      ;; for now; that path is deeply rare in the bootstrap subset).
      (if (>= (str-len payload-slot) (+ start 2))
          (if (= (str-byte-at payload-slot start) 45)
              (nelisp_reader_p_decode_char_meta_target
               payload-slot (+ start 1))
            0)
        0))

    (defun nelisp_reader_p_decode_char_meta_target (payload-slot at)
      ;; AT points at the byte after `M-'.  Handle nested `\\C-X' /
      ;; named escape inside Meta; otherwise the raw byte.
      (if (= (str-byte-at payload-slot at) 92)
          ;; \\M-\\X — nested escape.
          (if (>= (str-len payload-slot) (+ at 2))
              (logior
               (nelisp_reader_p_decode_char_meta_nested
                payload-slot (+ at 1)
                (str-byte-at payload-slot (+ at 1)))
               134217728)             ; 0x8000000
            0)
        ;; Raw byte target.
        (logior (str-byte-at payload-slot at) 134217728)))

    (defun nelisp_reader_p_decode_char_meta_nested (payload-slot start inner)
      (cond
       ;; \\M-\\C-X: control over the byte at START+2.
       ((= inner 67)
        (if (>= (str-len payload-slot) (+ start 3))
            (if (= (str-byte-at payload-slot (+ start 1)) 45)
                (if (= (str-byte-at payload-slot (+ start 2)) 63)
                    127
                  (logand (str-byte-at payload-slot (+ start 2)) 31))
              0)
          0))
       ((= inner 94)
        (nelisp_reader_p_decode_char_caret payload-slot (+ start 1)))
       ((= inner 110) 10)
       ((= inner 116) 9)
       ((= inner 114) 13)
       (t inner)))

    ;; Complete modifier-chain decoder used by `decode_char'.  The older
    ;; single-prefix helpers above remain for ABI stability, but no longer
    ;; drive the production path.
    (defun nelisp_reader_p_decode_char_resolve (chr modifiers)
      ;; Control folds only GNU's ASCII control range.  For any other target
      ;; the control event bit remains pending on the character literal.
      (if (= (logand modifiers 67108864) 67108864)
          (cond
           ((= chr 63) (+ 127 (- modifiers 67108864)))
           ((and (>= chr 64) (<= chr 95))
            (+ (logand chr 31) (- modifiers 67108864)))
           ((and (>= chr 97) (<= chr 122))
            (+ (logand chr 31) (- modifiers 67108864)))
           (t (+ chr modifiers)))
        (+ chr modifiers)))

    (defun nelisp_reader_p_decode_char_base
        (payload-slot at modifiers selector escaped)
      (nelisp_reader_p_decode_char_resolve
       (if (= escaped 0)
           selector
         (cond
          ((= selector 110) 10)
          ((= selector 116) 9)
          ((= selector 114) 13)
          ((= selector 115) 32)
          ((= selector 101) 27)
          ((= selector 98) 8)
          ((= selector 100) 127)
          ((= selector 97) 7)
          ((= selector 102) 12)
          ((= selector 118) 11)
          ((= selector 48) 0)
          ((= selector 120)
           (nelisp_reader_p_decode_char_hex payload-slot (+ at 1)))
          (t selector)))
       modifiers))

    (defun nelisp_reader_p_decode_char_modifier
        (payload-slot at modifiers escaped)
      (if (>= at (str-len payload-slot))
          0
        (let* ((selector (str-byte-at payload-slot at)))
          (cond
           ;; Meta/Shift/Hyper/Alt/Super.
           ((and (= escaped 1)
                 (or (= selector 77) (= selector 83) (= selector 72)
                     (= selector 65)
                     (and (= selector 115)
                          (< (+ at 1) (str-len payload-slot))
                          (= (str-byte-at payload-slot (+ at 1)) 45))))
            (if (or (>= (+ at 1) (str-len payload-slot))
                    (= (= (str-byte-at payload-slot (+ at 1)) 45) 0))
                0
              (let* ((next (+ at 2))
                     (bit (cond
                           ((= selector 77) 134217728)
                           ((= selector 83) 33554432)
                           ((= selector 72) 16777216)
                           ((= selector 65) 4194304)
                           (t 8388608))))
                (if (and (< next (str-len payload-slot))
                         (= (str-byte-at payload-slot next) 92))
                    (nelisp_reader_p_decode_char_modifier
                     payload-slot (+ next 1) (logior modifiers bit) 1)
                  (nelisp_reader_p_decode_char_modifier
                   payload-slot next (logior modifiers bit) 0)))))
           ;; Control and caret synonym.
           ((and (= escaped 1) (= selector 67))
            (if (or (>= (+ at 1) (str-len payload-slot))
                    (= (= (str-byte-at payload-slot (+ at 1)) 45) 0))
                0
              (let* ((next (+ at 2)))
                (if (and (< next (str-len payload-slot))
                         (= (str-byte-at payload-slot next) 92))
                    (nelisp_reader_p_decode_char_modifier
                     payload-slot (+ next 1)
                     (logior modifiers 67108864) 1)
                  (nelisp_reader_p_decode_char_modifier
                   payload-slot next (logior modifiers 67108864) 0)))))
           ((and (= escaped 1) (= selector 94))
            (let* ((next (+ at 1)))
              (if (and (< next (str-len payload-slot))
                       (= (str-byte-at payload-slot next) 92))
                  (nelisp_reader_p_decode_char_modifier
                   payload-slot (+ next 1)
                   (logior modifiers 67108864) 1)
                (nelisp_reader_p_decode_char_modifier
                 payload-slot next (logior modifiers 67108864) 0))))
           (t
            (nelisp_reader_p_decode_char_base
             payload-slot at modifiers selector escaped))))))

    ;; ===========================================================
    ;; Materialise a leaf token into RESULT-SLOT.  RESULT-SLOT
    ;; must be pre-Nil on entry.  Returns 1 on success.
    ;; ===========================================================

    (defun nelisp_reader_p_leaf (kind result-slot payload-slot)
      (let* ((w (nelisp_reader_p_leaf_w kind 0 payload-slot)))
        (if (= w 0) -1
          (if (= w 2) 2 (seq (nl_abi_word_to_rc w result-slot) 1)))))
    ;; Doc 215: produce the WORD directly; legacy callers retain their slot ABI.
    (defun nelisp_reader_p_leaf_w (kind _pad payload-slot)
      (cond
       ((= kind 20) (nl_read_int_or_bignum_w payload-slot 0))
       ((= kind 22)
        (let ((object (alloc-bytes 32 8)))
          (seq (nelisp_reader_p_copy_str object payload-slot) object)))
       ((= kind 23)
        (cond ((= (nelisp_reader_p_is_nil_name payload-slot) 1) 3)
              ((= (nelisp_reader_p_is_t_name payload-slot) 1) 7)
              (t (let* ((w (nl_alloc_symbol_named_w
                            (heap-read-u64 payload-slot 16)
                            (heap-read-u64 payload-slot 24) payload-slot 0)))
                   ;; Doc 226 D9 stage 3.1/3.4: the symbol gets its object when it is
                   ;; read, and the reader answers that object's WORD.
                   (if (= w 0) 0 (extern-call nl_symobj_of w))))))
       ((= kind 24) (nl_imm_int (nelisp_reader_p_decode_char payload-slot)))
       ((= kind 25) (nl_imm_int (nelisp_reader_p_decode_radix payload-slot)))
       ((= kind 21)
        (let ((object (alloc-bytes 32 8)))
          (if (= (extern-call nl_str_to_float
                              (heap-read-u64 payload-slot 16)
                              (heap-read-u64 payload-slot 24) object) 1)
              object
            (nl_abi_fail 0 0))))
       (t (nl_abi_fail -1 0))))

    ;; ===========================================================
    ;; Reset the scratch-mutstr (= drain it for the next lex run).
    ;; ===========================================================

    (defun nelisp_reader_p_reset_scratch (scratch-slot)
      (mut-str-make-empty scratch-slot 64))

    ;; ===========================================================
    ;; Drive one lex call.  Returns the i64 kind code.
    ;; ===========================================================

    (defun nelisp_reader_p_lex_one
        (str-ptr cursor-slot payload-slot scratch-slot)
      (nelisp_reader_p_prog2
       (nelisp_reader_p_reset_scratch scratch-slot)
       (extern-call nelisp_reader_lex_one
                    str-ptr
                    (sexp-int-unwrap cursor-slot)
                    payload-slot
                    cursor-slot
                    scratch-slot)))

    ;; ===========================================================
    ;; Build the Sexp::Symbol of one of the quote-family heads into HEAD-SLOT.
    ;; TAG-BYTE selects: 113 quote, 98 backquote, 99 comma, 100 comma-at,
    ;; 102 function.  Doc 226 D9 stage 3.4: GNU reads these as the interned
    ;; Qquote, Qbackquote, Qcomma, Qcomma_at and Qfunction, so the head is the
    ;; interned symbol's canonical view (find-or-insert in the intern table,
    ;; as for a leaf), whose object exists (`nl_symobj_note') and which every
    ;; store turns into its WORD.  A fresh string-backed view had no canonical
    ;; name buffer, so it stayed a view in 4,515 places of the cold image.
    ;; SCRATCH-SLOT is unused.  Returns 1 on success, 0 on unknown TAG-BYTE.
    (defun nelisp_reader_p_build_head_symbol
        (head-slot scratch-slot tag-byte)
      (let* ((bytes (alloc-bytes 8 1)))
        (cond
         ((= tag-byte 113)
          (seq (ptr-write-u64 bytes 0 435745158513)            ; quote
               (nl_alloc_symbol bytes 5 head-slot)
               (extern-call nl_symobj_note head-slot)
               1))
         ((= tag-byte 98)
          (seq (ptr-write-u64 bytes 0 96)                       ; `
               (nl_alloc_symbol bytes 1 head-slot)
               (extern-call nl_symobj_note head-slot)
               1))
         ((= tag-byte 99)
          (seq (ptr-write-u64 bytes 0 44)                       ; ,
               (nl_alloc_symbol bytes 1 head-slot)
               (extern-call nl_symobj_note head-slot)
               1))
         ((= tag-byte 100)
          (seq (ptr-write-u64 bytes 0 16428)                    ; ,@
               (nl_alloc_symbol bytes 2 head-slot)
               (extern-call nl_symobj_note head-slot)
               1))
         ((= tag-byte 102)
          (seq (ptr-write-u64 bytes 0 7957695015192261990)      ; function
               (nl_alloc_symbol bytes 8 head-slot)
               (extern-call nl_symobj_note head-slot)
               1))
         (t 0))))

    ;; ===========================================================
    ;; Read labels (`#N=' / `#N#').  Slot 3 stores the entry count.  Two
    ;; slots per entry grow down from the pool tail: a Nil-tagged metadata
    ;; slot (kind at +8, label at +16) and one rooted Sexp value.  Kind 1 is
    ;; a pending definition, 2 a completed definition, 3 a reference proxy.
    ;; ===========================================================

    (defun nelisp_reader_p_label_entry_index (i)
      (- (nelisp_reader_p_pool_cap) (* 2 (+ i 1))))

    (defun nelisp_reader_p_label_meta (pool i)
      (nelisp_reader_p_slot pool (nelisp_reader_p_label_entry_index i)))

    (defun nelisp_reader_p_label_value (pool i)
      (nelisp_reader_p_slot pool (+ (nelisp_reader_p_label_entry_index i) 1)))

    (defun nelisp_reader_p_label_set_count (pool count)
      (ptr-write-u64 (nelisp_reader_p_slot pool 3) 8 count))

    (defun nelisp_reader_p_label_find_kind (pool label i)
      (if (>= i (nelisp_reader_p_label_count pool))
          0
        (let* ((meta (nelisp_reader_p_label_meta pool i))
               (kind (heap-read-u64 meta 8)))
          (if (and (or (= kind 1) (= kind 2))
                   (= (heap-read-u64 meta 16) label))
              kind
            (nelisp_reader_p_label_find_kind pool label (+ i 1))))))

    (defun nelisp_reader_p_label_find_value (pool label i)
      (if (>= i (nelisp_reader_p_label_count pool))
          0
        (let* ((meta (nelisp_reader_p_label_meta pool i))
               (kind (heap-read-u64 meta 8)))
          (if (and (= kind 2) (= (heap-read-u64 meta 16) label))
              (heap-read-u64 (nelisp_reader_p_label_value pool i) 0)
            (nelisp_reader_p_label_find_value pool label (+ i 1))))))

    (defun nelisp_reader_p_label_add_pending (pool depth label)
      (let* ((count (nelisp_reader_p_label_count pool))
             (entry-index (nelisp_reader_p_label_entry_index count)))
        (if (<= entry-index (nelisp_reader_p_spare_idx depth))
            0
          (let* ((meta (nelisp_reader_p_label_meta pool count))
                 (value (nelisp_reader_p_label_value pool count)))
            (seq
             (ptr-write-u64 meta 0 0)
             (ptr-write-u64 meta 8 1)
             (ptr-write-u64 meta 16 label)
             (ptr-write-u64 meta 24 0)
             (nelisp_reader_p_word_slot_set value 3)
             (nelisp_reader_p_label_set_count pool (+ count 1))
             meta)))))

    (defun nelisp_reader_p_label_add_proxy (pool depth label proxy)
      (let* ((count (nelisp_reader_p_label_count pool))
             (entry-index (nelisp_reader_p_label_entry_index count)))
        (if (<= entry-index (nelisp_reader_p_spare_idx depth))
            0
          (let* ((meta (nelisp_reader_p_label_meta pool count))
                 (value (nelisp_reader_p_label_value pool count)))
            (seq
             (ptr-write-u64 meta 0 0)
             (ptr-write-u64 meta 8 3)
             (ptr-write-u64 meta 16 label)
             (ptr-write-u64 meta 24 0)
             (nelisp_reader_p_word_slot_set value proxy)
             (nelisp_reader_p_label_set_count pool (+ count 1))
             1)))))

    (defun nelisp_reader_p_label_proxy_number (object pool i)
      (if (>= i (nelisp_reader_p_label_count pool))
          -1
        (let* ((meta (nelisp_reader_p_label_meta pool i))
               (proxy (heap-read-u64 (nelisp_reader_p_label_value pool i) 0)))
          (if (and (= (heap-read-u64 meta 8) 3)
                   (= (nl_val_tag object) 7)
                   (= (nl_word_payload object) (nl_word_payload proxy)))
              (heap-read-u64 meta 16)
            (nelisp_reader_p_label_proxy_number object pool (+ i 1))))))

    (defun nelisp_reader_p_parse_label_def (str-ptr cursor-slot result-slot pool depth)
      (let* ((w (nelisp_reader_p_parse_label_def_w str-ptr cursor-slot 0 pool depth)))
        (if (= w 0) -1
          (if (= w 2) 2 (seq (nl_abi_word_to_rc w result-slot) 1)))))
    ;; Doc 215: parsed values are WORDs throughout the recursive reader.
    (defun nelisp_reader_p_parse_label_def_w (str-ptr cursor-slot _pad pool depth)
  (let* ((label (nelisp_reader_p_atoi (nelisp_reader_p_slot pool 1))))
    (if (/= (nelisp_reader_p_label_find_kind pool label 0) 0)
        (nl_abi_fail (nelisp_reader_p_fail_syntax str-ptr) 0)
      (let* ((meta (nelisp_reader_p_label_add_pending pool depth label)))
        (if (= meta 0) (nl_abi_fail (nelisp_reader_p_fail_syntax str-ptr) 0)
          (let* ((w (nelisp_reader_p_parse_at_w str-ptr cursor-slot 0 pool (+ depth 1))))
            (if (or (= w 0) (= w 2)) (nl_abi_fail (nelisp_reader_p_fail_syntax str-ptr) 0)
              (let* ((proxy-label (nelisp_reader_p_label_proxy_number w pool 0)))
                (if (>= proxy-label 0)
                    (setq w (nelisp_reader_p_label_find_value pool proxy-label 0)) 0)
                (if (= w 0) (nl_abi_fail (nelisp_reader_p_fail_syntax str-ptr) 0)
                  (seq (nelisp_reader_p_word_slot_set (+ meta 32) w)
                       (ptr-write-u64 meta 8 2) w))))))))))

    (defun nelisp_reader_p_parse_label_ref (result-slot pool depth)
      (let* ((w (nelisp_reader_p_parse_label_ref_w 0 pool depth)))
        (if (= w 0) -1
          (if (= w 2) 2 (seq (nl_abi_word_to_rc w result-slot) 1)))))
    ;; Doc 215: parsed values are WORDs throughout the recursive reader.
    (defun nelisp_reader_p_parse_label_ref_w (_pad pool depth)
  (let* ((label (nelisp_reader_p_atoi (nelisp_reader_p_slot pool 1)))
         (kind (nelisp_reader_p_label_find_kind pool label 0)))
    (if (= kind 0) (nl_abi_fail -1 0)
      (let* ((proxy (nelisp_cons_construct_w (nl_imm_int label) 3 0)))
        (if (= (nelisp_reader_p_label_add_proxy pool depth label proxy) 1)
            proxy (nl_abi_fail -1 0))))))

    (defun nelisp_reader_p_label_resolve_vector (vec pool i n)
      (if (>= i n)
          vec
        (let* ((resolved
                (nelisp_reader_p_label_resolve
                 (nl_vector_slot_word vec i) pool)))
          (nelisp_reader_p_prog2
           (vector-slot-set vec i resolved)
           (nelisp_reader_p_label_resolve_vector vec pool (+ i 1) n)))))

    (defun nelisp_reader_p_label_resolve_record (record pool i n)
      (if (>= i n)
          record
        (let* ((resolved
                (nelisp_reader_p_label_resolve
                 (nl_record_slot_word record i) pool)))
          (nelisp_reader_p_prog2
           (record-slot-set record i resolved)
           (nelisp_reader_p_label_resolve_record
            record pool (+ i 1) n)))))

    (defun nelisp_reader_p_label_resolve (object pool)
      (let* ((label (nelisp_reader_p_label_proxy_number object pool 0)))
        (if (>= label 0)
            (nelisp_reader_p_label_find_value pool label 0)
          (cond
           ((= (nl_val_tag object) 7)
            (let* ((car-value
                    (nelisp_reader_p_label_resolve
                     (nl_cons_car_word object) pool)))
              (seq
               (cons-set-car object car-value)
               (let* ((cdr-value
                       (nelisp_reader_p_label_resolve
                        (nl_cons_cdr_word object) pool)))
                 (seq (cons-set-cdr object cdr-value) object)))))
           ((= (nl_val_tag object) 8)
            (nelisp_reader_p_label_resolve_vector
             object pool 0 (vector-len object)))
           ((= (nl_val_tag object) 12)
            (nelisp_reader_p_label_resolve_record
             object pool 0 (record-slot-count object)))
           ((= (nl_val_tag object) 17)
            (nelisp_reader_p_label_resolve_record
             object pool 0 (record-slot-count object)))
           (t object)))))

    ;; ===========================================================
    ;; parse_at — lex one token at the current cursor + dispatch.
    ;; Writes the parsed Sexp into RESULT-SLOT.  Returns 1 on
    ;; success, -1 on error.  DEPTH is the current recursion depth
    ;; (= used to index SLOT-POOL for per-depth working slots).
    ;; ===========================================================

    (defun nelisp_reader_p_parse_at (str-ptr cursor-slot result-slot slot-pool depth)
      (let* ((w (nelisp_reader_p_parse_at_w str-ptr cursor-slot 0 slot-pool depth)))
        (if (= w 0) -1
          (if (= w 2) 2 (seq (nl_abi_word_to_rc w result-slot) 1)))))
    ;; Doc 215: produce the WORD directly; legacy callers retain their slot ABI.
    (defun nelisp_reader_p_parse_at_w (str-ptr cursor-slot _pad slot-pool depth)
      (nelisp_reader_p_dispatch_w
       str-ptr cursor-slot 0 slot-pool depth
       (nelisp_reader_p_lex_one
        str-ptr cursor-slot (nelisp_reader_p_slot slot-pool 1)
        (nelisp_reader_p_slot slot-pool 0)))) ; scratch-slot

    (defun nelisp_reader_p_dispatch (str-ptr cursor-slot result-slot slot-pool depth kind)
      (let* ((w (nelisp_reader_p_dispatch_w str-ptr cursor-slot 0 slot-pool depth kind)))
        (if (= w 0) -1
          (if (= w 2) 2 (seq (nl_abi_word_to_rc w result-slot) 1)))))
    ;; Doc 215: parsed values are WORDs throughout the recursive reader.
    (defun nelisp_reader_p_dispatch_w (str-ptr cursor-slot _pad slot-pool depth kind)
  (cond
   ((= (nelisp_reader_p_depth_ok_p slot-pool depth) 0)
    (nl_abi_fail (nelisp_reader_p_stash_excessive_nesting (nelisp_reader_p_max_depth) 0) 0))
   ((= kind -2) (nl_abi_fail (nelisp_reader_p_stash_raw_byte_unrepresentable) 0))
   ((= kind 1) (nelisp_reader_p_parse_list_step_w str-ptr cursor-slot 0 slot-pool depth))
   ((= kind 3) (nelisp_reader_p_parse_vector_w str-ptr cursor-slot 0 slot-pool depth 0))
   ((= kind 5) (nelisp_reader_p_wrap_w str-ptr cursor-slot 0 slot-pool depth 113))
   ((= kind 6) (nelisp_reader_p_wrap_w str-ptr cursor-slot 0 slot-pool depth 98))
   ((= kind 7) (nelisp_reader_p_wrap_w str-ptr cursor-slot 0 slot-pool depth 99))
   ((= kind 8) (nelisp_reader_p_wrap_w str-ptr cursor-slot 0 slot-pool depth 100))
   ((= kind 9) (nelisp_reader_p_wrap_w str-ptr cursor-slot 0 slot-pool depth 102))
   ((= kind 11) (nelisp_reader_p_parse_sharps_paren_w str-ptr cursor-slot 0 slot-pool depth 0))
   ((= kind 12) (nelisp_reader_p_parse_vector_w str-ptr cursor-slot 0 slot-pool depth 0))
   ((= kind 13) (nelisp_reader_p_parse_propertized_string_w str-ptr cursor-slot 0 slot-pool depth))
   ((= kind 14) (nelisp_reader_p_parse_bool_vector_w str-ptr cursor-slot 0 slot-pool depth))
   ((= kind 15) (nelisp_reader_p_parse_byte_code_w str-ptr cursor-slot 0 slot-pool depth))
   ((= kind 26) (nelisp_reader_p_parse_label_def_w str-ptr cursor-slot 0 slot-pool depth))
   ((= kind 27) (nelisp_reader_p_parse_label_ref_w 0 slot-pool depth))
   ((= kind 28) (heap-read-u64 (nelisp_reader_p_slot slot-pool (nelisp_reader_p_pool_cap)) 0))
   ((= kind 29)
    (let* ((object (alloc-bytes 32 8)))
      (nelisp_reader_p_symbol_name_kind str-ptr (nelisp_reader_p_slot slot-pool 1))
      (if (= (nelisp_reader_p_make_uninterned_symbol object (nelisp_reader_p_slot slot-pool 1)) 1)
          object (nl_abi_fail (nelisp_reader_p_fail_syntax str-ptr) 0))))
   ((>= kind 20)
    (seq (if (= kind 23)
             (nelisp_reader_p_symbol_name_kind str-ptr (nelisp_reader_p_slot slot-pool 1))
           0)
         (nelisp_reader_p_leaf_w kind 0 (nelisp_reader_p_slot slot-pool 1))))
   ((= kind 0) 2)
   (t (nl_abi_fail (nelisp_reader_p_fail_syntax str-ptr) 0))))

    (defun nelisp_reader_p_parse_propertized_string (str-ptr cursor-slot result-slot slot-pool depth)
      (let* ((w (nelisp_reader_p_parse_propertized_string_w str-ptr cursor-slot 0 slot-pool depth)))
        (if (= w 0) -1
          (if (= w 2) 2 (seq (nl_abi_word_to_rc w result-slot) 1)))))
    ;; Doc 215: parsed values are WORDs throughout the recursive reader.
    (defun nelisp_reader_p_parse_propertized_string_w (str-ptr cursor-slot _pad slot-pool depth)
  (let* ((body (nelisp_reader_p_parse_list_step_w str-ptr cursor-slot 0 slot-pool (+ depth 1))))
    (if (and (and (/= body 0) (/= body 2)) (= (nl_val_tag body) 7))
        (let* ((first (nl_cons_car_word body)))
          (if (or (= (nl_val_tag first) 5) (= (nl_val_tag first) 14))
              first (nl_abi_fail (nelisp_reader_p_fail_syntax str-ptr) 0)))
      (nl_abi_fail (nelisp_reader_p_fail_syntax str-ptr) 0))))

    ;; `#&LENGTH"BYTES"' bool-vector literals.  The prefix is validated
    ;; before recursive parsing so whitespace and signs cannot be accepted
    ;; between `#&' and LENGTH, or between LENGTH and the opening quote.
    (defun nelisp_reader_p_bv_string_tag_p (tag)
      (if (= tag 5) 1 (if (= tag 6) 1 (if (= tag 14) 1 (if (= tag 15) 1 0)))))

    (defun nelisp_reader_p_bv_str_ptr (s)
      (if (if (= (sexp-tag s) 6) 1 (if (= (sexp-tag s) 15) 1 0))
          (heap-read-u64 (heap-read-u64 s 8) 8)
        (heap-read-u64 s 16)))

    (defun nelisp_reader_p_bv_str_len (s)
      (if (if (= (sexp-tag s) 6) 1 (if (= (sexp-tag s) 15) 1 0))
          (heap-read-u64 (heap-read-u64 s 8) 16)
        (heap-read-u64 s 24)))

    (defun nelisp_reader_p_build_bool_vector (n-slot str-slot result-slot)
      (let* ((w (nelisp_reader_p_build_bool_vector_w n-slot str-slot 0)))
        (if (= w 0) -1
          (if (= w 2) 2 (seq (nl_abi_word_to_rc w result-slot) 1)))))
    ;; Doc 215: produce the WORD directly; legacy callers retain their slot ABI.
    (defun nelisp_reader_p_build_bool_vector_w (n-slot str-slot _pad)
      (if (= (nl_val_tag n-slot) 2)
          (let ((n (nl_word_payload n-slot)))
            (if (and (>= n 0)
                     (= (nelisp_reader_p_bv_string_tag_p (heap-read-u64 str-slot 0)) 1)
                     (>= (nelisp_reader_p_bv_str_len str-slot) (nl_bv_bytelen n)))
                (nl_alloc_bool_vector_from_bytes_w
                 n (nelisp_reader_p_bv_str_ptr str-slot) 0)
              (nl_abi_fail -1 0)))
        (nl_abi_fail -1 0)))

    (defun nelisp_reader_p_bool_vector_prefix_p (str-ptr cursor-slot)
      (let* ((cursor (nl_word_payload cursor-slot)))
        (if (< cursor (str-len str-ptr))
            (nelisp_reader_p_is_digit (str-byte-at str-ptr cursor))
          0)))

    (defun nelisp_reader_p_bool_vector_digits_end (str-ptr i)
      (if (and (< i (str-len str-ptr))
               (= (nelisp_reader_p_is_digit (str-byte-at str-ptr i)) 1))
          (nelisp_reader_p_bool_vector_digits_end str-ptr (+ i 1))
        i))

    (defun nelisp_reader_p_parse_bool_vector (str-ptr cursor-slot result-slot slot-pool depth)
      (let* ((w (nelisp_reader_p_parse_bool_vector_w str-ptr cursor-slot 0 slot-pool depth)))
        (if (= w 0) -1
          (if (= w 2) 2 (seq (nl_abi_word_to_rc w result-slot) 1)))))
    ;; Doc 215: parsed values are WORDs throughout the recursive reader.
    (defun nelisp_reader_p_parse_bool_vector_w (str-ptr cursor-slot _pad slot-pool depth)
  (if (= (nelisp_reader_p_bool_vector_prefix_p str-ptr cursor-slot) 0)
      (nl_abi_fail (nelisp_reader_p_fail_syntax str-ptr) 0)
    (let* ((digits-end (nelisp_reader_p_bool_vector_digits_end str-ptr (nl_word_payload cursor-slot))))
      (if (and (< digits-end (str-len str-ptr)) (= (str-byte-at str-ptr digits-end) 34))
          (let* ((n (nelisp_reader_p_parse_at_w str-ptr cursor-slot 0 slot-pool (+ depth 1))))
            (if (or (= n 0) (= n 2)) (nl_abi_fail (nelisp_reader_p_fail_syntax str-ptr) 0)
              (let* ((s (nelisp_reader_p_parse_at_w str-ptr cursor-slot 0 slot-pool (+ depth 1))))
                (if (or (= s 0) (= s 2)) (nl_abi_fail (nelisp_reader_p_fail_syntax str-ptr) 0)
                  (nelisp_reader_p_build_bool_vector_w n s 0)))))
        (nl_abi_fail (nelisp_reader_p_fail_syntax str-ptr) 0)))))

    ;; ===========================================================
    ;; Build a (HEAD INNER) quote wrapper at the current depth.
    ;; INNER is parsed into slot-pool car[d]; HEAD symbol into
    ;; slot-pool head[d]; tail (INNER . nil) into slot-pool cdr[d];
    ;; final cons into RESULT-SLOT.
    ;; ===========================================================

    (defun nelisp_reader_p_wrap (str-ptr cursor-slot result-slot slot-pool depth tag-byte)
      (let* ((w (nelisp_reader_p_wrap_w str-ptr cursor-slot 0 slot-pool depth tag-byte)))
        (if (= w 0) -1
          (if (= w 2) 2 (seq (nl_abi_word_to_rc w result-slot) 1)))))
    ;; Doc 215: produce the WORD directly; legacy callers retain their slot ABI.
    (defun nelisp_reader_p_wrap_w (str-ptr cursor-slot _pad slot-pool depth tag-byte)
  (let* ((inner (nelisp_reader_p_parse_at_w str-ptr cursor-slot 0 slot-pool (+ depth 1))))
    (if (or (= inner 0) (= inner 2)) (nl_abi_fail (nelisp_reader_p_fail_syntax str-ptr) 0)
      (let* ((head (alloc-bytes 32 8)))
        (if (= (nelisp_reader_p_build_head_symbol head (nelisp_reader_p_slot slot-pool 0) tag-byte) 1)
            (nelisp_cons_construct_w head (nelisp_cons_construct_w inner 3 0) 0)
          (nl_abi_fail (nelisp_reader_p_fail_syntax str-ptr) 0))))))

    ;; ===========================================================
    ;; Parse a list body: peek the next token, dispatch.
    ;;
    ;; RParen     -> result-slot := Nil
    ;; Dot        -> parse one form into result-slot, expect RParen
    ;; otherwise  -> parse item; recurse for tail; cons-make-with-clone
    ;; ===========================================================

;; Iterative sequence reader shared by list bodies (MODE 0, terminator
    ;; RParen, dotted tail allowed) and vector bodies (MODE 1, terminator
    ;; RBracket).  The previous shape recursed once per ELEMENT (the tail was
    ;; parsed at depth+1 and consed on the way back), so a list of N items
    ;; cost 4*N pool slots, capped the readable list length by the pool size,
    ;; and forced callers to allocate pools proportional to the source size.
    ;; This version appends in place: the pool cost is 4 slots per NESTING
    ;; level only, and the work per element is O(1).
    ;;
    ;; Slots at DEPTH: head = list under construction (Nil until the first
    ;; element), spare = last cons, cdr = scratch cons, car = element.
    ;; The public entry points keep their historical (even) arities.
    (defun nelisp_reader_p_copy32 (to src)
      (seq (ptr-write-u64 to 0 (heap-read-u64 src 0))
           (ptr-write-u64 to 8 (heap-read-u64 src 8))
           (ptr-write-u64 to 16 (heap-read-u64 src 16))
           (ptr-write-u64 to 24 (heap-read-u64 src 24))
           0))

    (defun nelisp_reader_p_seq_link (head tail node _pad)
      (if (= (sexp-tag head) 0)
          (seq (nelisp_reader_p_copy32 head node)
               (nelisp_reader_p_copy32 tail node)
               0)
        (seq (cons-set-cdr tail node)
             (nelisp_reader_p_copy32 tail node)
             0)))

    (defun nelisp_reader_p_parse_seq (str-ptr cursor-slot result-slot slot-pool depth mode)
      (let* ((w (nelisp_reader_p_parse_seq_w str-ptr cursor-slot 0 slot-pool depth mode)))
        (if (= w 0) -1
          (if (= w 2) 2 (seq (nl_abi_word_to_rc w result-slot) 1)))))
    ;; Doc 215: produce the WORD directly; legacy callers retain their slot ABI.
    (defun nelisp_reader_p_parse_seq_w (str-ptr cursor-slot _pad slot-pool depth mode)
  (if (= (nelisp_reader_p_depth_ok_p slot-pool depth) 0)
      (nl_abi_fail (nelisp_reader_p_stash_excessive_nesting (nelisp_reader_p_max_depth) 0) 0)
    (seq (nelisp_reader_p_word_slot_set (nelisp_reader_p_slot slot-pool (nelisp_reader_p_head_idx depth)) 3)
         (nelisp_reader_p_seq_loop_w str-ptr cursor-slot 0 slot-pool depth mode))))

    (defun nelisp_reader_p_seq_loop (str-ptr cursor-slot result-slot slot-pool depth mode)
      (let* ((w (nelisp_reader_p_seq_loop_w str-ptr cursor-slot 0 slot-pool depth mode)))
        (if (= w 0) -1
          (if (= w 2) 2 (seq (nl_abi_word_to_rc w result-slot) 1)))))
    ;; Doc 215: produce the WORD directly; legacy callers retain their slot ABI.
    (defun nelisp_reader_p_seq_loop_w (str-ptr cursor-slot _pad slot-pool depth mode)
      (nelisp_reader_p_seq_dispatch_w
       str-ptr cursor-slot 0 slot-pool depth
       (+ (nelisp_reader_p_lex_one
           str-ptr cursor-slot (nelisp_reader_p_slot slot-pool 1)
           (nelisp_reader_p_slot slot-pool 0))
          (+ 100 (* 200 mode)))))

    (defun nelisp_reader_p_seq_dot (str-ptr cursor-slot result-slot slot-pool depth _pad)
      (let* ((w (nelisp_reader_p_seq_dot_w str-ptr cursor-slot 0 slot-pool depth _pad)))
        (if (= w 0) -1
          (if (= w 2) 2 (seq (nl_abi_word_to_rc w result-slot) 1)))))
    ;; Doc 215: parsed values are WORDs throughout the recursive reader.
    (defun nelisp_reader_p_seq_dot_w (str-ptr cursor-slot _pad slot-pool depth _pad2)
  (let* ((tail (nelisp_reader_p_parse_at_w str-ptr cursor-slot 0 slot-pool (+ depth 1))))
    (if (and (and (/= tail 0) (/= tail 2))
             (= (nelisp_reader_p_lex_one str-ptr cursor-slot (nelisp_reader_p_slot slot-pool 1)
                                         (nelisp_reader_p_slot slot-pool 0)) 2))
        (seq (cons-set-cdr (heap-read-u64 (nelisp_reader_p_slot slot-pool (nelisp_reader_p_spare_idx depth)) 0) tail)
             (heap-read-u64 (nelisp_reader_p_slot slot-pool (nelisp_reader_p_head_idx depth)) 0))
      (nl_abi_fail (nelisp_reader_p_fail_syntax str-ptr) 0))))

    (defun nelisp_reader_p_seq_dispatch (str-ptr cursor-slot result-slot slot-pool depth km)
      (let* ((w (nelisp_reader_p_seq_dispatch_w str-ptr cursor-slot 0 slot-pool depth km)))
        (if (= w 0) -1
          (if (= w 2) 2 (seq (nl_abi_word_to_rc w result-slot) 1)))))
    ;; Doc 215: parsed values are WORDs throughout the recursive reader.
    (defun nelisp_reader_p_seq_dispatch_w (str-ptr cursor-slot _pad slot-pool depth km)
  (let* ((head-slot (nelisp_reader_p_slot slot-pool (nelisp_reader_p_head_idx depth)))
         (tail-slot (nelisp_reader_p_slot slot-pool (nelisp_reader_p_spare_idx depth))))
    (cond
     ((= km (if (>= km 200) 304 102)) (heap-read-u64 head-slot 0))
     ((= km 110)
      (if (= (sexp-tag head-slot) 3)
          (nl_abi_fail (nelisp_reader_p_fail_syntax str-ptr) 0)
        (nelisp_reader_p_seq_dot_w str-ptr cursor-slot 0 slot-pool depth 0)))
     ((= (- km (if (>= km 200) 300 100)) -2)
      (nl_abi_fail (nelisp_reader_p_stash_raw_byte_unrepresentable) 0))
     ((or (< (- km (if (>= km 200) 300 100)) 1)
          (= (- km (if (>= km 200) 300 100)) 2)
          (= (- km (if (>= km 200) 300 100)) 4)
          (= (- km (if (>= km 200) 300 100)) 10))
      (nl_abi_fail (nelisp_reader_p_fail_syntax str-ptr) 0))
     (t
      (let* ((item (nelisp_reader_p_dispatch_w str-ptr cursor-slot 0 slot-pool (+ depth 1)
                                               (- km (if (>= km 200) 300 100)))))
        (if (or (= item 0) (= item 2)) (nl_abi_fail (nelisp_reader_p_fail_syntax str-ptr) 0)
          (let* ((node (nelisp_cons_construct_w item 3 0)))
            (if (= (sexp-tag head-slot) 3)
                (nelisp_reader_p_word_slot_set head-slot node)
              (cons-set-cdr (heap-read-u64 tail-slot 0) node))
            (nelisp_reader_p_word_slot_set tail-slot node)
            (nelisp_reader_p_seq_loop_w str-ptr cursor-slot 0 slot-pool depth (if (>= km 200) 1 0)))))))))

    ;; Parse a list body (the open paren is already consumed).
    (defun nelisp_reader_p_parse_list_step (str-ptr cursor-slot result-slot slot-pool depth)
      (let* ((w (nelisp_reader_p_parse_list_step_w str-ptr cursor-slot 0 slot-pool depth)))
        (if (= w 0) -1
          (if (= w 2) 2 (seq (nl_abi_word_to_rc w result-slot) 1)))))
    ;; Doc 215: produce the WORD directly; legacy callers retain their slot ABI.
    (defun nelisp_reader_p_parse_list_step_w (str-ptr cursor-slot _pad slot-pool depth)
      (nelisp_reader_p_parse_seq_w str-ptr cursor-slot 0 slot-pool depth 0))

    ;; ===========================================================
    ;; `#s(hash-table ... data (...))' reader literal support.
    ;;
    ;; Emacs prints hash tables as a special `#s(hash-table ... data
    ;; (KEY VAL ...))' record.  Vendor data files such as emoji-labels.el
    ;; use this inside a quoted defconst, so the parser must materialise
    ;; the runtime hash-table value directly instead of returning a
    ;; constructor form.
    ;;
    ;; The standalone reader's current hash-table representation is
    ;; `(MARKER . ALIST)' where ALIST is `((KEY . VALUE) ...)'.
    ;; The marker is ignored by gethash/puthash and exists only to keep
    ;; the table distinguishable from a plain alist in diagnostics.
    ;; ===========================================================

    (defun nelisp_reader_p_symbol_hash_table_p (sym)
      (if (= (nl_val_tag sym) 4)
          (if (and (= (str-len sym) 10)
                   (= (str-byte-at sym 0) 104)
                   (= (str-byte-at sym 1) 97)
                   (= (str-byte-at sym 2) 115)
                   (= (str-byte-at sym 3) 104)
                   (= (str-byte-at sym 4) 45)
                   (= (str-byte-at sym 5) 116)
                   (= (str-byte-at sym 6) 97)
                   (= (str-byte-at sym 7) 98)
                   (= (str-byte-at sym 8) 108)
                   (= (str-byte-at sym 9) 101))
              1
            0)
        0))

    (defun nelisp_reader_p_symbol_data_p (sym)
      (if (= (nl_val_tag sym) 4)
          (if (and (= (str-len sym) 4)
                   (= (str-byte-at sym 0) 100)
                   (= (str-byte-at sym 1) 97)
                   (= (str-byte-at sym 2) 116)
                   (= (str-byte-at sym 3) 97))
              1
            0)
        0))

    (defun nelisp_reader_p_symbol_test_p (sym)
      (if (= (nl_val_tag sym) 4)
          (if (and (= (str-len sym) 4)
                   (= (str-byte-at sym 0) 116)
                   (= (str-byte-at sym 1) 101)
                   (= (str-byte-at sym 2) 115)
                   (= (str-byte-at sym 3) 116))
              1
            0)
        0))

    (defun nelisp_reader_p_hash_table_body_p (body)
      (if (= (nl_val_tag body) 7)
          (nelisp_reader_p_symbol_hash_table_p
           (nl_cons_car_ptr body))
        0))

    (defun nelisp_reader_p_hash_plist_find_data (plist)
      ;; Doc 215: walk and return by WORD (nl_val_tag classifies a view
      ;; and a WORD alike); an immediate key is never the marker symbol.
      (if (= (nl_val_tag plist) 7)
          (let* ((key (nl_cons_car_word plist))
                 (rest (nl_cons_cdr_word plist)))
            (if (= (nl_val_tag rest) 7)
                (if (if (= (logand key 1) 0)
                        (= (nelisp_reader_p_symbol_data_p key) 1) 0)
                    (nl_cons_car_word rest)
                  (nelisp_reader_p_hash_plist_find_data
                   (nl_cons_cdr_word rest)))
              0))
        0))

    ;; `#s(hash-table test equal data (...))' -- the TEST decides how gethash
    ;; compares, so it has to reach the marker.  Reading it as `eql' made every
    ;; string key in a generated literal unfindable.
    (defun nelisp_reader_p_hash_plist_find_test (plist)
      ;; Doc 215: walk and return by WORD (nl_val_tag classifies a view
      ;; and a WORD alike); an immediate key is never the marker symbol.
      (if (= (nl_val_tag plist) 7)
          (let* ((key (nl_cons_car_word plist))
                 (rest (nl_cons_cdr_word plist)))
            (if (= (nl_val_tag rest) 7)
                (if (if (= (logand key 1) 0)
                        (= (nelisp_reader_p_symbol_test_p key) 1) 0)
                    (nl_cons_car_word rest)
                  (nelisp_reader_p_hash_plist_find_test
                   (nl_cons_cdr_word rest)))
              0))
        0))

    ;; DATA is 0 (not a Nil sexp) when the literal has no `data' key AND
    ;; fewer than five elements, so the positional fallback in
    ;; `nelisp_reader_p_build_hash_table' finds nothing either.  That is
    ;; the shape Emacs 30+ prints for EVERY empty table --
    ;; `#s(hash-table test equal)' -- and savehist writes one per empty
    ;; history variable.  Reading the tag word of a 0 pointer is a NULL
    ;; dereference: the consumer's real-init audit died there, on
    ;; `(savehist-mode 1)' loading `projectile-project-command-history'
    ;; (SIGSEGV in this function, rdi = 0, 2026-09-06).  The fill step
    ;; below already guards `(= data 0)'; the count must too.
    (defun nelisp_reader_p_hash_pair_count (data n)
      (if (= data 0) n
        (if (= (nl_val_tag data) 7)
            (let* ((rest (nl_cons_cdr_word data)))
              (if (= (nl_val_tag rest) 7)
                  (nelisp_reader_p_hash_pair_count
                   (nl_cons_cdr_word rest) (+ n 1))
                n))
          n)))

    (defun nelisp_reader_p_list_nth_ptr (node n)
      ;; Doc 215: returns the element WORD (0 when the list is too short).
      (if (= (nl_val_tag node) 7)
          (if (= n 0)
              (nl_cons_car_word node)
            (nelisp_reader_p_list_nth_ptr (nl_cons_cdr_word node) (- n 1)))
        0))

    (defun nelisp_reader_p_hash_fill_table
        (data table pair-slot newhead-slot)
      ;; Preserve the pool's WORD representation even when this depth was
      ;; previously used by a list. A legacy constructor only writes the tag
      ;; byte, leaving the old WORD's high tag bytes in its physical scratch.
      (while (= (nl_val_tag data) 7)
        (let* ((rest (nl_cons_cdr_word data)))
          (if (= (nl_val_tag rest) 7)
              (seq
               (nelisp_reader_p_word_slot_set pair-slot
                (nelisp_cons_construct_w (nl_cons_car_word data)
                                          (nl_cons_car_word rest) 0))
               (nelisp_reader_p_word_slot_set newhead-slot
                (nelisp_cons_construct_w (heap-read-u64 pair-slot 0)
                                          (nl_cons_cdr_word table) 0))
               (cons-set-cdr table (heap-read-u64 newhead-slot 0))
               (setq data (nl_cons_cdr_word rest)))
            (setq data 3))))
      1)

    (defun nelisp_reader_p_build_hash_table (body result-slot slot-pool depth)
      (let* ((w (nelisp_reader_p_build_hash_table_w body 0 slot-pool depth)))
        (if (= w 0) -1
          (if (= w 2) 2 (seq (nl_abi_word_to_rc w result-slot) 1)))))
    ;; Doc 215: parsed values are WORDs throughout the recursive reader.
    (defun nelisp_reader_p_build_hash_table_w (body _pad slot-pool depth)
  (let* ((data (nelisp_reader_p_hash_plist_find_data (nl_cons_cdr_word body)))
         (test (nelisp_reader_p_hash_plist_find_test (nl_cons_cdr_word body)))
         (head (nl_w_vector_make 3 0)))
    (if (= data 0) (setq data (nelisp_reader_p_list_nth_ptr body 4)) 0)
    (vector-slot-set head 0 (nl_cons_car_word body))
    (vector-slot-set head 1 (nl_imm_int (nelisp_reader_p_hash_pair_count data 0)))
    (if (= test 0) 0 (vector-slot-set head 2 test))
    (let* ((object (nelisp_cons_construct_w head 3 0)))
      (if (= data 0) 0
        (nelisp_reader_p_hash_fill_table data object
          (nelisp_reader_p_slot slot-pool (nelisp_reader_p_cdr_idx depth))
          (nelisp_reader_p_slot slot-pool (nelisp_reader_p_spare_idx depth))))
      object)))

    (defun nelisp_reader_p_record_tag_p (tag)
      (let* ((kind (nl_val_tag tag)))
        (if (= kind 0) 1
          (if (= kind 4) 1 0))))

    (defun nelisp_reader_p_record_count (slots n)
      (let* ((kind (nl_val_tag slots)))
        (if (= kind 0)
            n
          (if (= kind 7)
              (nelisp_reader_p_record_count
               (nl_cons_cdr_word slots) (+ n 1))
            -1))))

    ;; Doc 215: RECORD-SLOT is the record under construction (in/out), not a result.
    (defun nelisp_reader_p_record_fill (slots idx record-slot)
      (let* ((kind (nl_val_tag slots)))
        (if (= kind 0)
            1
          (if (= kind 7)
              (and (record-slot-set record-slot idx
                                    (nl_cons_car_word slots))
                   (nelisp_reader_p_record_fill
                    (nl_cons_cdr_word slots) (+ idx 1) record-slot))
            0))))

    (defun nelisp_reader_p_build_record (body result-slot)
      (let* ((w (nelisp_reader_p_build_record_w body 0)))
        (if (= w 0) -1
          (if (= w 2) 2 (seq (nl_abi_word_to_rc w result-slot) 1)))))
    ;; Doc 215: parsed values are WORDs throughout the recursive reader.
    (defun nelisp_reader_p_build_record_w (body _pad)
  (if (= (nl_val_tag body) 7)
      (let* ((tag (nl_cons_car_word body)) (slots (nl_cons_cdr_word body))
             (count (nelisp_reader_p_record_count slots 0)))
        (if (and (= (nelisp_reader_p_record_tag_p tag) 1) (>= count 0))
            (let* ((object (alloc-bytes 32 8)))
              (seq (record-make tag count object)
                   (nelisp_reader_p_record_fill slots 0 object) object))
          (nl_abi_fail -1 0)))
    (nl_abi_fail -1 0)))

    (defun nelisp_reader_p_parse_sharps_paren (str-ptr cursor-slot result-slot slot-pool depth _pad)
      (let* ((w (nelisp_reader_p_parse_sharps_paren_w str-ptr cursor-slot 0 slot-pool depth _pad)))
        (if (= w 0) -1
          (if (= w 2) 2 (seq (nl_abi_word_to_rc w result-slot) 1)))))
    ;; Doc 215: parsed values are WORDs throughout the recursive reader.
    (defun nelisp_reader_p_parse_sharps_paren_w (str-ptr cursor-slot _pad slot-pool depth _pad2)
  (let* ((body (nelisp_reader_p_parse_list_step_w str-ptr cursor-slot 0 slot-pool (+ depth 1))))
    (if (or (= body 0) (= body 2)) (nl_abi_fail (nelisp_reader_p_fail_syntax str-ptr) 0)
      (if (= (nelisp_reader_p_hash_table_body_p body) 1)
          (nelisp_reader_p_build_hash_table_w body 0 slot-pool depth)
        (nelisp_reader_p_build_record_w body 0)))))

    ;; ===========================================================
    ;; Vector body parser (= mirror of `parse_list_step' but with
    ;; RBracket (kind 4) as terminator instead of RParen (kind 2)).
    ;; Produces a cons-list at LIST-SLOT terminating in Nil.  Dot
    ;; tokens inside the body are an error (= `(cons . cdr)' shape
    ;; is meaningless inside `[...]').
    ;;
    ;; Arity 6 (even) — though this defun does NOT itself invoke
    ;; `vector-make' / `vector-slot-set' (those run in the outer
    ;; `parse_vector' driver), the trailing `_pad' arg keeps the
    ;; call-site rsp alignment uniform with the rest of the parser
    ;; family.
    ;; ===========================================================

    (defun nelisp_reader_p_parse_vector_step
        (str-ptr cursor-slot list-slot slot-pool depth _pad)
      (nelisp_reader_p_parse_seq
       str-ptr cursor-slot list-slot slot-pool depth 1))

    ;; ===========================================================
    ;; Cons-list length walker — raw NlConsBox* iteration via the
    ;; §101.B `cons-cdr-raw-from-box' op.  Identical shape to
    ;; `nelisp_length_cons_walk' in `nelisp-cc-length-cons.el'.
    ;; ===========================================================

    (defun nelisp_reader_p_cons_list_len_walk (cur-box-ptr n)
      (if (= cur-box-ptr 0)
          n
        (nelisp_reader_p_cons_list_len_walk
         (cons-cdr-raw-from-box cur-box-ptr)
         (+ n 1))))

    ;; ===========================================================
    ;; Fill the freshly-allocated Sexp::Vector at VEC-HANDLE from
    ;; the cons-list rooted at CONS-HANDLE.  Walks the list one
    ;; cell at a time via `nl_cons_car_ptr' / `nl_cons_cdr_ptr'
    ;; externs (Doc 120 §120.C narrow Rust helpers) to obtain
    ;; *const Sexp pointers into the inline car/cdr fields, then
    ;; uses `vector-slot-set' (§111.E) for refcount-safe writes.
    ;;
    ;; Arity 6 (even) — required because the body invokes the
    ;; static-emit `vector-slot-set' op (Doc 124 §124.F.diag SysV
    ;; AMD64 stack-alignment fix).
    ;; ===========================================================

    (defun nelisp_reader_p_fill_vec
        (cons-handle vec-handle i slot-pool depth _pad)
      (if (= (cons-null-p cons-handle) 1)
          1
        (and (vector-slot-set vec-handle i
                              (extern-call nl_cons_car_word cons-handle))
             (nelisp_reader_p_fill_vec
              (extern-call nl_cons_cdr_ptr cons-handle)
              vec-handle (+ i 1) slot-pool depth 0))))

    ;; ===========================================================
    ;; Top-level vector arm (= `p_dispatch' kind 3).  Three-step
    ;; pipeline:
    ;;
    ;;   1. Parse body items into a cons-list at car[d] (= the
    ;;      depth's primary working slot).  `parse_vector_step'
    ;;      recurses at depth+1 so per-element working slots don't
    ;;      collide with the accumulator.
    ;;
    ;;   2. Allocate `Sexp::Vector(N)' at result-slot via the
    ;;      Doc 115 §115.1 `vector-make' grammar op, where N is
    ;;      the length of the cons-list (= one raw box walk).
    ;;
    ;;   3. Fill vector slots [0, N) by re-walking the cons-list
    ;;      via `fill_vec' (= refcount-safe `vector-slot-set' per
    ;;      element).
    ;;
    ;; Arity 6 (even) for the static-emit alignment invariant.
    ;; ===========================================================

    (defun nelisp_reader_p_parse_vector (str-ptr cursor-slot result-slot slot-pool depth _pad)
      (let* ((w (nelisp_reader_p_parse_vector_w str-ptr cursor-slot 0 slot-pool depth _pad)))
        (if (= w 0) -1
          (if (= w 2) 2 (seq (nl_abi_word_to_rc w result-slot) 1)))))
    ;; Doc 215: parsed values are WORDs throughout the recursive reader.
    (defun nelisp_reader_p_parse_vector_w (str-ptr cursor-slot _pad slot-pool depth _pad2)
  (let* ((body (nelisp_reader_p_parse_seq_w str-ptr cursor-slot 0 slot-pool (+ depth 1) 1)))
    (if (or (= body 0) (= body 2)) (nl_abi_fail (nelisp_reader_p_fail_syntax str-ptr) 0)
      (let* ((n (nelisp_reader_p_record_count body 0)))
        (if (< n 0) (nl_abi_fail (nelisp_reader_p_fail_syntax str-ptr) 0)
          (let* ((object (nl_w_vector_make n 0)) (cur body) (i 0))
            (while (= (nl_val_tag cur) 7)
              (vector-slot-set object i (nl_cons_car_word cur))
              (setq cur (nl_cons_cdr_word cur)) (setq i (+ i 1)))
            object))))))

    ;; ===========================================================
    ;; Compiled-function literal (`#[ARGDESC CODE CONSTANTS DEPTH ...]').
    ;; A generic record is allocated and filled while still tag 12, then
    ;; retagged 17 exactly as `bf_make_byte_code' does. This keeps the true
    ;; callable representation instead of returning an ordinary vector.
    ;; ===========================================================

    (defun nelisp_reader_p_byte_code_valid_p (vec)
      (let ((count (vector-len vec)))
        (if (or (< count 4) (> count 6))
            0
          (let* ((arglist (nl_vector_slot_word vec 0))
                 (arg-tag (nl_val_tag arglist))
                 (code-tag (nl_val_tag (nl_vector_slot_word vec 1)))
                 (constants-tag (nl_val_tag (nl_vector_slot_word vec 2)))
                 (depth (nl_vector_slot_word vec 3))
                 (depth-value (if (= (nl_val_tag depth) 2)
                                  (nl_word_payload depth) -1))
                 (min-fixnum -2305843009213693952)
                 (max-fixnum 2305843009213693951))
            ;; GNU lread.c accepts at most six closure slots. Its ARGLIST may
            ;; be nil, a fixnum, or any cons; a boolean or other object is
            ;; invalid.
            (if (and (or (= arg-tag 0) (= arg-tag 7)
                         (and (= arg-tag 2)
                              (>= (nl_word_payload arglist) min-fixnum)
                              (<= (nl_word_payload arglist) max-fixnum)))
                   ;; This runtime has no distinct interpreted-function
                   ;; representation. GNU accepts cons CODE as a lazy or
                   ;; interpreted closure, but it must not be mislabeled 17.
                   (or (= code-tag 5) (= code-tag 6)
                       (= code-tag 14) (= code-tag 15))
                   (= constants-tag 8)
                   (= (nl_val_tag depth) 2)
                   (>= depth-value 0)
                   (<= depth-value max-fixnum))
                1
              0)))))

    (defun nelisp_reader_p_fill_byte_code (vec record i n)
      (if (>= i n)
          1
        (and (record-slot-set record i (nl_vector_slot_word vec i))
             (nelisp_reader_p_fill_byte_code vec record (+ i 1) n))))

    (defun nelisp_reader_p_parse_byte_code (str-ptr cursor-slot result-slot slot-pool depth)
      (let* ((w (nelisp_reader_p_parse_byte_code_w str-ptr cursor-slot 0 slot-pool depth)))
        (if (= w 0) -1
          (if (= w 2) 2 (seq (nl_abi_word_to_rc w result-slot) 1)))))
    ;; Doc 215: parsed values are WORDs throughout the recursive reader.
    (defun nelisp_reader_p_parse_byte_code_w (str-ptr cursor-slot _pad slot-pool depth)
  (let* ((vec (nelisp_reader_p_parse_vector_w str-ptr cursor-slot 0 slot-pool depth 0)))
    (if (and (and (/= vec 0) (/= vec 2)) (= (nelisp_reader_p_byte_code_valid_p vec) 1))
        (let* ((object (alloc-bytes 32 8)))
          (seq (record-make (nl_vector_slot_word vec 0) (vector-len vec) object)
               (nelisp_reader_p_fill_byte_code vec object 0 (vector-len vec))
               (ptr-write-u64 object 0 17) object))
      (nl_abi_fail (nelisp_reader_p_fail_syntax str-ptr) 0))))

    ;; ===========================================================
    ;; Top-level entry — start at depth 0.
    ;; ===========================================================

    (defun nelisp_reader_parse_one
        (str-ptr cursor-slot result-slot slot-pool depth)
      (nelisp_reader_parse_one_with_context
       str-ptr cursor-slot result-slot slot-pool depth 0))

    (defun nelisp_reader_parse_one_with_load_file_name
        (str-ptr cursor-slot result-slot slot-pool depth load-file-name-slot)
      (nelisp_reader_parse_one_with_context
       str-ptr cursor-slot result-slot slot-pool depth load-file-name-slot))
    ;; Doc 215: produce the WORD directly; legacy callers retain their slot ABI.
    (defun nelisp_reader_parse_one_with_load_file_name_w
        (str-ptr cursor-slot _pad slot-pool depth load-file-name-word)
      (nelisp_reader_parse_one_with_context_w
       str-ptr cursor-slot 0 slot-pool depth load-file-name-word))

    (defun nelisp_reader_parse_one_with_context
        (str-ptr cursor-slot result-slot slot-pool depth context-slot)
      ;; The physical compatibility entry owns a copy, including boxed atoms.
      ;; It can outlive CONTEXT-SLOT when #$ becomes a container child. The
      ;; recursive reader has no evaluation/collection safepoint.
      (let* ((context-word (if (= context-slot 0) 3 (alloc-bytes 32 8))))
        (seq
         (if (/= context-slot 0) (nl_sexp_clone_into context-slot context-word) 0)
         (let* ((w (nelisp_reader_parse_one_with_context_w
                    str-ptr cursor-slot 0 slot-pool depth context-word)))
           (if (= w 0) -1
             (if (= w 2) 2 (seq (nl_abi_word_to_rc w result-slot) 1)))))))
    ;; Doc 215: parsed values are WORDs throughout the recursive reader.
    (defun nelisp_reader_parse_one_with_context_w (str-ptr cursor-slot _pad slot-pool depth context-word)
  (let* ((pool-cap (nelisp_reader_p_pool_cap)) (parse-cap (- pool-cap 1)) (w 0)
         (context-pool-slot (nelisp_reader_p_slot slot-pool parse-cap)))
    (if (< pool-cap 16) (nl_abi_fail (nelisp_reader_p_fail_syntax str-ptr) 0)
      (seq
       (nelisp_reader_p_word_slot_set context-pool-slot (if (= context-word 0) 3 context-word))
       (ptr-write-u64 268436448 0 parse-cap)
       (ptr-write-u64 (nelisp_reader_p_slot slot-pool 3) 0 0)
       (ptr-write-u64 (nelisp_reader_p_slot slot-pool 3) 8 0)
       (setq w (nelisp_reader_p_parse_at_w str-ptr cursor-slot 0 slot-pool depth))
       (if (and (/= w 0) (/= w 2))
           (if (>= (nelisp_reader_p_label_proxy_number w slot-pool 0) 0)
               (setq w (nl_abi_fail (nelisp_reader_p_fail_syntax str-ptr) 0))
             (setq w (nelisp_reader_p_label_resolve w slot-pool))) 0)
       (nelisp_reader_p_clear_used slot-pool parse-cap)
       (ptr-write-u64 268436448 0 pool-cap) w)))))

  "AOT source for Doc 116 §116.B pure-elisp Reader parser.

Implements the recursive-descent parser in `reader/parser.rs' as a
collection of mutually-recursive tail-call helpers + one public
entry `nelisp_reader_parse_one'.  Consumes the §116.A lexer's
token stream via `extern-call nelisp_reader_lex_one' and produces
Sexp values via the §101 / §111 / §122 grammar primitives.

Kinds dispatched: 0 EOF, 1 LParen, 2 RParen, 3 LBracket, 5 Quote,
6 Backquote, 7 Comma, 8 CommaAt, 9 FunctionQuote, 10 Dot, 11
SharpsParen, 12 CharTableBracket, 13 PropertizedString, 20 Int,
21 Float, 22 Str, 23 Sym, 24 Char, 25 RadixInt, 26 LabelDef,
27 LabelRef and 28 `#$' (dynamic `load-file-name' supplied by the
context-aware entry point).
Kind 3 LBracket drives the vector parser (Doc 116 §116.B+ —
`parse_vector_step' + `parse_vector' + `fill_vec' +
`cons_list_len_walk').  Kind 11 materialises Emacs hash-table reader
literals in the `#s(hash-table ... data (...))' shape used by generated
vendor data files; other record literals still surface as parse errors.
Kind 12 preserves `#^[...]' / `#^^[...]' char-table printer literals as
plain vectors until the standalone runtime grows a dedicated char-table
representation.
Doc 122 §122.G unlocks kind 21 Float by routing the payload bytes
through the `nl_str_to_float' extern (= `str::parse::<f64>()' with
direct `Sexp::Float' write into RESULT-SLOT).

All cons-cell construction uses `cons-make-with-clone' (Doc 120.E)
so NlConsBoxRef refcounts are bumped at write time.  The Rust
wrapper can therefore drop the slot-pool and stale result-slot
normally — no deep_clone pass and no mem::forget are required.

Slot-pool layout (= safe Rust wrapper allocates a raw slot buffer):
  slot 0          SCRATCH MutStr (drained between lex calls).
  slot 1          PAYLOAD slot   (lexer's text Sexp::Str output).
  slot 2          CONST-NIL      (forever Sexp::Nil; cdr source for
                                  tail of single-form quote wraps).
  slot 3          read-label count (reset for each public parse).
  slots 4+4d..7+4d  per-depth working slots: car/cdr/head/spare.
  pool tail       read-label metadata/value pairs, two slots each.")

(provide 'nelisp-cc-reader-parser)

;;; nelisp-cc-reader-parser.el ends here
