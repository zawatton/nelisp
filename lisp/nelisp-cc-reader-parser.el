;;; nelisp-cc-reader-parser.el --- Doc 116 §116.B pure-elisp Reader parser  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 116 §116.B — pure-elisp Phase 47 implementation of the
;; recursive-descent s-expression parser in
;; `build-tool/src/reader/parser.rs' (~485 LOC Rust → ~500 LOC elisp).
;; Consumes the Token stream emitted by §116.A's
;; `nelisp_reader_lex_one' and produces final `Sexp' values via the
;; existing Phase 47 grammar primitives (cons-make / sexp-write-symbol
;; / sexp-write-str / sexp-int-make + the §122.E raw-mem ops for
;; String header decoding).
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
;;                   slots 3 + 4d, 4d+1, 4d+2, 4d+3 (= per-depth
;;                            working slots):
;;                              +0  car / item
;;                              +1  cdr / tail
;;                              +2  head symbol (quote-wrap only)
;;                              +3  reserved / Str-staging
;;   DEPTH       — i64 recursion depth (= caller-provided; the safe
;;                 Rust wrapper passes 0).
;;
;; Returns: i64.  1 = success, anything else (-1, 0) = error.  On
;; error, `*RESULT-SLOT' is `Sexp::Nil'.
;;
;; The safe Rust wrapper allocates SLOT-POOL with enough capacity for
;; the deepest expected nesting (= 3 + 4 * MAX_DEPTH slots).  For the
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
;; becomes `(HEAD INNER)' via two cons-makes (= (cons HEAD (cons
;; INNER nil))).  HEAD symbol bytes are pushed into SCRATCH-MUTSTR
;; via `mut-str-push-byte' then finalised + retag-byte-overwritten
;; to Sexp::Symbol.

;;; Code:

(defconst nelisp-cc-reader-parser--source
  '(seq

    ;; ===========================================================
    ;; `prog2 EFFECT VAL' — side-effect sequencer.
    ;; ===========================================================

    (defun nelisp_reader_p_prog2 (_eff val) val)

    ;; ===========================================================
    ;; Slot-pool indexing helpers (= compute slot index for the
    ;; per-depth working slots).  Each depth gets 4 slots starting
    ;; at offset (3 + d*4).
    ;; ===========================================================

    (defun nelisp_reader_p_car_idx   (d) (+ 3 (* d 4)))
    (defun nelisp_reader_p_cdr_idx   (d) (+ 4 (* d 4)))
    (defun nelisp_reader_p_head_idx  (d) (+ 5 (* d 4)))

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

    (defun nelisp_reader_p_copy_symbol (dest-slot src-str-slot)
      (sexp-write-symbol
       dest-slot
       (ptr-read-u64 src-str-slot 16)
       (ptr-read-u64 src-str-slot 24)))

    (defun nelisp_reader_p_copy_str (dest-slot src-str-slot)
      (sexp-write-str
       dest-slot
       (ptr-read-u64 src-str-slot 16)
       (ptr-read-u64 src-str-slot 24)))

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
    ;;   - Single ASCII byte X        -> codepoint X.
    ;;   - `\\X...' escape sequence   -> named escape / hex / C- / M-.
    ;;
    ;; Mirror `build-tool/src/reader/lexer.rs::read_char_literal'.
    ;; ===========================================================

    (defun nelisp_reader_p_decode_char (payload-slot)
      (if (>= (str-len payload-slot) 1)
          (if (= (str-byte-at payload-slot 0) 92)
              ;; Escape body starts at byte 1.
              (if (>= (str-len payload-slot) 2)
                  (nelisp_reader_p_decode_char_escape
                   payload-slot 1 (str-byte-at payload-slot 1))
                0)
            ;; Plain `?X' — payload byte 0 is the codepoint.
            (str-byte-at payload-slot 0))
        0))

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
      ;; Control char = (byte & 0x1f).
      (if (>= (str-len payload-slot) (+ start 2))
          (if (= (str-byte-at payload-slot start) 45)
              (logand (str-byte-at payload-slot (+ start 1)) 31)
            0)
        0))

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
                (logand (str-byte-at payload-slot (+ start 2)) 31)
              0)
          0))
       ((= inner 110) 10)
       ((= inner 116) 9)
       ((= inner 114) 13)
       (t inner)))

    ;; ===========================================================
    ;; Materialise a leaf token into RESULT-SLOT.  RESULT-SLOT
    ;; must be pre-Nil on entry.  Returns 1 on success.
    ;; ===========================================================

    (defun nelisp_reader_p_leaf (kind result-slot payload-slot)
      (cond
       ;; Int
       ((= kind 20)
        (nelisp_reader_p_prog2
         (sexp-int-make result-slot
                        (nelisp_reader_p_atoi payload-slot))
         1))
       ;; Str
       ((= kind 22)
        (nelisp_reader_p_prog2
         (nelisp_reader_p_copy_str result-slot payload-slot)
         1))
       ;; Sym
       ((= kind 23)
        (cond
         ;; "nil" -> force tag byte to SEXP_TAG_NIL (0).  Must overwrite
         ;; the slot even when previously holding a non-Nil value (=
         ;; e.g., the parser reuses car[d] across list elements;
         ;; without this explicit write, a `nil' element after a prior
         ;; quote-wrap would silently leave the prior symbol in place).
         ((= (nelisp_reader_p_is_nil_name payload-slot) 1)
          (nelisp_reader_p_prog2 (ptr-write-u8 result-slot 0 0) 1))
         ((= (nelisp_reader_p_is_t_name payload-slot) 1)
          (nelisp_reader_p_prog2 (ptr-write-u8 result-slot 0 1) 1))
         (t
          (nelisp_reader_p_prog2
           (nelisp_reader_p_copy_symbol result-slot payload-slot)
           1))))
       ;; Char (kind 24) -> decode body to codepoint.
       ((= kind 24)
        (nelisp_reader_p_prog2
         (sexp-int-make result-slot
                        (nelisp_reader_p_decode_char payload-slot))
         1))
       ;; RadixInt (kind 25) -> decode body to i64.
       ((= kind 25)
        (nelisp_reader_p_prog2
         (sexp-int-make result-slot
                        (nelisp_reader_p_decode_radix payload-slot))
         1))
       ;; Float (kind 21) -> Doc 122 §122.G unlock.  Payload Sexp::Str
       ;; carries the lexer's snapshot text (= digits + optional `.'
       ;; / `e` exponent / leading sign).  `nl_str_to_float' parses
       ;; the UTF-8 bytes via `str::parse::<f64>()' and writes
       ;; `Sexp::Float(parsed)' into RESULT-SLOT directly, returning
       ;; 1 on success / 0 on parse failure.  Bytes pointer + length
       ;; come from the Sexp::Str header (offsets 16 / 24 per
       ;; `nelisp-sexp-layout.el').
       ((= kind 21)
        (extern-call nl_str_to_float
                     (ptr-read-u64 payload-slot 16)
                     (ptr-read-u64 payload-slot 24)
                     result-slot))
       (t -1)))

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
    ;; Quote-family head-symbol byte writers.  Each push the
    ;; canonical name into a (freshly-reset) SCRATCH MutStr.  The
    ;; caller then `mut-str-finalize's into a slot and overwrites
    ;; the tag byte to convert Sexp::Str -> Sexp::Symbol.
    ;; ===========================================================

    (defun nelisp_reader_p_push_quote_bytes (scratch)
      ;; "quote" = q u o t e
      (nelisp_reader_p_prog2 (mut-str-push-byte scratch 113)
       (nelisp_reader_p_prog2 (mut-str-push-byte scratch 117)
        (nelisp_reader_p_prog2 (mut-str-push-byte scratch 111)
         (nelisp_reader_p_prog2 (mut-str-push-byte scratch 116)
                                (mut-str-push-byte scratch 101))))))

    (defun nelisp_reader_p_push_backquote_bytes (scratch)
      ;; "backquote"
      (nelisp_reader_p_prog2 (mut-str-push-byte scratch 98)
       (nelisp_reader_p_prog2 (mut-str-push-byte scratch 97)
        (nelisp_reader_p_prog2 (mut-str-push-byte scratch 99)
         (nelisp_reader_p_prog2 (mut-str-push-byte scratch 107)
          (nelisp_reader_p_prog2 (mut-str-push-byte scratch 113)
           (nelisp_reader_p_prog2 (mut-str-push-byte scratch 117)
            (nelisp_reader_p_prog2 (mut-str-push-byte scratch 111)
             (nelisp_reader_p_prog2 (mut-str-push-byte scratch 116)
                                    (mut-str-push-byte scratch 101)))))))))) ; e

    (defun nelisp_reader_p_push_comma_bytes (scratch)
      ;; "comma"
      (nelisp_reader_p_prog2 (mut-str-push-byte scratch 99)
       (nelisp_reader_p_prog2 (mut-str-push-byte scratch 111)
        (nelisp_reader_p_prog2 (mut-str-push-byte scratch 109)
         (nelisp_reader_p_prog2 (mut-str-push-byte scratch 109)
                                (mut-str-push-byte scratch 97))))))

    (defun nelisp_reader_p_push_comma_at_bytes (scratch)
      ;; "comma-at"
      (nelisp_reader_p_prog2 (mut-str-push-byte scratch 99)
       (nelisp_reader_p_prog2 (mut-str-push-byte scratch 111)
        (nelisp_reader_p_prog2 (mut-str-push-byte scratch 109)
         (nelisp_reader_p_prog2 (mut-str-push-byte scratch 109)
          (nelisp_reader_p_prog2 (mut-str-push-byte scratch 97)
           (nelisp_reader_p_prog2 (mut-str-push-byte scratch 45)
            (nelisp_reader_p_prog2 (mut-str-push-byte scratch 97)
                                   (mut-str-push-byte scratch 116)))))))))

    (defun nelisp_reader_p_push_function_bytes (scratch)
      ;; "function"
      (nelisp_reader_p_prog2 (mut-str-push-byte scratch 102)
       (nelisp_reader_p_prog2 (mut-str-push-byte scratch 117)
        (nelisp_reader_p_prog2 (mut-str-push-byte scratch 110)
         (nelisp_reader_p_prog2 (mut-str-push-byte scratch 99)
          (nelisp_reader_p_prog2 (mut-str-push-byte scratch 116)
           (nelisp_reader_p_prog2 (mut-str-push-byte scratch 105)
            (nelisp_reader_p_prog2 (mut-str-push-byte scratch 111)
                                   (mut-str-push-byte scratch 110))))))))) ; n

    ;; Build a fresh Sexp::Symbol whose name is one of the
    ;; quote-family heads, written into HEAD-SLOT.  TAG-BYTE
    ;; selects: 113 quote, 98 backquote, 99 comma, 100 comma-at,
    ;; 102 function.  Side effects: scratch is reset + drained.
    ;; Returns 1 on success, 0 on unknown TAG-BYTE.
    (defun nelisp_reader_p_build_head_symbol
        (head-slot scratch-slot tag-byte)
      (cond
       ((= tag-byte 113)
        (and (nelisp_reader_p_reset_scratch scratch-slot)
             (nelisp_reader_p_push_quote_bytes scratch-slot)
             (mut-str-finalize scratch-slot head-slot)
             ;; SEXP_TAG_SYMBOL = 4.
             (ptr-write-u8 head-slot 0 4)
             1))
       ((= tag-byte 98)
        (and (nelisp_reader_p_reset_scratch scratch-slot)
             (nelisp_reader_p_push_backquote_bytes scratch-slot)
             (mut-str-finalize scratch-slot head-slot)
             (ptr-write-u8 head-slot 0 4)
             1))
       ((= tag-byte 99)
        (and (nelisp_reader_p_reset_scratch scratch-slot)
             (nelisp_reader_p_push_comma_bytes scratch-slot)
             (mut-str-finalize scratch-slot head-slot)
             (ptr-write-u8 head-slot 0 4)
             1))
       ((= tag-byte 100)
        (and (nelisp_reader_p_reset_scratch scratch-slot)
             (nelisp_reader_p_push_comma_at_bytes scratch-slot)
             (mut-str-finalize scratch-slot head-slot)
             (ptr-write-u8 head-slot 0 4)
             1))
       ((= tag-byte 102)
        (and (nelisp_reader_p_reset_scratch scratch-slot)
             (nelisp_reader_p_push_function_bytes scratch-slot)
             (mut-str-finalize scratch-slot head-slot)
             (ptr-write-u8 head-slot 0 4)
             1))
       (t 0)))

    ;; ===========================================================
    ;; parse_at — lex one token at the current cursor + dispatch.
    ;; Writes the parsed Sexp into RESULT-SLOT.  Returns 1 on
    ;; success, -1 on error.  DEPTH is the current recursion depth
    ;; (= used to index SLOT-POOL for per-depth working slots).
    ;; ===========================================================

    (defun nelisp_reader_p_parse_at
        (str-ptr cursor-slot result-slot slot-pool depth)
      (nelisp_reader_p_dispatch
       str-ptr cursor-slot result-slot slot-pool depth
       (nelisp_reader_p_lex_one
        str-ptr cursor-slot
        (vector-ref-ptr slot-pool 1)   ; payload-slot
        (vector-ref-ptr slot-pool 0)))) ; scratch-slot

    (defun nelisp_reader_p_dispatch
        (str-ptr cursor-slot result-slot slot-pool depth kind)
      (cond
       ;; LParen
       ((= kind 1)
        (nelisp_reader_p_parse_list_step
         str-ptr cursor-slot result-slot slot-pool depth))
       ;; LBracket → vector literal `[...]' (Doc 116 §116.B+ extension,
       ;; this commit).  Body parsed as a cons-list at car[d] (terminator
       ;; = RBracket), then converted to a fresh Sexp::Vector at
       ;; result-slot via `vector-make' + `vector-slot-set' fill loop.
       ((= kind 3)
        (nelisp_reader_p_parse_vector
         str-ptr cursor-slot result-slot slot-pool depth 0))
       ;; Quote prefixes (5,6,7,8,9) → wrap (HEAD INNER).
       ((= kind 5)
        (nelisp_reader_p_wrap str-ptr cursor-slot result-slot
                              slot-pool depth 113))
       ((= kind 6)
        (nelisp_reader_p_wrap str-ptr cursor-slot result-slot
                              slot-pool depth 98))
       ((= kind 7)
        (nelisp_reader_p_wrap str-ptr cursor-slot result-slot
                              slot-pool depth 99))
       ((= kind 8)
        (nelisp_reader_p_wrap str-ptr cursor-slot result-slot
                              slot-pool depth 100))
       ((= kind 9)
        (nelisp_reader_p_wrap str-ptr cursor-slot result-slot
                              slot-pool depth 102))
       ;; Leaf payloads.
       ((>= kind 20)
        (nelisp_reader_p_leaf kind result-slot
                              (vector-ref-ptr slot-pool 1)))
       ;; Stray close / dot / EOF / error / record / byte-code → error.
       (t -1)))

    ;; ===========================================================
    ;; Build a (HEAD INNER) quote wrapper at the current depth.
    ;; INNER is parsed into slot-pool car[d]; HEAD symbol into
    ;; slot-pool head[d]; tail (INNER . nil) into slot-pool cdr[d];
    ;; final cons into RESULT-SLOT.
    ;; ===========================================================

    (defun nelisp_reader_p_wrap
        (str-ptr cursor-slot result-slot slot-pool depth tag-byte)
      (and (= (nelisp_reader_p_parse_at
               str-ptr cursor-slot
               (vector-ref-ptr slot-pool
                               (nelisp_reader_p_car_idx depth))
               slot-pool (+ depth 1))
              1)
           ;; tail = (INNER . nil) at cdr[d].  nil source = slot 2.
           (cons-make (vector-ref-ptr slot-pool
                                      (nelisp_reader_p_car_idx depth))
                      (vector-ref-ptr slot-pool 2)
                      (vector-ref-ptr slot-pool
                                      (nelisp_reader_p_cdr_idx depth)))
           ;; Build HEAD symbol at head[d].
           (= (nelisp_reader_p_build_head_symbol
               (vector-ref-ptr slot-pool
                               (nelisp_reader_p_head_idx depth))
               (vector-ref-ptr slot-pool 0)
               tag-byte)
              1)
           ;; cons-make HEAD TAIL -> result-slot.
           (cons-make (vector-ref-ptr slot-pool
                                      (nelisp_reader_p_head_idx depth))
                      (vector-ref-ptr slot-pool
                                      (nelisp_reader_p_cdr_idx depth))
                      result-slot)
           1))

    ;; ===========================================================
    ;; Parse a list body: peek the next token, dispatch.
    ;;
    ;; RParen     -> result-slot := Nil
    ;; Dot        -> parse one form into result-slot, expect RParen
    ;; otherwise  -> parse item; recurse for tail; cons-make
    ;; ===========================================================

    (defun nelisp_reader_p_parse_list_step
        (str-ptr cursor-slot result-slot slot-pool depth)
      (nelisp_reader_p_list_dispatch
       str-ptr cursor-slot result-slot slot-pool depth
       (nelisp_reader_p_lex_one
        str-ptr cursor-slot
        (vector-ref-ptr slot-pool 1)
        (vector-ref-ptr slot-pool 0))))

    (defun nelisp_reader_p_list_dispatch
        (str-ptr cursor-slot result-slot slot-pool depth kind)
      (cond
       ;; RParen: empty (rest of) list.  Force Nil tag (= 0) into
       ;; result-slot in case it was previously written (= safety).
       ((= kind 2)
        (nelisp_reader_p_prog2 (ptr-write-u8 result-slot 0 0) 1))
       ;; Dot tail.
       ((= kind 10)
        (and (= (nelisp_reader_p_parse_at
                 str-ptr cursor-slot result-slot
                 slot-pool (+ depth 1))
                1)
             ;; Consume the closing RParen.
             (= (nelisp_reader_p_lex_one
                 str-ptr cursor-slot
                 (vector-ref-ptr slot-pool 1)
                 (vector-ref-ptr slot-pool 0))
                2)
             1))
       ;; EOF / stray RBracket / error: parse error.
       ((= kind 0) -1)
       ((= kind 4) -1)
       ((< kind 0) -1)
       ;; Record `#s(...)' / byte-code `#[...]' (kind 11) still
       ;; out-of-scope for §116.B (= the stdlib subset does not need
       ;; them, only the user-facing reader in `nelisp-stdlib-reader.el'
       ;; does); kind 3 LBracket falls through to the recursive parse
       ;; arm below so `[..]' vectors compose inside lists.
       ((= kind 11) -1)
       (t
        (and (= (nelisp_reader_p_dispatch
                 str-ptr cursor-slot
                 (vector-ref-ptr slot-pool
                                 (nelisp_reader_p_car_idx depth))
                 slot-pool (+ depth 1) kind)
                1)
             (= (nelisp_reader_p_parse_list_step
                 str-ptr cursor-slot
                 (vector-ref-ptr slot-pool
                                 (nelisp_reader_p_cdr_idx depth))
                 slot-pool (+ depth 1))
                1)
             (cons-make (vector-ref-ptr slot-pool
                                        (nelisp_reader_p_car_idx depth))
                        (vector-ref-ptr slot-pool
                                        (nelisp_reader_p_cdr_idx depth))
                        result-slot)
             1))))

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
      (nelisp_reader_p_vec_dispatch
       str-ptr cursor-slot list-slot slot-pool depth
       (nelisp_reader_p_lex_one
        str-ptr cursor-slot
        (vector-ref-ptr slot-pool 1)
        (vector-ref-ptr slot-pool 0))))

    (defun nelisp_reader_p_vec_dispatch
        (str-ptr cursor-slot list-slot slot-pool depth kind)
      (cond
       ;; RBracket — body terminated.  Force list-slot tag to Nil (= 0).
       ((= kind 4)
        (nelisp_reader_p_prog2 (ptr-write-u8 list-slot 0 0) 1))
       ;; EOF / stray RParen / stray Dot / lex error → parse error.
       ((= kind 0) -1)
       ((= kind 2) -1)
       ((= kind 10) -1)
       ((< kind 0) -1)
       ;; Otherwise parse one item into car[d], recurse for tail at
       ;; cdr[d], cons-make into list-slot.
       (t
        (and (= (nelisp_reader_p_dispatch
                 str-ptr cursor-slot
                 (vector-ref-ptr slot-pool
                                 (nelisp_reader_p_car_idx depth))
                 slot-pool (+ depth 1) kind)
                1)
             (= (nelisp_reader_p_parse_vector_step
                 str-ptr cursor-slot
                 (vector-ref-ptr slot-pool
                                 (nelisp_reader_p_cdr_idx depth))
                 slot-pool (+ depth 1) 0)
                1)
             (cons-make (vector-ref-ptr slot-pool
                                        (nelisp_reader_p_car_idx depth))
                        (vector-ref-ptr slot-pool
                                        (nelisp_reader_p_cdr_idx depth))
                        list-slot)
             1))))

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
                              (extern-call nl_cons_car_ptr cons-handle))
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

    (defun nelisp_reader_p_parse_vector
        (str-ptr cursor-slot result-slot slot-pool depth _pad)
      (and (= (nelisp_reader_p_parse_vector_step
               str-ptr cursor-slot
               (vector-ref-ptr slot-pool
                               (nelisp_reader_p_car_idx depth))
               slot-pool (+ depth 1) 0)
              1)
           ;; vector-make CAP SLOT — CAP = walker(payload(car[d]), 0).
           ;; `sexp-payload-ptr' returns the NlConsBox* for Cons and 0
           ;; for Nil, so the empty-vector case `[]' threads CAP = 0.
           (vector-make
            (nelisp_reader_p_cons_list_len_walk
             (sexp-payload-ptr
              (vector-ref-ptr slot-pool
                              (nelisp_reader_p_car_idx depth)))
             0)
            result-slot)
           (= (nelisp_reader_p_fill_vec
               (vector-ref-ptr slot-pool
                               (nelisp_reader_p_car_idx depth))
               result-slot 0 slot-pool depth 0)
              1)))

    ;; ===========================================================
    ;; Top-level entry — start at depth 0.
    ;; ===========================================================

    (defun nelisp_reader_parse_one
        (str-ptr cursor-slot result-slot slot-pool depth)
      (nelisp_reader_p_parse_at
       str-ptr cursor-slot result-slot slot-pool depth)))

  "Phase 47 source for Doc 116 §116.B pure-elisp Reader parser.

Implements the recursive-descent parser in `reader/parser.rs' as a
collection of mutually-recursive tail-call helpers + one public
entry `nelisp_reader_parse_one'.  Consumes the §116.A lexer's
token stream via `extern-call nelisp_reader_lex_one' and produces
Sexp values via the §101 / §111 / §122 grammar primitives.

Kinds dispatched: 0 EOF, 1 LParen, 2 RParen, 3 LBracket, 5 Quote,
6 Backquote, 7 Comma, 8 CommaAt, 9 FunctionQuote, 10 Dot, 20 Int,
21 Float, 22 Str, 23 Sym, 24 Char, 25 RadixInt.  Kind 3 LBracket
drives the vector parser (Doc 116 §116.B+ — `parse_vector_step'
+ `parse_vector' + `fill_vec' + `cons_list_len_walk').  Kind 11
SharpsParen (record `#s(...)') still surfaces as a parse error.
Doc 122 §122.G unlocks kind 21 Float by routing the payload bytes
through the `nl_str_to_float' extern (= `str::parse::<f64>()' with
direct `Sexp::Float' write into RESULT-SLOT).

Slot-pool layout (= safe Rust wrapper allocates a Sexp::Vector of
3 + 4 * MAX_DEPTH slots, all pre-Nil):
  slot 0          SCRATCH MutStr (drained between lex calls).
  slot 1          PAYLOAD slot   (lexer's text Sexp::Str output).
  slot 2          CONST-NIL      (forever Sexp::Nil; cdr source for
                                  tail of single-form quote wraps).
  slots 3+4d..6+4d  per-depth working slots: car/cdr/head/spare.")

(provide 'nelisp-cc-reader-parser)

;;; nelisp-cc-reader-parser.el ends here
