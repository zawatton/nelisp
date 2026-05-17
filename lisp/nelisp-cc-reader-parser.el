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
         ((= (nelisp_reader_p_is_nil_name payload-slot) 1) 1)
         ((= (nelisp_reader_p_is_t_name payload-slot) 1)
          (nelisp_reader_p_prog2 (ptr-write-u8 result-slot 0 1) 1))
         (t
          (nelisp_reader_p_prog2
           (nelisp_reader_p_copy_symbol result-slot payload-slot)
           1))))
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
       ;; Stray close / dot / EOF / error / vec / record → error.
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
       ;; Vec / record: out-of-scope for §116.B MVP.
       ((= kind 3) -1)
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

Kinds dispatched: 0 EOF, 1 LParen, 2 RParen, 5 Quote, 6 Backquote,
7 Comma, 8 CommaAt, 9 FunctionQuote, 10 Dot, 20 Int, 22 Str, 23 Sym.
Kinds 3/4 LBracket/RBracket and 11 SharpsParen route to error (=
vectors + records are out of §116.B scope; §116.C / §116.D follow-up).

Slot-pool layout (= safe Rust wrapper allocates a Sexp::Vector of
3 + 4 * MAX_DEPTH slots, all pre-Nil):
  slot 0          SCRATCH MutStr (drained between lex calls).
  slot 1          PAYLOAD slot   (lexer's text Sexp::Str output).
  slot 2          CONST-NIL      (forever Sexp::Nil; cdr source for
                                  tail of single-form quote wraps).
  slots 3+4d..6+4d  per-depth working slots: car/cdr/head/spare.")

(provide 'nelisp-cc-reader-parser)

;;; nelisp-cc-reader-parser.el ends here
