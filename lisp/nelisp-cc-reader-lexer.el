;;; nelisp-cc-reader-lexer.el --- Doc 116 §116.A pure-elisp Reader lexer  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 116 §116.A — pure-elisp AOT implementation of the
;; tokenizer state machine in `build-tool/src/reader/lexer.rs'
;; (~885 LOC Rust → ~400 LOC elisp source).  The classical Lisp
;; self-host milestone: NeLisp's boot path is the pure-elisp
;; reader (Doc 126.B) — no Rust-side reader code in the production
;; eval-boot loop.
;;
;; ABI shape (= one token per call; caller drives the loop):
;;
;;   nelisp_reader_lex_one(STR-PTR, CURSOR, PAYLOAD-SLOT,
;;                         CURSOR-OUT-SLOT, SCRATCH-MUTSTR-SLOT) -> i64
;;
;; Returns an i64 token kind code:
;;
;;   0   EOF                  payload untouched
;;   1   LParen   `('
;;   2   RParen   `)'
;;   3   LBracket `['
;;   4   RBracket `]'
;;   5   Quote    `''
;;   6   Backquote `\`'
;;   7   Comma    `,'
;;   8   CommaAt  `,@'
;;   9   FunctionQuote `#''
;;   10  Dot      `.'
;;   11  SharpsParen `#s('
;;   12  CharTableBracket `#^[' / `#^^['
;;   20  Int                   payload = Sexp::Str of digit text
;;   21  Float                 payload = Sexp::Str of text
;;   22  Str                   payload = Sexp::Str of resolved body
;;   23  Sym                   payload = Sexp::Str of symbol name
;;   24  Char     `?X' / `?\X'    payload = Sexp::Str of body bytes
;;                                (after the leading `?'; parser decodes)
;;   25  RadixInt `#x..'/`#o..'/  payload = Sexp::Str of base byte
;;                `#b..'           (`x'/`o'/`b'/`X'/`O'/`B') + digit text
;;   -1  Error / unexpected
;;
;; The Rust safe wrapper in `lib.rs::elisp_cc_spike::reader_lex_one'
;; maps (kind, payload) -> the `(KIND-SYMBOL . PAYLOAD)' Sexp::Cons
;; that the §116.B parser will consume.
;;
;; CURSOR is a byte offset into STR-PTR's UTF-8 payload.  CURSOR-OUT-
;; SLOT receives `Sexp::Int(next-cursor)' via `sexp-int-make'.
;; SCRATCH-MUTSTR-SLOT is a caller-owned `Sexp::MutStr' (= the
;; caller must `mut-str-make-empty(slot, CAP)' it before calling).
;; Caller should reset the MutStr between calls (= a fresh
;; `mut-str-make-empty' is the safest pattern given clone-not-move
;; finalize semantics).
;;
;; Scope discipline:
;;   - Lexer ONLY.  Parser is §116.B.  Top-level wrapper is §116.C.
;;   - Token kinds covered: lparen / rparen / lbracket / rbracket /
;;     quote / backquote / comma / comma-at / function-quote / dot /
;;     sharps-paren / char-table bracket / int / float / str / sym / char /
;;     radix-int / eof.
;;   - String escapes: \\n, \\t, \\r, \\f, \\v, \\e, \\a, \\b, \\d,
;;     \\s, \\\\, \\\"; line continuation `\\<SP>' / `\\<LF>';
;;     any other `\\X' drops the backslash + pushes X (= Doc 51
;;     Phase 3-A''-1 Emacs reader compat).
;;   - Char literals `?X' / `?\\C-a' / `?\\M-X' covered (Doc 116 §116.B+).
;;     Payload is the raw body bytes (after the `?'); parser-side decoder
;;     in `nelisp-cc-reader-parser.el' handles the escape + Meta-modifier
;;     reduction.  Bare `?' (followed by whitespace / EOF) still lexes
;;     as the 1-char symbol `?'.
;;   - Escaped symbol bytes `\"' / `\(' / `foo\ bar' covered by dropping
;;     the backslash and admitting the escaped byte into the atom payload.
;;   - Radix integers `#x10' / `#o17' / `#b1010' covered (Doc 116 §116.B+).
;;     Payload first byte = base marker (`x'/`o'/`b'); remaining bytes =
;;     the digit text (incl. optional `+'/`-' sign).  Parser converts.
;;   - Byte-code `#[..]' literal still deferred (rare; falls through to
;;     the Rust legacy path in §116.C).
;;
;; Side-effect sequencing pattern (= no `(seq ...)' in value form):
;;   The AOT grammar's `seq' is statement-only; defun bodies
;;   are value-producing expressions.  To compose "do side effect
;;   E, then return value V" we use the `nelisp_reader_prog2 E V'
;;   helper which evaluates both args left-to-right (= argument
;;   evaluation order is fixed by the SysV ABI marshalling phase)
;;   and returns its second arg.  AOT's `mut-str-push-byte' /
;;   `sexp-int-make' / `mut-str-finalize' all return a value (i64
;;   sentinel or slot pointer); we ignore it via `prog2'.
;;
;; ABI deps satisfied:
;;   §101.C  `str-len' + `str-byte-at'   source byte iteration.
;;   §100.B  `sexp-int-make'             cursor output via Sexp::Int.
;;   §122.B  `mut-str-push-byte' /       atom + string text
;;           `mut-str-finalize'          accumulation -> Sexp::Str.
;;   §100/§115 arith + logical           byte-class checks, cursor
;;                                       arithmetic, atom classifier.
;;   §111/§99 recursive defun ABI         every helper tail-calls.

;;; Code:

(defconst nelisp-cc-reader-lexer--source
  '(seq

    ;; ===========================================================
    ;; Side-effect sequencer.  `(nelisp_reader_prog2 EFFECT VAL)'
    ;; evaluates both args left-to-right (= arg eval order is the
    ;; SysV ABI marshal phase, well-defined), returns VAL.  Used
    ;; throughout the lexer to thread `mut-str-push-byte' /
    ;; `sexp-int-make' / `mut-str-finalize' side effects through
    ;; value-producing recursive tail calls without needing a
    ;; `seq' statement form (= AOT reserves `seq' for
    ;; top-level / statement context only).
    ;; ===========================================================

    (defun nelisp_reader_prog2 (_eff val) val)

    ;; ===========================================================
    ;; Char-class predicates.  All return i64 1 on hit, 0 on miss.
    ;; ===========================================================

    (defun nelisp_reader_is_ws (b)
      ;; Whitespace: SP (32), TAB (9), LF (10), CR (13), VT (11),
      ;; FF (12).
      (cond
       ((= b 32) 1)
       ((= b 9) 1)
       ((= b 10) 1)
       ((= b 13) 1)
       ((= b 11) 1)
       ((= b 12) 1)
       (t 0)))

    (defun nelisp_reader_is_digit (b)
      ;; ASCII digit 0..9 (0x30..0x39).
      (if (>= b 48)
          (if (<= b 57) 1 0)
        0))

    (defun nelisp_reader_is_atom_term (b)
      ;; Atom terminators: whitespace + ( ) [ ] ' ` , ; "
      (cond
       ((= b 40) 1)
       ((= b 41) 1)
       ((= b 91) 1)
       ((= b 93) 1)
       ((= b 39) 1)
       ((= b 96) 1)
       ((= b 44) 1)
       ((= b 59) 1)
       ((= b 34) 1)
       (t (nelisp_reader_is_ws b))))

    ;; ===========================================================
    ;; Whitespace + comment skip.  Tail-recursive; returns the
    ;; new cursor where neither whitespace nor a `;'-comment line
    ;; stands.
    ;; ===========================================================

    (defun nelisp_reader_skip_comment (str-ptr cursor n)
      ;; Comment runs to LF (10) or EOF.
      (if (>= cursor n)
          cursor
        (if (= (str-byte-at str-ptr cursor) 10)
            cursor
          (nelisp_reader_skip_comment str-ptr (+ cursor 1) n))))

    (defun nelisp_reader_skip_ws (str-ptr cursor n)
      (if (>= cursor n)
          cursor
        (if (= (nelisp_reader_is_ws (str-byte-at str-ptr cursor)) 1)
            (nelisp_reader_skip_ws str-ptr (+ cursor 1) n)
          (if (= (str-byte-at str-ptr cursor) 59)
              (nelisp_reader_skip_ws
               str-ptr
               (nelisp_reader_skip_comment str-ptr cursor n)
               n)
            cursor))))

    ;; ===========================================================
    ;; Atom scanner: push bytes from STR-PTR[CURSOR..] into SCRATCH
    ;; until an atom-terminator is reached.  Returns the post-atom
    ;; cursor (= the first non-atom byte, or N at EOF).
    ;; ===========================================================

    (defun nelisp_reader_scan_atom (str-ptr cursor n scratch)
      (if (>= cursor n)
          cursor
        (if (= (str-byte-at str-ptr cursor) 92)
            (nelisp_reader_scan_atom_escape
             str-ptr cursor n scratch)
          (if (= (nelisp_reader_is_atom_term (str-byte-at str-ptr cursor)) 1)
              cursor
            (nelisp_reader_prog2
             (mut-str-push-byte scratch (str-byte-at str-ptr cursor))
             (nelisp_reader_scan_atom str-ptr (+ cursor 1) n scratch))))))

    (defun nelisp_reader_scan_atom_escape (str-ptr cursor n scratch)
      ;; Emacs Lisp symbol escape: `\X' contributes X to the symbol name,
      ;; even when X would normally terminate an atom (space, quote, paren).
      ;; A trailing bare backslash is kept as a literal backslash.
      (if (>= (+ cursor 1) n)
          (nelisp_reader_prog2
           (mut-str-push-byte scratch (str-byte-at str-ptr cursor))
           (+ cursor 1))
        (nelisp_reader_prog2
         (mut-str-push-byte scratch (str-byte-at str-ptr (+ cursor 1)))
         (nelisp_reader_scan_atom str-ptr (+ cursor 2) n scratch))))

    ;; ===========================================================
    ;; Atom classification (= re-scan the source byte range).
    ;;
    ;; CLASS state encoded as i64:
    ;;   bit 0  has `.'              (-> Float candidate)
    ;;   bit 1  has `e' or `E'       (-> Float candidate)
    ;;   bit 2  non-numeric byte seen (-> force Sym)
    ;;   bit 3  saw an ASCII digit   (-> required for Int/Float)
    ;;
    ;; Final mapping:
    ;;   sym-bit (bit 2) set        -> 23 (Sym)
    ;;   saw-digit (bit 3) NOT set  -> 23 (Sym) — bare `+', `-', `.'
    ;;                                  symbols (= no digits seen).
    ;;   has-dot or has-e (bit 0|1) -> 21 (Float)
    ;;   else                       -> 20 (Int)
    ;; ===========================================================

    (defun nelisp_reader_classify_step (str-ptr i end class)
      (if (>= i end)
          (cond
           ;; Non-numeric byte seen -> force Sym.
           ((= (logand class 4) 4) 23)
           ;; No digits seen -> bare sign / dot / sign-only-with-non-digit
           ;; -> Sym.  Without this, `(+ x y)' lexes as Int "+".
           ((= (logand class 8) 0) 23)
           ;; Has dot OR exponent -> Float.
           ((> (logand class 3) 0) 21)
           ;; Otherwise -> Int.
           (t 20))
        (cond
         ;; Bit 4 tracks "last byte was e/E" for valid exponent-sign detection.
         ;; All transitions other than e/E clear it; e/E sets bits 1 AND 4.
         ;; Digit: set saw-digit (bit 3), clear last-was-e (bit 4).
         ((= (nelisp_reader_is_digit (str-byte-at str-ptr i)) 1)
          (nelisp_reader_classify_step str-ptr (+ i 1) end
                                       (logior (logand class 15) 8)))
         ;; `.': set dot (bit 0), clear last-was-e.
         ((= (str-byte-at str-ptr i) 46)
          (nelisp_reader_classify_step str-ptr (+ i 1) end
                                       (logior (logand class 15) 1)))
         ;; `e' / `E': set has-e (bit 1) AND last-was-e (bit 4).
         ((= (str-byte-at str-ptr i) 101)
          (nelisp_reader_classify_step str-ptr (+ i 1) end
                                       (logior class 18)))
         ((= (str-byte-at str-ptr i) 69)
          (nelisp_reader_classify_step str-ptr (+ i 1) end
                                       (logior class 18)))
         ;; `+'/`-': sign after digit (bit 3 set) AND not just-after-e
         ;; (bit 4 clear) forces sym (`1+' / `1-' lex as symbols, not
         ;; `Int(1)' + dropped sign).  Otherwise (start, after e/E)
         ;; tolerated.  Clear bit 4 in both branches.
         ((= (str-byte-at str-ptr i) 43)
          (if (= (logand class 8) 8)
              (if (= (logand class 16) 0)
                  (nelisp_reader_classify_step str-ptr (+ i 1) end
                                               (logior (logand class 15) 4))
                (nelisp_reader_classify_step str-ptr (+ i 1) end
                                             (logand class 15)))
            (nelisp_reader_classify_step str-ptr (+ i 1) end
                                         (logand class 15))))
         ((= (str-byte-at str-ptr i) 45)
          (if (= (logand class 8) 8)
              (if (= (logand class 16) 0)
                  (nelisp_reader_classify_step str-ptr (+ i 1) end
                                               (logior (logand class 15) 4))
                (nelisp_reader_classify_step str-ptr (+ i 1) end
                                             (logand class 15)))
            (nelisp_reader_classify_step str-ptr (+ i 1) end
                                         (logand class 15))))
         ;; Anything else: force sym, clear last-was-e.
         (t
          (nelisp_reader_classify_step str-ptr (+ i 1) end
                                       (logior (logand class 15) 4))))))

    (defun nelisp_reader_classify_first_p (b)
      ;; 1 if BYTE starts a numeric atom (digit / `+'/`-' / `.').
      (cond
       ((= (nelisp_reader_is_digit b) 1) 1)
       ((= b 43) 1)
       ((= b 45) 1)
       ((= b 46) 1)
       (t 0)))

    (defun nelisp_reader_classify_atom (str-ptr start end)
      ;; Lone `.' is handled by the caller before reaching here.
      ;; If the first byte isn't a numeric-starter, force sym.
      (if (>= start end)
          23
        (if (= (nelisp_reader_classify_first_p
                (str-byte-at str-ptr start)) 1)
            (nelisp_reader_classify_step str-ptr start end 0)
          23)))

    ;; ===========================================================
    ;; Char literal body scanner (post-`?').  CURSOR points at the
    ;; first byte AFTER `?'.  Pushes the body bytes into SCRATCH +
    ;; returns the post-body cursor.  Two shapes:
    ;;
    ;;   `?X'       — one UTF-8 codepoint X (any non-`\\').
    ;;   `?\\X...'  — backslash-prefixed sequence; consume the `\\'
    ;;                + ALL subsequent non-atom-terminator bytes
    ;;                (= covers `\\C-a' / `\\M-a' / `\\xff' / `\\(' /
    ;;                `\\\\' etc.).  The parser-side decoder will then
    ;;                walk the captured body to compute the codepoint.
    ;;
    ;; Side effect: SCRATCH receives the raw body bytes (no `?').
    ;; Returns: post-body cursor (= first byte AFTER the literal), or
    ;; -1 if the body is empty (= EOF right after `?').
    ;; ===========================================================

    (defun nelisp_reader_char_body_tail (str-ptr cursor n scratch)
      ;; Inside a `?\\...' body — keep pushing while bytes are NOT
      ;; atom terminators.  Differs from `scan_atom' in that we have
      ;; already accepted the leading `\\' + escape char and continue
      ;; greedily; for `?\\C-a' this walks `C', `-', `a'.
      (if (>= cursor n)
          cursor
        (if (= (nelisp_reader_is_atom_term (str-byte-at str-ptr cursor)) 1)
            cursor
          (nelisp_reader_prog2
           (mut-str-push-byte scratch (str-byte-at str-ptr cursor))
           (nelisp_reader_char_body_tail
            str-ptr (+ cursor 1) n scratch)))))

    (defun nelisp_reader_utf8_width (b)
      ;; Return the UTF-8 byte width for leading byte B.  Invalid leading
      ;; bytes fall back to one byte so the reader still advances.
      (if (< b 128)
          1
        (if (>= b 240)
            (if (<= b 247) 4 1)
          (if (>= b 224)
              (if (<= b 239) 3 1)
            (if (>= b 192)
                (if (<= b 223) 2 1)
              1)))))

    (defun nelisp_reader_push_plain_char_bytes
        (str-ptr cursor n scratch width pushed)
      (if (>= pushed width)
          (+ cursor pushed)
        (if (>= (+ cursor pushed) n)
            -1
          (nelisp_reader_prog2
           (mut-str-push-byte scratch
                              (str-byte-at str-ptr (+ cursor pushed)))
           (nelisp_reader_push_plain_char_bytes
            str-ptr cursor n scratch width (+ pushed 1))))))

    (defun nelisp_reader_char_body (str-ptr cursor n scratch)
      (if (>= cursor n)
          -1
        (if (= (str-byte-at str-ptr cursor) 92)
            ;; `?\\X...' — push the `\\' + the next byte unconditionally
            ;; (handles `?\\)' / `?\\\"' which would otherwise terminate),
            ;; then continue with the tail scanner.
            (if (>= (+ cursor 1) n)
                -1
              (nelisp_reader_prog2
               (mut-str-push-byte scratch 92)
               (nelisp_reader_prog2
                (mut-str-push-byte scratch (str-byte-at str-ptr (+ cursor 1)))
                (nelisp_reader_char_body_tail
                 str-ptr (+ cursor 2) n scratch))))
          ;; `?X' — push the complete UTF-8 codepoint byte sequence.
          (nelisp_reader_push_plain_char_bytes
           str-ptr cursor n scratch
           (nelisp_reader_utf8_width (str-byte-at str-ptr cursor))
           0))))

    (defun nelisp_reader_lex_char_finalize
        (end-or-err payload-slot cursor-out-slot scratch)
      (if (< end-or-err 0)
          (nelisp_reader_prog2
           (sexp-int-make cursor-out-slot 0)
           -1)
        (nelisp_reader_prog2
         (sexp-int-make cursor-out-slot end-or-err)
         (nelisp_reader_prog2
          (mut-str-finalize scratch payload-slot)
          24))))

    (defun nelisp_reader_lex_char
        (str-ptr cursor n payload-slot cursor-out-slot scratch)
      ;; CURSOR points at the byte AFTER the leading `?'.
      (nelisp_reader_lex_char_finalize
       (nelisp_reader_char_body str-ptr cursor n scratch)
       payload-slot cursor-out-slot scratch))

    ;; ===========================================================
    ;; Radix-int body scanner.  CURSOR points at the first byte AFTER
    ;; the radix marker (= `#x' / `#X' / `#o' / `#O' / `#b' / `#B').
    ;; BASE-BYTE is the lowercase marker (`x'/`o'/`b') — pushed FIRST
    ;; into SCRATCH so the parser can dispatch on radix without
    ;; re-reading the source.  Then push every non-atom-terminator
    ;; byte (sign, digits, `_' separators).
    ;; ===========================================================

    (defun nelisp_reader_radix_body
        (str-ptr cursor n scratch)
      (if (>= cursor n)
          cursor
        (if (= (nelisp_reader_is_atom_term (str-byte-at str-ptr cursor)) 1)
            cursor
          (nelisp_reader_prog2
           (mut-str-push-byte scratch (str-byte-at str-ptr cursor))
           (nelisp_reader_radix_body str-ptr (+ cursor 1) n scratch)))))

    (defun nelisp_reader_lex_radix
        (str-ptr cursor n payload-slot cursor-out-slot scratch)
      ;; CURSOR points at the first DIGIT byte (= post `#x'/`#o'/`#b').
      ;; The caller has ALREADY pushed the lowercase base marker
      ;; (`x'/`o'/`b') onto SCRATCH before calling.  Body bytes follow.
      (nelisp_reader_lex_radix_finalize
       (nelisp_reader_radix_body str-ptr cursor n scratch)
       payload-slot cursor-out-slot scratch))

    (defun nelisp_reader_lex_radix_finalize
        (end payload-slot cursor-out-slot scratch)
      (nelisp_reader_prog2
       (sexp-int-make cursor-out-slot end)
       (nelisp_reader_prog2
        (mut-str-finalize scratch payload-slot)
        25)))

    ;; ===========================================================
    ;; String literal scanner.  Caller has already consumed `"'.
    ;; Returns: i64 next-cursor (= byte AFTER closing `"') on
    ;; success, or -1 on unterminated.  Side effect: pushes
    ;; resolved bytes into SCRATCH.
    ;; ===========================================================

    (defun nelisp_reader_string_body (str-ptr cursor n scratch)
      (if (>= cursor n)
          -1
        (cond
         ((= (str-byte-at str-ptr cursor) 34)
          ;; Closing quote.
          (+ cursor 1))
         ((= (str-byte-at str-ptr cursor) 92)
          (if (>= (+ cursor 1) n)
              -1
            (nelisp_reader_string_escape str-ptr (+ cursor 1) n scratch)))
         (t
          (nelisp_reader_prog2
           (mut-str-push-byte scratch (str-byte-at str-ptr cursor))
           (nelisp_reader_string_body str-ptr (+ cursor 1) n scratch))))))

    (defun nelisp_reader_string_escape (str-ptr cursor n scratch)
      ;; CURSOR points at the byte AFTER `\\'.
      (cond
       ;; `\\a' -> BEL
       ((= (str-byte-at str-ptr cursor) 97)
        (nelisp_reader_prog2
         (mut-str-push-byte scratch 7)
         (nelisp_reader_string_body str-ptr (+ cursor 1) n scratch)))
       ;; `\\b' -> BS
       ((= (str-byte-at str-ptr cursor) 98)
        (nelisp_reader_prog2
         (mut-str-push-byte scratch 8)
         (nelisp_reader_string_body str-ptr (+ cursor 1) n scratch)))
       ;; `\\d' -> DEL
       ((= (str-byte-at str-ptr cursor) 100)
        (nelisp_reader_prog2
         (mut-str-push-byte scratch 127)
         (nelisp_reader_string_body str-ptr (+ cursor 1) n scratch)))
       ;; `\\e' -> ESC
       ((= (str-byte-at str-ptr cursor) 101)
        (nelisp_reader_prog2
         (mut-str-push-byte scratch 27)
         (nelisp_reader_string_body str-ptr (+ cursor 1) n scratch)))
       ;; `\\f' -> FF
       ((= (str-byte-at str-ptr cursor) 102)
        (nelisp_reader_prog2
         (mut-str-push-byte scratch 12)
         (nelisp_reader_string_body str-ptr (+ cursor 1) n scratch)))
       ;; `\\n' -> LF
       ((= (str-byte-at str-ptr cursor) 110)
        (nelisp_reader_prog2
         (mut-str-push-byte scratch 10)
         (nelisp_reader_string_body str-ptr (+ cursor 1) n scratch)))
       ;; `\\r' -> CR
       ((= (str-byte-at str-ptr cursor) 114)
        (nelisp_reader_prog2
         (mut-str-push-byte scratch 13)
         (nelisp_reader_string_body str-ptr (+ cursor 1) n scratch)))
       ;; `\\s' -> SP
       ((= (str-byte-at str-ptr cursor) 115)
        (nelisp_reader_prog2
         (mut-str-push-byte scratch 32)
         (nelisp_reader_string_body str-ptr (+ cursor 1) n scratch)))
       ;; `\\t' -> TAB
       ((= (str-byte-at str-ptr cursor) 116)
        (nelisp_reader_prog2
         (mut-str-push-byte scratch 9)
         (nelisp_reader_string_body str-ptr (+ cursor 1) n scratch)))
       ;; `\\v' -> VT
       ((= (str-byte-at str-ptr cursor) 118)
        (nelisp_reader_prog2
         (mut-str-push-byte scratch 11)
         (nelisp_reader_string_body str-ptr (+ cursor 1) n scratch)))
       ;; `\\0' -> NUL
       ((= (str-byte-at str-ptr cursor) 48)
        (nelisp_reader_prog2
         (mut-str-push-byte scratch 0)
         (nelisp_reader_string_body str-ptr (+ cursor 1) n scratch)))
       ;; `\\\\' -> backslash
       ((= (str-byte-at str-ptr cursor) 92)
        (nelisp_reader_prog2
         (mut-str-push-byte scratch 92)
         (nelisp_reader_string_body str-ptr (+ cursor 1) n scratch)))
       ;; `\\"' -> double quote
       ((= (str-byte-at str-ptr cursor) 34)
        (nelisp_reader_prog2
         (mut-str-push-byte scratch 34)
         (nelisp_reader_string_body str-ptr (+ cursor 1) n scratch)))
       ;; `\\<SP>' -> line continuation (drop both bytes).
       ((= (str-byte-at str-ptr cursor) 32)
        (nelisp_reader_string_body str-ptr (+ cursor 1) n scratch))
       ;; `\\<LF>' -> line continuation (drop both bytes).
       ((= (str-byte-at str-ptr cursor) 10)
        (nelisp_reader_string_body str-ptr (+ cursor 1) n scratch))
       ;; Unknown escape: drop backslash, push the byte literal.
       (t
        (nelisp_reader_prog2
         (mut-str-push-byte scratch (str-byte-at str-ptr cursor))
         (nelisp_reader_string_body str-ptr (+ cursor 1) n scratch)))))

    ;; ===========================================================
    ;; Lone-`.' detector by scanning source bytes (no
    ;; `mut-str-byte-at' grammar op exists).
    ;; ===========================================================

    (defun nelisp_reader_atom_is_lone_dot (str-ptr start end)
      (if (= (- end start) 1)
          (if (= (str-byte-at str-ptr start) 46)
              1
            0)
        0))

    ;; ===========================================================
    ;; Per-token leaf dispatchers.  Each writes the cursor-out
    ;; via `sexp-int-make' (= side effect on the caller-owned
    ;; cursor slot) and returns the kind code.
    ;; ===========================================================

    (defun nelisp_reader_emit_eof (cursor-out-slot cursor)
      (nelisp_reader_prog2 (sexp-int-make cursor-out-slot cursor) 0))

    (defun nelisp_reader_emit_single (cursor-out-slot cursor kind)
      (nelisp_reader_prog2
       (sexp-int-make cursor-out-slot (+ cursor 1)) kind))

    (defun nelisp_reader_emit_double (cursor-out-slot cursor kind)
      (nelisp_reader_prog2
       (sexp-int-make cursor-out-slot (+ cursor 2)) kind))

    (defun nelisp_reader_emit_triple (cursor-out-slot cursor kind)
      (nelisp_reader_prog2
       (sexp-int-make cursor-out-slot (+ cursor 3)) kind))

    (defun nelisp_reader_emit_quadruple (cursor-out-slot cursor kind)
      (nelisp_reader_prog2
       (sexp-int-make cursor-out-slot (+ cursor 4)) kind))

    (defun nelisp_reader_emit_error (cursor-out-slot cursor)
      (nelisp_reader_prog2 (sexp-int-make cursor-out-slot cursor) -1))

    ;; ===========================================================
    ;; Atom lex driver: scan, optionally finalize payload.  END
    ;; is the post-atom cursor (= return value of `scan_atom').
    ;; ===========================================================

    (defun nelisp_reader_lex_atom_finalize
        (str-ptr start end payload-slot cursor-out-slot scratch)
      (if (= (nelisp_reader_atom_is_lone_dot str-ptr start end) 1)
          ;; Dot token: no payload.
          (nelisp_reader_prog2
           (sexp-int-make cursor-out-slot end)
           10)
        (if (>= start end)
            (nelisp_reader_prog2
             (sexp-int-make cursor-out-slot end)
             -1)
          (nelisp_reader_finalize_classified
           scratch payload-slot cursor-out-slot end
           (nelisp_reader_classify_atom str-ptr start end)))))

    (defun nelisp_reader_finalize_classified
        (scratch payload-slot cursor-out-slot end kind)
      ;; Write cursor-out, finalize scratch -> payload-slot, return kind.
      (nelisp_reader_prog2
       (sexp-int-make cursor-out-slot end)
       (nelisp_reader_prog2
        (mut-str-finalize scratch payload-slot)
        kind)))

    (defun nelisp_reader_lex_atom
        (str-ptr cursor n payload-slot cursor-out-slot scratch)
      (if (= (nelisp_reader_leading_dot_float_p str-ptr cursor n) 1)
          (nelisp_reader_prog2
           (mut-str-push-byte scratch 48)
           (nelisp_reader_lex_atom_finalize
            str-ptr cursor
            (nelisp_reader_scan_atom str-ptr cursor n scratch)
            payload-slot cursor-out-slot scratch))
        (if (= (nelisp_reader_signed_leading_dot_float_p str-ptr cursor n) 1)
            (nelisp_reader_prog2
             (mut-str-push-byte scratch (str-byte-at str-ptr cursor))
             (nelisp_reader_prog2
              (mut-str-push-byte scratch 48)
              (nelisp_reader_lex_atom_finalize
               str-ptr cursor
               (nelisp_reader_scan_atom str-ptr (+ cursor 1) n scratch)
               payload-slot cursor-out-slot scratch)))
          (nelisp_reader_lex_atom_finalize
           str-ptr cursor
           (nelisp_reader_scan_atom str-ptr cursor n scratch)
           payload-slot cursor-out-slot scratch))))

    (defun nelisp_reader_leading_dot_float_p (str-ptr cursor n)
      (if (>= (+ cursor 1) n)
          0
        (if (= (str-byte-at str-ptr cursor) 46)
            (= (nelisp_reader_is_digit (str-byte-at str-ptr (+ cursor 1))) 1)
          0)))

    (defun nelisp_reader_signed_leading_dot_float_p (str-ptr cursor n)
      (if (>= (+ cursor 2) n)
          0
        (if (= (str-byte-at str-ptr cursor) 43)
            (if (= (str-byte-at str-ptr (+ cursor 1)) 46)
                (= (nelisp_reader_is_digit
                    (str-byte-at str-ptr (+ cursor 2))) 1)
              0)
          (if (= (str-byte-at str-ptr cursor) 45)
              (if (= (str-byte-at str-ptr (+ cursor 1)) 46)
                  (= (nelisp_reader_is_digit
                      (str-byte-at str-ptr (+ cursor 2))) 1)
                0)
            0))))

    ;; ===========================================================
    ;; String lex driver.
    ;; ===========================================================

    (defun nelisp_reader_lex_string_finalize
        (end-or-err payload-slot cursor-out-slot scratch)
      (if (< end-or-err 0)
          ;; Error: write a stable cursor (= 0) and return -1.
          (nelisp_reader_prog2
           (sexp-int-make cursor-out-slot 0)
           -1)
        (nelisp_reader_prog2
         (sexp-int-make cursor-out-slot end-or-err)
         (nelisp_reader_prog2
          (mut-str-finalize scratch payload-slot)
          22))))

    (defun nelisp_reader_lex_string
        (str-ptr cursor n payload-slot cursor-out-slot scratch)
      (nelisp_reader_lex_string_finalize
       (nelisp_reader_string_body str-ptr cursor n scratch)
       payload-slot cursor-out-slot scratch))

    ;; ===========================================================
    ;; Sharpsign dispatch: `#'' / `##' / `#s(' / `#^[' / `#^^[' /
    ;; `#x..' / `#o..' / `#b..' / fail.
    ;; ===========================================================

    (defun nelisp_reader_lex_sharpsign
        (str-ptr cursor n payload-slot cursor-out-slot scratch)
      (if (>= (+ cursor 1) n)
          (nelisp_reader_emit_error cursor-out-slot (+ cursor 1))
        (cond
         ;; `#''  -> function-quote (kind 9), 2-byte token.
         ((= (str-byte-at str-ptr (+ cursor 1)) 39)
          (nelisp_reader_emit_double cursor-out-slot cursor 9))
         ;; `##' is an ordinary symbol in Emacs Lisp arglists, notably
         ;; in Org's declare-function hints.  Other unknown # dispatches
         ;; remain reader errors.
         ((= (str-byte-at str-ptr (+ cursor 1)) 35)
          (nelisp_reader_lex_atom
           str-ptr cursor n payload-slot cursor-out-slot scratch))
         ;; `#s(' -> sharps-paren (kind 11), 3-byte token.
         ((= (str-byte-at str-ptr (+ cursor 1)) 115)
          (if (>= (+ cursor 2) n)
              (nelisp_reader_emit_error cursor-out-slot (+ cursor 2))
            (if (= (str-byte-at str-ptr (+ cursor 2)) 40)
                (nelisp_reader_emit_triple cursor-out-slot cursor 11)
              (nelisp_reader_emit_error cursor-out-slot (+ cursor 2)))))
         ;; `#^[' / `#^^[' -> char-table/sub-char-table bracket.  Parser
         ;; currently materialises these as vector literals so generated
         ;; vendor char-table data can pass through the standalone reader.
         ((= (str-byte-at str-ptr (+ cursor 1)) 94)
          (if (>= (+ cursor 2) n)
              (nelisp_reader_emit_error cursor-out-slot (+ cursor 2))
            (if (= (str-byte-at str-ptr (+ cursor 2)) 91)
                (nelisp_reader_emit_triple cursor-out-slot cursor 12)
              (if (= (str-byte-at str-ptr (+ cursor 2)) 94)
                  (if (>= (+ cursor 3) n)
                      (nelisp_reader_emit_error cursor-out-slot (+ cursor 3))
                    (if (= (str-byte-at str-ptr (+ cursor 3)) 91)
                        (nelisp_reader_emit_quadruple
                         cursor-out-slot cursor 12)
                      (nelisp_reader_emit_error
                       cursor-out-slot (+ cursor 3))))
                (nelisp_reader_emit_error cursor-out-slot (+ cursor 2))))))
         ;; `#x..' / `#X..' -> RadixInt (kind 25, base 16).  Pre-push
         ;; the lowercase `x' marker onto SCRATCH so `lex_radix' fits
         ;; the 6-reg ABI.
         ((= (str-byte-at str-ptr (+ cursor 1)) 120)
          (nelisp_reader_prog2
           (mut-str-push-byte scratch 120)
           (nelisp_reader_lex_radix str-ptr (+ cursor 2) n
                                    payload-slot cursor-out-slot scratch)))
         ((= (str-byte-at str-ptr (+ cursor 1)) 88)
          (nelisp_reader_prog2
           (mut-str-push-byte scratch 120)
           (nelisp_reader_lex_radix str-ptr (+ cursor 2) n
                                    payload-slot cursor-out-slot scratch)))
         ;; `#o..' / `#O..' -> RadixInt (kind 25, base 8).
         ((= (str-byte-at str-ptr (+ cursor 1)) 111)
          (nelisp_reader_prog2
           (mut-str-push-byte scratch 111)
           (nelisp_reader_lex_radix str-ptr (+ cursor 2) n
                                    payload-slot cursor-out-slot scratch)))
         ((= (str-byte-at str-ptr (+ cursor 1)) 79)
          (nelisp_reader_prog2
           (mut-str-push-byte scratch 111)
           (nelisp_reader_lex_radix str-ptr (+ cursor 2) n
                                    payload-slot cursor-out-slot scratch)))
         ;; `#b..' / `#B..' -> RadixInt (kind 25, base 2).
         ((= (str-byte-at str-ptr (+ cursor 1)) 98)
          (nelisp_reader_prog2
           (mut-str-push-byte scratch 98)
           (nelisp_reader_lex_radix str-ptr (+ cursor 2) n
                                    payload-slot cursor-out-slot scratch)))
         ((= (str-byte-at str-ptr (+ cursor 1)) 66)
          (nelisp_reader_prog2
           (mut-str-push-byte scratch 98)
           (nelisp_reader_lex_radix str-ptr (+ cursor 2) n
                                    payload-slot cursor-out-slot scratch)))
         (t
          (nelisp_reader_emit_error cursor-out-slot (+ cursor 1))))))

    ;; ===========================================================
    ;; Comma dispatch: `,' or `,@'.
    ;; ===========================================================

    (defun nelisp_reader_lex_comma
        (str-ptr cursor n cursor-out-slot)
      (if (>= (+ cursor 1) n)
          (nelisp_reader_emit_single cursor-out-slot cursor 7)
        (if (= (str-byte-at str-ptr (+ cursor 1)) 64)
            (nelisp_reader_emit_double cursor-out-slot cursor 8)
          (nelisp_reader_emit_single cursor-out-slot cursor 7))))

    ;; ===========================================================
    ;; Main dispatch: first byte -> token kind branch.
    ;; ===========================================================

    ;; ===========================================================
    ;; `?'-dispatcher.  Emacs reader compat: bare `?' (= followed by
    ;; whitespace OR EOF) is the symbol named `?'.  Otherwise the byte
    ;; is the start of a char literal body (= kind 24).
    ;; ===========================================================

    (defun nelisp_reader_lex_question
        (str-ptr cursor n payload-slot cursor-out-slot scratch)
      (if (>= (+ cursor 1) n)
          ;; Bare `?' at EOF -> symbol `?'.
          (nelisp_reader_lex_atom
           str-ptr cursor n payload-slot cursor-out-slot scratch)
        (if (= (nelisp_reader_is_ws (str-byte-at str-ptr (+ cursor 1))) 1)
            (nelisp_reader_lex_atom
             str-ptr cursor n payload-slot cursor-out-slot scratch)
          (nelisp_reader_lex_char
           str-ptr (+ cursor 1) n payload-slot cursor-out-slot scratch))))

    (defun nelisp_reader_dispatch
        (str-ptr cursor n payload-slot cursor-out-slot scratch)
      (if (>= cursor n)
          (nelisp_reader_emit_eof cursor-out-slot cursor)
        ;; First byte at CURSOR drives the dispatch.  Re-read at
        ;; each arm via `str-byte-at' rather than passing as a
        ;; 7th arg (= would exceed AOT's 6-reg ABI).
        (cond
         ((= (str-byte-at str-ptr cursor) 40)
          (nelisp_reader_emit_single cursor-out-slot cursor 1))
         ((= (str-byte-at str-ptr cursor) 41)
          (nelisp_reader_emit_single cursor-out-slot cursor 2))
         ((= (str-byte-at str-ptr cursor) 91)
          (nelisp_reader_emit_single cursor-out-slot cursor 3))
         ((= (str-byte-at str-ptr cursor) 93)
          (nelisp_reader_emit_single cursor-out-slot cursor 4))
         ((= (str-byte-at str-ptr cursor) 39)
          (nelisp_reader_emit_single cursor-out-slot cursor 5))
         ((= (str-byte-at str-ptr cursor) 96)
          (nelisp_reader_emit_single cursor-out-slot cursor 6))
         ((= (str-byte-at str-ptr cursor) 44)
          (nelisp_reader_lex_comma str-ptr cursor n cursor-out-slot))
         ((= (str-byte-at str-ptr cursor) 35)
          (nelisp_reader_lex_sharpsign
           str-ptr cursor n payload-slot cursor-out-slot scratch))
         ((= (str-byte-at str-ptr cursor) 34)
          (nelisp_reader_lex_string
           str-ptr (+ cursor 1) n payload-slot cursor-out-slot scratch))
         ((= (str-byte-at str-ptr cursor) 63)
          (nelisp_reader_lex_question
           str-ptr cursor n payload-slot cursor-out-slot scratch))
         (t
          (nelisp_reader_lex_atom
           str-ptr cursor n payload-slot cursor-out-slot scratch)))))

    ;; ===========================================================
    ;; Top-level entry.  Skips leading whitespace + comments,
    ;; then dispatches.
    ;; ===========================================================

    (defun nelisp_reader_lex_one
        (str-ptr cursor payload-slot cursor-out-slot scratch)
      (nelisp_reader_dispatch
       str-ptr
       (nelisp_reader_skip_ws str-ptr cursor (str-len str-ptr))
       (str-len str-ptr)
       payload-slot cursor-out-slot scratch)))

  "AOT source for Doc 116 §116.A pure-elisp Reader lexer.

Implements the tokenizer state machine in `reader/lexer.rs' as a
collection of tail-recursive defuns sharing the SCRATCH MutStr and
the source `Sexp::Str' pointer.  Returns an i64 token kind code;
the Rust safe wrapper assembles the final `(KIND-SYM . PAYLOAD)'
Sexp::Cons that the §116.B parser will consume.

Kinds: 0 EOF, 1 LParen, 2 RParen, 3 LBracket, 4 RBracket, 5 Quote,
6 Backquote, 7 Comma, 8 CommaAt, 9 FunctionQuote, 10 Dot,
11 SharpsParen, 12 CharTableBracket, 20 Int, 21 Float, 22 Str,
23 Sym, 24 Char, 25 RadixInt, -1 Error.")

(provide 'nelisp-cc-reader-lexer)

;;; nelisp-cc-reader-lexer.el ends here
