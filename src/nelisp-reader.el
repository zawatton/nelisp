;;; nelisp-reader.el --- Phase 7+A reader (Doc 40 §3.A)  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; Author: zawatton <kurozawawo@gmail.com>

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Phase 7+A reader (Doc 40 §3.A LOCKED v2).  Self-contained
;; s-expression parser implementing the public surface required by
;; the Phase 7+ evaluator pipeline:
;;
;;   (nelisp-reader-read STREAM)
;;       Read one form from STREAM.  STREAM is a string today; the
;;       interface is shaped so a buffer / function backend can be
;;       plugged in by Phase 7+B without changing the call sites.
;;
;;   (nelisp-reader-read-from-string STRING &optional START END)
;;       Emacs-compatible signature.  Returns (FORM . NEXT-POS).
;;
;;   (nelisp-reader-read-from-string-with-position STRING POS)
;;       Explicit position variant.  Returns (FORM . NEXT-POS) where
;;       NEXT-POS is the index just past the parsed form so callers
;;       can pump multiple forms out of one buffer.
;;
;; This file is parallel to (and independent from) `nelisp-read.el',
;; the Phase 3a SHIPPED reader.  Both readers MUST stay byte-perfect
;; compatible until Phase 7+B picks one as the canonical front-end.
;; The split exists because Doc 40 §4.1 names a fresh public API
;; surface; keeping the legacy file intact preserves the Phase 3a
;; ERT pin (`test/nelisp-read-test.el`) during the transition.
;;
;; Token coverage (mirrors Doc 40 §3.A scope):
;;
;;   atoms            nil / t / integer / float / symbol / string
;;   list             () / (a b c) / (a (b c)) / (a . b) / (a b . c)
;;   vector           [] / [1 2 3] / #(1 2 3) (alias)
;;   quoted           'X (quote X)   #'X (function X)
;;   backquote        `X / ,X / ,@X  one-level expansion (cons / append)
;;   character        ?a / ?\\n / ?\\t / ?\\r / ?\\\\ / ?\\\" / ?\\<NN>
;;   string escapes   \\\\ \\\" \\n \\t \\r \\f \\a \\b \\<oct> \\u<hex>
;;   comments         ; ... newline   #| ... |#  (block, balanced)
;;   read syntax      #b1010 / #o17 / #x1F / #s(NAME slot val ...)
;;
;; Phase 7+A excludes (carry-forward to later sub-phases per Doc 40):
;;
;;   #@N byte-skip                               §3.A risks
;;   #,FORM eval-at-read                         §3.A risks
;;   ## empty-symbol literal                     §3.A risks
;;   ?\\C-a / ?\\M-x meta/control char escape    §3.A scope (TODO)
;;   #N= / #N# circular reference                §3.A out of scope
;;
;; The implementation is pure string-walking — no buffer primitives,
;; no `read' fallback — so the parser ports cleanly to NeLisp itself
;; once the bootstrap evaluator is online (Phase 7+B+).

;;; Code:

(define-error 'nelisp-reader-error "NeLisp reader error (Phase 7+A)")

(defconst nelisp-reader--atom-terminators
  '(?\s ?\t ?\n ?\r ?\f
    ?\( ?\) ?\[ ?\] ?\" ?\' ?\` ?\, ?\;)
  "Characters that end an atom token.")

(defconst nelisp-reader--numeric-regexp
  "\\`[-+]?\\(?:[0-9]+\\(?:\\.[0-9]*\\)?\\|\\.[0-9]+\\)\\(?:[eE][-+]?[0-9]+\\)?\\'"
  "Regexp matching a decimal integer or float token.
Examples: 42 / -3 / +5 / 3.14 / .5 / 1. / -2.5e3.  Type discrimination
is delegated to `string-to-number' plus a `[.eE]' look-ahead so tokens
like \"1.\" coerce to float reliably across Emacs versions.")

(defvar nelisp-reader--bq-depth 0
  "Backquote nesting depth.
Bound dynamically by `nelisp-reader--read-backquote' / `--read-comma'
so a stray `,' / `,@' outside any `\\=`' signals a clean reader error
instead of producing a ghost form.")

(defconst nelisp-reader--bq-quasi-symbol (intern "`")
  "Symbol head used to preserve depth>=2 backquote markers as data.")

(defconst nelisp-reader--bq-comma-symbol (intern ",")
  "Symbol head used to preserve depth>=2 unquote markers as data.")

(defconst nelisp-reader--bq-splice-symbol (intern ",@")
  "Symbol head used to preserve depth>=2 splice markers as data.")

;;; Internal helpers ---------------------------------------------------

(defsubst nelisp-reader--atom-char-p (c)
  "Return non-nil if character C may appear inside an atom token."
  (not (memq c nelisp-reader--atom-terminators)))

(defun nelisp-reader--skip-ws (str pos)
  "Skip whitespace and comments in STR starting at POS.
Recognises both line comments (`;' through newline) and balanced
block comments (`#| ... |#').  Returns the new position."
  (let ((len (length str))
        (done nil))
    (while (and (not done) (< pos len))
      (let ((c (aref str pos)))
        (cond
         ((memq c '(?\s ?\t ?\n ?\r ?\f))
          (setq pos (1+ pos)))
         ((eq c ?\;)
          ;; Line comment: discard through next newline (or EOF).
          (while (and (< pos len) (not (eq (aref str pos) ?\n)))
            (setq pos (1+ pos))))
         ((and (eq c ?#)
               (< (1+ pos) len)
               (eq (aref str (1+ pos)) ?|))
          (setq pos (nelisp-reader--skip-block-comment str (+ pos 2))))
         (t (setq done t)))))
    pos))

(defun nelisp-reader--skip-block-comment (str pos)
  "Skip a balanced `#| ... |#' block comment in STR.
POS is the index just past the opening `#|'.  Returns the index
just past the matching `|#'.  Signals on EOF without close."
  (let ((len (length str))
        (depth 1))
    (while (and (> depth 0) (< pos len))
      (let ((c (aref str pos)))
        (cond
         ((and (eq c ?|)
               (< (1+ pos) len)
               (eq (aref str (1+ pos)) ?#))
          (setq depth (1- depth))
          (setq pos (+ pos 2)))
         ((and (eq c ?#)
               (< (1+ pos) len)
               (eq (aref str (1+ pos)) ?|))
          (setq depth (1+ depth))
          (setq pos (+ pos 2)))
         (t (setq pos (1+ pos))))))
    (when (> depth 0)
      (signal 'nelisp-reader-error
              (list "unterminated block comment" pos)))
    pos))

(defun nelisp-reader--atom-end (str pos)
  "Return position one past the last atom char in STR starting at POS."
  (let ((len (length str)))
    (while (and (< pos len)
                (nelisp-reader--atom-char-p (aref str pos)))
      (setq pos (1+ pos)))
    pos))

(defun nelisp-reader--read-atom (str pos)
  "Read a number-or-symbol atom in STR at POS.
Return (VALUE . NEW-POS).  Numeric tokens with `.' or `eE' coerce to
float to match Emacs `read' semantics across versions."
  (let* ((end (nelisp-reader--atom-end str pos))
         (tok (substring str pos end)))
    (when (string-empty-p tok)
      (signal 'nelisp-reader-error
              (list "expected atom" pos)))
    (cons (if (string-match-p nelisp-reader--numeric-regexp tok)
              (let ((n (string-to-number tok)))
                (if (and (integerp n)
                         (string-match-p "[.eE]" tok))
                    (float n)
                  n))
            (intern tok))
          end)))

(defun nelisp-reader--read-string (str pos)
  "Read a string literal in STR at the opening quote at POS.
Return (STRING . NEW-POS) where NEW-POS is past the closing quote.

Recognised escapes:

  \\\\        backslash             \\\"        double quote
  \\n         newline               \\t         tab
  \\r         carriage return       \\f         form feed
  \\a         bell                  \\b         backspace
  \\<NL>      line continuation (drop both bytes)
  \\<oct><oct><oct>     up to 3 octal digits      → unsigned char
  \\u<HHHH>             4 hex digits              → unicode codepoint
  \\U<HHHHHHHH>         8 hex digits              → unicode codepoint"
  (let ((len (length str))
        (chars nil)
        (pos (1+ pos)))
    (catch 'done
      (while t
        (when (>= pos len)
          (signal 'nelisp-reader-error (list "unterminated string")))
        (let ((c (aref str pos)))
          (cond
           ((eq c ?\")
            (throw 'done nil))
           ((eq c ?\\)
            (when (>= (1+ pos) len)
              (signal 'nelisp-reader-error (list "unterminated escape")))
            (let ((res (nelisp-reader--read-string-escape str (1+ pos) len)))
              (when (car res)
                (push (car res) chars))
              (setq pos (cdr res))))
           (t
            (push c chars)
            (setq pos (1+ pos)))))))
    (cons (apply #'string (nreverse chars))
          (1+ pos))))

(defun nelisp-reader--read-string-escape (str pos len)
  "Decode one string escape sequence in STR at POS (past the `\\').
LEN is (length STR).  Return (CHAR-OR-NIL . NEW-POS).  CHAR-OR-NIL
is nil for line continuations so the caller drops both bytes.

Octal payloads consume up to three `[0-7]' digits.  Unicode escapes
consume exactly 4 (`\\u') or 8 (`\\U') hex digits."
  (let ((e (aref str pos)))
    (cond
     ((eq e ?\\) (cons ?\\ (1+ pos)))
     ((eq e ?\") (cons ?\" (1+ pos)))
     ((eq e ?n)  (cons ?\n (1+ pos)))
     ((eq e ?t)  (cons ?\t (1+ pos)))
     ((eq e ?r)  (cons ?\r (1+ pos)))
     ((eq e ?f)  (cons ?\f (1+ pos)))
     ((eq e ?a)  (cons ?\a (1+ pos)))
     ((eq e ?b)  (cons ?\b (1+ pos)))
     ((eq e ?e)  (cons 27  (1+ pos)))
     ((eq e ?s)  (cons ?\s (1+ pos)))
     ((eq e ?\') (cons ?\' (1+ pos)))
     ((eq e ?\?) (cons ??  (1+ pos)))
     ((eq e ?\n) (cons nil (1+ pos))) ; line continuation
     ((and (>= e ?0) (<= e ?7))
      (nelisp-reader--read-octal-escape str pos len))
     ((eq e ?u)
      (nelisp-reader--read-unicode-escape str (1+ pos) len 4))
     ((eq e ?U)
      (nelisp-reader--read-unicode-escape str (1+ pos) len 8))
     ((eq e ?x)
      (nelisp-reader--read-hex-escape str (1+ pos) len))
     (t
      (signal 'nelisp-reader-error
              (list "unknown string escape" e pos))))))

(defun nelisp-reader--read-octal-escape (str pos len)
  "Read up to 3 octal digits at POS in STR.  Return (CHAR . NEW-POS)."
  (let ((end pos)
        (cap (min len (+ pos 3))))
    (while (and (< end cap)
                (let ((c (aref str end)))
                  (and (>= c ?0) (<= c ?7))))
      (setq end (1+ end)))
    (cons (string-to-number (substring str pos end) 8) end)))

(defun nelisp-reader--read-unicode-escape (str pos len digits)
  "Read exactly DIGITS hex characters at POS in STR.
LEN is (length STR).  Return (CHAR . NEW-POS).  Signals when the
required digits are not all present or not all hex."
  (when (< (- len pos) digits)
    (signal 'nelisp-reader-error
            (list "short unicode escape" pos digits)))
  (let* ((tok (substring str pos (+ pos digits))))
    (unless (string-match-p "\\`[0-9a-fA-F]+\\'" tok)
      (signal 'nelisp-reader-error
              (list "non-hex unicode escape" tok pos)))
    (cons (string-to-number tok 16) (+ pos digits))))

(defun nelisp-reader--read-hex-escape (str pos len)
  "Read a `\\x' hex escape at POS in STR.
Greedy: consumes every `[0-9a-fA-F]' until non-hex or EOF.
Return (CHAR . NEW-POS)."
  (let ((end pos))
    (while (and (< end len)
                (let ((c (aref str end)))
                  (or (and (>= c ?0) (<= c ?9))
                      (and (>= c ?a) (<= c ?f))
                      (and (>= c ?A) (<= c ?F)))))
      (setq end (1+ end)))
    (when (= end pos)
      (signal 'nelisp-reader-error
              (list "empty hex escape" pos)))
    (cons (string-to-number (substring str pos end) 16) end)))

(defun nelisp-reader--dotted-pair-marker-p (str pos len)
  "Return non-nil if STR[POS] starts a dotted-pair `.' separator.
A bare `.' followed by whitespace, `)' or EOF separates the last
element from the cdr of an improper list."
  (and (eq (aref str pos) ?.)
       (or (>= (1+ pos) len)
           (memq (aref str (1+ pos))
                 '(?\s ?\t ?\n ?\r ?\f ?\))))))

(defun nelisp-reader--read-list (str pos)
  "Read the tail of a list in STR.  POS is the index just past `('.
Return (LIST . NEW-POS) where NEW-POS is past the closing `)'."
  (let ((len (length str))
        (acc nil)
        (tail nil)
        (have-tail nil))
    (catch 'done
      (while t
        (setq pos (nelisp-reader--skip-ws str pos))
        (when (>= pos len)
          (signal 'nelisp-reader-error (list "unterminated list")))
        (let ((c (aref str pos)))
          (cond
           ((eq c ?\))
            (throw 'done nil))
           ((nelisp-reader--dotted-pair-marker-p str pos len)
            (unless acc
              (signal 'nelisp-reader-error
                      (list "dot before first element in list")))
            (setq pos (nelisp-reader--skip-ws str (1+ pos)))
            (let ((res (nelisp-reader--read-sexp str pos)))
              (setq tail (car res))
              (setq have-tail t)
              (setq pos (cdr res)))
            (setq pos (nelisp-reader--skip-ws str pos))
            (unless (and (< pos len) (eq (aref str pos) ?\)))
              (signal 'nelisp-reader-error
                      (list "expected `)' after dotted tail" pos)))
            (throw 'done nil))
           (t
            (let ((res (nelisp-reader--read-sexp str pos)))
              (push (car res) acc)
              (setq pos (cdr res))))))))
    (let ((lst (nreverse acc)))
      (when have-tail
        (let ((head lst))
          (while (cdr head) (setq head (cdr head)))
          (setcdr head tail)))
      (cons lst (1+ pos)))))

(defun nelisp-reader--read-vector (str pos)
  "Read the tail of a vector in STR.  POS is the index just past `['.
Return (VECTOR . NEW-POS) where NEW-POS is past the closing `]'.
Dotted-pair syntax is rejected inside vectors per Emacs semantics."
  (let ((len (length str))
        (acc nil))
    (catch 'done
      (while t
        (setq pos (nelisp-reader--skip-ws str pos))
        (when (>= pos len)
          (signal 'nelisp-reader-error (list "unterminated vector")))
        (let ((c (aref str pos)))
          (cond
           ((eq c ?\])
            (throw 'done nil))
           ((nelisp-reader--dotted-pair-marker-p str pos len)
            (signal 'nelisp-reader-error
                    (list "dot not allowed in vector" pos)))
           (t
            (let ((res (nelisp-reader--read-sexp str pos)))
              (push (car res) acc)
              (setq pos (cdr res))))))))
    (cons (apply #'vector (nreverse acc)) (1+ pos))))

(defun nelisp-reader--read-paren-vector (str pos)
  "Read a `#(...)' vector body in STR.  POS is just past the `('.
Return (VECTOR . NEW-POS) where NEW-POS is past the closing `)'.
Identical to `--read-vector' apart from the close-paren shape."
  (let ((len (length str))
        (acc nil))
    (catch 'done
      (while t
        (setq pos (nelisp-reader--skip-ws str pos))
        (when (>= pos len)
          (signal 'nelisp-reader-error (list "unterminated #(vector)")))
        (let ((c (aref str pos)))
          (cond
           ((eq c ?\))
            (throw 'done nil))
           ((nelisp-reader--dotted-pair-marker-p str pos len)
            (signal 'nelisp-reader-error
                    (list "dot not allowed in #(vector)" pos)))
           (t
            (let ((res (nelisp-reader--read-sexp str pos)))
              (push (car res) acc)
              (setq pos (cdr res))))))))
    (cons (apply #'vector (nreverse acc)) (1+ pos))))

(defun nelisp-reader--read-radix-int (str pos len radix charset)
  "Read a radix-prefixed integer in STR starting at POS.
LEN is (length STR).  RADIX is 2 / 8 / 16.  CHARSET is the regexp
character class fragment matching the digits of that radix.  Accepts
an optional sign prefix.  Returns (INTEGER . NEW-POS).  Signals on
empty payload or invalid digits."
  (let ((sign 1)
        (start pos))
    (when (< pos len)
      (let ((c (aref str pos)))
        (cond
         ((eq c ?-) (setq sign -1) (setq pos (1+ pos)))
         ((eq c ?+)               (setq pos (1+ pos))))))
    (let* ((digit-start pos)
           (end (nelisp-reader--atom-end str pos))
           (digits (substring str digit-start end)))
      (when (string-empty-p digits)
        (signal 'nelisp-reader-error
                (list "empty radix literal" radix start)))
      (unless (string-match-p (concat "\\`[" charset "]+\\'") digits)
        (signal 'nelisp-reader-error
                (list "invalid digit in radix literal"
                      radix digits start)))
      (cons (* sign (string-to-number digits radix)) end))))

(defun nelisp-reader--read-record (str pos len)
  "Read a `#s(NAME slot val ...)' record literal in STR at POS.
POS is the index just past `#s'.  Return (RECORD . NEW-POS).

Two output shapes are supported (Emacs cl-defstruct compatible):
- bare  `#s(NAME)' / `#s(NAME slot val ...)' — built into a real
  struct via `record' (Emacs 26+ primitive).  Slot order is the
  reader order; the evaluator can rebuild a typed value later.

This MVP does not consult any cl-defstruct registry — that lookup is
deferred to Phase 7+B once `cl-lib' is loaded inside NeLisp."
  (when (or (>= pos len) (not (eq (aref str pos) ?\()))
    (signal 'nelisp-reader-error
            (list "expected `(' after `#s'" pos)))
  (let* ((res (nelisp-reader--read-list str (1+ pos)))
         (lst (car res)))
    (when (null lst)
      (signal 'nelisp-reader-error
              (list "empty record literal" pos)))
    (let ((name (car lst))
          (slots (cdr lst)))
      (unless (symbolp name)
        (signal 'nelisp-reader-error
                (list "record name must be symbol" name pos)))
      (cons (apply #'record name slots) (cdr res)))))

(defun nelisp-reader--read-hash (str pos len)
  "Dispatch sharp-prefixed reader syntax in STR (POS = one past `#').
Phase 7+A handles:

  #\\='FORM      function-quote, expanded to (function FORM)
  #x.. #X..     hex integer literal with optional sign
  #o.. #O..     octal integer literal
  #b.. #B..     binary integer literal
  #s(NAME ...)  record literal (built via `record')
  #(...)        vector literal alias (some Elisp dialects)
  #|...|#       block comment is handled in `--skip-ws', never here

Anything else surfaces as a clean `nelisp-reader-error' so a stray
`#?' fails fast instead of silently miscompiling."
  (when (>= pos len)
    (signal 'nelisp-reader-error (list "lone `#' at EOF" pos)))
  (let ((c (aref str pos)))
    (cond
     ((eq c ?\')
      (let* ((after (nelisp-reader--skip-ws str (1+ pos)))
             (res (nelisp-reader--read-sexp str after)))
        (cons (list 'function (car res)) (cdr res))))
     ((memq c '(?x ?X))
      (nelisp-reader--read-radix-int str (1+ pos) len 16 "0-9a-fA-F"))
     ((memq c '(?o ?O))
      (nelisp-reader--read-radix-int str (1+ pos) len  8 "0-7"))
     ((memq c '(?b ?B))
      (nelisp-reader--read-radix-int str (1+ pos) len  2 "01"))
     ((eq c ?s)
      (nelisp-reader--read-record str (1+ pos) len))
     ((eq c ?\()
      ;; #(...) vector alias — closes with `)' rather than `]'.  This
      ;; departs from Emacs' propertized-string semantics intentionally
      ;; (the spec for Phase 7+A treats `#(...)' as a plain vector
      ;; alias; propertized strings are out of scope).
      (nelisp-reader--read-paren-vector str (1+ pos)))
     (t
      (signal 'nelisp-reader-error
              (list "unsupported `#' syntax" c pos))))))

(defun nelisp-reader--read-char-literal (str pos len)
  "Read the char literal payload in STR at POS (one past `?').
Return (INTEGER . NEW-POS)."
  (when (>= pos len)
    (signal 'nelisp-reader-error (list "char literal at EOF" pos)))
  (let ((c (aref str pos)))
    (cond
     ((eq c ?\\)
      (when (>= (1+ pos) len)
        (signal 'nelisp-reader-error
                (list "unterminated char escape" pos)))
      (let ((res (nelisp-reader--read-string-escape str (1+ pos) len)))
        (when (null (car res))
          (signal 'nelisp-reader-error
                  (list "char literal cannot use line continuation" pos)))
        res))
     (t
      (cons c (1+ pos))))))

(defun nelisp-reader--read-backquote (str pos)
  "Read a backquote (STR at POS = one past the backtick).
Outermost backquote returns (EXPANDED-FORM . NEW-POS) where the
expansion is cons / append construction code suitable for direct
eval.  Nested backquotes wrap their body in a literal data marker so
the surrounding expansion can rebuild the source structure at run
time."
  (let* ((nelisp-reader--bq-depth (1+ nelisp-reader--bq-depth))
         (outermost (= nelisp-reader--bq-depth 1))
         (after (nelisp-reader--skip-ws str pos))
         (res (nelisp-reader--read-sexp str after))
         (form (car res)))
    (cons (if outermost
              (nelisp-reader--bq-expand form)
            (list 'bq-quasi form))
          (cdr res))))

(defun nelisp-reader--read-comma (str pos len)
  "Read an unquote `,' or splice `,@' (STR at POS = one past `,').
Signals when used outside a backquote."
  (when (zerop nelisp-reader--bq-depth)
    (signal 'nelisp-reader-error
            (list "comma outside backquote" pos)))
  (let* ((is-splice (and (< pos len) (eq (aref str pos) ?@)))
         (after-marker (if is-splice (1+ pos) pos))
         (after-ws (nelisp-reader--skip-ws str after-marker))
         (nelisp-reader--bq-depth (1- nelisp-reader--bq-depth))
         (res (nelisp-reader--read-sexp str after-ws)))
    (cons (list (if is-splice 'bq-splice 'bq-unquote) (car res))
          (cdr res))))

(defun nelisp-reader--bq-expand (form &optional depth)
  "Expand FORM, the body of a backquote, into list-constructor code.
DEPTH defaults to 1.  At depth 1 atoms quote, `(bq-unquote X)' unwraps
to X, `(bq-splice X)' is illegal outside a list.  At depth >= 2 every
marker is preserved as literal data so reading + printing round-trip."
  (let ((depth (or depth 1)))
    (cond
     ((atom form) (list 'quote form))
     ((eq (car form) 'bq-quasi)
      (list 'list
            (list 'quote nelisp-reader--bq-quasi-symbol)
            (nelisp-reader--bq-expand (cadr form) (1+ depth))))
     ((eq (car form) 'bq-unquote)
      (if (= depth 1)
          (cadr form)
        (list 'list
              (list 'quote nelisp-reader--bq-comma-symbol)
              (nelisp-reader--bq-expand (cadr form) depth))))
     ((eq (car form) 'bq-splice)
      (if (= depth 1)
          (signal 'nelisp-reader-error
                  (list "splice outside list" form))
        (list 'list
              (list 'quote nelisp-reader--bq-splice-symbol)
              (nelisp-reader--bq-expand (cadr form) depth))))
     (t (nelisp-reader--bq-expand-list form depth)))))

(defun nelisp-reader--bq-expand-list (lst &optional depth)
  "Expand a list LST under backquote at DEPTH (default 1).
At depth 1 contiguous `(bq-splice X)' elements turn into `append'
fragments; otherwise everything else is preserved cons by cons."
  (let ((depth (or depth 1)))
    (cond
     ((null lst) nil)
     ((atom lst) (list 'quote lst))
     ((and (= depth 1)
           (consp (car lst))
           (eq (caar lst) 'bq-splice))
      (let ((splice (cadr (car lst)))
            (rest (nelisp-reader--bq-expand-list (cdr lst) depth)))
        (if (null rest)
            splice
          (list 'append splice rest))))
     (t
      (let ((head (nelisp-reader--bq-expand (car lst) depth))
            (tail (nelisp-reader--bq-expand-list (cdr lst) depth)))
        (list 'cons head tail))))))

(defun nelisp-reader--read-sexp (str pos)
  "Read one s-expression in STR at POS.
POS must already point at a non-whitespace character.  Return
\(VALUE . NEW-POS)."
  (let ((len (length str)))
    (when (>= pos len)
      (signal 'nelisp-reader-error (list "unexpected end of input")))
    (let ((c (aref str pos)))
      (cond
       ((eq c ?\()
        (nelisp-reader--read-list str (1+ pos)))
       ((eq c ?\")
        (nelisp-reader--read-string str pos))
       ((eq c ?\')
        (let* ((after (nelisp-reader--skip-ws str (1+ pos)))
               (res (nelisp-reader--read-sexp str after)))
          (cons (list 'quote (car res)) (cdr res))))
       ((eq c ?#)
        (nelisp-reader--read-hash str (1+ pos) len))
       ((eq c ?\`)
        (nelisp-reader--read-backquote str (1+ pos)))
       ((eq c ?\,)
        (nelisp-reader--read-comma str (1+ pos) len))
       ((eq c ??)
        (nelisp-reader--read-char-literal str (1+ pos) len))
       ((eq c ?\[)
        (nelisp-reader--read-vector str (1+ pos)))
       ((eq c ?\))
        (signal 'nelisp-reader-error (list "unexpected `)'" pos)))
       ((eq c ?\])
        (signal 'nelisp-reader-error (list "unexpected `]'" pos)))
       (t
        (nelisp-reader--read-atom str pos))))))

;;; Public API ---------------------------------------------------------

;;;###autoload
(defun nelisp-reader-read (stream)
  "Read one form from STREAM and return it.
STREAM must be a string for Phase 7+A; buffer / function backends
will be added by Phase 7+B without changing this signature.

Signals `nelisp-reader-error' on malformed input or trailing
non-whitespace data, `wrong-type-argument' when STREAM is not a
string."
  (unless (stringp stream)
    (signal 'wrong-type-argument (list 'stringp stream)))
  (let* ((start (nelisp-reader--skip-ws stream 0))
         (res (nelisp-reader--read-sexp stream start))
         (after (nelisp-reader--skip-ws stream (cdr res))))
    (when (< after (length stream))
      (signal 'nelisp-reader-error
              (list "trailing input after sexp" after)))
    (car res)))

;;;###autoload
(defun nelisp-reader-read-from-string (string &optional start end)
  "Read one form from STRING in the START..END range.
Returns (FORM . NEXT-POS), Emacs `read-from-string' compatible.
START defaults to 0; END defaults to (length STRING)."
  (unless (stringp string)
    (signal 'wrong-type-argument (list 'stringp string)))
  (let* ((start (or start 0))
         (end (or end (length string)))
         (slice (if (and (zerop start) (= end (length string)))
                    string
                  (substring string start end)))
         (res (nelisp-reader-read-from-string-with-position slice 0)))
    (cons (car res) (+ start (cdr res)))))

;;;###autoload
(defun nelisp-reader-read-from-string-with-position (string pos)
  "Read one form from STRING starting at POS.
Returns (FORM . NEXT-POS) where NEXT-POS sits one past the parsed
form so callers can pump multiple forms through the same buffer.
Trailing whitespace / comments are not an error here — that is
`nelisp-reader-read''s job."
  (unless (stringp string)
    (signal 'wrong-type-argument (list 'stringp string)))
  (unless (and (integerp pos) (>= pos 0))
    (signal 'wrong-type-argument (list 'natnump pos)))
  (let ((skipped (nelisp-reader--skip-ws string pos)))
    (nelisp-reader--read-sexp string skipped)))

;;;###autoload
(defun nelisp-reader-read-all (string)
  "Read every top-level form in STRING and return them as a list.
Whitespace, line comments, and block comments between forms are
skipped.  Signals `nelisp-reader-error' on malformed input."
  (unless (stringp string)
    (signal 'wrong-type-argument (list 'stringp string)))
  (let ((pos 0)
        (len (length string))
        (forms nil))
    (while (progn
             (setq pos (nelisp-reader--skip-ws string pos))
             (< pos len))
      (let ((res (nelisp-reader--read-sexp string pos)))
        (push (car res) forms)
        (setq pos (cdr res))))
    (nreverse forms)))

(provide 'nelisp-reader)

;;; nelisp-reader.el ends here
