;;; nelisp-read.el --- Lisp reader in pure Elisp  -*- lexical-binding: t; -*-

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

;; NeLisp reader.  Phase 1 covered atoms (nil / t / integer / symbol /
;; string), cons via parens, dotted pair, quote shorthand, and line
;; comments.  Phase 2 Week 3-6 broadens the surface enough to host
;; real Elisp macros and numeric code without backquote juggling:
;;
;;   - floats: 3.14 / .5 / 1. / -2.5 / 1e10 / 1.5e-3 parse via
;;     `string-to-number'; integer detection stays lossless
;;   - character literals: ?a (= 97) and common escapes
;;     (\\n \\t \\r \\f \\s \\\\ \\\" \\\' \\?)
;;   - backquote / unquote / splice: `x / ,x / ,@x — one nesting level,
;;     expanded at read time into cons / append calls so no macro
;;     installation is required before the reader works
;;
;; Phase 3a (2026-04-23) adds:
;;
;;   - hex / oct / bin integer prefixes: #xFF / #o17 / #b1010
;;     (upper-case variants and optional sign supported)
;;   - vector literal syntax: [] / [1 2 3] / nested
;;   - nested backquote: ` `(a ,b) works for depth >= 2
;;
;; Phase 3b (2026-05-06) adds:
;;
;;   - chord modifier escapes: ?\\C-a / ?\\M-x / ?\\S-A / ?\\A-? / ?\\H-/
;;     ?\\s- (super), with arbitrary nesting (?\\C-\\M-x).  Bits match
;;     Emacs's `event-modifiers' convention (meta=2^27, ctrl=2^26,
;;     shift=2^25, hyper=2^24, super=2^23, alt=2^22).  Control on ASCII
;;     letters / `?@' / `?\\s' / shift-row punctuation collapses to the
;;     canonical control code (?\\C-a = 1, ?\\C-? = 127, ?\\C-@ = 0).
;;
;; Still deferred:
;;
;;   - bignums, #NrXX radix syntax
;;   - hash tables, records, bool-vector syntax
;;   - circular / shared structure syntax (#N=, #N#)
;;   - hex char escapes (?\\xNN / ?\\uNNNN)
;;
;; The implementation is pure string-walking so it can port to NeLisp
;; itself in Phase 2 without relying on buffer primitives.

;;; Code:

(define-error 'nelisp-read-error "NeLisp reader error")

(defconst nelisp-read--atom-terminators
  '(?\s ?\t ?\n ?\r ?\f
    ?\( ?\) ?\[ ?\] ?\" ?\' ?\` ?\, ?\;)
  "Characters that end an atom token.")

(defconst nelisp-read--numeric-regexp
  "\\`[-+]?\\(?:[0-9]+\\(?:\\.[0-9]*\\)?\\|\\.[0-9]+\\)\\(?:[eE][-+]?[0-9]+\\)?\\'"
  "Decimal integer or float token.
Matches `42' / `-3' / `3.14' / `.5' / `1.' / `-2.5e3'.  Parsing is
handed to `string-to-number', which returns the correct type.")

(defvar nelisp-read--bq-depth 0
  "Tracks how deep we are inside nested backquotes.
`,' and `,@' are only legal when this is non-zero.  Phase 3a lifts
the depth=1 restriction so `` `(a `(b ,x)) '' parses; comma
propagation (`,,x', `,@,y') is still out of scope and would require
the full qbq-comma dance.")

;; Symbols used to mark nested backquote / comma / splice as literal
;; data when expanding depth >= 2 forms.  Interned via `intern' so
;; the source stays parseable by NeLisp's reader, which does not
;; support backslash-escaped symbol literals such as `\\=`' / `\\,'.
(defconst nelisp-read--bq-quasi-symbol (intern "`")
  "Symbol placed at the head of a nested-backquote cons at runtime.
Emacs' printer renders this symbol as `\\=`', giving the familiar
`\\=`(...)' output for data containing preserved nested backquotes.")
(defconst nelisp-read--bq-comma-symbol (intern ",")
  "Symbol used to preserve a depth>=2 `,X' marker as literal data.")
(defconst nelisp-read--bq-splice-symbol (intern ",@")
  "Symbol used to preserve a depth>=2 `,@X' marker as literal data.")

(defsubst nelisp-read--atom-char-p (c)
  "Non-nil if C can appear inside an atom token."
  (not (memq c nelisp-read--atom-terminators)))

(defun nelisp-read--skip-ws (str pos)
  "Skip whitespace and line comments in STR starting at POS.
Return the new position."
  (let ((len (length str))
        (done nil))
    (while (and (not done) (< pos len))
      (let ((c (aref str pos)))
        (cond
         ((memq c '(?\s ?\t ?\n ?\r ?\f))
          (setq pos (1+ pos)))
         ((eq c ?\;)
          (while (and (< pos len) (not (eq (aref str pos) ?\n)))
            (setq pos (1+ pos))))
         (t (setq done t)))))
    pos))

(defun nelisp-read--atom-end (str pos)
  "Return position one past the last atom char in STR from POS."
  (let ((len (length str)))
    (while (and (< pos len)
                (nelisp-read--atom-char-p (aref str pos)))
      (setq pos (1+ pos)))
    pos))

(defun nelisp-read--atom (str pos)
  "Read a number-or-symbol atom in STR at POS.
Return (VALUE . NEW-POS)."
  (let* ((end (nelisp-read--atom-end str pos))
         (tok (substring str pos end)))
    (when (string-empty-p tok)
      (signal 'nelisp-read-error
              (list "expected atom" pos)))
    (cons (if (string-match-p nelisp-read--numeric-regexp tok)
              (let ((n (string-to-number tok)))
                ;; `string-to-number' drops the fractional part from
                ;; tokens like "1." on some Emacs versions, yielding
                ;; an integer where the source clearly asked for a
                ;; float.  Coerce to float whenever the token itself
                ;; contains a decimal point or an exponent marker.
                (if (and (integerp n)
                         (string-match-p "[.eE]" tok))
                    (float n)
                  n))
            (intern tok))
          end)))

(defun nelisp-read--hex-digit-value (c)
  "Return the numeric value of hex digit C, or nil when C is not hex."
  (cond
   ((and (>= c ?0) (<= c ?9)) (- c ?0))
   ((and (>= c ?a) (<= c ?f)) (+ 10 (- c ?a)))
   ((and (>= c ?A) (<= c ?F)) (+ 10 (- c ?A)))
   (t nil)))

(defun nelisp-read--read-string-octal-escape (str pos len)
  "Read up to three octal digits from STR at POS.
Return (CHAR . NEW-POS)."
  (let ((end pos)
        (cap (min len (+ pos 3))))
    (while (and (< end cap)
                (let ((c (aref str end)))
                  (and (>= c ?0) (<= c ?7))))
      (setq end (1+ end)))
    (cons (string-to-number (substring str pos end) 8) end)))

(defun nelisp-read--read-string-unicode-escape (str pos len digits)
  "Read exactly DIGITS hex chars from STR at POS.
Return (CHAR . NEW-POS)."
  (when (< (- len pos) digits)
    (signal 'nelisp-read-error
            (list "short unicode escape" pos digits)))
  (let ((tok (substring str pos (+ pos digits))))
    (unless (string-match-p "\\`[0-9a-fA-F]+\\'" tok)
      (signal 'nelisp-read-error
              (list "non-hex unicode escape" tok pos)))
    (cons (string-to-number tok 16) (+ pos digits))))

(defun nelisp-read--read-string-hex-escape (str pos len)
  "Read a greedy `\\x' escape from STR at POS.
Return (CHAR . NEW-POS)."
  (let ((end pos))
    (while (and (< end len)
                (nelisp-read--hex-digit-value (aref str end)))
      (setq end (1+ end)))
    (when (= end pos)
      (signal 'nelisp-read-error
              (list "empty hex escape" pos)))
    (cons (string-to-number (substring str pos end) 16) end)))

(defun nelisp-read--string-ctrl-char (c)
  "Return the Emacs control-code collapse of C, or nil if invalid."
  (cond
   ((and (>= c ?A) (<= c ?Z)) (1+ (- c ?A)))
   ((and (>= c ?a) (<= c ?z)) (1+ (- c ?a)))
   ((eq c ??) 127)
   ((eq c ?@) 0)
   ((eq c ?\s) 0)
   ((and (>= c 91) (<= c 95)) (- c 64))
   (t nil)))

(defun nelisp-read--read-string-basic-escape (str pos len)
  "Decode one non-modifier string escape in STR at POS.
POS points at the byte after `\\'.  Return (CHAR-OR-NIL . NEW-POS)."
  (let ((e (aref str pos)))
    (cond
     ((eq e ?\\) (cons ?\\ (1+ pos)))
     ((eq e ?\") (cons ?\" (1+ pos)))
     ((eq e ?\') (cons ?\' (1+ pos)))
     ((eq e ?\?) (cons ?? (1+ pos)))
     ((eq e ?a)  (cons ?\a (1+ pos)))
     ((eq e ?b)  (cons ?\b (1+ pos)))
     ((eq e ?d)  (cons 127 (1+ pos)))
     ((eq e ?e)  (cons 27 (1+ pos)))
     ((eq e ?f)  (cons ?\f (1+ pos)))
     ((eq e ?n)  (cons ?\n (1+ pos)))
     ((eq e ?r)  (cons ?\r (1+ pos)))
     ((eq e ?s)  (cons ?\s (1+ pos)))
     ((eq e ?t)  (cons ?\t (1+ pos)))
     ((eq e ?v)  (cons 11 (1+ pos)))
     ((eq e ?\s) (cons nil (1+ pos)))
     ((eq e ?\n) (cons nil (1+ pos)))
     ((and (>= e ?0) (<= e ?7))
      (nelisp-read--read-string-octal-escape str pos len))
     ((eq e ?x)
      (nelisp-read--read-string-hex-escape str (1+ pos) len))
     ((eq e ?u)
      (nelisp-read--read-string-unicode-escape str (1+ pos) len 4))
     ((eq e ?U)
      (nelisp-read--read-string-unicode-escape str (1+ pos) len 8))
     (t (cons e (1+ pos))))))

(defun nelisp-read--read-string-modified-target (str pos len meta ctrl shift)
  "Read one string escape target from STR at POS, applying modifiers.
META, CTRL, and SHIFT are booleans tracking `\\M-', `\\C-'/`\\^',
and `\\S-' prefixes respectively.  Return (CHAR . NEW-POS)."
  (when (>= pos len)
    (signal 'nelisp-read-error
            (list "invalid string modifier" pos)))
  (when (and shift ctrl)
    (signal 'nelisp-read-error
            (list "invalid string modifier" pos)))
  (let ((res
         (if (eq (aref str pos) ?\\)
             (progn
               (when (>= (1+ pos) len)
                 (signal 'nelisp-read-error
                         (list "unterminated escape")))
               (let ((e (aref str (1+ pos))))
                 (cond
                  ((and (memq e '(?C ?M ?S))
                        (< (+ pos 2) len)
                        (eq (aref str (+ pos 2)) ?-))
                   (nelisp-read--read-string-modified-target
                    str (+ pos 3) len
                    (or meta (eq e ?M))
                    (or ctrl (eq e ?C))
                    (or shift (eq e ?S))))
                  ((eq e ?^)
                   (nelisp-read--read-string-modified-target
                    str (+ pos 2) len meta t shift))
                  (t
                   (nelisp-read--read-string-basic-escape
                    str (1+ pos) len)))))
           (cons (aref str pos) (1+ pos)))))
    (let ((code (car res)))
      (when shift
        (when (and (>= code ?a) (<= code ?z))
          (setq code (- code 32))))
      (when ctrl
        (setq code (nelisp-read--string-ctrl-char code))
        (unless code
          (signal 'nelisp-read-error
                  (list "invalid string modifier" pos))))
      (when meta
        (setq code (logior code 128)))
      (cons code (cdr res)))))

(defun nelisp-read--read-string-escape (str pos len)
  "Decode one Emacs-compatible string escape in STR at POS.
POS points at the byte after `\\'.  Return (CHAR-OR-NIL . NEW-POS)."
  (let ((e (aref str pos)))
    (cond
     ((and (memq e '(?C ?M ?S))
           (< (1+ pos) len)
           (eq (aref str (1+ pos)) ?-))
      (nelisp-read--read-string-modified-target
       str (+ pos 2) len (eq e ?M) (eq e ?C) (eq e ?S)))
     ((eq e ?^)
      (nelisp-read--read-string-modified-target
       str (1+ pos) len nil t nil))
     (t
      (nelisp-read--read-string-basic-escape str pos len)))))

(defun nelisp-read--string (str pos)
  "Read a string literal in STR at the opening quote at POS.
Return (VALUE . NEW-POS) where NEW-POS is past the closing quote."
  (let ((len (length str))
        (chars nil)
        (pos (1+ pos)))
    (catch 'done
      (while t
        (when (>= pos len)
          (signal 'nelisp-read-error (list "unterminated string")))
        (let ((c (aref str pos)))
          (cond
           ((eq c ?\")
            (throw 'done nil))
           ((eq c ?\\)
            (when (>= (1+ pos) len)
              (signal 'nelisp-read-error (list "unterminated escape")))
            (let ((res (nelisp-read--read-string-escape
                        str (1+ pos) len)))
              (when (car res)
                (push (car res) chars))
              (setq pos (cdr res))))
           (t
            (push c chars)
            (setq pos (1+ pos)))))))
    (cons (apply #'string (nreverse chars))
          (1+ pos))))

(defun nelisp-read--dotted-pair-marker-p (str pos len)
  "Non-nil if POS in STR is a standalone dotted-pair `.' marker.
A bare `.' followed by whitespace, `)' or EOF separates the last
element from the cdr of an improper list."
  (and (eq (aref str pos) ?.)
       (or (>= (1+ pos) len)
           (memq (aref str (1+ pos))
                 '(?\s ?\t ?\n ?\r ?\f ?\))))))

(defun nelisp-read--list (str pos)
  "Read the tail of a list in STR; POS is just past `('.
Return (VALUE . NEW-POS) where NEW-POS is past the closing `)'."
  (let ((len (length str))
        (acc nil)
        (tail nil)
        (have-tail nil))
    (catch 'done
      (while t
        (setq pos (nelisp-read--skip-ws str pos))
        (when (>= pos len)
          (signal 'nelisp-read-error (list "unterminated list")))
        (let ((c (aref str pos)))
          (cond
           ((eq c ?\))
            (throw 'done nil))
           ((nelisp-read--dotted-pair-marker-p str pos len)
            (unless acc
              (signal 'nelisp-read-error
                      (list "dot before first element in list")))
            (setq pos (nelisp-read--skip-ws str (1+ pos)))
            (let ((res (nelisp-read--sexp str pos)))
              (setq tail (car res))
              (setq have-tail t)
              (setq pos (cdr res)))
            (setq pos (nelisp-read--skip-ws str pos))
            (unless (and (< pos len) (eq (aref str pos) ?\)))
              (signal 'nelisp-read-error
                      (list "expected `)' after dotted tail" pos)))
            (throw 'done nil))
           (t
            (let ((res (nelisp-read--sexp str pos)))
              (push (car res) acc)
              (setq pos (cdr res))))))))
    (let ((lst (nreverse acc)))
      (when have-tail
        (let ((head lst))
          (while (cdr head) (setq head (cdr head)))
          (setcdr head tail)))
      (cons lst (1+ pos)))))

(defun nelisp-read--vector (str pos)
  "Read the tail of a vector in STR; POS is just past `['.
Return (VECTOR . NEW-POS) where NEW-POS is past the closing `]'.
Dotted-pair syntax is not allowed inside a vector."
  (let ((len (length str))
        (acc nil))
    (catch 'done
      (while t
        (setq pos (nelisp-read--skip-ws str pos))
        (when (>= pos len)
          (signal 'nelisp-read-error (list "unterminated vector")))
        (let ((c (aref str pos)))
          (cond
           ((eq c ?\])
            (throw 'done nil))
           ((nelisp-read--dotted-pair-marker-p str pos len)
            (signal 'nelisp-read-error
                    (list "dot not allowed in vector" pos)))
           (t
            (let ((res (nelisp-read--sexp str pos)))
              (push (car res) acc)
              (setq pos (cdr res))))))))
    (cons (apply #'vector (nreverse acc)) (1+ pos))))

(defun nelisp-read--radix-number (str pos len radix charset)
  "Read a radix-prefixed integer in STR starting at POS.
LEN is (length STR).  RADIX is 2 / 8 / 16.  CHARSET is a regexp
character class fragment (e.g. \"0-9a-fA-F\") matching digits of the
radix.  Accepts an optional sign prefix.  Returns (INTEGER . NEW-POS).
Signals `nelisp-read-error' on empty payload or invalid digits."
  (let ((sign 1)
        (start pos))
    (when (< pos len)
      (let ((c (aref str pos)))
        (cond
         ((eq c ?-) (setq sign -1) (setq pos (1+ pos)))
         ((eq c ?+)               (setq pos (1+ pos))))))
    (let* ((digit-start pos)
           (end (nelisp-read--atom-end str pos))
           (digits (substring str digit-start end)))
      (when (string-empty-p digits)
        (signal 'nelisp-read-error
                (list "empty radix literal" radix start)))
      (unless (string-match-p (concat "\\`[" charset "]+\\'") digits)
        (signal 'nelisp-read-error
                (list "invalid digit in radix literal"
                      radix digits start)))
      (cons (* sign (string-to-number digits radix)) end))))

(defun nelisp-read--hash (str pos len)
  "Dispatch sharp-prefixed reader syntax in STR (POS = one past `#').
Phase 2 supports `#''FORM (function-quote, expanded to (function FORM)).
Phase 3a adds `#x' / `#o' / `#b' (plus upper-case variants) for hex,
octal and binary integer literals with optional sign prefix.
Other `#'-prefixed syntax is rejected so it surfaces as a clean
reader error rather than a silent miscompilation."
  (when (>= pos len)
    (signal 'nelisp-read-error (list "lone `#' at EOF" pos)))
  (let ((c (aref str pos)))
    (cond
     ((eq c ?\')
      (let* ((after (nelisp-read--skip-ws str (1+ pos)))
             (res (nelisp-read--sexp str after)))
        (cons (list 'function (car res)) (cdr res))))
     ((memq c '(?x ?X))
      (nelisp-read--radix-number str (1+ pos) len 16 "0-9a-fA-F"))
     ((memq c '(?o ?O))
      (nelisp-read--radix-number str (1+ pos) len  8 "0-7"))
     ((memq c '(?b ?B))
      (nelisp-read--radix-number str (1+ pos) len  2 "01"))
     (t
      (signal 'nelisp-read-error
              (list "unsupported `#' syntax" c pos))))))

;; Chord-modifier bits per Emacs char modifier convention
;; (see lisp/keymap.el / lisp/subr.el / src/keyboard.c — these
;; values are the bit positions Emacs uses for `event-modifiers').
(defconst nelisp-read--meta-bit  134217728 "Meta modifier bit (2^27).")
(defconst nelisp-read--ctrl-bit   67108864 "Control modifier bit (2^26).")
(defconst nelisp-read--shift-bit  33554432 "Shift modifier bit (2^25).")
(defconst nelisp-read--hyper-bit  16777216 "Hyper modifier bit (2^24).")
(defconst nelisp-read--super-bit   8388608 "Super modifier bit (2^23).")
(defconst nelisp-read--alt-bit     4194304 "Alt modifier bit (2^22).")

(defun nelisp-read--apply-ctrl (c)
  "Apply the control modifier to C per Emacs char-literal convention.
ASCII letters and the shift-row punctuation collapse to their canonical
control codes (`?\\C-a' = 1, `?\\C-?' = 127); everything else gets the
generic `nelisp-read--ctrl-bit' OR'd in.  Multi-modifier chains can
still wrap this result with meta / shift / etc."
  (cond
   ((and (>= c ?A) (<= c ?Z)) (1+ (- c ?A)))
   ((and (>= c ?a) (<= c ?z)) (1+ (- c ?a)))
   ((eq c ??)  127)            ; ?\C-? = DEL
   ((eq c ?@)  0)              ; ?\C-@ = NUL
   ((eq c ?\s) 0)              ; ?\C-<space> = NUL
   ((and (>= c ?\[) (<= c ?_)) (- c (- ?\[ 27))) ; ?\C-[=27 .. ?\C-_=31
   (t (logior c nelisp-read--ctrl-bit))))

(defun nelisp-read--char-modifier-bits (e)
  "Return the modifier bit for backslash-letter E, or nil if E is not
a chord modifier letter (= one of C M S A H s)."
  (pcase e
    (?C nelisp-read--ctrl-bit)
    (?M nelisp-read--meta-bit)
    (?S nelisp-read--shift-bit)
    (?A nelisp-read--alt-bit)
    (?H nelisp-read--hyper-bit)
    (?s nelisp-read--super-bit)))

(defun nelisp-read--char-base (str pos len)
  "Read the *base* char of a char literal at POS (= no chord modifier
prefix on this segment).  Returns (INTEGER . NEW-POS).  Handles both
plain literal chars and the legacy backslash escape set."
  (when (>= pos len)
    (signal 'nelisp-read-error (list "char escape at EOF" pos)))
  (let ((c (aref str pos)))
    (cond
     ((eq c ?\\)
      (when (>= (1+ pos) len)
        (signal 'nelisp-read-error
                (list "unterminated char escape" pos)))
      (let ((e (aref str (1+ pos))))
        (cons (pcase e
                (?n  ?\n) (?t ?\t) (?r ?\r) (?f ?\f) (?a ?\a) (?b ?\b)
                (?e  27)  (?0 ?\0)
                (?s  ?\s) (?\s ?\s)
                (?\\ ?\\) (?\" ?\") (?\' ?\') (?\? ??)
                (_   e))
              (+ pos 2))))
     (t
      (cons c (1+ pos))))))

(defun nelisp-read--char-after-modifier (str pos len)
  "Read the char literal payload in STR at POS, including any leading
chord modifier escapes (`\\C-' / `\\M-' / `\\S-' / `\\A-' / `\\H-' /
`\\s-').  Returns (INTEGER . NEW-POS).

Modifiers are collected first, then the base char is read, and only
afterwards is the control modifier collapsed onto the base — matching
Emacs's reader so e.g. `?\\C-\\M-x' yields meta-bit OR ASCII-24, not
meta-bit OR ctrl-bit OR 120."
  (let ((have-ctrl nil)
        (mods 0))
    (catch 'collected
      (while t
        (when (>= pos len)
          (signal 'nelisp-read-error (list "char escape at EOF" pos)))
        (let ((c (aref str pos)))
          (cond
           ((and (eq c ?\\)
                 (< (1+ pos) len)
                 (let ((e (aref str (1+ pos))))
                   (and (nelisp-read--char-modifier-bits e)
                        (< (+ pos 2) len)
                        (eq (aref str (+ pos 2)) ?-))))
            (let* ((e   (aref str (1+ pos)))
                   (bit (nelisp-read--char-modifier-bits e)))
              (if (eq bit nelisp-read--ctrl-bit)
                  (setq have-ctrl t)
                (setq mods (logior mods bit)))
              (setq pos (+ pos 3))))
           (t
            (throw 'collected nil))))))
    (let* ((base (nelisp-read--char-base str pos len))
           (code (car base))
           (np   (cdr base)))
      (when have-ctrl
        (setq code (nelisp-read--apply-ctrl code)))
      (cons (logior code mods) np))))

(defun nelisp-read--char-literal (str pos len)
  "Read the char literal payload in STR at POS (one past `?').
Returns (INTEGER . NEW-POS).  Supports plain chars (incl. space-after-
`?'), the legacy backslash escape set, and chord modifier escapes
\\C-/\\M-/\\S-/\\A-/\\H-/\\s- with arbitrary nesting."
  (when (>= pos len)
    (signal 'nelisp-read-error (list "char literal at EOF" pos)))
  (nelisp-read--char-after-modifier str pos len))

(defun nelisp-read--backquote (str pos)
  "Read a backquote (STR at POS = one past the backtick).
The outermost backquote returns (EXPANDED . NEW-POS) where EXPANDED
is cons / append construction code ready to eval.  Inner (nested)
backquotes wrap their body in a `(bq-quasi FORM)' marker so the
surrounding expansion can rebuild the literal nested-backquote data
structure at runtime."
  (let* ((nelisp-read--bq-depth (1+ nelisp-read--bq-depth))
         (outermost (= nelisp-read--bq-depth 1))
         (after (nelisp-read--skip-ws str pos))
         (res (nelisp-read--sexp str after))
         (form (car res)))
    (cons (if outermost
              (nelisp-read--bq-expand form)
            (list 'bq-quasi form))
          (cdr res))))

(defun nelisp-read--unquote (str pos len)
  "Read an unquote or splice (STR at POS = one past the comma).
Signals when used outside a backquote.  Returns a marker pair
`(bq-unquote FORM)' or `(bq-splice FORM)' that the backquote
expander in `nelisp-read--bq-expand' consumes."
  (when (zerop nelisp-read--bq-depth)
    (signal 'nelisp-read-error
            (list "comma outside backquote" pos)))
  (let* ((is-splice (and (< pos len) (eq (aref str pos) ?@)))
         (after-marker (if is-splice (1+ pos) pos))
         (after-ws (nelisp-read--skip-ws str after-marker))
         (nelisp-read--bq-depth (1- nelisp-read--bq-depth))
         (res (nelisp-read--sexp str after-ws)))
    (cons (list (if is-splice 'bq-splice 'bq-unquote) (car res))
          (cdr res))))

(defun nelisp-read--bq-expand (form &optional depth)
  "Expand FORM, the body of a backquote, into list-constructor code.
DEPTH defaults to 1 (the outermost backquote).  Phase 3a adds a
depth parameter so nested backquote forms (`bq-quasi' markers) and
their inner commas are reconstructed as literal data at runtime via
the quasi / comma / splice symbols in `nelisp-read--bq-*-symbol'.

At depth 1: atoms quote, `(bq-unquote X)' unwraps to X, `(bq-splice X)'
outside a list signals.  At depth >= 2: everything is preserved as
literal data; commas/splices become cons cells headed by the preserved
`,' / `,@' symbols and nested backquotes deepen further."
  (let ((depth (or depth 1)))
    (cond
     ((atom form) (list 'quote form))
     ((eq (car form) 'bq-quasi)
      ;; Inner backquote: build (<bq-quasi-sym> INNER-BUILT) at runtime.
      (list 'list
            (list 'quote nelisp-read--bq-quasi-symbol)
            (nelisp-read--bq-expand (cadr form) (1+ depth))))
     ((eq (car form) 'bq-unquote)
      (if (= depth 1)
          (cadr form)
        (list 'list
              (list 'quote nelisp-read--bq-comma-symbol)
              (nelisp-read--bq-expand (cadr form) depth))))
     ((eq (car form) 'bq-splice)
      (if (= depth 1)
          (signal 'nelisp-read-error
                  (list "splice outside list" form))
        (list 'list
              (list 'quote nelisp-read--bq-splice-symbol)
              (nelisp-read--bq-expand (cadr form) depth))))
     (t (nelisp-read--bq-expand-list form depth)))))

(defun nelisp-read--bq-expand-list (lst &optional depth)
  "Expand a list LST under backquote at DEPTH (default 1).
At depth 1 contiguous `(bq-splice X)' elements turn into `append'
fragments.  At depth >= 2 splice markers are preserved as literal
`(\\,@ X)' cons cells built element-wise."
  (let ((depth (or depth 1)))
    (cond
     ((null lst) nil)
     ((atom lst) (list 'quote lst))
     ((and (= depth 1)
           (consp (car lst))
           (eq (caar lst) 'bq-splice))
      (let ((splice (cadr (car lst)))
            (rest (nelisp-read--bq-expand-list (cdr lst) depth)))
        (if (null rest)
            splice
          (list 'append splice rest))))
     (t
      (let ((head (nelisp-read--bq-expand (car lst) depth))
            (tail (nelisp-read--bq-expand-list (cdr lst) depth)))
        (list 'cons head tail))))))

(defun nelisp-read--sexp (str pos)
  "Read one sexp in STR at POS.
POS must already point at a non-whitespace character.
Return (VALUE . NEW-POS)."
  (let ((len (length str)))
    (when (>= pos len)
      (signal 'nelisp-read-error (list "unexpected end of input")))
    (let ((c (aref str pos)))
      (cond
       ((eq c ?\()
        (nelisp-read--list str (1+ pos)))
       ((eq c ?\")
        (nelisp-read--string str pos))
       ((eq c ?\')
        (let* ((after (nelisp-read--skip-ws str (1+ pos)))
               (res (nelisp-read--sexp str after)))
          (cons (list 'quote (car res)) (cdr res))))
       ((eq c ?#)
        (nelisp-read--hash str (1+ pos) len))
       ((eq c ?\`)
        (nelisp-read--backquote str (1+ pos)))
       ((eq c ?\,)
        (nelisp-read--unquote str (1+ pos) len))
       ((eq c ??)
        (nelisp-read--char-literal str (1+ pos) len))
       ((eq c ?\[)
        (nelisp-read--vector str (1+ pos)))
       ((eq c ?\))
        (signal 'nelisp-read-error (list "unexpected `)'" pos)))
       ((eq c ?\])
        (signal 'nelisp-read-error (list "unexpected `]'" pos)))
       (t
        (nelisp-read--atom str pos))))))

;;;###autoload
(defun nelisp-read (str)
  "Parse STR and return the first sexp.
Signal `nelisp-read-error' on malformed input or trailing non-whitespace."
  (unless (stringp str)
    (signal 'wrong-type-argument (list 'stringp str)))
  (let* ((start (nelisp-read--skip-ws str 0))
         (res (nelisp-read--sexp str start))
         (after (nelisp-read--skip-ws str (cdr res))))
    (when (< after (length str))
      (signal 'nelisp-read-error
              (list "trailing input after sexp" after)))
    (car res)))

;;;###autoload
(defun nelisp-read-from-string (str &optional start)
  "Parse one sexp in STR starting at optional START (default 0).
Return (VALUE . NEXT-POS) where NEXT-POS sits just past the sexp,
leaving any trailing text intact — unlike `nelisp-read', which
rejects trailing input."
  (unless (stringp str)
    (signal 'wrong-type-argument (list 'stringp str)))
  (let* ((pos (nelisp-read--skip-ws str (or start 0))))
    (nelisp-read--sexp str pos)))

;;;###autoload
(defun nelisp-read-all (str)
  "Parse STR and return a list of every top-level sexp it contains.
Whitespace and line comments between sexps are skipped.  The list
preserves source order.  Unlike `nelisp-read', trailing whitespace
after the final sexp is not an error."
  (unless (stringp str)
    (signal 'wrong-type-argument (list 'stringp str)))
  (let ((pos 0)
        (len (length str))
        (forms nil))
    (while (progn
             (setq pos (nelisp-read--skip-ws str pos))
             (< pos len))
      (let ((res (nelisp-read--sexp str pos)))
        (push (car res) forms)
        (setq pos (cdr res))))
    (nreverse forms)))

(provide 'nelisp-read)

;;; nelisp-read.el ends here
