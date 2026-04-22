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
;; Still deferred:
;;
;;   - bignums, hex / oct / bin integer prefixes (#x10 etc.)
;;   - vectors, hash tables, records, bool-vector syntax
;;   - nested backquote (depth > 1)
;;   - circular / shared structure syntax (#N=, #N#)
;;   - character escapes like ?\\C-a / ?\\M-b / hex char escapes
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
`,' and `,@' are only legal when this is non-zero.  Phase 2 supports
exactly one level; deeper nesting is out of scope (keeps reader /
macro expansion semantics honest without introducing the full
qbq-comma dance).")

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
            (let* ((e (aref str (1+ pos)))
                   (decoded
                    (pcase e
                      (?\\ ?\\)
                      (?\" ?\")
                      (?n  ?\n)
                      (?t  ?\t)
                      (?r  ?\r)
                      (?f  ?\f)
                      (?a  ?\a)
                      (?b  ?\b)
                      (?\n nil)         ; line continuation: drop both bytes
                      (_ (signal 'nelisp-read-error
                                 (list "unknown string escape" e))))))
              (when decoded (push decoded chars))
              (setq pos (+ pos 2))))
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

(defun nelisp-read--hash (str pos len)
  "Dispatch sharp-prefixed reader syntax in STR (POS = one past `#').
Phase 2 supports `#''FORM (function-quote, expanded to (function FORM)).
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
     (t
      (signal 'nelisp-read-error
              (list "unsupported `#' syntax" c pos))))))

(defun nelisp-read--char-literal (str pos len)
  "Read the char literal payload in STR at POS (one past `?').
Return (INTEGER . NEW-POS).  Supports a short set of backslash
escapes plus any single literal character."
  (when (>= pos len)
    (signal 'nelisp-read-error (list "char literal at EOF" pos)))
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

(defun nelisp-read--backquote (str pos)
  "Read a backquote (STR at POS = one past the backtick).
Returns (EXPANDED . NEW-POS) — the expansion is already cons / append
code, ready to eval.  Increments `nelisp-read--bq-depth' during the
recursive read so inner `,' / `,@' are accepted."
  (let* ((nelisp-read--bq-depth (1+ nelisp-read--bq-depth))
         (after (nelisp-read--skip-ws str pos))
         (res (nelisp-read--sexp str after)))
    (cons (nelisp-read--bq-expand (car res)) (cdr res))))

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

(defun nelisp-read--bq-expand (form)
  "Expand FORM, the body of a backquote, into list-constructor code.
Atoms become `quote' forms, `(bq-unquote X)' unwraps to X, and
lists are built up via `cons' / `append'."
  (cond
   ((atom form) (list 'quote form))
   ((eq (car form) 'bq-unquote) (cadr form))
   ((eq (car form) 'bq-splice)
    (signal 'nelisp-read-error
            (list "splice outside list" form)))
   (t (nelisp-read--bq-expand-list form))))

(defun nelisp-read--bq-expand-list (lst)
  "Expand a list LST under backquote.
Contiguous `(bq-splice X)' elements turn into `append' fragments,
everything else becomes a `cons' chain."
  (cond
   ((null lst) nil)
   ((atom lst) (list 'quote lst))
   ((and (consp (car lst))
         (eq (caar lst) 'bq-splice))
    (let ((splice (cadr (car lst)))
          (rest (nelisp-read--bq-expand-list (cdr lst))))
      (if (null rest)
          splice
        (list 'append splice rest))))
   (t
    (let ((head (nelisp-read--bq-expand (car lst)))
          (tail (nelisp-read--bq-expand-list (cdr lst))))
      (list 'cons head tail)))))

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
       ((eq c ?\))
        (signal 'nelisp-read-error (list "unexpected `)'" pos)))
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
