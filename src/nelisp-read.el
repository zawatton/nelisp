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

;; Phase 1 Week 3-4 reader for a conservative Elisp subset.  Supported:
;;
;;   - atoms: nil, t, integer (decimal), symbol, string
;;   - cons via parens, improper list via dotted pair (a . b)
;;   - quote shorthand: 'x -> (quote x)
;;   - line comments (; to end of line)
;;
;; Deliberately out of scope (Phase 2+):
;;
;;   - floats, bignums, hex/oct/bin integer prefixes
;;   - vectors, hash tables, records, bool-vector syntax
;;   - backquote / unquote / splice
;;   - character literals (?a), string meta escapes beyond \\ \" \n \t \r
;;   - circular / shared structure syntax (#N=, #N#)
;;
;; The implementation is pure string-walking so it can port to NeLisp
;; itself in Phase 2 without relying on buffer primitives.

;;; Code:

(define-error 'nelisp-read-error "NeLisp reader error")

(defconst nelisp-read--atom-terminators
  '(?\s ?\t ?\n ?\r ?\f
    ?\( ?\) ?\[ ?\] ?\" ?\' ?\` ?\, ?\;)
  "Characters that end an atom token.")

(defconst nelisp-read--integer-regexp
  "\\`[-+]?[0-9]+\\'"
  "Phase 1 decimal integer token: no float, no base prefix.")

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
    (cons (if (string-match-p nelisp-read--integer-regexp tok)
              (string-to-number tok)
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
            (let ((e (aref str (1+ pos))))
              (push (pcase e
                      (?\\ ?\\)
                      (?\" ?\")
                      (?n  ?\n)
                      (?t  ?\t)
                      (?r  ?\r)
                      (_ (signal 'nelisp-read-error
                                 (list "unknown string escape" e))))
                    chars)
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
