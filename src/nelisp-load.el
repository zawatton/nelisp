;;; nelisp-load.el --- Multi-form NeLisp loader  -*- lexical-binding: t; -*-

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

;; Phase 2 Week 1-2 entry point: read multi-form NeLisp source from a
;; string or file and evaluate every top-level form against the same
;; global state (defun installs persist, defvar values persist, etc.).
;;
;; This is the substrate the rest of Phase 2 builds on.  Once the
;; reader handles backquote / char literals / floats and the evaluator
;; can host the helpers used inside `nelisp-eval.el' itself
;; (`make-hash-table' / `define-error' / `dolist' etc.), the same
;; loader will execute NeLisp's own implementation files unchanged —
;; the cycle 1 / cycle 2 fixpoint promised in 05-roadmap §2.1.
;;
;; The `.nl' extension is convention only; any text file containing
;; valid NeLisp sexps works.

;;; Code:

(require 'nelisp-read)
(require 'nelisp-eval)

(define-error 'nelisp-load-error "NeLisp load error")

(defun nelisp-load--pos-to-line-col (str pos)
  "Return (LINE . COLUMN) for POS in STR, 1-indexed.
Used by `nelisp-load-string' / `nelisp-load-file' to annotate
read / eval failures with source positions."
  (let ((line 1)
        (line-start 0)
        (i 0))
    (while (< i pos)
      (when (eq (aref str i) ?\n)
        (setq line (1+ line)
              line-start (1+ i)))
      (setq i (1+ i)))
    (cons line (1+ (- pos line-start)))))

(defun nelisp-load--signal (source pos str form-index phase cause)
  "Re-signal CAUSE as `nelisp-load-error' with position + phase info.
STR is the original source string, POS is where the failing form
starts inside STR, SOURCE is the file path (or nil when loading a
bare string), FORM-INDEX is the 0-based sexp position within the
load, PHASE is `read' or `eval'."
  (let ((lc (nelisp-load--pos-to-line-col str pos)))
    (signal 'nelisp-load-error
            (list :source source
                  :form-index form-index
                  :line (car lc)
                  :column (cdr lc)
                  :phase phase
                  :cause cause))))

;;;###autoload
(defun nelisp-load-string (str &optional source-file)
  "Read every sexp in STR and evaluate them in order.
Return the value of the last form, or nil if STR contained none.
Defuns / defvars / defmacros installed during loading persist in
the global NeLisp tables exactly as if the user had typed each
form interactively.

Errors during read or eval are re-signaled as `nelisp-load-error'
carrying a plist with keys :source, :form-index, :line, :column,
:phase (either `read' or `eval'), and :cause (the original signal
data).  Forms successfully evaluated before the failure keep their
side-effects — load is not transactional.  Optional SOURCE-FILE is
attached to the signal so callers like `nelisp-load-file' can tell
users where the failure lives."
  (unless (stringp str)
    (signal 'wrong-type-argument (list 'stringp str)))
  (let ((pos 0)
        (len (length str))
        (last nil)
        (form-index 0))
    (while (progn
             (setq pos (nelisp-read--skip-ws str pos))
             (< pos len))
      (let ((form-start pos)
            form)
        (condition-case err
            (let ((res (nelisp-read--sexp str pos)))
              (setq form (car res)
                    pos (cdr res)))
          (nelisp-read-error
           (nelisp-load--signal source-file form-start str
                                form-index 'read err)))
        (condition-case err
            (setq last (nelisp-eval form))
          (error
           (nelisp-load--signal source-file form-start str
                                form-index 'eval err)))
        (setq form-index (1+ form-index))))
    last))

;;;###autoload
(defun nelisp-load (str &optional source-file)
  "Doc 12 §2.1 B surface — thin wrapper around `nelisp-load-string'.
Kept as a `defun' rather than `defalias' so that NeLisp self-host
evaluation (which walks this file through its own evaluator) can
install this symbol without needing a `defalias' primitive.

See `nelisp-load-string' for the full contract; prefer
`nelisp-load-file' for disk sources and `nelisp-require' for
feature lookups."
  (nelisp-load-string str source-file))

;;;###autoload
(defun nelisp-load-file (path)
  "Read the file at PATH and evaluate every top-level sexp in order.
Return the value of the last form.  PATH must already exist; no
load-path search is performed at this layer (see `nelisp-require'
+ `nelisp-load-path' for feature resolution).

Read / eval failures are propagated as `nelisp-load-error' with
PATH recorded in the :source plist slot."
  (unless (file-readable-p path)
    (signal 'file-error (list "Cannot read NeLisp file" path)))
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents path))
    (nelisp-load-string (buffer-string) path)))

(provide 'nelisp-load)

;;; nelisp-load.el ends here
