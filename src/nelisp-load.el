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

;;;###autoload
(defun nelisp-load-string (str)
  "Read every sexp in STR and evaluate them in order.
Return the value of the last form, or nil if STR contained none.
Defuns / defvars / defmacros installed during loading persist in
the global NeLisp tables exactly as if the user had typed each
form interactively."
  (let ((last nil))
    (dolist (form (nelisp-read-all str))
      (setq last (nelisp-eval form)))
    last))

;;;###autoload
(defun nelisp-load-file (path)
  "Read the file at PATH and evaluate every top-level sexp in order.
Return the value of the last form.  PATH must already exist; no
load-path search is performed in Phase 2."
  (unless (file-readable-p path)
    (signal 'file-error (list "Cannot read NeLisp file" path)))
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents path))
    (nelisp-load-string (buffer-string))))

(provide 'nelisp-load)

;;; nelisp-load.el ends here
