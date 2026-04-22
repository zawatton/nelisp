;;; nelisp.el --- Self-hosted Emacs Lisp VM in pure Elisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; Author: zawatton <kurozawawo@gmail.com>
;; Version: 0.0.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: lisp, languages
;; URL: https://github.com/zawatton/nelisp

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

;; NeLisp is a research project to implement an Emacs Lisp VM in pure
;; Emacs Lisp, following the SBCL construction (Rhodes 2008) applied to
;; Emacs Lisp.  This file is the package entry point; actual interpreter,
;; reader, and bytecode modules arrive in Phase 1 Week 3+.
;;
;; See docs/05-roadmap.org for the phase plan and docs/03-architecture.org
;; for the locked Phase 1 Elisp subset.

;;; Code:

(require 'nelisp-read)
(require 'nelisp-eval)
(require 'nelisp-macro)
(require 'nelisp-load)

;; Now that `defmacro' dispatch is wired (nelisp-macro.el) install the
;; NeLisp-native macros that depend on it — `dolist' / `push' / etc.
(nelisp--install-core-macros)

(defconst nelisp-version "0.0.0"
  "Current version of NeLisp.
Phase 1 complete (reader + eval + macro + dynamic + cond + stdlib);
Phase 2 multi-form file loader + reader extensions + core macros are in.")

(defgroup nelisp nil
  "Self-hosted Emacs Lisp VM in pure Elisp."
  :group 'lisp
  :prefix "nelisp-")

(provide 'nelisp)

;;; nelisp.el ends here
