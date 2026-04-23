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

(defun nelisp-bootstrap-shared-tables ()
  "Seed NeLisp globals with the host evaluator's own hash tables.
This lets NeLisp code installed into the NeLisp environment read and
write the same `nelisp--functions' / `nelisp--globals' /
`nelisp--specials' / `nelisp--macros' tables that the host evaluator
uses — a prerequisite for cycle-1 = cycle-2 fixpoint.  Without it,
a NeLisp `defvar nelisp--functions (make-hash-table ...)' produces a
fresh NeLisp-side table that shares no entries with the host, so code
installed by cycle-1 (into the host) is invisible to cycle-2 (which
consults the NeLisp-side fresh table).

The shared-state approach is a Phase 2 expedient.  A proper Rhodes
5-stage genesis (=docs/03-architecture.org= §1) will eventually replace
this with a cold-image build that hands the genesis host's tables
directly to the cross-compiled NeLisp runtime."
  (dolist (sym '(nelisp--functions nelisp--globals nelisp--specials
                                   nelisp--macros nelisp--unbound))
    (puthash sym t nelisp--specials)
    (puthash sym (symbol-value sym) nelisp--globals)))

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
