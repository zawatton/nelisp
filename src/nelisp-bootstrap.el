;;; nelisp-bootstrap.el --- Convenience self-host bootstrap entry point  -*- lexical-binding: t; -*-

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

;;; Commentary:

;; Convenience wrapper around the NeLisp self-host bootstrap so external
;; callers (= scripts, demos, docs) can do
;;
;;   (require 'nelisp-bootstrap)
;;   (nelisp-bootstrap-init)
;;   (nelisp-eval '(funcall (lambda (n) (* n n)) 7))   ; => 49
;;
;; without manually replicating the dependency-ordered file load + the
;; deep-stack wrapper that `test/nelisp-self-host-test.el' uses.
;;
;; The bootstrap loads the same five core source files that the
;; self-host probe loads (= reader / eval / macro / load / nelisp.el)
;; through the NeLisp evaluator itself.  After init, the NeLisp
;; environment carries closure copies of `lambda', `funcall', `mapcar'
;; etc., and the host-side `nelisp-eval' delegates to those closures.
;;
;; This module is a pure orchestration layer.  It does NOT modify
;; `nelisp-eval' / `nelisp-read'.  Loading the module without calling
;; `nelisp-bootstrap-init' is a no-op — the predicate
;; `nelisp-bootstrap-bootstrapped-p' returns nil and the module stays
;; quiet.  Init is idempotent: the second call short-circuits unless
;; FORCE is non-nil.

;;; Code:

(require 'nelisp)

(defconst nelisp-bootstrap--source-dir
  (expand-file-name
   "."
   (file-name-directory (or load-file-name buffer-file-name)))
  "Absolute path of the NeLisp `src/' directory containing the bootstrap files.")

(defconst nelisp-bootstrap--files
  '("nelisp-read.el"
    "nelisp-eval.el"
    "nelisp-macro.el"
    "nelisp-load.el"
    "nelisp.el")
  "NeLisp source files to load through the NeLisp evaluator, in dependency order.

Mirrors `nelisp-self-host-test--files' from `test/nelisp-self-host-test.el'
— kept as an independent constant so this module does not depend on the
test tree.  Should the self-host file list ever grow, update both lists.")

(defvar nelisp-bootstrap--bootstrapped nil
  "Non-nil once `nelisp-bootstrap-init' has finished a successful load.
Reset to nil whenever `nelisp--reset' is called from this module.")

(defconst nelisp-bootstrap--evaluator-helpers
  '(nelisp-eval nelisp-eval-form
                nelisp--apply nelisp--apply-closure
                nelisp--eval-call nelisp--eval-body
                nelisp--bind-params nelisp--lookup
                nelisp--function-of nelisp--make-closure
                nelisp--eval-function
                ;; Higher-order primitives: their bodies call
                ;; `nelisp--apply' at NeLisp level (= broken once the
                ;; helpers above are pruned).  Pruning these forces
                ;; lookup to fall back on the host #\\='-quoted
                ;; primitives re-installed by `nelisp--install-primitives'
                ;; in `nelisp-bootstrap--prune-evaluator-helpers'.
                nelisp--builtin-funcall nelisp--builtin-apply
                nelisp--builtin-mapcar nelisp--builtin-mapc
                nelisp--builtin-mapconcat nelisp--builtin-maphash)
  "NeLisp evaluator helpers whose self-host closure copies must be pruned
post-bootstrap so host-side `nelisp-eval' dispatch keeps using the
host primitives rather than recursing into NeLisp closures (= the
cycle-1=cycle-2 mutual-recursion deadend).

After the self-host walk installs every defun in these files as a
NeLisp closure, the evaluator helpers themselves end up duplicated:
the host functions are still defined (= we required them at load
time) AND the same names exist as installed closures.  Host
`nelisp--eval-call' resolves callable symbols through
`nelisp--function-of', which prefers the closure copy, dispatch flips
into the NeLisp evaluator, and trivial forms like `(funcall (lambda
\\='(n) \\='(+ n 100)) 42)' fault with `nelisp-void-function' deep
inside the recursion.

User-level defuns (= the caller's actual application code) stay
installed; only the evaluator's own internals are pruned.")

(defun nelisp-bootstrap--prune-evaluator-helpers ()
  "Remove `nelisp-bootstrap--evaluator-helpers' from `nelisp--functions'.
Lets host-side dispatch fall through to the host primitive bindings.

Also re-runs `nelisp--install-primitives' so callable primitives (=
`funcall' / `mapcar' / `apply' etc.) point at host #\\='-quoted
functions rather than the NeLisp-installed closure copies — closure
copies of these primitives reference the now-pruned evaluator helpers
and would fault with `nelisp-void-function' on the first call.

User-level `defun' installations are untouched."
  (dolist (sym nelisp-bootstrap--evaluator-helpers)
    (remhash sym nelisp--functions))
  (nelisp--install-primitives))

(defun nelisp-bootstrap--eval-file (path)
  "Read every form in PATH and evaluate it through `nelisp-eval'.
Return the count of forms processed.

Mirrors `nelisp-self-host-test--eval-file' (= same `excessive-lisp-nesting'
catch + `in-form' error wrap) but is duplicated here so this module can
be required without pulling the test tree in."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents path))
    (let ((forms (nelisp-read-all (buffer-string)))
          (count 0))
      (dolist (form forms)
        (setq count (1+ count))
        (condition-case err
            (nelisp-eval form)
          (excessive-lisp-nesting nil)
          (error
           (signal (car err) (cons (list 'in-form (car-safe form))
                                   (cdr err))))))
      count)))

;;;###autoload
(defun nelisp-bootstrap-bootstrapped-p ()
  "Return non-nil when the NeLisp self-host bootstrap has been loaded."
  nelisp-bootstrap--bootstrapped)

;;;###autoload
(defun nelisp-bootstrap-init (&optional force)
  "Bootstrap the NeLisp self-host into the current Emacs session.

Idempotent: returns t immediately when the bootstrap has already
finished (= `nelisp-bootstrap-bootstrapped-p' is non-nil) unless FORCE
is non-nil.

Internally:
  1. Raises `max-lisp-eval-depth' / `max-specpdl-size' to 13000 to clear
     the host stack budget the self-host walk consumes.
  2. Calls `nelisp--reset' so the load starts from a clean NeLisp env.
  3. Reads each file in `nelisp-bootstrap--files' through the NeLisp
     reader and evaluates every top-level form through `nelisp-eval'.
  4. Prunes the NeLisp closure copies of the evaluator helpers (=
     `nelisp-bootstrap--evaluator-helpers') so host-side dispatch
     keeps using the host primitives.  Without this prune, host
     `nelisp--eval-call' would resolve `funcall' / `mapcar' through
     the installed closures and recurse into the NeLisp evaluator,
     which faults with `nelisp-void-function' on legitimate forms.
  5. Records success in `nelisp-bootstrap--bootstrapped' and returns t.

Signals on read / eval failure (caller's stack frame is preserved by
`nelisp-bootstrap--eval-file' wrapping the error in an `in-form' tag)."
  (if (and nelisp-bootstrap--bootstrapped (not force))
      t
    (setq nelisp-bootstrap--bootstrapped nil)
    ;; `max-specpdl-size' is obsolete in Emacs 29+ but still honored
    ;; when bound — raises the same ceiling without warning at run
    ;; time.  with-suppressed-warnings keeps byte-compile quiet under
    ;; `byte-compile-error-on-warn'.
    (with-suppressed-warnings ((obsolete max-specpdl-size))
      (let ((max-lisp-eval-depth (max 13000 max-lisp-eval-depth))
            (max-specpdl-size (max 13000 max-specpdl-size)))
        (nelisp--reset)
        (dolist (rel nelisp-bootstrap--files)
          (let ((path (expand-file-name rel nelisp-bootstrap--source-dir)))
            (unless (file-readable-p path)
              (error "nelisp-bootstrap: source file missing: %s" path))
            (nelisp-bootstrap--eval-file path)))
        (nelisp-bootstrap--prune-evaluator-helpers)
        (setq nelisp-bootstrap--bootstrapped t)
        t))))

;;;###autoload
(defun nelisp-bootstrap-eval (form)
  "Evaluate FORM through NeLisp, ensuring the bootstrap has been loaded.
Convenience wrapper: calls `nelisp-bootstrap-init' if needed, then
delegates to `nelisp-eval'.  Bootstrap also raises the host stack
budget while evaluating so deep self-host calls (= recursion through
installed closures like `mapcar') do not depth-trap on a fresh Emacs."
  (unless nelisp-bootstrap--bootstrapped
    (nelisp-bootstrap-init))
  (with-suppressed-warnings ((obsolete max-specpdl-size))
    (let ((max-lisp-eval-depth (max 13000 max-lisp-eval-depth))
          (max-specpdl-size (max 13000 max-specpdl-size)))
      (nelisp-eval form))))

(provide 'nelisp-bootstrap)

;;; nelisp-bootstrap.el ends here
