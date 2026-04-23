;;; nelisp-gc.el --- Phase 3c GC / NeLisp object tracking  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 3c ships a NeLisp-level object tracker.  Host Emacs GC still
;; owns memory reclamation — this module answers "which NeLisp values
;; are reachable from NeLisp roots?" rather than freeing anything.
;;
;; Purpose (see docs/design/09-phase3c-gc.org):
;;   1. Phase 4 actor boundary reachability (who owns what?)
;;   2. Finalizer hooks for host-GC-invisible resources
;;   3. Heap introspection for diagnostics / MCP
;;
;; Phase 3c.1 scope: skeleton + root set enumeration only.  Mark pass,
;; finalizer registry, MCP tools and actor boundary API land in
;; 3c.2-5.  No public GC API yet except `nelisp-gc-root-set'.

;;; Code:

(require 'cl-lib)

(defvar nelisp-gc--active-vms nil
  "Stack of bytecode-VM state vectors currently executing.
`nelisp-bc-run' pushes the VM on entry and pops on exit via
`unwind-protect', so at GC scan time this holds only in-flight
stacks.  Leaf = outermost call; top-of-list = innermost.")

(defun nelisp-gc--globals-root ()
  "Return the four NeLisp-level global hash tables as a single plist root.
Each element of the returned list is `(:kind K :value HT)' so downstream
walkers treat each table as its own subtree without unpacking here."
  (let (out)
    (dolist (sym '(nelisp--globals nelisp--functions
                   nelisp--macros nelisp--specials))
      (when (boundp sym)
        (push (list :kind sym :value (symbol-value sym)) out)))
    (nreverse out)))

(defun nelisp-gc--vm-stacks-root ()
  "Return each live VM state vector as its own root entry.
Empty list when no bytecode VM is running.  Covers both classical
bcl-run and JIT-marker bcl, though JIT stacks never push onto
`nelisp-gc--active-vms' — the JIT fast-path skips VM setup entirely."
  (mapcar (lambda (vm) (list :kind 'vm-stack :value vm))
          nelisp-gc--active-vms))

(defun nelisp-gc-root-set ()
  "Return the current NeLisp root set as a list of `(:kind K :value V)' plists.
The set is recomputed on every call — call sites that scan repeatedly
during one mark pass should bind the result themselves.

Root categories (Phase 3c.1):
  `nelisp--globals'     — user-level defvar storage
  `nelisp--functions'   — defun / defalias cells
  `nelisp--macros'      — defmacro cells
  `nelisp--specials'    — dynamic-scope marker table
  `vm-stack'            — each active bytecode-VM state vector

specpdl is deliberately NOT a top-level root — it lives inside each
VM state vector (slot 8) and is already reached via `vm-stack'.  JIT
closure constants pools are host-Elisp-reachable; re-walking them
here would duplicate work the host GC already handles (design doc
§2.2 decision A)."
  (append (nelisp-gc--globals-root)
          (nelisp-gc--vm-stacks-root)))

(defmacro nelisp-gc--with-active-vm (vm &rest body)
  "Execute BODY with VM pushed onto `nelisp-gc--active-vms'.
Pops on both normal and non-local exit so the active-VM stack stays
consistent with actual dispatch depth.  Called by `nelisp-bc-run'
via bytecode.el to keep that file's external surface unchanged."
  (declare (indent 1))
  `(let ((nelisp-gc--active-vms (cons ,vm nelisp-gc--active-vms)))
     ,@body))

;;; Mark pass (Phase 3c.2) --------------------------------------------

(defun nelisp-gc--seed-work (root-override)
  "Return the initial work list for a mark pass.
Extract `:value' from each root plist (yielding the actual NeLisp
object) so the walker can treat roots and transitive edges uniformly
without carrying the plist shape through the loop."
  (let ((seeds (or root-override (nelisp-gc-root-set)))
        out)
    (dolist (r seeds)
      (push (plist-get r :value) out))
    out))

(defun nelisp-gc-reachable-set (&optional root-override)
  "Return a hash-table of `eq'-unique objects reachable from roots.

Walk is iterative (explicit work list) so deeply nested or cyclic
structures cannot trip `max-lisp-eval-depth'.  ROOT-OVERRIDE, if
given, replaces the default `nelisp-gc-root-set' as the seed; it
must share the plist shape `(:kind K :value V)' per root entry.

Edge definition (design doc §2.3 decision A, Elisp structural):
  cons        → car, cdr
  vector      → every element
  hash-table  → every key and every value
  closure /
    bcl-tag   → reached via their list/vector structure (no special
                case; nelisp-closure / nelisp-bcl are cons chains)
  other       → leaf (numbers, strings, symbols, buffers, etc.)

Keys of the returned hash-table map to t.  Callers wanting object
counts or type histograms iterate via `maphash'."
  (let ((visited (make-hash-table :test 'eq))
        (work    (nelisp-gc--seed-work root-override)))
    (while work
      (let ((obj (pop work)))
        (unless (or (null obj) (gethash obj visited))
          (puthash obj t visited)
          (cond
           ((consp obj)
            (push (car obj) work)
            (push (cdr obj) work))
           ((vectorp obj)
            (let ((i (length obj)))
              (while (> i 0)
                (setq i (1- i))
                (push (aref obj i) work))))
           ((hash-table-p obj)
            (maphash (lambda (k v) (push k work) (push v work)) obj))
           ;; Leaf (numbers, strings, symbols, other primitives).
           (t nil)))))
    visited))

(defun nelisp-gc-reachable-count (&optional root-override)
  "Return the count of objects in `nelisp-gc-reachable-set'.
Convenience for callers that only need the size (e.g. bench harness,
=nelisp-heap-count=)."
  (hash-table-count (nelisp-gc-reachable-set root-override)))

(provide 'nelisp-gc)
;;; nelisp-gc.el ends here
