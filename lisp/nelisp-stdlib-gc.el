;;; nelisp-stdlib-gc.el --- Bacon-Rajan cycle collector skeleton  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Doc 79 v7 Phase C Stage 5.3.a (2026-05-09) — initial skeleton for
;; the elisp-side Bacon-Rajan cycle collector.  Stage 5.3.a ships:
;;
;;   - data-structure shells (= `gc-cycle-roots-buffer',
;;     `gc-color-table', `gc-internal-rc-table'),
;;   - `gc-collect-cycles' top-level entry point as a no-op stub,
;;   - `gc-stats' MVP accessor returning a 3-key alist,
;;   - thin elisp wrappers over the 10 `nl-rc-*' / `nl-gc-*'
;;     primitives shipped in the same atomic commit (= `rc_primitives.rs').
;;
;; Stage 5.3.b〜.e replace the stubs with the full algorithm
;; (= mark-roots / scan-trial-delete / scan / collect-white).  The
;; signatures and data-structure shells in this file are the
;; consumer-side anchor that prevents the primitives from being
;; dead code (= Doc 79 §11.7 atomic-with-consumer pattern, project
;; memory `feedback_commit_dead_code_claim_must_grep.md').
;;
;; Substrate dependencies (Doc 80 + nelisp-stdlib-hash):
;;   - `cond' / `when' / `unless' / `null' / `defun' / `defvar' from
;;     `nelisp-jit-substrate.el' (loads first in STDLIB chain),
;;   - `make-hash-table' from `nelisp-stdlib-hash.el' (loads
;;     immediately before this file in the STDLIB chain).
;;
;; Public surface (Stage 5.3.a):
;;   `gc-collect-cycles'   - run one synchronous CC pass (no-op MVP)
;;   `gc-stats'            - return alist of GC counters (MVP keys)
;;
;; Internal helpers exposed for Stage 5.3.b〜.e to extend:
;;   `gc-cycle-roots-buffer'   - suspect handle list
;;   `gc-color-table'          - per-handle color (white/gray/black/purple)
;;   `gc-internal-rc-table'    - per-handle trial-delete refcount

;;; Code:

;; ---- Tunables -------------------------------------------------------

(defvar gc-cycle-roots-buffer nil
  "List of suspect handles awaiting cycle detection.
Pushed by Stage 5.3.b's dec hook, drained by `gc-collect-cycles'
(Bacon-Rajan Phase 1 buffer).  Stage 5.3.a starts empty and stays
empty because the dec hook is not yet wired.")

(defvar gc-color-table (make-hash-table :test 'eq)
  "Map of handle -> color symbol.
Color values during Bacon-Rajan are `white', `gray', `black',
`purple'.  Reset between collection cycles via `clrhash'.")

(defvar gc-internal-rc-table (make-hash-table :test 'eq)
  "Map of handle -> internal refcount during trial deletion.
Stage 5.3.b populates this on the way down the suspect graph and
compares against the strong count to detect cycles.")

(defvar gc-stats-counters
  ;; Use `cons' explicitly so this defvar runs before `list' is
  ;; available (= `nelisp-stdlib-list' loads later in some bake
  ;; orderings).  3-key MVP alist.
  (cons (cons 'total-allocs 0)
        (cons (cons 'total-frees 0)
              (cons (cons 'pending-cycles 0) nil)))
  "Alist of GC observability counters.
Stage 5.3.a ships only the 3 MVP keys; Stage 5.5 (= self-host gate
bench) expands to the 10-key plist articulated in Doc 79 §5.5.4.")

;; ---- Top-level entry point ------------------------------------------

(defun gc-collect-cycles ()
  "Run one synchronous cycle-collection pass.
Stage 5.3.a is a no-op stub: drains `gc-cycle-roots-buffer' without
inspecting it and returns nil.  Stage 5.3.b〜.e replace the body
with the four-phase Bacon-Rajan algorithm:

  Phase 1 (mark-roots) — color suspect handles `purple',
  Phase 2 (scan-trial-delete) — decrement internal-rc on outgoing
                               edges, color reachable nodes `gray',
  Phase 3 (scan) — re-color survivors `black', dead-cycle members
                  stay `white',
  Phase 4 (collect-white) — `nl-gc-finalize' every white handle."
  ;; Drain the buffer (no-op MVP — Stage 5.3.b implements the real
  ;; mark-roots pass here).
  (setq gc-cycle-roots-buffer nil)
  ;; Reset the bookkeeping tables for the next cycle.
  (clrhash gc-color-table)
  (clrhash gc-internal-rc-table)
  nil)

;; ---- Observability --------------------------------------------------

(defun gc-stats ()
  "Return the MVP GC stats alist.
Stage 5.3.a returns 3 keys: `total-allocs', `total-frees',
`pending-cycles' (= length of `gc-cycle-roots-buffer').  The first
two are 0 until Stage 5.3.b wires the alloc / dec hooks; the third
is computed live from the buffer."
  ;; Re-build the alist with the current `pending-cycles' value so
  ;; observers always see a fresh snapshot.  We deliberately avoid
  ;; mutating `gc-stats-counters' in place: Stage 5.5 turns this into
  ;; a plist with stable accessors and the immutable-snapshot return
  ;; pattern is the contract Stage 5.5 inherits.
  (cons (cons 'total-allocs (cdr (assq 'total-allocs gc-stats-counters)))
        (cons (cons 'total-frees (cdr (assq 'total-frees gc-stats-counters)))
              (cons (cons 'pending-cycles
                          (length gc-cycle-roots-buffer))
                    nil))))

;; ---- Primitive wrappers (= consumer-side anchor) --------------------
;;
;; These keep the 10 primitives from `rc_primitives.rs' reachable
;; from elisp without circular dependencies on Stage 5.3.b〜.e
;; collector internals.  Each wrapper is a 1-line passthrough so
;; Stage 5.3.b〜.e can replace bodies with caching / hook insertion
;; without breaking call sites.

(defun gc--rc-kind (handle)
  "Return the kind tag (= integer) of HANDLE.
Wraps `nl-rc-kind'."
  (nl-rc-kind handle))

(defun gc--rc-payload-ptr (handle)
  "Return the payload pointer (= integer address) of HANDLE.
Wraps `nl-rc-payload-ptr'.  0 for unboxed Sexp variants."
  (nl-rc-payload-ptr handle))

(defun gc--rc-strong-count (handle)
  "Return the current strong refcount of HANDLE.
Wraps `nl-rc-strong-count'.  Errors on non-boxed HANDLE."
  (nl-rc-strong-count handle))

(defun gc--walk-children (handle)
  "Return the list of outgoing edges of HANDLE.
Wraps `nl-gc-walk-children'.  Stage 5.3.a returns (CAR CDR) for
cons handles and nil for everything else; Stage 5.3.b widens to
vector / record / cell / chartable."
  (nl-gc-walk-children handle))

(provide 'nelisp-stdlib-gc)
;;; nelisp-stdlib-gc.el ends here
