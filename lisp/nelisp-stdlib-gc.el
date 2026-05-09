;;; nelisp-stdlib-gc.el --- Bacon-Rajan cycle collector skeleton  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Doc 79 v7 Phase C Stage 5.3.a + 5.3.b — elisp-side Bacon-Rajan
;; cycle collector.  Stage 5.3.a shipped the skeleton; Stage 5.3.b
;; replaces the no-op stub with the full four-phase algorithm
;; (= mark-gray / scan / scan-black / collect-white) over the 10
;; `nl-rc-*' / `nl-gc-*' primitives in `rc_primitives.rs'.
;;
;; Stage 5.3.b ships (Doc 79 §5.3.5.b〜.d, consolidated):
;;   - `gc-collect-cycles' real entry point invoking the four phases,
;;   - per-phase walkers `gc--mark-gray-walk' / `gc--scan-walk' /
;;     `gc--scan-black-walk' / `gc--collect-white-walk',
;;   - per-kind children dispatch via `gc--children' /
;;     `gc--root-candidate-kind-p' (= delegates to `nl-gc-walk-children'
;;     for the actual edge list, returning nil for kinds whose
;;     primitives report no outgoing edges).
;;
;; Algorithm reference: Bacon & Rajan, "Concurrent Cycle Collection in
;; Reference Counted Systems" (ECOOP 2001), Algorithm 4 (synchronous).
;; elisp is single-threaded so we use the synchronous variant verbatim
;; (= no read/write barriers, Doc 79 §6.4).
;;
;; Substrate dependencies (Doc 80 + nelisp-stdlib-hash):
;;   - `cond' / `when' / `unless' / `null' / `defun' / `defvar' from
;;     `nelisp-jit-substrate.el' (loads first in STDLIB chain),
;;   - `make-hash-table' from `nelisp-stdlib-hash.el' (loads
;;     immediately before this file in the STDLIB chain).
;;
;; Public surface (Stage 5.3.b):
;;   `gc-collect-cycles'   - run one synchronous CC pass, returns # freed
;;   `gc-stats'            - return alist of GC counters (4 keys)
;;
;; Internal helpers (= Stage 5.3.c〜.e extension surface):
;;   `gc-cycle-roots-buffer'   - suspect handle list (drained per pass)
;;   `gc-color-table'          - per-handle color (white/gray/black/purple)
;;   `gc-internal-rc-table'    - per-handle trial-delete refcount
;;   `gc--children'            - outgoing edge enumeration
;;   `gc--root-candidate-kind-p' - predicate for cycle-capable kinds

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
  ;; orderings).  Stage 5.3.b extends the MVP alist with `passes'
  ;; (= `gc-collect-cycles' invocation count) so observers can tell
  ;; the difference between "no garbage" and "collector never ran".
  (cons (cons 'total-allocs 0)
        (cons (cons 'total-frees 0)
              (cons (cons 'pending-cycles 0)
                    (cons (cons 'passes 0) nil))))
  "Alist of GC observability counters.
Stage 5.3.b ships 4 keys: `total-allocs' / `total-frees' /
`pending-cycles' / `passes'.  Stage 5.5 (= self-host gate bench)
expands to the 10-key plist articulated in Doc 79 §5.5.4.")

;; ---- Top-level entry point ------------------------------------------

(defun gc-collect-cycles ()
  "Run one synchronous Bacon-Rajan cycle-collection pass.

Drains `gc-cycle-roots-buffer' into a snapshot of suspect handles,
runs the four phases over the snapshot, then increments the
`passes' / `boxes-freed' counters on `gc-stats-counters'.

  Phase 1 (mark-gray) — for each suspect, decrement the internal
                        rc of its outgoing edges and recolor reached
                        nodes `gray'.
  Phase 2 (scan)       — for each suspect, if internal rc < strong
                        count then recolor `black' and undo the
                        decrement (= live root); otherwise mark it
                        `white' and recurse into children.
  Phase 3 (scan-black) — restore counts on the live subgraph
                        (= performed inline by `gc--scan-walk').
  Phase 4 (collect-white) — finalize every `white' handle via
                        `nl-gc-finalize'.

Returns the number of boxes freed in this pass (= 0 when the
suspect buffer was empty)."
  (let ((roots (gc--snapshot-roots))
        (freed 0))
    (when roots
      (clrhash gc-color-table)
      (clrhash gc-internal-rc-table)
      (dolist (ptr roots) (gc--mark-gray-walk ptr))
      (dolist (ptr roots) (gc--scan-walk ptr))
      (dolist (ptr roots)
        (when (eq (gethash ptr gc-color-table) 'white)
          (setq freed (+ freed (gc--collect-white-walk ptr))))))
    (gc--bump-stat 'passes 1)
    (when (> freed 0)
      (gc--bump-stat 'total-frees freed))
    freed))

;; ---- Phase helpers --------------------------------------------------

(defun gc--snapshot-roots ()
  "Drain `gc-cycle-roots-buffer' into a fresh list (Phase-1 prep)."
  (let ((buf gc-cycle-roots-buffer))
    (setq gc-cycle-roots-buffer nil)
    buf))

(defun gc--bump-stat (key delta)
  "Add DELTA to the counter under KEY in `gc-stats-counters'."
  (let ((cell (assq key gc-stats-counters)))
    (when cell (setcdr cell (+ (cdr cell) delta)))))

(defun gc--mark-gray-walk (ptr)
  "Phase 1 walker: paint PTR `gray', decrement child internal rc."
  (unless (eq (gethash ptr gc-color-table) 'gray)
    (puthash ptr 'gray gc-color-table)
    ;; Seed the trial-delete count from the live strong count on
    ;; first visit so subsequent phases compare apples to apples.
    (unless (gethash ptr gc-internal-rc-table)
      (puthash ptr (gc--rc-strong-count ptr) gc-internal-rc-table))
    (dolist (child (gc--children ptr))
      (when (gc--root-candidate-kind-p child)
        ;; Virtual decrement: count the edge we just walked as if
        ;; it had vanished.  External roots will still exceed the
        ;; resulting internal count and survive Phase 2.
        (let ((cur (or (gethash child gc-internal-rc-table)
                       (gc--rc-strong-count child))))
          (puthash child (- cur 1) gc-internal-rc-table))
        (gc--mark-gray-walk child)))))

(defun gc--scan-walk (ptr)
  "Phase 2 walker: classify PTR as `black' (live) or `white' (cycle).
Per Bacon-Rajan Algorithm 4: after mark-gray subtracts every
intra-suspect-set edge from `gc-internal-rc-table', the post-
decrement count exceeding 0 means PTR has at least one referrer
*outside* the suspect set (= externally pinned, live).  When the
post-decrement count reaches 0 (or below in degenerate inputs),
all references to PTR come from inside the cycle, so it is white."
  (when (eq (gethash ptr gc-color-table) 'gray)
    (let ((internal (gethash ptr gc-internal-rc-table 0)))
      (cond
       ((> internal 0)
        ;; External reference survives — restore counts on the
        ;; reachable subgraph (Phase 3 scan-black inlined).
        (gc--scan-black-walk ptr))
       (t
        (puthash ptr 'white gc-color-table)
        (dolist (child (gc--children ptr))
          (when (gc--root-candidate-kind-p child)
            (gc--scan-walk child))))))))

(defun gc--scan-black-walk (ptr)
  "Phase 3 walker: re-mark PTR + reachable subgraph `black'."
  (puthash ptr 'black gc-color-table)
  (dolist (child (gc--children ptr))
    (when (gc--root-candidate-kind-p child)
      ;; Undo the Phase-1 virtual decrement on the survivor edge.
      (let ((cur (or (gethash child gc-internal-rc-table)
                     (gc--rc-strong-count child))))
        (puthash child (+ cur 1) gc-internal-rc-table))
      (unless (eq (gethash child gc-color-table) 'black)
        (gc--scan-black-walk child)))))

(defun gc--collect-white-walk (ptr)
  "Phase 4 walker: finalize every `white' handle reachable from PTR.
Returns the number of handles finalized in this subtree."
  (cond
   ((not (eq (gethash ptr gc-color-table) 'white)) 0)
   (t
    ;; Recolor first to break self-referential recursion before
    ;; descending into children.
    (puthash ptr 'black gc-color-table)
    (let ((freed 1))
      (dolist (child (gc--children ptr))
        (when (gc--root-candidate-kind-p child)
          (setq freed (+ freed (gc--collect-white-walk child)))))
      ;; Force-finalize the box.  Stage 5.3.b inline-only path
      ;; (= no `gc-finalize-queue' yet, threshold is nil).
      (nl-gc-finalize ptr)
      freed))))

;; ---- Per-kind children dispatch -------------------------------------

(defun gc--children (ptr)
  "Return the outgoing-edge list of PTR for cycle traversal.
Delegates to the Rust `nl-gc-walk-children' primitive, which
returns the proper child list for cycle-capable kinds (CONS /
VECTOR / CELL / RECORD / CHAR_TABLE) and nil for everything else."
  (gc--walk-children ptr))

(defun gc--root-candidate-kind-p (ptr)
  "Return non-nil iff PTR's kind tag has potential outgoing edges.
Cycle-capable kinds per Doc 79 §5.3.2: CONS (7), VECTOR (8),
CHAR_TABLE (9), CELL (11), RECORD (12).  Unboxed / leaf kinds
(NIL / T / INT / FLOAT / SYMBOL / STR / MUT_STR / BOOL_VECTOR)
are skipped early so the walkers never recurse through them."
  (let ((kind (gc--rc-kind ptr)))
    (or (eq kind 7) (eq kind 8) (eq kind 9)
        (eq kind 11) (eq kind 12))))

;; ---- Observability --------------------------------------------------

(defun gc-stats ()
  "Return the GC stats alist (Stage 5.3.b: 4 keys).
Keys: `total-allocs', `total-frees', `pending-cycles' (= live
length of `gc-cycle-roots-buffer'), `passes' (= `gc-collect-cycles'
invocation count).  `total-allocs' stays 0 until Stage 5.3.c wires
the alloc-hook side; the other three are driven by Stage 5.3.b."
  ;; Re-build the alist with the current `pending-cycles' value so
  ;; observers always see a fresh snapshot.  We deliberately avoid
  ;; mutating `gc-stats-counters' in place: Stage 5.5 turns this into
  ;; a plist with stable accessors and the immutable-snapshot return
  ;; pattern is the contract Stage 5.5 inherits.
  (cons (cons 'total-allocs (cdr (assq 'total-allocs gc-stats-counters)))
        (cons (cons 'total-frees (cdr (assq 'total-frees gc-stats-counters)))
              (cons (cons 'pending-cycles
                          (length gc-cycle-roots-buffer))
                    (cons (cons 'passes
                                (cdr (assq 'passes gc-stats-counters)))
                          nil)))))

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
