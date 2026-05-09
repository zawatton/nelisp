;;; nelisp-stdlib-gc.el --- Bacon-Rajan cycle collector skeleton  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Doc 79 v7 Phase C Stage 5.3.a + 5.3.b + 5.3.c — elisp-side
;; Bacon-Rajan cycle collector.  Stage 5.3.a shipped the skeleton;
;; Stage 5.3.b replaced the no-op stub with the full four-phase
;; algorithm (= mark-gray / scan / scan-black / collect-white);
;; Stage 5.3.c wires the deferred finalizer queue + threshold +
;; idle-timer auto-trigger on top of the four phases.
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
;; Stage 5.3.c ships (Doc 79 §5.4 articulation, idle-timer subset):
;;   - `gc-finalize-queue' FIFO + `gc-finalize-enqueue' /
;;     `gc-finalize-step' / `gc-finalize-flush' drain entries,
;;   - `gc--collect-white-walk' enqueues handles instead of inline
;;     `nl-gc-finalize'; the queue is drained synchronously at the
;;     top of `gc-collect-cycles' (= synchronous semantics preserved
;;     per Doc 79 §6 single-threaded model + user prompt constraint),
;;   - `gc-collect-cycles-threshold' defcustom auto-fires
;;     `gc-collect-cycles' when `gc-cycle-roots-buffer' length tops it,
;;   - `gc-finalize-large-threshold' / `gc--large-payload-p' route
;;     large boxes through the deferred queue (production behaviour);
;;     since `nl-rc-payload-size' is a Stage D primitive (= Doc 79
;;     §5.4.4 articulation, +12 Rust LOC deferred to a future commit),
;;     5.3.c gates the size predicate behind `fboundp' and falls back
;;     to the universal queue path on host Emacs / standalone NeLisp,
;;   - `gc-finalize-arm-timer' invokes `run-with-idle-timer' under
;;     `gc-finalize-flush-mode' = 'idle (host Emacs).  NeLisp runtime
;;     lacks `run-with-idle-timer' (= Doc 79 §5.4.4 — Stage D handoff,
;;     Doc 78 / Doc 47 Phase 6 prerequisite); 'sync mode is the safe
;;     default for batch / standalone, and `gc-finalize-flush-mode' is
;;     auto-set to 'sync at load time when the primitive is missing.
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
;; Public surface (Stage 5.3.c):
;;   `gc-collect-cycles'   - run one synchronous CC pass, returns # freed
;;   `gc-stats'            - return alist of GC counters (4 keys)
;;   `gc-finalize-flush'   - drain `gc-finalize-queue' synchronously
;;   `gc-finalize-step'    - drain up to `gc-finalize-budget-ms' of queue
;;   `gc-maybe-collect-cycles' - threshold-gated auto-trigger entry
;;
;; Internal helpers (= Stage 5.3.d〜.e extension surface):
;;   `gc-cycle-roots-buffer'   - suspect handle list (drained per pass)
;;   `gc-color-table'          - per-handle color (white/gray/black/purple)
;;   `gc-internal-rc-table'    - per-handle trial-delete refcount
;;   `gc-finalize-queue'       - FIFO of handles awaiting `nl-gc-finalize'
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

;; ---- Stage 5.3.c finalizer queue + auto-trigger tunables ------------

(defvar gc-finalize-queue nil
  "FIFO list of opaque handles awaiting `nl-gc-finalize'.
`gc--collect-white-walk' (Stage 5.3.c) enqueues handles here
instead of finalizing inline; `gc-finalize-flush' / `-step' drain
the queue.  Reserved as a defvar slot in Stage 5.3.b commentary,
activated as a real consumer in Stage 5.3.c.")

(defcustom gc-collect-cycles-threshold 1024
  "Auto-trigger threshold for `gc-maybe-collect-cycles'.
When `(length gc-cycle-roots-buffer)' meets or exceeds this
value, the next call to `gc-maybe-collect-cycles' runs a full
collection pass synchronously.  Default 1024 per Doc 79 §5.4.
Set to nil to disable auto-fire (= manual `gc-collect-cycles'
only)."
  :type '(choice (const :tag "Disabled" nil) integer)
  :group 'nelisp-gc)

(defcustom gc-finalize-large-threshold nil
  "Payload byte threshold above which finalize is deferred to queue.
nil means \"all boxes go through the deferred queue\" — the
default for Stage 5.3.c since `nl-rc-payload-size' is a Stage D
primitive (= Doc 79 §5.4.4, +12 Rust LOC, deferred).  Once the
primitive ships, flip this to e.g. 4096 so small boxes finalize
inline and only large NlVector / NlBoolVector / NlStr defer."
  :type '(choice (const :tag "All boxes deferred" nil) integer)
  :group 'nelisp-gc)

(defcustom gc-finalize-budget-ms 16
  "Wallclock budget (ms) per `gc-finalize-step' invocation.
A single step processes the queue head until either the queue
empties or this budget is exceeded.  16ms ≈ one display frame
on a 60Hz host (= Doc 79 §5.4.3)."
  :type 'integer :group 'nelisp-gc)

(defcustom gc-finalize-idle-delay 0.5
  "Idle seconds before `gc-finalize-step' fires via idle-timer.
Only consulted when `gc-finalize-flush-mode' = `idle' and the
host runtime exposes `run-with-idle-timer'."
  :type 'number :group 'nelisp-gc)

(defcustom gc-finalize-flush-mode
  ;; Default to `sync' on batch / standalone NeLisp where
  ;; `run-with-idle-timer' is unavailable (Doc 79 §5.4.4 / §5.4.6.1).
  ;; Production host Emacs flips this to `idle' once the primitive is
  ;; reachable.  We probe `fboundp' at load time so the binding is
  ;; correct for the target environment without runtime detection cost.
  (if (and (fboundp 'run-with-idle-timer)
           (not noninteractive))
      'idle
    'sync)
  "Symbol controlling finalizer queue drain strategy.
`sync' (default for batch / standalone NeLisp): `gc-collect-cycles'
drains the queue synchronously before returning, preserving the
synchronous-semantics constraint from Doc 79 §6 + the Stage 5.3.c
prompt.  `idle' (production host Emacs): `gc-finalize-step' fires
on `run-with-idle-timer' and `gc-collect-cycles' returns with the
queue still pending (drained on idle)."
  :type '(choice (const :tag "Synchronous drain" sync)
                 (const :tag "Idle-timer drain" idle))
  :group 'nelisp-gc)

(defvar gc-finalize-timer nil
  "Currently armed idle-timer object, or nil.
Used as a re-arm guard so `gc-finalize-arm-timer' is idempotent.
Cleared by `gc-finalize-step' on entry; re-armed if the queue
still has work after the budget expires.")

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
suspect buffer was empty).  Stage 5.3.c: Phase 4 enqueues
handles into `gc-finalize-queue' instead of finalizing inline;
the queue is drained synchronously here before returning so the
caller observes a fully-collected heap (= Doc 79 §6 single-
threaded model + user prompt synchronous-semantics constraint)."
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
    ;; Stage 5.3.c: drain the deferred-finalize queue synchronously
    ;; so `gc-collect-cycles' preserves end-to-end synchronous
    ;; semantics regardless of `gc-finalize-flush-mode'.  Idle-timer
    ;; mode amortizes future enqueues from background dec hooks; this
    ;; entry-point drain only flushes what THIS pass produced.
    (gc-finalize-flush)
    freed))

(defun gc-maybe-collect-cycles ()
  "Run `gc-collect-cycles' iff the suspect buffer crossed threshold.
Returns the number of boxes freed (= 0 when below threshold or
threshold is nil).  Doc 79 §5.4 articulation; intended hook for
allocator-side dec callbacks once Stage 5.3.d wires them.

Threshold is `gc-collect-cycles-threshold' (defcustom, default
1024).  Setting it to nil disables auto-fire; a positive integer
means \"fire when (length gc-cycle-roots-buffer) >= threshold\"."
  (cond
   ((null gc-collect-cycles-threshold) 0)
   ((>= (length gc-cycle-roots-buffer)
        gc-collect-cycles-threshold)
    (gc-collect-cycles))
   (t 0)))

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
Returns the number of handles finalized in this subtree.

Stage 5.3.c: `nl-gc-finalize' is no longer called inline.
Handles are pushed onto `gc-finalize-queue' and drained later
(synchronously by `gc-collect-cycles' before it returns, or
incrementally by `gc-finalize-step' on idle).  This lets the
scheduler amortize finalize cost without breaking the
synchronous-semantics contract observed by callers of
`gc-collect-cycles'."
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
      ;; Stage 5.3.c: enqueue instead of inline finalize.  The queue
      ;; is FIFO via `nconc' append; one-element list keeps the
      ;; recursion overhead at O(1) per node.
      (gc-finalize-enqueue (list ptr))
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

;; ---- Stage 5.3.c finalizer queue + idle-timer integration -----------
;;
;; The queue lives between Phase 4 (= white-walk) and the actual
;; `nl-gc-finalize' call.  Two drain entries:
;;
;;   `gc-finalize-flush' — synchronous; ignores budget.  Used by
;;     `gc-collect-cycles' before returning (= preserves the
;;     synchronous-semantics contract) and by tests / batch mode
;;     where idle-timers do not fire.
;;   `gc-finalize-step'  — budgeted; drains up to
;;     `gc-finalize-budget-ms' wallclock per call.  Re-arms the
;;     idle-timer if the queue still has work after the budget.
;;
;; The Stage D handoff (= NeLisp runtime exposing
;; `run-with-idle-timer') is fenced behind `fboundp'; on standalone
;; NeLisp the idle path is a no-op until the primitive lands.

(defun gc--large-payload-p (ptr)
  "Return non-nil iff PTR's payload byte size exceeds the threshold.
Stage 5.3.c: gated behind `fboundp' for `nl-rc-payload-size'
(= Doc 79 §5.4.4 Stage D primitive, +12 Rust LOC, deferred).
When the primitive is unavailable or the threshold is nil, every
box routes through the deferred queue (= conservative default)."
  (when (and gc-finalize-large-threshold
             (fboundp 'nl-rc-payload-size))
    (> (or (nl-rc-payload-size ptr) 0)
       gc-finalize-large-threshold)))

(defun gc-finalize-enqueue (ptrs)
  "Append PTRS (list of handles) onto `gc-finalize-queue' and arm timer.
Called by `gc--collect-white-walk' for every collected handle.
Re-arms the idle-timer iff `gc-finalize-flush-mode' = `idle' and
the queue actually got new entries."
  (when ptrs
    (setq gc-finalize-queue (nconc gc-finalize-queue ptrs))
    (gc-finalize-arm-timer)))

(defun gc-finalize-arm-timer ()
  "Re-arm the idle-timer if not already active (idempotent).
No-op unless `gc-finalize-flush-mode' = `idle' AND host runtime
exposes `run-with-idle-timer' AND the queue has pending work AND
no timer is already in flight.  NeLisp standalone runtime lacks
the idle-timer primitive (= Doc 79 §5.4.4 Stage D handoff); on
that runtime this stays a no-op."
  (when (and (eq gc-finalize-flush-mode 'idle)
             (null gc-finalize-timer)
             gc-finalize-queue
             (fboundp 'run-with-idle-timer))
    (setq gc-finalize-timer
          (run-with-idle-timer gc-finalize-idle-delay nil
                               #'gc-finalize-step))))

(defun gc-finalize-step ()
  "Drain `gc-finalize-queue' for up to `gc-finalize-budget-ms' wallclock.
Phase 4 of the Doc 79 §5.4.1 pipeline.  Returns the number of
handles finalized in this step.  Re-arms the idle-timer when the
queue still has pending work after the budget expires."
  (setq gc-finalize-timer nil)
  (let ((t0 (current-time))
        (deadline-sec (/ (float gc-finalize-budget-ms) 1000.0))
        (processed 0))
    (while (and gc-finalize-queue
                (< (float-time (time-since t0)) deadline-sec))
      (let ((ptr (pop gc-finalize-queue)))
        (nl-gc-finalize ptr)
        (setq processed (1+ processed))))
    (when gc-finalize-queue
      (gc-finalize-arm-timer))
    processed))

(defun gc-finalize-flush ()
  "Drain `gc-finalize-queue' synchronously, ignoring budget.
Returns the number of handles finalized.  Used by:
  - `gc-collect-cycles' before returning (= synchronous-semantics
    contract preservation),
  - tests / batch-mode where `run-with-idle-timer' cannot fire,
  - `gc-finalize-flush-mode' = `sync' production deployments."
  (let ((processed 0))
    (while gc-finalize-queue
      (nl-gc-finalize (pop gc-finalize-queue))
      (setq processed (1+ processed)))
    ;; Disarm any pending idle-timer; the queue is empty now and
    ;; firing it would just be a wasted wakeup.
    (when (and gc-finalize-timer
               (fboundp 'cancel-timer))
      (cancel-timer gc-finalize-timer)
      (setq gc-finalize-timer nil))
    processed))

(provide 'nelisp-stdlib-gc)
;;; nelisp-stdlib-gc.el ends here
