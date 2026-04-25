;;; nelisp-gc-inner.el --- Phase 7.3.1 mark phase + GC metadata decoder  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; Author: zawatton <kurozawawo@gmail.com>

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 7.3.1 — *MVP subset* of the Doc 30 v2 LOCKED inner GC.  Three
;; pieces ship here:
;;
;;   1. *Wire decoder* (Doc 28 §2.9 → Doc 30 §1.4)
;;      The native compiler (`nelisp-cc-runtime.el', T11) produces a
;;      *wire contract* — three structurally-typed fragments per safe-
;;      point (uint32 ID, BOOL-VECTOR live-roots, frame layout
;;      descriptor).  The collector consumes them only after the
;;      decode pass converts them into the Doc 30 §1.4 *internal
;;      representation* — a plist tagged with the contract version.
;;      Phase 7.3.1 freezes the decoder/encoder interface; the binary
;;      bytes themselves arrive in Phase 7.5 once the runtime FFI
;;      lands, so the producer here uses a simulator format
;;      (plist-of-plists serialization) that round-trips losslessly.
;;
;;   2. *Tri-color mark phase* (Doc 30 §2.3 + §3.1)
;;      Object-address-keyed mark state hash.  White = candidate for
;;      sweep, grey = scan pending, black = scanned.  An iterative
;;      work-list walks edges via the *heap-region registry* (Doc 29
;;      §1.4 consumer) so the mark phase never touches the host
;;      Emacs object graph and stays purely on simulated NeLisp heap
;;      addresses.
;;
;;   3. *Finalizer queue* (Doc 30 §6.9 v2 BLOCKER 2 mitigation)
;;      Pre-allocated bounded ring buffer.  Sweep would `enqueue';
;;      mutator context calls `drain' to actually fire user thunks.
;;      Resurrection policy `:once' is enforced at `drain' time —
;;      already-fired entries do not re-queue.  Phase 7.3.1 ships the
;;      queue itself; the *enqueueing* call site is wired up in 7.3.2
;;      (sweep) and the *poll loop* in 7.3.6 (scheduler).
;;
;; Out of scope for 7.3.1 (covered by later sub-phases):
;;   - sweep phase / per-pool free-list rebuild (7.3.2)
;;   - Cheney semispace nursery copy (7.3.3)
;;   - card-marking write barrier (7.3.4)
;;   - promotion + age policy (7.3.5)
;;   - scheduler + safe-point poll plumbing (7.3.6)
;;
;; Naming convention: every public symbol in this file is prefixed
;; `nelisp-gc-inner-' to keep it disjoint from Phase 3c
;; `nelisp-gc.el' (which models *host* Emacs reachability and is
;; orthogonal to the simulated NeLisp heap walked here).

;;; Code:

(require 'cl-lib)


;;; Versioning -------------------------------------------------------

(defconst nelisp-gc-inner-metadata-version 1
  "Doc 30 §1.4 internal representation version.
Bumped on any breaking change to the decoded plist shape.  Stays in
sync with `nelisp-cc-runtime-gc-metadata-version' (Phase 7.1
producer) by integration test, but each side may bump independently
provided the decoder rejects unknown versions.")

(defconst nelisp-gc-inner-heap-region-version 1
  "Doc 29 §1.4 / §2.6 heap-region registry version we consume.
The allocator (Phase 7.2) is the producer of this number; the
collector accepts the registry only when the producer's version
matches what we know how to walk.")


;;; §1. Wire contract decoder ---------------------------------------

;; The wire format is *Doc 28 §2.9*: three fragments per safe-point.
;; Phase 7.5 fixes the binary layout once the FFI bridge is live; the
;; structural-equivalent simulator form used here is a list of plist
;; cells, each with `:id' (uint32), `:live-roots' (bool-vector), and
;; `:frame-layout' (plist with `:size' / `:slots' / `:caller'-shape).
;; Round-tripping through (encode-wire-for-test → decode-wire) is the
;; lossless invariant the test suite enforces.

(defun nelisp-gc-inner--wire-safe-point-p (sp)
  "Return non-nil when SP looks like a wire-format safe-point fragment.
Three required keys per Doc 28 §2.9: `:id', `:live-roots',
`:frame-layout'.  We do *not* recurse into `:frame-layout' here —
that schema ride-along is whatever the compiler chose to ship and we
forward it verbatim into the internal representation."
  (and (listp sp)
       (integerp (plist-get sp :id))
       (>= (plist-get sp :id) 0)
       (let ((bv (plist-get sp :live-roots)))
         (or (null bv) (bool-vector-p bv)))
       (listp (plist-get sp :frame-layout))))

(defun nelisp-gc-inner--decode-one-safe-point (sp)
  "Lift a single wire SP into the Doc 30 §1.4 internal shape.

Wire keys (Doc 28 §2.9):
  :id            uint32, sequentially allocated by the compiler
  :live-roots    BOOL-VECTOR over value ids
  :frame-layout  plist: :size SIZE :slots COUNT :caller-shape SYM

Internal keys (this layer adds):
  :kind          {entry, exit, back-edge}, defaulted to `entry' when
                 the wire fragment doesn't carry one explicitly
  :pc-offset     defaulted to 0 — Phase 7.5 backend resolves the
                 final byte offset; the decoder must accept fragments
                 that have not yet been linked
  :frame-size    convenience accessor lifted from `:frame-layout'

Unknown wire keys are *passed through* under their original
spelling so a future version-2 wire can extend without breaking
older readers (forward-compat per Doc 28 §6.10)."
  (unless (nelisp-gc-inner--wire-safe-point-p sp)
    (signal 'wrong-type-argument (list 'wire-safe-point-p sp)))
  (let* ((layout (plist-get sp :frame-layout))
         (frame-size (or (plist-get layout :size) 0)))
    (list :id          (plist-get sp :id)
          :kind        (or (plist-get sp :kind) 'entry)
          :pc-offset   (or (plist-get sp :pc-offset) 0)
          :live-roots  (plist-get sp :live-roots)
          :frame-size  frame-size
          :frame-layout layout)))

(defun nelisp-gc-inner-decode-wire (wire)
  "Decode WIRE (Doc 28 §2.9 producer output) → Doc 30 §1.4 plist.

WIRE is the simulator-format payload — a plist with:
  :gc-metadata-version  must equal `nelisp-gc-inner-metadata-version'
  :function-name        symbol identifying the compiled function
  :safe-points          list of wire-format safe-point fragments

Returns:
  (:gc-metadata-version 1
   :function-name SYM
   :safe-points  ((:id N :kind K :pc-offset OFF :live-roots BV
                   :frame-size SIZE :frame-layout LAYOUT) ...))

Signals `wrong-type-argument' on shape mismatch and a plain `error'
on version skew so the caller can distinguish \"malformed payload\"
from \"newer producer than this collector understands\"."
  (unless (listp wire)
    (signal 'wrong-type-argument (list 'listp wire)))
  (let ((ver (plist-get wire :gc-metadata-version)))
    (unless (eq ver nelisp-gc-inner-metadata-version)
      (error "nelisp-gc-inner: unsupported GC metadata version %S (expected %S)"
             ver nelisp-gc-inner-metadata-version)))
  (let ((sps (plist-get wire :safe-points)))
    (unless (listp sps)
      (signal 'wrong-type-argument (list 'listp sps)))
    (list :gc-metadata-version nelisp-gc-inner-metadata-version
          :function-name       (plist-get wire :function-name)
          :safe-points         (mapcar #'nelisp-gc-inner--decode-one-safe-point
                                       sps))))

(defun nelisp-gc-inner-encode-wire-for-test (internal)
  "Inverse of `nelisp-gc-inner-decode-wire' — *test helper only*.
Useful for the round-trip ERT and as documentation of the simulator
wire shape.  The real producer is `nelisp-cc-runtime' and uses its
own emit pass; this function exists to give the decoder ERT a
pure-data fixture."
  (list :gc-metadata-version (or (plist-get internal :gc-metadata-version)
                                 nelisp-gc-inner-metadata-version)
        :function-name (plist-get internal :function-name)
        :safe-points
        (mapcar
         (lambda (sp)
           (list :id           (plist-get sp :id)
                 :kind         (plist-get sp :kind)
                 :pc-offset    (plist-get sp :pc-offset)
                 :live-roots   (plist-get sp :live-roots)
                 :frame-layout (or (plist-get sp :frame-layout)
                                   (list :size (or (plist-get sp :frame-size)
                                                   0)))))
         (plist-get internal :safe-points))))


;;; §2. Tri-color mark state ----------------------------------------

;; Mark state is a hash from object *address* (an integer in the
;; simulator; an actual mmap-allocated address in Phase 7.5) to one
;; of the three colours.  We default to `:white' implicitly — looking
;; up an unmarked address returns :white via the optional fallback in
;; `nelisp-gc-inner-mark-color'.  Storing :white explicitly is also
;; valid (e.g. when a sweep cycle resets a region of the heap).

(defvar nelisp-gc-inner--mark-state (make-hash-table :test 'eql)
  "Object address (eql-keyed) → :white / :grey / :black for the live cycle.
Reset by `nelisp-gc-inner-init-mark-state' at the start of every
mark phase so a stale post-sweep colouring cannot leak into the
next cycle.")

(defvar nelisp-gc-inner--mark-cycle 0
  "Monotonic counter — increments once per `nelisp-gc-inner-init-mark-state'.
Useful for plist tagging (`:mark-cycle' in result returns) so callers
can correlate before/after snapshots without tracking timing
themselves.")

(defun nelisp-gc-inner-init-mark-state ()
  "Reset mark state at the start of a mark cycle.
All previously-coloured addresses revert to implicit `:white' (we
clear the table entirely rather than walk it).  Returns the new
cycle counter so the caller can stamp a result plist with it."
  (clrhash nelisp-gc-inner--mark-state)
  (cl-incf nelisp-gc-inner--mark-cycle)
  nelisp-gc-inner--mark-cycle)

(defun nelisp-gc-inner-mark-color (addr)
  "Return mark colour of ADDR — `:white' when never set."
  (or (gethash addr nelisp-gc-inner--mark-state) :white))

(defun nelisp-gc-inner-set-color (addr color)
  "Set mark colour of ADDR to COLOR (one of :white / :grey / :black)."
  (unless (memq color '(:white :grey :black))
    (signal 'wrong-type-argument (list 'mark-color-p color)))
  (puthash addr color nelisp-gc-inner--mark-state)
  color)


;;; §3. Heap-region registry consumer (Doc 29 §1.4) -----------------

;; The Phase 7.2 allocator hands the collector a *registry* — a list
;; of plist region descriptors with the schema documented at Doc 29
;; §1.4.  Phase 7.3.1 only reads it; the actual region walk (every
;; live address in every region) lands in 7.3.2 sweep when the
;; allocator is wired through.  We model a region descriptor and a
;; minimal "find children of an address" hook.

(defun nelisp-gc-inner--region-p (region)
  "Return non-nil when REGION matches the Doc 29 §1.4 v1 schema.
Required keys: `:region-id', `:start', `:end', `:generation',
`:family'.  `:children-of' is an optional callback used by the mark
phase to walk edges out of an address inside the region."
  (and (listp region)
       (integerp (plist-get region :region-id))
       (integerp (plist-get region :start))
       (integerp (plist-get region :end))
       (>= (plist-get region :end) (plist-get region :start))
       (memq (plist-get region :generation) '(:nursery :tenured))
       (memq (plist-get region :family)
             '(:cons-pool :string-span :vector-span
                          :closure-pool :large-object))))

(defun nelisp-gc-inner--region-contains (region addr)
  "Return non-nil when ADDR falls inside REGION (start ≤ addr < end)."
  (and (integerp addr)
       (>= addr (plist-get region :start))
       (<  addr (plist-get region :end))))

(defun nelisp-gc-inner--region-of (regions addr)
  "Return the first REGION whose `[:start :end)' covers ADDR, or nil."
  (cl-find-if (lambda (r) (nelisp-gc-inner--region-contains r addr))
              regions))

(defun nelisp-gc-inner--children-of-addr (regions addr)
  "Return the list of child addresses reachable from object at ADDR.

Lookup chain:
  1. Find the region containing ADDR (Doc 29 §1.4 consumer).
  2. If that region carries `:children-of' (a function of one
     argument, the address), funcall it.  Allocator-supplied walker
     keeps us decoupled from per-family layout.
  3. Otherwise return nil — leaf object (or unwalkable region).

When ADDR falls outside every registered region we treat it as a
*foreign root* (e.g. interned symbol address kept in a register and
spilled into the live-roots bitmap).  Phase 7.3.1 marks foreign
roots in `:black' but does not recurse — Phase 7.5 wires Doc 29's
foreign-root accept-list."
  (let ((region (nelisp-gc-inner--region-of regions addr)))
    (when region
      (let ((walker (plist-get region :children-of)))
        (when (functionp walker)
          (funcall walker addr))))))


;;; §4. Mark phase work-list algorithm (Doc 30 §3.1) ----------------

(defvar nelisp-gc-inner--worklist nil
  "Pending grey addresses to scan during the active mark cycle.
Reset by `nelisp-gc-inner-run-mark-phase' so a previously-aborted
cycle cannot bleed leftover work into the next.")

(defun nelisp-gc-inner--enqueue-grey (addr)
  "Set ADDR to :grey and push it onto the worklist.
No-op when ADDR is already :grey or :black — that's the standard
tri-color invariant guard against re-enqueueing on a cycle."
  (let ((c (nelisp-gc-inner-mark-color addr)))
    (when (eq c :white)
      (nelisp-gc-inner-set-color addr :grey)
      (push addr nelisp-gc-inner--worklist))))

(defun nelisp-gc-inner-run-mark-phase (root-set heap-regions)
  "Run a Doc 30 §3.1 tri-color mark cycle.

ROOT-SET is the live-root address list (the output of
`nelisp-gc-inner-collect-roots').  HEAP-REGIONS is a Doc 29 §1.4
registry snapshot (any sequence whose elements satisfy
`nelisp-gc-inner--region-p').

Algorithm:
  1. `init-mark-state' — reset all colours.
  2. For every ADDR in ROOT-SET, mark grey + push.
  3. While the worklist is non-empty: pop ADDR, walk its children
     (via the region's `:children-of' callback), mark white-children
     grey, then mark ADDR black.
  4. At end, `:white' addresses in the registry are unreachable —
     candidates for sub-phase 7.3.2 sweep.

Returns a result plist:
  :mark-cycle    monotonic counter from `init-mark-state'
  :marked-count  number of addresses now coloured :black
  :grey-count    must always be 0 at termination (invariant gate)
  :white-count   number of addresses we *saw* still :white (only
                 those explicitly placed there; not a heap-wide scan)
  :elapsed-ms    integer ms taken (uses `float-time' diff)

Phase 7.3.1 only verifies the *logical* tri-color invariant — actual
heap walk over all live regions is added in 7.3.2.  See
docs/design/30-phase7.3-gc-inner.org §3.1."
  (let ((cycle (nelisp-gc-inner-init-mark-state))
        (start (float-time)))
    (setq nelisp-gc-inner--worklist nil)
    ;; Step 2: seed the work-list from ROOT-SET.
    (dolist (root root-set)
      (when (integerp root)
        (nelisp-gc-inner--enqueue-grey root)))
    ;; Step 3: drain the work-list.  Iterative — never recurses, so a
    ;; deeply-nested object graph cannot blow `max-lisp-eval-depth'.
    (let ((marked 0))
      (while nelisp-gc-inner--worklist
        (let ((addr (pop nelisp-gc-inner--worklist)))
          (when (eq (nelisp-gc-inner-mark-color addr) :grey)
            ;; Walk edges first, then settle on :black so mid-walk
            ;; observers see the parent in :grey (Doc 30 §6.10
            ;; double-scan invariant relies on this ordering).
            (dolist (child (nelisp-gc-inner--children-of-addr
                            heap-regions addr))
              (when (integerp child)
                (nelisp-gc-inner--enqueue-grey child)))
            (nelisp-gc-inner-set-color addr :black)
            (cl-incf marked))))
      ;; Step 4: tally remaining colours for the result plist.
      (let ((grey 0) (white 0))
        (maphash (lambda (_a c)
                   (cond ((eq c :grey)  (cl-incf grey))
                         ((eq c :white) (cl-incf white))))
                 nelisp-gc-inner--mark-state)
        (list :mark-cycle   cycle
              :marked-count marked
              :grey-count   grey
              :white-count  white
              :elapsed-ms   (round (* 1000.0
                                      (- (float-time) start))))))))


;;; §5. Root scan (Doc 28 §2.9 + Doc 29 §1.4 fusion) ---------------

(defun nelisp-gc-inner--addr-from-vid (frame-layout vid)
  "Compute the address of value-id VID inside FRAME-LAYOUT.

Doc 28 §2.9 represents a frame as `(:size S :slots N :base BASE)':
slot N's address is BASE + N * word-size, where word-size is 8 on
the 64-bit targets we ship.  When `:slot-table' is present (a
sparse alist mapping VID → byte offset), it overrides the linear
indexing — that's how the producer forwards register-pinned values
that don't have a stable stack slot.

Returns nil when the layout doesn't carry enough information to
resolve an address — caller treats nil as \"this VID is in a
non-spillable register and is conservatively traced via Phase 7.5
register snapshots\" (out of scope here)."
  (let ((slot-table (plist-get frame-layout :slot-table))
        (base       (plist-get frame-layout :base))
        (slots      (plist-get frame-layout :slots)))
    (cond
     ((and slot-table (assq vid slot-table))
      (let ((off (cdr (assq vid slot-table))))
        (when (and (integerp base) (integerp off))
          (+ base off))))
     ((and (integerp base) (integerp slots) (integerp vid)
           (>= vid 0) (< vid slots))
      (+ base (* vid 8)))
     (t nil))))

(defun nelisp-gc-inner--roots-from-safe-point (sp)
  "Return live root addresses contributed by safe-point SP.
Walk SP's `:live-roots' BOOL-VECTOR; for each set bit, resolve the
corresponding stack slot via `--addr-from-vid'.  Bits without a
resolvable address are dropped — see the docstring there for why."
  (let ((bv     (plist-get sp :live-roots))
        (layout (plist-get sp :frame-layout))
        (out nil))
    (when (bool-vector-p bv)
      (dotimes (vid (length bv))
        (when (aref bv vid)
          (let ((addr (nelisp-gc-inner--addr-from-vid layout vid)))
            (when (integerp addr) (push addr out))))))
    (nreverse out)))

(defun nelisp-gc-inner-collect-roots (gc-metadata-list heap-regions)
  "Build the live root set from decoded GC metadata + heap-region registry.

GC-METADATA-LIST is a list of decoded Doc 30 §1.4 plists (one per
function whose frame is currently on the stack).  HEAP-REGIONS is
the Doc 29 §1.4 registry snapshot.

For each function metadata, every safe-point's live-root bitmap
contributes addresses via `--roots-from-safe-point'.  For each
region, the descriptor itself contributes `:start' as a synthetic
root so the region table is held live across mark cycles (Doc 29
§4.2 region-table-as-root invariant).

Returns a deduplicated list of integer addresses suitable as the
ROOT-SET argument to `nelisp-gc-inner-run-mark-phase'."
  (let ((seen (make-hash-table :test 'eql))
        (out  nil))
    (cl-flet ((push-uniq (a)
                (when (and (integerp a) (not (gethash a seen)))
                  (puthash a t seen)
                  (push a out))))
      (dolist (meta gc-metadata-list)
        (dolist (sp (plist-get meta :safe-points))
          (dolist (a (nelisp-gc-inner--roots-from-safe-point sp))
            (push-uniq a))))
      (dolist (region heap-regions)
        (when (nelisp-gc-inner--region-p region)
          (push-uniq (plist-get region :start)))))
    (nreverse out)))


;;; §6. Finalizer queue (Doc 30 §6.9 v2 BLOCKER 2) ------------------

;; Bounded ring buffer.  Sweep enqueues `(THUNK . ADDR)' pairs;
;; mutator context drains them.  Overflow drops the *oldest* entry —
;; the alternative (drop-newest) silently loses the finalizer the
;; user just registered, which is harder to debug than \"my long-
;; queued finalizer never fired\".  Both are unsound by Doc 30 §6.9
;; semantics; the queue is sized to make either case rare.

(defcustom nelisp-gc-inner-finalizer-queue-capacity 1024
  "Maximum live finalizer entries between drain calls.
Set to a power of two so the ring buffer's modulus is a fast `logand'
on real hardware (Phase 7.5 lowers this from a `mod' call once the
JIT inlines the queue helper)."
  :type 'integer
  :group 'nelisp)

(defcustom nelisp-gc-inner-resurrection-policy :once
  "Doc 30 §6.9 v2 LOCK — finalizer resurrection policy.
`:once'   — finalizer fires exactly once even if its body resurrects
            the object; the entry is removed before the thunk runs
            so a re-register of the same object is a no-op for that
            cycle.
`:never'  — out of scope for Phase 7.3.1 MVP; placeholder for v2.0
            study."
  :type '(choice (const :once) (const :never))
  :group 'nelisp)

(defvar nelisp-gc-inner--finalizer-queue
  (make-vector nelisp-gc-inner-finalizer-queue-capacity nil)
  "Ring buffer storage for queued finalizers — `(FN . ADDR)' or nil.")

(defvar nelisp-gc-inner--finalizer-head 0
  "Index of the oldest queued finalizer entry (read pointer).")

(defvar nelisp-gc-inner--finalizer-tail 0
  "Index where the next enqueue will write (write pointer).")

(defvar nelisp-gc-inner--finalizer-count 0
  "Live entry count — `tail - head' modulo capacity, cached for O(1) read.")

(defvar nelisp-gc-inner--finalizer-overflow-count 0
  "How many times an enqueue dropped the oldest entry.
Exposed for the `:overflow' field of the post-drain stats plist so
operators can detect a too-small queue without introspecting state.")

(defvar nelisp-gc-inner--finalizer-fired (make-hash-table :test 'eql)
  "ADDR → t once a finalizer has fired for that address.
Backs the `:once' resurrection policy: a re-enqueue of the same
ADDR while it sits in this set is a no-op.  Cleared by
`nelisp-gc-inner-finalizer-queue-reset' between cycles when the
caller signals all references to the object are gone.")

(defun nelisp-gc-inner-finalizer-queue-reset ()
  "Reset the finalizer queue + fired-set.
Used by tests and by the post-major-GC handshake when the entire
heap is known to have been swept (so resurrection bookkeeping
restarts from scratch)."
  (setq nelisp-gc-inner--finalizer-queue
        (make-vector nelisp-gc-inner-finalizer-queue-capacity nil)
        nelisp-gc-inner--finalizer-head  0
        nelisp-gc-inner--finalizer-tail  0
        nelisp-gc-inner--finalizer-count 0
        nelisp-gc-inner--finalizer-overflow-count 0)
  (clrhash nelisp-gc-inner--finalizer-fired))

(defun nelisp-gc-inner--finalizer-queue-advance (idx)
  "Return IDX + 1 modulo the ring buffer capacity."
  (mod (1+ idx) (length nelisp-gc-inner--finalizer-queue)))

(defun nelisp-gc-inner-finalizer-queue-length ()
  "Return the number of live entries currently in the finalizer queue."
  nelisp-gc-inner--finalizer-count)

(defun nelisp-gc-inner-enqueue-finalizer (fn addr)
  "Push (FN . ADDR) onto the bounded finalizer queue.

When the queue is full the oldest entry is *dropped* and
`nelisp-gc-inner--finalizer-overflow-count' increments — Doc 30
§6.9 explicitly accepts overflow as a soft fault since the queue
sizing is operator-tunable.  The dropped entry's finalizer never
fires; this is documented as the price of bounded buffer.

Under the `:once' resurrection policy a second enqueue for an
already-fired ADDR is silently skipped.  Returns the queue length
after enqueue (or unchanged on dedup-skip)."
  (unless (functionp fn)
    (signal 'wrong-type-argument (list 'functionp fn)))
  (unless (integerp addr)
    (signal 'wrong-type-argument (list 'integerp addr)))
  (cond
   ;; :once policy — already fired, drop on the floor.
   ((and (eq nelisp-gc-inner-resurrection-policy :once)
         (gethash addr nelisp-gc-inner--finalizer-fired))
    nelisp-gc-inner--finalizer-count)
   (t
    (let ((cap (length nelisp-gc-inner--finalizer-queue)))
      ;; Overflow → drop oldest.
      (when (= nelisp-gc-inner--finalizer-count cap)
        (aset nelisp-gc-inner--finalizer-queue
              nelisp-gc-inner--finalizer-head nil)
        (setq nelisp-gc-inner--finalizer-head
              (nelisp-gc-inner--finalizer-queue-advance
               nelisp-gc-inner--finalizer-head)
              nelisp-gc-inner--finalizer-count
              (1- nelisp-gc-inner--finalizer-count))
        (cl-incf nelisp-gc-inner--finalizer-overflow-count))
      (aset nelisp-gc-inner--finalizer-queue
            nelisp-gc-inner--finalizer-tail
            (cons fn addr))
      (setq nelisp-gc-inner--finalizer-tail
            (nelisp-gc-inner--finalizer-queue-advance
             nelisp-gc-inner--finalizer-tail)
            nelisp-gc-inner--finalizer-count
            (1+ nelisp-gc-inner--finalizer-count)))
    nelisp-gc-inner--finalizer-count)))

(defun nelisp-gc-inner-drain-finalizer-queue ()
  "Run every queued finalizer in mutator context.

Doc 30 §6.9 v2: collector code never calls this — it's invoked by
the mutator after the collector lock is released, so allocation /
root mutation / resurrection inside FN are all legal.  A signal
inside one FN is caught with `condition-case' so a misbehaving
finalizer doesn't strand the rest of the queue.

Resurrection policy `:once' is enforced by recording ADDR in
`--finalizer-fired' *before* calling FN — a re-enqueue from inside
the thunk takes the dedup path documented at
`nelisp-gc-inner-enqueue-finalizer'.

Returns a stats plist:
  :fired       count of FN invocations attempted
  :errors      count of FN that signalled
  :overflow    cumulative overflow count since reset (drop-oldest)
  :remaining   queue length after drain (always 0)"
  (let ((fired 0) (errors 0))
    (while (> nelisp-gc-inner--finalizer-count 0)
      (let* ((entry (aref nelisp-gc-inner--finalizer-queue
                          nelisp-gc-inner--finalizer-head))
             (fn   (car-safe entry))
             (addr (cdr-safe entry)))
        (aset nelisp-gc-inner--finalizer-queue
              nelisp-gc-inner--finalizer-head nil)
        (setq nelisp-gc-inner--finalizer-head
              (nelisp-gc-inner--finalizer-queue-advance
               nelisp-gc-inner--finalizer-head)
              nelisp-gc-inner--finalizer-count
              (1- nelisp-gc-inner--finalizer-count))
        (when (functionp fn)
          ;; :once policy — record fired *before* calling FN so a
          ;; re-enqueue from inside the thunk dedups.
          (when (eq nelisp-gc-inner-resurrection-policy :once)
            (puthash addr t nelisp-gc-inner--finalizer-fired))
          (cl-incf fired)
          (condition-case err
              (funcall fn addr)
            (error
             (cl-incf errors)
             (message "nelisp-gc-inner: finalizer error for %S: %S"
                      addr err))))))
    (list :fired     fired
          :errors    errors
          :overflow  nelisp-gc-inner--finalizer-overflow-count
          :remaining nelisp-gc-inner--finalizer-count)))


(provide 'nelisp-gc-inner)
;;; nelisp-gc-inner.el ends here
