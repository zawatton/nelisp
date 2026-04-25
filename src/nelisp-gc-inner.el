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


;;; §7. Sweep phase — per-pool free-list rebuild (Doc 30 v2 §3.2) ----

;; T17 (Phase 7.3.1) shipped the *mark* half of mark-sweep.  T21 wires
;; the *sweep* half: after mark phase termination, every region in the
;; Doc 29 §1.4 heap-region registry is walked once, each object's
;; mark colour decides its fate:
;;
;;   :black  — live, *not* added to the free-list.
;;   :white  — unreachable garbage; (addr . size) is appended to the
;;             family-keyed free-list, and if a finalizer was
;;             registered for this address, it is *enqueued* (not
;;             fired — Doc 30 §6.9 v2 BLOCKER 2 mitigation: real
;;             execution happens later in mutator context via
;;             `nelisp-gc-inner-drain-finalizer-queue').
;;   :grey   — invariant violation; the mark phase must terminate
;;             with grey-count = 0.  We `signal' a hard error rather
;;             than silently lose objects.
;;
;; Each region's descriptor must carry an `:objects' callback (a
;; function of one argument, the region descriptor) returning a list
;; of `(ADDR . SIZE)' pairs — analogous to the `:children-of'
;; callback used by the mark phase.  The allocator (Phase 7.2.x) is
;; the producer of this callback; tests stub it directly.
;;
;; Per-family free-lists live in `nelisp-gc-inner--free-lists', a
;; hash whose keys are the Doc 29 §2.5 family symbols (`cons-pool',
;; `closure-pool', `string-span', `vector-span', `large-object').
;; The sweep phase *rebuilds* them from scratch — any leftover entry
;; from a previous cycle would be wrong, since the live/dead
;; classification is decided by the just-completed mark phase.
;;
;; Coalescing of physically-adjacent free blocks is *not* part of
;; this sub-phase; it is left to a defragmentation pass (Doc 30 §6.5
;; / §8.2 — Phase 7.5+).  The fragmentation stats reported here
;; therefore reflect the *post-sweep, pre-coalesce* state, which is
;; what Doc 30 §6.5 calls the "fragmentation high-water mark".

(defconst nelisp-gc-inner--known-families
  '(:cons-pool :closure-pool :string-span :vector-span :large-object)
  "Doc 29 §2.5 v2 object family table — keyword form.
T17 froze the consumer-side family encoding as keyword symbols
(see `--region-p').  The Phase 7.2 producer (Doc 29 allocator) uses
bare symbols (`cons-pool', etc.); the wire from producer to consumer
is bridged at `nelisp-allocator-snapshot-regions' time.  Unknown
families coming through the registry signal an error rather than
silently bypass the family-aware sweep path.")

(defvar nelisp-gc-inner--free-lists (make-hash-table :test 'eq)
  "Family symbol → list of `(ADDR . SIZE)' pairs (sweep output).
Rebuilt fresh on every `nelisp-gc-inner-run-sweep-phase' call — the
old free-list is discarded because the mark cycle that produced it
already fed the allocator and any stale entry would re-hand-out a
now-live address.")

(defun nelisp-gc-inner--reset-free-lists ()
  "Wipe `--free-lists' so a sweep cycle rebuilds from scratch."
  (clrhash nelisp-gc-inner--free-lists)
  ;; Pre-seed every known family with an empty list so downstream
  ;; callers (fragmentation stats, allocator pop) get deterministic
  ;; per-family entries even when a family had zero whites.
  (dolist (fam nelisp-gc-inner--known-families)
    (puthash fam nil nelisp-gc-inner--free-lists)))

(defun nelisp-gc-inner-free-list-for-family (family)
  "Return the current free-list for FAMILY (a list of `(ADDR . SIZE)').
Empty list when FAMILY has no whites *or* the family has never been
swept this process — the two are indistinguishable by design (and
that's the right answer: an unswept family yields nothing for the
allocator to pop)."
  (gethash family nelisp-gc-inner--free-lists))

(defun nelisp-gc-inner--region-objects (region)
  "Return the `(ADDR . SIZE)' object list for REGION.

Lookup chain mirrors `--children-of-addr' (Doc 30 §3.1):
  1. REGION's `:objects' key — preferred, used by tests directly.
  2. REGION's `:objects-of' callback — funcall on REGION; allocator-
     supplied walker pattern, parallel to `:children-of'.
  3. nil — empty region (legitimate during nursery start-up).

Each element is a `(ADDR . SIZE)' cons; SIZE in bytes.  The walker
must *not* return overlapping ranges — Doc 29 §2.5 family-aware
allocation guarantees disjointness within a single region, and the
sweep relies on it to bound the per-region work to O(objects)."
  (let ((static (plist-get region :objects)))
    (cond
     ((listp static) static)
     (t
      (let ((walker (plist-get region :objects-of)))
        (when (functionp walker)
          (funcall walker region)))))))

(defun nelisp-gc-inner--sweep-region (region &optional finalizer-table)
  "Sweep one heap region, returning a per-region stats plist.

REGION is a Doc 29 §1.4 plist descriptor (matches `--region-p').
FINALIZER-TABLE — optional `(addr . fn)' alist *or* hash-table; whites
that have a registered finalizer are pushed onto the queue via
`nelisp-gc-inner-enqueue-finalizer'.

Returns:
  (:family FAMILY-SYM
   :swept-count NUM-WHITES-FOUND
   :freed-bytes SUM-OF-WHITE-SIZES
   :live-count  NUM-BLACKS-FOUND
   :finalizers-queued FINALIZER-PUSHES)

Side effects: appends `(ADDR . SIZE)' for each white onto the
family's entry in `--free-lists'; calls
`nelisp-gc-inner-enqueue-finalizer' once per registered-finalizer
white.  Order of free-list entries reflects walker order; sweep
does not sort."
  (unless (nelisp-gc-inner--region-p region)
    (signal 'wrong-type-argument (list 'nelisp-gc-inner--region-p region)))
  (let ((family (plist-get region :family))
        (objects (nelisp-gc-inner--region-objects region))
        (swept 0) (freed 0) (live 0) (queued 0))
    (unless (memq family nelisp-gc-inner--known-families)
      (signal 'wrong-type-argument
              (list 'nelisp-gc-inner--known-family family)))
    (dolist (obj objects)
      (let* ((addr (car-safe obj))
             (size (or (cdr-safe obj) 0))
             (color (nelisp-gc-inner-mark-color addr)))
        (cond
         ((eq color :black)
          (cl-incf live))
         ((eq color :white)
          (cl-incf swept)
          (cl-incf freed size)
          ;; Append to free-list (prepend; sweep does not sort).
          (puthash family
                   (cons (cons addr size)
                         (gethash family nelisp-gc-inner--free-lists))
                   nelisp-gc-inner--free-lists)
          ;; Finalizer enqueue (Doc 30 §6.9 v2).
          (when finalizer-table
            (let ((fn (cond
                       ((hash-table-p finalizer-table)
                        (gethash addr finalizer-table))
                       ((listp finalizer-table)
                        (cdr-safe (assq addr finalizer-table))))))
              (when (functionp fn)
                (nelisp-gc-inner-enqueue-finalizer fn addr)
                (cl-incf queued)))))
         ((eq color :grey)
          ;; Mark phase invariant breach — fail loud rather than
          ;; silently free a still-grey object.
          (error
           "nelisp-gc-inner: sweep saw :grey addr %S in region %S (mark phase did not terminate)"
           addr (plist-get region :region-id))))))
    (list :family family
          :swept-count swept
          :freed-bytes freed
          :live-count live
          :finalizers-queued queued)))

(defun nelisp-gc-inner-run-sweep-phase (heap-regions &optional finalizer-table)
  "Run sweep phase across HEAP-REGIONS using the *current* mark state.

Pre-condition: `nelisp-gc-inner-run-mark-phase' has just run and
populated the per-address mark state (we read but do not modify it).

For every region:
  - Walk its objects via `--region-objects'.
  - :black objects stay live (no free-list entry).
  - :white objects are enqueued onto the family-keyed free-list, and
    any registered finalizer is pushed onto the finalizer queue.
  - :grey objects are an invariant violation and `error'.

FINALIZER-TABLE is forwarded to `--sweep-region' verbatim; pass nil
if there are no registered finalizers.

Returns a stats plist:
  (:swept-count   N    ;; total whites across all regions
   :freed-bytes   N    ;; sum of white sizes
   :live-count    N    ;; total blacks
   :region-count  N    ;; regions visited
   :finalizers-queued N
   :elapsed-ms    INT
   :per-family ((FAMILY . (:swept-count N :freed-bytes N
                          :live-count N :finalizers-queued N)) ...))

`per-family' is sorted in `--known-families' order so test diffs are
stable across walker output ordering."
  (nelisp-gc-inner--reset-free-lists)
  (let ((start (float-time))
        (swept 0) (freed 0) (live 0) (queued 0)
        (regions 0)
        ;; Per-family accumulator: hash of family → (swept freed live queued).
        (per-fam (make-hash-table :test 'eq)))
    (dolist (fam nelisp-gc-inner--known-families)
      (puthash fam (list 0 0 0 0) per-fam))
    (dolist (region heap-regions)
      (cl-incf regions)
      (let* ((rstats (nelisp-gc-inner--sweep-region region finalizer-table))
             (fam   (plist-get rstats :family))
             (rs    (plist-get rstats :swept-count))
             (rf    (plist-get rstats :freed-bytes))
             (rl    (plist-get rstats :live-count))
             (rq    (plist-get rstats :finalizers-queued))
             (acc   (gethash fam per-fam (list 0 0 0 0))))
        (cl-incf swept rs) (cl-incf freed rf)
        (cl-incf live rl)  (cl-incf queued rq)
        (puthash fam
                 (list (+ (nth 0 acc) rs)
                       (+ (nth 1 acc) rf)
                       (+ (nth 2 acc) rl)
                       (+ (nth 3 acc) rq))
                 per-fam)))
    (let ((per-family-out
           (mapcar (lambda (fam)
                     (let ((a (gethash fam per-fam (list 0 0 0 0))))
                       (cons fam (list :swept-count (nth 0 a)
                                       :freed-bytes (nth 1 a)
                                       :live-count  (nth 2 a)
                                       :finalizers-queued (nth 3 a)))))
                   nelisp-gc-inner--known-families)))
      (list :swept-count swept
            :freed-bytes freed
            :live-count live
            :region-count regions
            :finalizers-queued queued
            :elapsed-ms (round (* 1000.0 (- (float-time) start)))
            :per-family per-family-out))))

(defun nelisp-gc-inner--enqueue-finalizers-for-white (mark-state-or-nil
                                                     finalizer-table)
  "Standalone finalizer enqueue helper for white addresses.

Walks FINALIZER-TABLE — `(addr . fn)' alist or hash-table — and for
each address whose current mark colour is :white pushes the thunk
onto the finalizer queue.  Used by tests that exercise finalizer
plumbing without a full sweep walk; the production sweep path goes
through `--sweep-region' instead so the same address isn't double-
queued.

MARK-STATE-OR-NIL is reserved for a future API where the caller
supplies an alternative mark-state hash; nil means \"use the current
global state\" (Phase 7.3.2 only ships the nil case).  Returns the
number of finalizers actually queued."
  (when mark-state-or-nil
    (error "nelisp-gc-inner: alternate mark-state arg not implemented"))
  (let ((queued 0)
        (entries (cond
                  ((hash-table-p finalizer-table)
                   (let (acc)
                     (maphash (lambda (k v) (push (cons k v) acc))
                              finalizer-table)
                     acc))
                  ((listp finalizer-table) finalizer-table)
                  (t nil))))
    (dolist (e entries)
      (let ((addr (car-safe e))
            (fn   (cdr-safe e)))
        (when (and (integerp addr)
                   (functionp fn)
                   (eq (nelisp-gc-inner-mark-color addr) :white))
          (nelisp-gc-inner-enqueue-finalizer fn addr)
          (cl-incf queued))))
    queued))

(defun nelisp-gc-inner-rebuild-free-list-for-family (family)
  "Convenience: return the just-rebuilt free-list for FAMILY.
Equivalent to `nelisp-gc-inner-free-list-for-family' but spelled to
make the *rebuild-after-sweep* invariant explicit at call sites.
Doc 29 §2.5 family-aware: each family is independent; sweeping
cons-pool never disturbs closure-pool's free-list."
  (unless (memq family nelisp-gc-inner--known-families)
    (signal 'wrong-type-argument
            (list 'nelisp-gc-inner--known-family family)))
  (gethash family nelisp-gc-inner--free-lists))


;;; §8. Fragmentation stats (Doc 30 v2 §6.5) -------------------------

(defun nelisp-gc-inner-sweep-fragmentation-stats (&optional free-lists)
  "Compute fragmentation metrics from the rebuilt free-lists.

FREE-LISTS — optional alternative source `((FAMILY . FL) ...)';
nil means \"use the global `--free-lists'\" written by the most
recent `run-sweep-phase'.

Returns:
  (:total-free-bytes BYTES
   :total-free-blocks COUNT
   :largest-free-block BYTES   ;; 0 when no whites
   :fragmentation-ratio RATIO  ;; largest / total, 1.0 = none, 0.0 = ∞
   :per-family ((FAMILY . (:free-blocks N :largest BYTES
                          :total-bytes N)) ...))

Doc 30 §6.5 calls a ratio < 0.5 a fragmentation high-water mark
(largest block holds < half of free space) — the allocator is then
forced through small-block paths that historically hit pathological
behaviour.  This sub-phase only *measures*; defragmentation is
deferred to Phase 7.5+.

The :fragmentation-ratio convention follows SBCL precedent:
  ratio = largest_free / total_free
  - 1.0 means every free byte is in one contiguous block (ideal).
  - small ratio means many tiny blocks (pathological)."
  (let ((source (cond
                 ((null free-lists)
                  (let (acc)
                    (dolist (fam nelisp-gc-inner--known-families)
                      (push (cons fam (gethash fam nelisp-gc-inner--free-lists))
                            acc))
                    (nreverse acc)))
                 ((listp free-lists) free-lists)
                 (t (signal 'wrong-type-argument
                            (list 'listp free-lists)))))
        (total 0) (blocks 0) (largest 0)
        (per-family-out nil))
    (dolist (entry source)
      (let* ((fam (car entry))
             (fl  (cdr entry))
             (fam-bytes 0) (fam-blocks 0) (fam-largest 0))
        (dolist (b fl)
          (let ((sz (or (cdr-safe b) 0)))
            (cl-incf fam-bytes sz)
            (cl-incf fam-blocks)
            (when (> sz fam-largest) (setq fam-largest sz))))
        (cl-incf total fam-bytes)
        (cl-incf blocks fam-blocks)
        (when (> fam-largest largest) (setq largest fam-largest))
        (push (cons fam (list :free-blocks fam-blocks
                              :largest fam-largest
                              :total-bytes fam-bytes))
              per-family-out)))
    (list :total-free-bytes total
          :total-free-blocks blocks
          :largest-free-block largest
          :fragmentation-ratio (if (zerop total) 1.0
                                 (/ (float largest) (float total)))
          :per-family (nreverse per-family-out))))


;;; §9. Full-cycle helper (mark + sweep) -----------------------------

(defun nelisp-gc-inner-run-full-cycle (root-set heap-regions
                                                &optional finalizer-table)
  "Run one complete GC cycle: mark + sweep + finalizer enqueue.

ROOT-SET / HEAP-REGIONS as for `nelisp-gc-inner-run-mark-phase'.
FINALIZER-TABLE forwarded to `nelisp-gc-inner-run-sweep-phase'.

Returns the *combined* stats plist:
  (:mark   MARK-PLIST    ;; full output of run-mark-phase
   :sweep  SWEEP-PLIST   ;; full output of run-sweep-phase
   :elapsed-ms INT       ;; mark + sweep elapsed (sum)
   :grey-count 0)        ;; convenience: re-exposed from mark stats

Phase 7.3.6 (scheduler) will replace this with a stop-the-world
driver that adds safe-point handshake; for Phase 7.3.2 callers
(tests + future consumers that don't care about pause time) the
simple sequencing is enough."
  (let* ((mark  (nelisp-gc-inner-run-mark-phase  root-set heap-regions))
         (sweep (nelisp-gc-inner-run-sweep-phase heap-regions finalizer-table)))
    (list :mark mark
          :sweep sweep
          :elapsed-ms (+ (or (plist-get mark  :elapsed-ms) 0)
                         (or (plist-get sweep :elapsed-ms) 0))
          :grey-count (plist-get mark :grey-count))))


(provide 'nelisp-gc-inner)
;;; nelisp-gc-inner.el ends here
