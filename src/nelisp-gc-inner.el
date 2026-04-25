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
(require 'nelisp-allocator)
(require 'nelisp-integration)


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


;;; §10. Cheney semispace nursery copy (Doc 30 v2 §3.3) --------------

;; T24 (Phase 7.3.3) extends the inner GC with the *minor* half of the
;; Cheney generational scheme: the nursery is split into two
;; semispaces (from-space + to-space) of equal size, allocator hands
;; out from `from-space' until full, then a stop-the-world *copy*
;; phase walks the live root set and copies every reachable object
;; into `to-space'.  After copy, the spaces are *flipped* — to-space
;; becomes the new from-space for subsequent allocation.
;;
;; Cheney's elegance is that the work-list is the to-space itself: a
;; `scan-pointer' chases the `free-pointer' until they meet, at which
;; point every reachable object has been copied and (transitively)
;; scanned for outgoing pointers.  No external work-list, no
;; recursion, O(live-bytes) total.
;;
;; Forwarding pointer encoding piggy-backs on the Doc 30 v2 §2.13
;; object header: bit 62 marks the cell as forwarded, and bits 47:0
;; (formerly the size field) are repurposed to store the new address
;; in to-space.  This is the same overload SBCL's gencgc uses; the
;; reason it's safe is that forwarded objects in from-space are dead
;; data that nothing inspects size-of after the copy completes.
;;
;; Promotion (age-based tenuring per Doc 30 v2 §2.5) is *split* between
;; this sub-phase and 7.3.5: T24 only *identifies* candidates (objects
;; whose age >= threshold survive the copy and are returned in the
;; result plist's `:promotion-candidates'); the actual move-to-tenured
;; copy + age reset is the 7.3.5 deliverable.  Keeping the split lets
;; us land the copy core first and validate Cheney correctness in
;; isolation before adding the inter-generational hand-off.
;;
;; In the simulator path used by the ERTs and by Phase 7.3.x consumers
;; before the Phase 7.5 backend lands, "object" is a plist keyed by
;; `:header' (uint64) and `:fields' (list of integer addresses to
;; child objects).  This shape lines up with `--children-of-addr' in
;; §3 so the same heap-region descriptor walker idiom keeps the
;; collector decoupled from per-family concrete layout.

;;;; §10.1 Object header bit layout (Doc 30 v2 §2.13) ----------------

;; Doc 30 v2 §2.13 LOCKED layout:
;;
;;   bit 63       — mark bit (set during the major mark phase)
;;   bit 62       — forwarding bit (1 = forwarded, [47:0] holds new addr)
;;   bits 61:56   — age (6-bit, 0..63 nursery survival count)
;;   bits 55:48   — family-tag (8-bit, Doc 29 §2.5 family enum)
;;   bits 47:0    — size (when forwarding-bit=0) OR new-addr (=1)
;;
;; All offsets here are bit positions from LSB.

(defconst nelisp-gc-inner--header-mark-bit         63
  "Bit position of the major-mark bit in the object header.")
(defconst nelisp-gc-inner--header-forwarding-bit   62
  "Bit position of the forwarding bit in the object header.")
(defconst nelisp-gc-inner--header-age-shift        56
  "LSB position of the 6-bit age field in the object header.")
(defconst nelisp-gc-inner--header-age-mask         #x3F
  "Width-mask for the 6-bit age field (`age' field is 6 bits wide).")
(defconst nelisp-gc-inner--header-family-shift     48
  "LSB position of the 8-bit family-tag field in the object header.")
(defconst nelisp-gc-inner--header-family-mask      #xFF
  "Width-mask for the 8-bit family-tag field.")
(defconst nelisp-gc-inner--header-size-mask
  ;; (1 << 48) - 1 — a 48-bit address mask that fits the v2 §2.13
  ;; "address space ≤ 256 TiB" assumption used across the doc.  We
  ;; spell it out as a literal rather than `(1- (ash 1 48))' so the
  ;; constant is byte-compile-resolvable on every host (the ash form
  ;; is also fine, but the literal removes one tiny startup cost).
  #xFFFFFFFFFFFF
  "Width-mask for the 48-bit size/new-addr field (= 2^48 − 1).")

(defun nelisp-gc-inner--make-header (size family-tag age
                                          &optional mark-bit forwarding-bit)
  "Construct a Doc 30 v2 §2.13 object header (uint64 integer).

SIZE — object size in bytes (must fit in 48 bits when
       FORWARDING-BIT is nil; if non-nil, callers should pass the
       forwarded address here instead).
FAMILY-TAG — small integer, 0..255 (Doc 29 §2.5 family enum).
AGE — small integer, 0..63 (nursery survival count).
MARK-BIT — non-nil to set bit 63.
FORWARDING-BIT — non-nil to set bit 62 (and reinterpret SIZE as
                 new-addr).

Out-of-range arguments signal `wrong-type-argument' rather than
silently truncating — overflow here produces a malformed object that
would corrupt the heap walk."
  (unless (and (integerp size) (>= size 0)
               (<= size nelisp-gc-inner--header-size-mask))
    (signal 'wrong-type-argument
            (list 'nelisp-gc-inner--header-size-fits-p size)))
  (unless (and (integerp family-tag) (>= family-tag 0)
               (<= family-tag nelisp-gc-inner--header-family-mask))
    (signal 'wrong-type-argument
            (list 'nelisp-gc-inner--header-family-fits-p family-tag)))
  (unless (and (integerp age) (>= age 0)
               (<= age nelisp-gc-inner--header-age-mask))
    (signal 'wrong-type-argument
            (list 'nelisp-gc-inner--header-age-fits-p age)))
  (logior
   (if mark-bit       (ash 1 nelisp-gc-inner--header-mark-bit) 0)
   (if forwarding-bit (ash 1 nelisp-gc-inner--header-forwarding-bit) 0)
   (ash (logand age nelisp-gc-inner--header-age-mask)
        nelisp-gc-inner--header-age-shift)
   (ash (logand family-tag nelisp-gc-inner--header-family-mask)
        nelisp-gc-inner--header-family-shift)
   (logand size nelisp-gc-inner--header-size-mask)))

(defun nelisp-gc-inner--header-mark-bit-p (header)
  "Return non-nil when HEADER's major-mark bit (63) is set."
  (/= 0 (logand header (ash 1 nelisp-gc-inner--header-mark-bit))))

(defun nelisp-gc-inner--header-age (header)
  "Extract the 6-bit age field from HEADER."
  (logand (ash header (- nelisp-gc-inner--header-age-shift))
          nelisp-gc-inner--header-age-mask))

(defun nelisp-gc-inner--header-family (header)
  "Extract the 8-bit family-tag field from HEADER."
  (logand (ash header (- nelisp-gc-inner--header-family-shift))
          nelisp-gc-inner--header-family-mask))

(defun nelisp-gc-inner--header-size (header)
  "Extract the 48-bit size field from HEADER (only valid when not forwarded)."
  (logand header nelisp-gc-inner--header-size-mask))

(defun nelisp-gc-inner--forwarded-p (header)
  "Return non-nil when HEADER's forwarding bit (62) is set.
Per Doc 30 v2 §2.13 a forwarded header carries the new (to-space)
address in bits 47:0 and the original size field is irretrievable —
which is fine because the from-space cell is dead after copy."
  (/= 0 (logand header (ash 1 nelisp-gc-inner--header-forwarding-bit))))

(defun nelisp-gc-inner--forwarded-addr (header)
  "Return the to-space address encoded in a forwarded HEADER (bits 47:0).
Result is meaningful only when `--forwarded-p' returns non-nil; for a
non-forwarded header this returns the size field, which is the
documented overload semantics from Doc 30 v2 §2.13."
  (logand header nelisp-gc-inner--header-size-mask))

(defun nelisp-gc-inner--header-with-age (header new-age)
  "Return HEADER with the age field replaced by NEW-AGE (clamped 0..63)."
  (let* ((age (logand (max 0 new-age) nelisp-gc-inner--header-age-mask))
         (clear-mask (lognot
                      (ash nelisp-gc-inner--header-age-mask
                           nelisp-gc-inner--header-age-shift))))
    (logior (logand header clear-mask)
            (ash age nelisp-gc-inner--header-age-shift))))


;;;; §10.2 Semispace state -----------------------------------------

(cl-defstruct (nelisp-gc-inner--semispace
               (:constructor nelisp-gc-inner--semispace-make)
               (:copier nil))
  "Cheney semispace state for the minor GC nursery copy.

Slots:
  FROM-SPACE — Doc 29 §1.4 region descriptor of the *current allocate
               target* (the half mutator threads `bump-alloc' into).
               At minor GC entry this region is the source of live
               objects to copy.
  TO-SPACE   — Doc 29 §1.4 region descriptor of the *next cycle's*
               allocate target.  Copy phase fills it; afterwards the
               two roles flip and TO-SPACE becomes the new
               FROM-SPACE.
  SCAN-POINTER — Cheney scan pointer.  Advances object-by-object
               through TO-SPACE, scanning each object's outgoing
               pointer fields and forwarding any from-space references
               that haven't yet been forwarded.
  FREE-POINTER — Next allocate position in TO-SPACE.  Forward
               operations bump-allocate here.
  COPIED-OBJECTS — Hash from from-addr → to-addr.  In production
               this is *implicit* in the from-space header's
               forwarding bit, but the simulator's plist-shaped object
               representation has no in-place mutable header word, so
               we keep an explicit side-table that tracks the same
               forwarding semantics."
  (from-space    nil)
  (to-space      nil)
  (scan-pointer  0)
  (free-pointer  0)
  (copied-objects (make-hash-table :test 'eql)))

(defun nelisp-gc-inner-init-semispace (from-region to-region)
  "Initialize a Cheney semispace from FROM-REGION + TO-REGION.

FROM-REGION is the current nursery half (already populated with
allocations during the previous mutator phase).  TO-REGION is the
fresh half whose `[start, end)' will fill with copied live objects
during the upcoming minor GC.

Both regions must be Doc 29 §1.4 nursery descriptors of equal size
(Cheney requires symmetric semispaces) and family `:nursery-half' or
any of the per-family nursery families — Phase 7.3.3 doesn't enforce
the latter (the allocator producer is responsible for handing us a
matched pair).  We *do* assert generation `:nursery' on both.

Returns a freshly-constructed `nelisp-gc-inner--semispace' with
`scan-pointer' and `free-pointer' both initialized to TO-REGION's
`:start'."
  (unless (nelisp-gc-inner--region-p from-region)
    (signal 'wrong-type-argument
            (list 'nelisp-gc-inner--region-p from-region)))
  (unless (nelisp-gc-inner--region-p to-region)
    (signal 'wrong-type-argument
            (list 'nelisp-gc-inner--region-p to-region)))
  (unless (eq (plist-get from-region :generation) :nursery)
    (signal 'wrong-type-argument (list :nursery-region-p from-region)))
  (unless (eq (plist-get to-region   :generation) :nursery)
    (signal 'wrong-type-argument (list :nursery-region-p to-region)))
  (let ((to-start (plist-get to-region :start)))
    (nelisp-gc-inner--semispace-make
     :from-space    from-region
     :to-space      to-region
     :scan-pointer  to-start
     :free-pointer  to-start
     :copied-objects (make-hash-table :test 'eql))))

(defun nelisp-gc-inner-flip-semispace (semi)
  "Swap FROM-SPACE ↔ TO-SPACE on SEMI (post-minor-GC Cheney flip).

After a minor GC, the `to-space' is the new live nursery (everything
that survived the copy lives there); we make it the next cycle's
`from-space' for allocation, and the now-vacated old from-space
becomes the next cycle's empty to-space.  Pointers reset to the new
to-space `:start'.

Returns SEMI (mutated in place) for convenient threading."
  (let ((from (nelisp-gc-inner--semispace-from-space semi))
        (to   (nelisp-gc-inner--semispace-to-space   semi)))
    (setf (nelisp-gc-inner--semispace-from-space   semi) to)
    (setf (nelisp-gc-inner--semispace-to-space     semi) from)
    (let ((new-to-start (plist-get from :start)))
      (setf (nelisp-gc-inner--semispace-scan-pointer semi) new-to-start)
      (setf (nelisp-gc-inner--semispace-free-pointer semi) new-to-start))
    (clrhash (nelisp-gc-inner--semispace-copied-objects semi))
    semi))


;;;; §10.3 Forwarding pointer simulator ----------------------------

;; In production the forwarding pointer lives *in* the from-space cell
;; (bit 62 of its header).  In the simulator we keep an explicit
;; `from-addr → to-addr' hash on the `nelisp-gc-inner--semispace'
;; struct; semantically identical, just easier to test on plist-shaped
;; objects that don't have a single mutable header word.
;;
;; The two helpers below — `--mark-forwarded' and
;; `--lookup-forwarded' — are the simulator-side implementation of
;; the §10.1 header bit operations: callers in §10.4 should use these,
;; not the bit-manipulation helpers, until the Phase 7.5 backend
;; replaces the simulator with real mmap-allocated cells.

(defun nelisp-gc-inner--mark-forwarded (semi from-addr to-addr)
  "Record that the object at FROM-ADDR has been forwarded to TO-ADDR.

Side effect on SEMI's `copied-objects' hash.  Idempotent: a second
call with the same FROM-ADDR but a different TO-ADDR is treated as a
programming error and signals (forwarding once-only is a Cheney
invariant; if it triggered twice we'd have lost the original
to-address mid-cycle)."
  (let ((existing (gethash from-addr
                           (nelisp-gc-inner--semispace-copied-objects semi))))
    (cond
     ((null existing)
      (puthash from-addr to-addr
               (nelisp-gc-inner--semispace-copied-objects semi))
      to-addr)
     ((eql existing to-addr)
      to-addr)
     (t
      (error
       "nelisp-gc-inner: double-forward from %S → was %S now %S"
       from-addr existing to-addr)))))

(defun nelisp-gc-inner--lookup-forwarded (semi from-addr)
  "If FROM-ADDR has been forwarded on SEMI, return the to-address; else nil."
  (gethash from-addr
           (nelisp-gc-inner--semispace-copied-objects semi)))

(defun nelisp-gc-inner--forward (header to-addr)
  "Apply the Doc 30 v2 §2.13 forwarding-bit overload to HEADER.

Returns a new uint64 with bit 62 set and bits 47:0 replaced by
TO-ADDR.  Mark bit / age / family-tag are preserved verbatim — they
are still meaningful for debugging diagnostics that walk a forwarded
cell post-copy.

This is the *bit-level* helper that the production backend will call
when the forwarding pointer must live inside the cell itself; the
simulator path goes through `--mark-forwarded' / `--lookup-forwarded'
on the side-table instead.  Both forms are kept in sync so the two
expressions of the same invariant can be cross-checked from ERTs."
  (unless (and (integerp to-addr) (>= to-addr 0)
               (<= to-addr nelisp-gc-inner--header-size-mask))
    (signal 'wrong-type-argument
            (list 'nelisp-gc-inner--forwarded-addr-fits-p to-addr)))
  (let ((preserved (logand header
                           (lognot nelisp-gc-inner--header-size-mask))))
    (logior preserved
            (ash 1 nelisp-gc-inner--header-forwarding-bit)
            (logand to-addr nelisp-gc-inner--header-size-mask))))


;;;; §10.4 Promotion candidate identification (Doc 30 v2 §2.5) ------

(defcustom nelisp-gc-inner-promotion-age-threshold 2
  "Doc 30 v2 §2.5 default — survivor age that triggers tenured promotion.

An object's age increments by 1 each minor GC it survives (the copy
phase reads the from-cell age, increments, and writes it back into
the to-cell).  When the post-increment age equals or exceeds this
threshold, the object becomes a *promotion candidate*: Phase 7.3.5
will copy candidates straight into a tenured region instead of the
nursery to-space.  Phase 7.3.3 only *identifies* candidates and
returns them in the minor-GC stats plist; the actual move is the
7.3.5 deliverable."
  :type 'integer
  :group 'nelisp)

(defun nelisp-gc-inner--promotion-candidate-p (header)
  "Return non-nil when HEADER's age >= the promotion threshold."
  (>= (nelisp-gc-inner--header-age header)
      nelisp-gc-inner-promotion-age-threshold))


;;;; §10.5 Minor GC copy phase (Doc 30 v2 §3.3) --------------------

;; Simulator object representation shipping with T24:
;;
;;   (:addr ADDR :header HEADER :fields (CHILD-ADDR ...))
;;
;; The from-space `:objects' callback (parallel to the §3 / §7
;; `:children-of' / `:objects' callbacks) returns a list of these
;; plists.  T24 doesn't mutate the original from-space objects; it
;; *constructs* fresh plists for to-space and remembers the mapping
;; on `--semispace-copied-objects'.  The Phase 7.5 backend will
;; replace this with real bump-pointer mmap copies.
;;
;; The `from-space-objects-fn' callback resolves a from-addr to its
;; plist (so we can read its header / fields when transitively
;; copying).  Callers (and ERTs) supply this in `run-minor-gc'.

(defun nelisp-gc-inner--minor-copy-one
    (semi from-addr from-space-objects-fn to-space-objects-table
          promotion-candidates-acc)
  "Copy a single object FROM-ADDR into SEMI's to-space.

Cheney's invariant: this function is called only when the object has
*not* yet been forwarded (the caller checks `--lookup-forwarded'
first).  Side effects: bumps the semispace `free-pointer', records
the forwarding via `--mark-forwarded', appends a fresh plist to
TO-SPACE-OBJECTS-TABLE keyed by the new to-addr, and pushes the
to-addr onto PROMOTION-CANDIDATES-ACC when post-increment age >=
threshold.

FROM-SPACE-OBJECTS-FN — a function `(lambda (addr) PLIST-OR-NIL)'
that resolves a from-space address to its simulator object plist.

Returns the to-space address the object was copied to.  Signals if
FROM-SPACE-OBJECTS-FN returns nil (= dangling reference; caller's bug)."
  (let ((src (funcall from-space-objects-fn from-addr)))
    (unless src
      (error "nelisp-gc-inner: from-addr %S has no source object"
             from-addr))
    (let* ((header (or (plist-get src :header) 0))
           (fields (plist-get src :fields))
           (size   (or (plist-get src :size)
                       (nelisp-gc-inner--header-size header)))
           ;; Bump-allocate in to-space.  Production backend would
           ;; advance free-pointer by SIZE; the simulator advances by
           ;; SIZE so to-space addresses remain disjoint and
           ;; address-comparable.
           (to-addr (nelisp-gc-inner--semispace-free-pointer semi))
           ;; Increment age first; promotion check uses post-increment
           ;; (the standard "this is the Nth survival" reading).
           (new-age (min nelisp-gc-inner--header-age-mask
                         (1+ (nelisp-gc-inner--header-age header))))
           (new-header (nelisp-gc-inner--header-with-age header new-age))
           (to-cell (list :addr to-addr
                          :header new-header
                          :fields fields
                          :size   size)))
      (setf (nelisp-gc-inner--semispace-free-pointer semi)
            (+ to-addr (max size 1)))
      (puthash to-addr to-cell to-space-objects-table)
      (nelisp-gc-inner--mark-forwarded semi from-addr to-addr)
      (when (nelisp-gc-inner--promotion-candidate-p new-header)
        (push to-addr (car promotion-candidates-acc)))
      to-addr)))

(defun nelisp-gc-inner--minor-update-pointer
    (semi child-addr from-space-objects-fn to-space-objects-table
          promotion-candidates-acc)
  "Resolve CHILD-ADDR to its post-copy address, copying if needed.

Returns the to-space address the child now lives at.  If CHILD-ADDR
points outside the from-space (e.g. into a tenured region or a
foreign root), it is returned unchanged — the minor GC walk does not
chase tenured edges (Doc 30 v2 §3.4 write-barrier territory)."
  (let ((from (nelisp-gc-inner--semispace-from-space semi)))
    (cond
     ((or (null child-addr)
          (not (integerp child-addr))
          (not (nelisp-gc-inner--region-contains from child-addr)))
      child-addr)
     ((nelisp-gc-inner--lookup-forwarded semi child-addr))
     (t
      (nelisp-gc-inner--minor-copy-one
       semi child-addr from-space-objects-fn to-space-objects-table
       promotion-candidates-acc)))))

(defun nelisp-gc-inner-run-minor-gc (semi root-set from-space-objects-fn)
  "Cheney minor GC: copy live nursery objects from FROM-SPACE → TO-SPACE.

SEMI — `nelisp-gc-inner--semispace' from `init-semispace'.
ROOT-SET — list of integer addresses (mutator roots, output of
           `nelisp-gc-inner-collect-roots').
FROM-SPACE-OBJECTS-FN — `(lambda (addr) → object-plist)' walker.

Algorithm (Cheney 1970):
  1. For each root in ROOT-SET, if it points into from-space:
     forward it (copy if not yet forwarded; otherwise look up the
     existing to-addr).
  2. Scan loop: while scan-pointer < free-pointer, read the to-space
     object at scan-pointer, walk its `:fields', and update each
     pointer via `--minor-update-pointer'.  Advance scan-pointer past
     the object.
  3. Bump scan-pointer until it catches free-pointer — Cheney's
     termination condition (no unscanned live objects remain).

Returns a stats plist:
  (:copied-count N
   :copied-bytes N
   :promotion-candidates (TO-ADDR ...)
   :forwarded-roots (TO-ADDR ...)   ;; updated root addresses
   :scan-pointer SCAN
   :free-pointer FREE
   :elapsed-ms INT)

Note: SEMI is *not* flipped here — caller invokes
`flip-semispace' explicitly when ready to recycle from-space."
  (unless (nelisp-gc-inner--semispace-p semi)
    (signal 'wrong-type-argument (list 'nelisp-gc-inner--semispace-p semi)))
  (unless (functionp from-space-objects-fn)
    (signal 'wrong-type-argument (list 'functionp from-space-objects-fn)))
  (let* ((start (float-time))
         (to-objects (make-hash-table :test 'eql))
         (promo-acc (list nil))   ; one-element box, mutated by --minor-copy-one
         (forwarded-roots nil)
         (initial-free (nelisp-gc-inner--semispace-free-pointer semi))
         (copied-count 0))
    ;; Step 1: forward roots.
    (dolist (root root-set)
      (let ((new-addr (nelisp-gc-inner--minor-update-pointer
                       semi root from-space-objects-fn to-objects promo-acc)))
        (push new-addr forwarded-roots)))
    ;; Step 2: Cheney scan loop.  We walk the to-space object table in
    ;; insertion order — `--minor-copy-one' assigns monotonically
    ;; increasing to-addrs by bumping the free-pointer, so the natural
    ;; iteration order through `to-objects' (sorted by addr) matches
    ;; Cheney's "scan-pointer chases free-pointer" invariant.
    (let ((scanned (make-hash-table :test 'eql)))
      (cl-block scan-loop
        (while t
          (let ((next-unscanned nil))
            ;; Find the lowest to-addr that hasn't been scanned yet —
            ;; cheap because `to-objects' grows append-only and Cheney
            ;; never revisits an already-scanned cell.
            (maphash
             (lambda (addr _cell)
               (unless (gethash addr scanned)
                 (when (or (null next-unscanned)
                           (< addr next-unscanned))
                   (setq next-unscanned addr))))
             to-objects)
            (unless next-unscanned
              (cl-return-from scan-loop nil))
            (setf (nelisp-gc-inner--semispace-scan-pointer semi)
                  next-unscanned)
            (let* ((cell (gethash next-unscanned to-objects))
                   (fields (plist-get cell :fields))
                   (new-fields nil))
              (dolist (child-addr fields)
                (let ((updated (nelisp-gc-inner--minor-update-pointer
                                semi child-addr from-space-objects-fn
                                to-objects promo-acc)))
                  (push updated new-fields)))
              ;; Replace the cell's :fields with the post-update list
              ;; so callers reading to-space see the rewired pointers.
              (let ((new-cell (copy-sequence cell)))
                (plist-put new-cell :fields (nreverse new-fields))
                (puthash next-unscanned new-cell to-objects))
              (puthash next-unscanned t scanned)
              (cl-incf copied-count))))))
    ;; Bookkeeping: record final pointers + return stats.
    (setf (nelisp-gc-inner--semispace-scan-pointer semi)
          (nelisp-gc-inner--semispace-free-pointer semi))
    (let ((copied-bytes
           (- (nelisp-gc-inner--semispace-free-pointer semi) initial-free)))
      (list :copied-count copied-count
            :copied-bytes copied-bytes
            :promotion-candidates (nreverse (car promo-acc))
            :forwarded-roots (nreverse forwarded-roots)
            :scan-pointer (nelisp-gc-inner--semispace-scan-pointer semi)
            :free-pointer (nelisp-gc-inner--semispace-free-pointer semi)
            :to-space-objects to-objects
            :elapsed-ms (round (* 1000.0 (- (float-time) start)))))))


;;;; §10.6 Cycle dispatch (minor / major) --------------------------

(defun nelisp-gc-inner-run-cycle (cycle-kind &rest args)
  "Dispatch a GC cycle by CYCLE-KIND.

Supported kinds:
  :minor — `(:minor SEMI ROOT-SET FROM-SPACE-OBJECTS-FN)'.  Forwards
           to `nelisp-gc-inner-run-minor-gc' and returns its plist
           tagged with `:kind :minor'.
  :major — `(:major ROOT-SET HEAP-REGIONS [FINALIZER-TABLE])'.
           Forwards to `nelisp-gc-inner-run-full-cycle' and returns
           its plist tagged with `:kind :major'.

Returns the wrapped result plist; callers can demultiplex by
`(plist-get RESULT :kind)' without re-dispatching on argument
shape."
  (cond
   ((eq cycle-kind :minor)
    (let ((semi (nth 0 args))
          (root-set (nth 1 args))
          (objs-fn (nth 2 args)))
      (let ((res (nelisp-gc-inner-run-minor-gc semi root-set objs-fn)))
        (cons :kind (cons :minor res)))))
   ((eq cycle-kind :major)
    (let ((root-set (nth 0 args))
          (heap-regions (nth 1 args))
          (finalizer-table (nth 2 args)))
      (let ((res (nelisp-gc-inner-run-full-cycle
                  root-set heap-regions finalizer-table)))
        (cons :kind (cons :major res)))))
   (t
    (signal 'wrong-type-argument
            (list 'nelisp-gc-inner--cycle-kind-p cycle-kind)))))


;;;; §11. Phase 7.3.4 — write barrier (card-marking 4KB page) -------
;;
;; *Doc 30 v2 LOCKED §2.6 + §3.4 + §6.7 W6 replan gate ready.*
;;
;; The card-marking write barrier is the cross-generation pointer
;; tracking primitive that lets the *young-root-limited* minor GC mode
;; (Phase 7.3.5 default) avoid scanning the *entire* tenured region for
;; tenured → nursery edges on every minor cycle.  Instead we record, at
;; *write time*, which 4KB-aligned pages of the tenured region have
;; been mutated to contain a pointer into the nursery; the minor GC
;; then walks only those *dirty cards* to extend its root set with
;; cross-generation references.
;;
;; *Doc 29 §6.7 generational interim invariant* (Phase 7.2 in flight):
;;   "minor GC is *not* triggered while Phase 7.2 is unfinished, and
;;    tenured → nursery edges are forbidden by the allocator until
;;    Phase 7.3.4 ships the card-marking barrier."
;; This sub-phase *activates* the second half of that invariant — once
;; the barrier exists, the allocator may again allow tenured → nursery
;; writes (the barrier records them; Phase 7.3.5 minor GC consumes the
;; dirty bits).  Phase 7.3.4 itself does not yet wire the activation
;; (that happens when 7.3.5 lands and flips the default minor-GC mode).
;;
;; *Granularity choice (§2.6 recommendation A)*: 4KB == OS page size,
;; matching Doc 29 §2.3 page granularity.  For a 64MB tenured region
;; that's 16384 cards; we use a `bool-vector' (1 bit/card, ~16KB
;; metadata for 64MB tenured = 0.025% overhead).  Production may swap
;; to a `byte-vector' if per-card aging or reference-count info is
;; needed; the *simulator* sticks to bool-vector for predictability and
;; is the contract Phase 7.5 native code will replicate.
;;
;; *Out of scope for 7.3.4* (covered by later sub-phases):
;;   - promotion (age policy, tenured age table) → 7.3.5
;;   - young-root-limited minor GC mode flip      → 7.3.5
;;   - scheduler / safe-point poll plumbing       → 7.3.6
;;   - Phase 7.1 native JIT inline of the barrier → Phase 7.5

(defcustom nelisp-gc-inner-card-size (* 4 1024)
  "Card size in bytes for the write barrier (Doc 30 v2 §2.6, default 4KB).

Cards are the granularity at which `nelisp-gc-inner-write-barrier'
records dirty pages.  4KB == OS page size on every host NeLisp targets
(x86_64 / arm64 with default mmap) so card boundaries land naturally
on mprotect-aligned pages, and Phase 7.5 native code can collapse the
`addr / card-size' division into a single right-shift instruction.

Changing this at runtime invalidates every existing card table; the
allocator's region-table reset path must re-init each card table to
the new size.  Phase 7.3.4 does not enforce this — production should
treat it as a constant, set via `nelisp-runtime --gc-card-size=' once
at startup."
  :type 'integer
  :group 'nelisp-gc-inner)

(cl-defstruct (nelisp-gc-inner--card-table
               (:constructor nelisp-gc-inner--card-table-make)
               (:copier nil))
  "Card-marking dirty-bit table covering one heap region.

Slots:
  REGION       — Doc 29 §1.4 region descriptor this table covers
                 (typically the tenured region).
  DIRTY-BITS   — `bool-vector' of length CARD-COUNT.  Each bit is t
                 iff the corresponding 4KB page has had at least one
                 cross-generation write since the last
                 `clear-card-table' call.
  CARD-COUNT   — number of cards = ceil((REGION-END - REGION-START)
                 / nelisp-gc-inner-card-size).
  CARD-SIZE    — snapshot of `nelisp-gc-inner-card-size' at
                 init time.  Stored on the struct so a runtime change
                 to the defcustom doesn't silently invalidate
                 already-built tables.
  REGION-START — copy of REGION's :start.  Cached so the barrier hot
                 path doesn't dereference the plist on every write
                 (Phase 7.5 native code needs a register-resident
                 base address).
  REGION-END   — copy of REGION's :end.  Symmetric reason."
  (region nil)
  (dirty-bits nil)
  (card-count 0)
  (card-size 0)
  (region-start 0)
  (region-end 0))

(defun nelisp-gc-inner-init-card-table (region)
  "Initialize a card table covering REGION (Doc 29 §1.4 descriptor).

REGION must be a region descriptor with `:start' / `:end' integer
keys.  The returned `nelisp-gc-inner--card-table' has every dirty bit
clear (i.e. no cross-generation writes recorded yet).

Card count is `ceil((end - start) / card-size)' so a region whose
size isn't a multiple of `card-size' still gets a final partial card
covering the tail bytes.  An empty region (size 0) yields a 0-card
table — the barrier becomes a no-op for it, which is the natural
identity element."
  (unless (nelisp-gc-inner--region-p region)
    (signal 'wrong-type-argument
            (list 'nelisp-gc-inner--region-p region)))
  (let* ((start (plist-get region :start))
         (end   (plist-get region :end))
         (size  (max 0 (- end start)))
         (card-size nelisp-gc-inner-card-size)
         ;; ceil(size / card-size).  The (+ size (1- card-size)) trick
         ;; works for size >= 0 because integer division floors towards
         ;; zero in Elisp's `/' on non-negative operands.
         (card-count (if (zerop size)
                         0
                       (/ (+ size (1- card-size)) card-size)))
         (dirty (make-bool-vector card-count nil)))
    (nelisp-gc-inner--card-table-make
     :region       region
     :dirty-bits   dirty
     :card-count   card-count
     :card-size    card-size
     :region-start start
     :region-end   end)))

(defun nelisp-gc-inner--card-of (card-table addr)
  "Return the card index in CARD-TABLE that covers ADDR.

ADDR must fall inside the table's `[region-start, region-end)' window;
otherwise this returns nil so callers can distinguish out-of-range
writes (which the barrier ignores silently — they cannot be cross-
generation pointers *into* the table's covered region)."
  (when (and (integerp addr)
             (>= addr (nelisp-gc-inner--card-table-region-start card-table))
             (<  addr (nelisp-gc-inner--card-table-region-end   card-table)))
    (/ (- addr (nelisp-gc-inner--card-table-region-start card-table))
       (nelisp-gc-inner--card-table-card-size card-table))))

(defun nelisp-gc-inner--addr-in-region-p (region addr)
  "Return non-nil when ADDR falls inside REGION's `[:start, :end)' window.

Helper shared by the write barrier and remembered-set scan; equivalent
to `--region-contains' but accepts a *plist* region (the same shape
the card table caches via `region-start' / `region-end' slots).  We
keep both helpers because callers in §10 are wired to the plist form
and rewriting them would touch the T17/T21/T24 freeze."
  (and (integerp addr)
       (integerp (plist-get region :start))
       (integerp (plist-get region :end))
       (>= addr (plist-get region :start))
       (<  addr (plist-get region :end))))

(defun nelisp-gc-inner-write-barrier
    (card-table addr written-value &optional nursery-region)
  "Card-marking write barrier — record cross-generation writes.

This is the primary mutator hook the Phase 7.1 native JIT inlines (per
§2.6 recommendation A: 1-2 instructions on x86_64 — RAX-relative
shift + bts).  The Elisp simulator path runs ~30ns/call which is
*not* hot-path-grade but is fine for ERT correctness checks.

Algorithm:
  1. card-index = (ADDR - region-start) / card-size
     ↳ if ADDR is outside CARD-TABLE's region, return nil (no-op —
       this address isn't in the generation we're tracking).
  2. if WRITTEN-VALUE is an integer that points *into* NURSERY-REGION,
     the write creates a cross-generation reference: mark
     dirty-bits[card-index] = t.
  3. Otherwise the write either replaces a pointer with a non-pointer,
     or stays within the same generation — *no* dirty bit is set.

Returning t indicates `dirtied'; nil indicates `no-op' (out of range
or intra-generation).  The return value is mostly diagnostic; the
mutator typically ignores it.

NURSERY-REGION is optional: when nil, the barrier is reduced to its
`unconditional dirty' degenerate (every in-range write dirties its
card).  This preserves correctness — a superset of the precise dirty
set still gets walked by Phase 7.3.5's remembered-set scan — at the
cost of remembered-set scan throughput.  We expose it because the
allocator may call `write-barrier' before `nursery-region' is fully
materialized at boot, and because some debug builds want to verify
the precise-vs-conservative approximation invariant."
  (unless (nelisp-gc-inner--card-table-p card-table)
    (signal 'wrong-type-argument
            (list 'nelisp-gc-inner--card-table-p card-table)))
  (let ((card (nelisp-gc-inner--card-of card-table addr)))
    (cond
     ;; ADDR not in this card table's region — no-op.  This is the
     ;; common case for intra-nursery writes when the table covers
     ;; tenured: the barrier sees them but has no card to mark.
     ((null card) nil)
     ;; No nursery boundary specified → conservative dirty.
     ((null nursery-region)
      (aset (nelisp-gc-inner--card-table-dirty-bits card-table) card t)
      t)
     ;; WRITTEN-VALUE points into nursery → cross-generation, dirty.
     ((nelisp-gc-inner--addr-in-region-p nursery-region written-value)
      (aset (nelisp-gc-inner--card-table-dirty-bits card-table) card t)
      t)
     ;; WRITTEN-VALUE doesn't point into nursery → intra-generation
     ;; (or non-pointer), don't dirty.
     (t nil))))

(defun nelisp-gc-inner-clear-card-table (card-table)
  "Reset every dirty bit in CARD-TABLE to nil.

Called after Phase 7.3.5 minor GC + promotion: once the remembered
set has been scanned and any tenured → nursery pointers have been
forwarded into to-space, the card table no longer reflects live
cross-generation edges.  Subsequent mutator writes will repopulate it.

Returns CARD-TABLE (mutated in place) for threading."
  (unless (nelisp-gc-inner--card-table-p card-table)
    (signal 'wrong-type-argument
            (list 'nelisp-gc-inner--card-table-p card-table)))
  (let ((bv (nelisp-gc-inner--card-table-dirty-bits card-table)))
    (fillarray bv nil))
  card-table)

(defun nelisp-gc-inner-card-table-stats (card-table)
  "Return a stats plist describing CARD-TABLE state (Doc 30 v2 §2.11).

Plist shape:
  (:total-cards N
   :dirty-cards N
   :dirty-percent FLOAT      ;; 0.0..100.0
   :region-start ADDR
   :region-end   ADDR
   :card-size BYTES)

`:dirty-percent' is a `float' for non-integer ratios; an empty table
(card-count = 0) reports `:dirty-percent 0.0' as the natural identity."
  (unless (nelisp-gc-inner--card-table-p card-table)
    (signal 'wrong-type-argument
            (list 'nelisp-gc-inner--card-table-p card-table)))
  (let* ((bv (nelisp-gc-inner--card-table-dirty-bits card-table))
         (total (nelisp-gc-inner--card-table-card-count card-table))
         (dirty 0))
    (dotimes (i total)
      (when (aref bv i) (cl-incf dirty)))
    (list :total-cards   total
          :dirty-cards   dirty
          :dirty-percent (if (zerop total)
                             0.0
                           (* 100.0 (/ (float dirty) (float total))))
          :region-start  (nelisp-gc-inner--card-table-region-start card-table)
          :region-end    (nelisp-gc-inner--card-table-region-end   card-table)
          :card-size     (nelisp-gc-inner--card-table-card-size    card-table))))


;;;; §11.2 remembered-set scan -------------------------------------
;;
;; The remembered set is the *output* of the card table — for each
;; dirty card we walk the objects whose addresses fall on that card and
;; report any pointer fields that target the nursery.  Phase 7.3.5
;; minor GC unions this with its base root set and forwards the lot.
;;
;; The scanner is *Phase 7.3.4* not *Phase 7.3.5* because the barrier
;; would be useless without a way to consume its output, and shipping
;; both halves together lets the ERT suite verify barrier ↔ scanner
;; round-trip correctness independent of the minor GC integration that
;; comes later.
;;
;; *Implementation strategy*: the scanner accepts an `objects-fn'
;; callback identical to the `from-space-objects-fn' contract used by
;; `--minor-update-pointer' (§10.5).  This keeps the simulator
;; architecture uniform — the test suite already builds these
;; callbacks for Cheney tests, so remembered-set tests reuse the same
;; harness.

(defun nelisp-gc-inner--objects-on-card (card-table card-index objects)
  "Return OBJECTS (alist `(addr . plist)') whose ADDR falls on CARD-INDEX.

`addr' is on CARD-INDEX iff
  (addr - region-start) / card-size == CARD-INDEX
which is equivalent to
  card-start <= addr < card-start + card-size
where card-start = region-start + CARD-INDEX * card-size."
  (let* ((card-size   (nelisp-gc-inner--card-table-card-size    card-table))
         (region-start (nelisp-gc-inner--card-table-region-start card-table))
         (card-start  (+ region-start (* card-index card-size)))
         (card-end    (+ card-start card-size))
         (out nil))
    (dolist (entry objects)
      (let ((addr (car entry)))
        (when (and (>= addr card-start) (< addr card-end))
          (push entry out))))
    (nreverse out)))

(defun nelisp-gc-inner-scan-remembered-set
    (card-table nursery-region objects-fn)
  "Walk CARD-TABLE's dirty cards; return per-card cross-generation pointers.

Arguments:
  CARD-TABLE     — `nelisp-gc-inner--card-table' from `init-card-table'.
  NURSERY-REGION — Doc 29 §1.4 region descriptor; pointers into this
                   region's `[:start :end)' are the cross-generation
                   edges we report.
  OBJECTS-FN     — `(lambda () → ALIST)' or `(lambda () → LIST)' that
                   yields the *current* objects covered by the card
                   table.  Each entry is `(ADDR . PLIST)' with PLIST's
                   `:fields' carrying outgoing pointer addresses.
                   Phase 7.3.5 will swap this for a real region walker
                   that consults the allocator's per-page object index.

Returns a list of `(CARD-INDEX . NURSERY-POINTERS)' cells, one per
dirty card that yields at least one nursery pointer.  Cards that are
dirty but contain *no* nursery pointers (false positives — e.g. the
mutator overwrote a pointer with a non-pointer between barrier and
scan) are silently dropped.

A clean card-table yields nil.  A dirty card with no nursery edges
is dropped (so the result is the *minimum* over-approximation of the
true cross-generation root set), which is what Phase 7.3.5 consumes
without filtering."
  (unless (nelisp-gc-inner--card-table-p card-table)
    (signal 'wrong-type-argument
            (list 'nelisp-gc-inner--card-table-p card-table)))
  (unless (functionp objects-fn)
    (signal 'wrong-type-argument (list 'functionp objects-fn)))
  (let* ((bv (nelisp-gc-inner--card-table-dirty-bits card-table))
         (total (nelisp-gc-inner--card-table-card-count card-table))
         (objects (funcall objects-fn))
         (out nil))
    (dotimes (i total)
      (when (aref bv i)
        (let ((card-objs (nelisp-gc-inner--objects-on-card
                          card-table i objects))
              (ptrs nil))
          (dolist (entry card-objs)
            (let ((fields (plist-get (cdr entry) :fields)))
              (dolist (field fields)
                (when (nelisp-gc-inner--addr-in-region-p
                       nursery-region field)
                  (push field ptrs)))))
          (when ptrs
            (push (cons i (nreverse ptrs)) out)))))
    (nreverse out)))


;;;; §11.3 minor GC + barrier integration --------------------------
;;
;; The minor GC entry point in §10.5 (`run-minor-gc') accepts a
;; `root-set' caller-supplied list.  In *young-root-limited* mode
;; (Phase 7.3.5 default) that root set is incomplete — it covers the
;; mutator's known roots but *not* the cross-generation edges from
;; tenured.  `run-minor-gc-with-barrier' is the thin wrapper that
;; extends the root set with the remembered-set scan output before
;; delegating to `run-minor-gc'.
;;
;; This wrapper *also* clears the card table after the GC cycle, since
;; the cross-generation edges it records have either been forwarded
;; (now intra-tenured + tenured-to-tenured) or the source object has
;; been promoted (now tenured-to-tenured by definition).  Either way
;; the previously-dirty cards no longer reflect live cross-generation
;; references and the bits should reset.

(defun nelisp-gc-inner-run-minor-gc-with-barrier
    (semi card-table root-set from-space-objects-fn
          tenured-objects-fn)
  "Run minor GC with CARD-TABLE-augmented root set.

Pipeline:
  1. Scan CARD-TABLE for cross-generation pointers via
     `scan-remembered-set' (TENURED-OBJECTS-FN supplies the tenured
     objects).
  2. Flatten the result into a list of nursery addresses and union
     with ROOT-SET (deduplicated).
  3. Delegate to `run-minor-gc' with the extended root set.
  4. Clear CARD-TABLE — every cross-generation edge it recorded has
     either been forwarded to to-space (then promoted, in 7.3.5) or
     dropped (the source pointer was overwritten with a non-pointer).

Arguments:
  SEMI                  — `nelisp-gc-inner--semispace' (init-semispace
                          output).
  CARD-TABLE            — `nelisp-gc-inner--card-table' covering the
                          tenured region.
  ROOT-SET              — list of integer mutator-root addresses.
  FROM-SPACE-OBJECTS-FN — `(lambda (addr) → object-plist)' for the
                          nursery (passed through to `run-minor-gc').
  TENURED-OBJECTS-FN    — `(lambda () → ALIST)' yielding the tenured
                          region's `(addr . plist)' entries; used for
                          the remembered-set scan.

Returns the plist from `run-minor-gc' augmented with:
  :cross-gen-roots N   — count of nursery pointers added from the
                         remembered set
  :card-table-cleared t

Side effects: CARD-TABLE is cleared in place after the GC cycle."
  (unless (nelisp-gc-inner--card-table-p card-table)
    (signal 'wrong-type-argument
            (list 'nelisp-gc-inner--card-table-p card-table)))
  (let* ((nursery-region (nelisp-gc-inner--semispace-from-space semi))
         (rem-set (nelisp-gc-inner-scan-remembered-set
                   card-table nursery-region tenured-objects-fn))
         (cross-gen-ptrs (apply #'append (mapcar #'cdr rem-set)))
         ;; Union root-set with cross-gen-ptrs, preserving order +
         ;; deduplicating.  We append in *that* order so explicit
         ;; mutator roots remain at the head — useful for promotion
         ;; tie-breaking later (a mutator root reaches its target
         ;; before a remembered-set forwarding does).
         (extended-roots (cl-remove-duplicates
                          (append root-set cross-gen-ptrs)
                          :test #'eql :from-end t))
         (stats (nelisp-gc-inner-run-minor-gc
                 semi extended-roots from-space-objects-fn)))
    (nelisp-gc-inner-clear-card-table card-table)
    (append stats
            (list :cross-gen-roots    (length cross-gen-ptrs)
                  :card-table-cleared t))))


;;;; §12. Phase 7.3.5 — promotion + age policy + young-root-limited mode
;;
;; *Doc 30 v2 LOCKED §3.5 + §2.5 + §0 BLOCKER 1 解消.*
;;
;; T29 (Phase 7.3.5) wires the *promotion* half of the generational scheme
;; that T24 (7.3.3) only *identified*: when a nursery survivor's age
;; crosses `nelisp-gc-inner-promotion-age-threshold' (default 2), the
;; nursery copy is followed by a *second* copy that moves the live cell
;; from to-space into a tenured region via the allocator's
;; `nelisp-allocator-tenured-alloc' API.  The forwarding side-table on
;; `nelisp-gc-inner--semispace' is updated so subsequent root walks see
;; the tenured address rather than the (now-dead) to-space address.
;;
;; Sub-phase 7.3.5 also flips the *default* major-GC mode from full
;; mark-sweep (T21, ~500ms-1s pause for a 64MB tenured) to
;; *young-root-limited* (Doc 30 v2 §0 BLOCKER 1 mitigation, user signoff
;; 2026-04-25): only dirty cards from the write barrier (T26) seed the
;; mark phase + only objects on dirty cards are swept.  Pause-time
;; envelope is 50ms — the cost of skipping the tenured cold pages is
;; that *non-cross-gen* dead tenured objects aren't reclaimed until
;; the next *full* major (opt-in via `nelisp-gc-major-mode = 'full').
;;
;; Layering with previous sub-phases:
;;   - T17 (7.3.1) provides the mark phase + tri-color state.
;;   - T21 (7.3.2) provides per-pool free-list rebuild (full sweep).
;;   - T24 (7.3.3) provides Cheney semispace + minor copy.
;;   - T26 (7.3.4) provides write barrier + remembered-set scan +
;;     `run-minor-gc-with-barrier' wrapper.
;;   - T29 (7.3.5, this sub-phase) wires *promotion* + *age policy* +
;;     *young-root-limited major mode* + a v2 cycle dispatcher.
;;
;; Out of scope for 7.3.5 (covered by 7.3.6):
;;   - scheduler / allocation-triggered cycle entry
;;   - safe-point handshake plumbing
;;   - finalizer queue executor (the *queue* itself is in 7.3.1; the
;;     mutator-context drain executor lands in 7.3.6)

;;;; §12.1 Major mode defcustom (Doc 30 v2 §0 BLOCKER 1 解消) -------

(defcustom nelisp-gc-major-mode 'young-root-limited
  "Major GC mode (Doc 30 v2 §0, user signoff 2026-04-25 default switch).

`young-root-limited' (DEFAULT, ~50ms pause) — card-table dirty page
                only scan, interactive UX target.  Roots = mutator stack
                roots ∪ remembered-set cross-gen pointers; sweep walks
                only objects whose addresses fall on dirty cards.  Cold
                tenured garbage accumulates until a full cycle runs.

`full' (opt-out, ~500ms-1s pause) — stop-the-world full mark-sweep
                across every heap region.  Used for `gc-stress' bench
                and explicit `(let ((nelisp-gc-major-mode \\='full))
                ...)' contexts.  Phase 7.5 daemon profile may schedule
                this hourly during idle windows."
  :type '(choice (const :tag "Young-root-limited (default, ~50ms)" young-root-limited)
                 (const :tag "Full mark-sweep (~500ms-1s)"            full))
  :group 'nelisp-gc-inner)


;;;; §12.2 Age policy helpers (Doc 30 v2 §2.5) ---------------------

(defun nelisp-gc-inner-age-tick (to-space-objects-table)
  "Increment the age field of every object in TO-SPACE-OBJECTS-TABLE.

TO-SPACE-OBJECTS-TABLE is the `:to-space-objects' hash returned by
`run-minor-gc' — `(addr → cell-plist)' with each plist carrying a
`:header' uint64.  Each header's 6-bit age field (Doc 30 v2 §2.13
bits 61:56) is bumped by 1, saturating at 63.

This helper is *idempotent-aware*: `run-minor-gc' already increments
age once during `--minor-copy-one' (the in-line bump that decides
promotion candidacy).  Callers that want a *separate* age-tick step —
e.g. for per-cycle survivor aging policies that don't piggyback on
the copy itself, or to re-age objects that were copied with their
existing age preserved — invoke this explicitly.  In the current
Phase 7.3.5 pipeline this is not part of the default cycle (the
implicit copy bump is enough); the function is exposed so future
modes (incremental / concurrent collectors) can stage age policy
separately from the copy.

Returns the count of cells whose age was bumped (those at age < 63
before the call).  Cells already at age 63 are saturated and the
saturation is recorded in the count separately as the *return cell*
of every call (no-ops still report `:saturated-count').

Result plist:
  (:bumped-count N
   :saturated-count N
   :total-count N)"
  (unless (hash-table-p to-space-objects-table)
    (signal 'wrong-type-argument
            (list 'hash-table-p to-space-objects-table)))
  (let ((bumped 0)
        (saturated 0)
        (total 0))
    (maphash
     (lambda (addr cell)
       (cl-incf total)
       (let* ((header (or (plist-get cell :header) 0))
              (age (nelisp-gc-inner--header-age header)))
         (cond
          ((>= age nelisp-gc-inner--header-age-mask)
           (cl-incf saturated))
          (t
           (let ((new-cell (copy-sequence cell)))
             (plist-put new-cell :header
                        (nelisp-gc-inner--header-with-age header (1+ age)))
             (puthash addr new-cell to-space-objects-table)
             (cl-incf bumped))))))
     to-space-objects-table)
    (list :bumped-count    bumped
          :saturated-count saturated
          :total-count     total)))

(defun nelisp-gc-inner-collect-promotion-candidates (to-space-objects-table)
  "Walk TO-SPACE-OBJECTS-TABLE, return promotion candidate descriptors.

Each candidate is an entry whose header age is at or above
`nelisp-gc-inner-promotion-age-threshold'.  Returns a list of
`(ADDR . (FAMILY . SIZE))' triples in ascending ADDR order so the
caller can replay them deterministically (test diffs stable).

FAMILY is decoded via the family-tag enum (Doc 29 §2.5):
  0 → :cons-pool, 1 → :closure-pool, 2 → :string-span,
  3 → :vector-span, 4 → :large-object.  Unknown tags fall back to
`:cons-pool' (the dominant family) with a backtrace-safe path so
callers don't need to handle nil.

SIZE is read from the cell's `:size' slot if present, else from the
header's 48-bit size field — both must agree in practice; the cell
slot wins to keep the simulator independent of header packing tests."
  (unless (hash-table-p to-space-objects-table)
    (signal 'wrong-type-argument
            (list 'hash-table-p to-space-objects-table)))
  (let ((acc nil))
    (maphash
     (lambda (addr cell)
       (let* ((header (or (plist-get cell :header) 0))
              (age (nelisp-gc-inner--header-age header)))
         (when (>= age nelisp-gc-inner-promotion-age-threshold)
           (let* ((family-tag (nelisp-gc-inner--header-family header))
                  (family (nelisp-gc-inner--family-tag-to-keyword family-tag))
                  (size (or (plist-get cell :size)
                            (nelisp-gc-inner--header-size header))))
             (push (cons addr (cons family size)) acc)))))
     to-space-objects-table)
    ;; Sort ascending by addr for deterministic test ordering.
    (sort acc (lambda (a b) (< (car a) (car b))))))

(defconst nelisp-gc-inner--family-tag-keyword-alist
  '((0 . :cons-pool)
    (1 . :closure-pool)
    (2 . :string-span)
    (3 . :vector-span)
    (4 . :large-object))
  "Bridge: 8-bit family-tag (Doc 29 §2.5 enum) → gc-inner keyword family.

The allocator (`nelisp-allocator--family-tag-alist') stores the
inverse mapping in *bare* symbols; this alist is the keyword-namespace
mirror used by `--family-tag-to-keyword' so the gc-inner side can
translate decoded headers without depending on the allocator at load
time.  Phase 7.5 cold-init verifies the two sides agree.")

(defun nelisp-gc-inner--family-tag-to-keyword (tag)
  "Translate 8-bit family TAG to a gc-inner keyword family.
Falls back to `:cons-pool' for unknown tags so promotion never
crashes on a malformed header (the resulting tenured allocation
will be size-correct even if the family is mis-classified — the
diagnostic is logged via the simulator's stats path)."
  (or (cdr (assq tag nelisp-gc-inner--family-tag-keyword-alist))
      :cons-pool))


;;;; §12.3 Promotion to tenured (Doc 30 v2 §2.5 + §3.5) -----------

(defun nelisp-gc-inner-promote-to-tenured
    (semi tenured to-space-objects-table candidate-addr family size)
  "Copy CANDIDATE-ADDR from to-space into TENURED, reset age, forward.

Pipeline:
  1. Translate FAMILY (gc-inner keyword) → bare symbol via
     `nelisp-integration-family-from-keyword' (T28 bridge).
  2. Allocate a tenured slot via `nelisp-allocator-tenured-alloc'.
  3. Copy the to-space cell's `:fields' (a fresh plist; the simulator
     keeps cells immutable so we never mutate the original).
  4. Reset the header's age to 0 — tenured objects start their age
     count fresh.  The mark and forwarding bits are cleared too
     (the cell is freshly allocated in tenured).
  5. Forward CANDIDATE-ADDR → tenured-addr on SEMI's `copied-objects'
     hash.  Subsequent calls to `--lookup-forwarded' resolve to the
     tenured address.

Arguments:
  SEMI                    — `nelisp-gc-inner--semispace'
  TENURED                 — `nelisp-allocator--tenured' from
                            `nelisp-allocator-init-tenured'
  TO-SPACE-OBJECTS-TABLE  — `:to-space-objects' hash from `run-minor-gc'
  CANDIDATE-ADDR          — to-space integer address of the cell to promote
  FAMILY                  — gc-inner keyword (`:cons-pool' etc.)
  SIZE                    — payload byte size (used for tenured-alloc bin)

Returns the new tenured address.

Side effects:
  - TENURED bump cursor or free-list mutated by tenured-alloc.
  - SEMI's `copied-objects' hash gains a CANDIDATE-ADDR → tenured
    mapping (so callers re-walking from-space → to-space → tenured
    stay consistent).
  - TO-SPACE-OBJECTS-TABLE entry at CANDIDATE-ADDR is *removed* (the
    cell now lives in tenured; leaving the to-space entry would let a
    follow-up `age-tick' double-age the same logical object)."
  (unless (nelisp-gc-inner--semispace-p semi)
    (signal 'wrong-type-argument (list 'nelisp-gc-inner--semispace-p semi)))
  (unless (hash-table-p to-space-objects-table)
    (signal 'wrong-type-argument
            (list 'hash-table-p to-space-objects-table)))
  (unless (and (integerp candidate-addr) (>= candidate-addr 0))
    (signal 'wrong-type-argument
            (list 'integerp candidate-addr)))
  (unless (and (integerp size) (> size 0))
    (signal 'wrong-type-argument
            (list 'positive-integer-p size)))
  (let ((cell (gethash candidate-addr to-space-objects-table)))
    (unless cell
      (error "nelisp-gc-inner-promote-to-tenured: addr %S not in to-space"
             candidate-addr))
    (let* ((bare-family (nelisp-integration-family-from-keyword family))
           ;; Step 2: tenured-alloc — Phase 7.5 backend signs off the
           ;; allocator + gc-inner family namespaces have a single
           ;; bridge (`nelisp-integration-keyword-family-map').  In the
           ;; simulator the allocator returns a fresh address that is
           ;; disjoint from any from-space / to-space address by
           ;; construction (region table separation, Doc 29 §1.4).
           (tenured-addr (nelisp-allocator-tenured-alloc
                          tenured bare-family size))
           ;; Step 3 + 4: build the tenured cell.  Header age = 0
           ;; (tenured objects start fresh under the v2 §2.13 invariant
           ;; "tenured headers always carry age >= 0", and the
           ;; allocator's promote already bumps to 1; we keep
           ;; tenured-alloc's freshly-installed header — it has age 0
           ;; and will get aged by future major cycles instead).
           (fields (plist-get cell :fields)))
      ;; The tenured-alloc above wrote the canonical Doc 30 v2 §2.13
      ;; header for us into the allocator's `headers' map; we don't
      ;; need to touch it.  But we *do* maintain a parallel cell in
      ;; the to-space-objects-table at the *new* tenured address so
      ;; downstream walkers (e.g. ERTs) can see the same `:fields'
      ;; without round-tripping through the allocator's hash.  This
      ;; mirrors how production code keeps a single header slot per
      ;; object — both sides tracked here are pointers to the same
      ;; logical heap word, just spread across two side-tables in the
      ;; simulator.
      (puthash tenured-addr
               (list :addr   tenured-addr
                     :header (nelisp-allocator-pack-header
                              bare-family size 0 nil nil)
                     :fields fields
                     :size   size
                     :tenured t)
               to-space-objects-table)
      ;; Step 5: forward + drop the old to-space entry so it doesn't
      ;; double-age on the next minor-GC's age-tick.
      ;;
      ;; The semispace `copied-objects' hash currently records
      ;;     from-addr → CANDIDATE-ADDR (to-space addr)
      ;; for every entry whose to-addr is CANDIDATE-ADDR — typically
      ;; one entry, but the simulator keeps the side-table inverted-
      ;; lookup-friendly so we walk it once and overwrite each
      ;; matching entry to point at TENURED-ADDR instead.  We *also*
      ;; install CANDIDATE-ADDR → TENURED-ADDR so a caller that has
      ;; only the to-space addr (e.g. ERT inspecting the stats plist)
      ;; resolves correctly.
      (remhash candidate-addr to-space-objects-table)
      (let ((co (nelisp-gc-inner--semispace-copied-objects semi))
            (rewires nil))
        (maphash (lambda (from-addr to-addr)
                   (when (eql to-addr candidate-addr)
                     (push from-addr rewires)))
                 co)
        (dolist (from-addr rewires)
          (puthash from-addr tenured-addr co))
        (puthash candidate-addr tenured-addr co))
      tenured-addr)))


;;;; §12.4 Minor GC + promotion integration ------------------------

(defun nelisp-gc-inner-run-minor-gc-with-promotion
    (semi tenured card-table root-set
          from-space-objects-fn tenured-objects-fn)
  "Run minor GC + age-tick + promote candidates into TENURED.

Pipeline:
  1. `run-minor-gc-with-barrier' (T26) — Cheney copy with the
     remembered-set extension and card-table clear.
  2. `collect-promotion-candidates' on the resulting to-space hash.
  3. For each candidate, `promote-to-tenured' it.

Arguments:
  SEMI                  — Cheney semispace
  TENURED               — `nelisp-allocator--tenured'
  CARD-TABLE            — `nelisp-gc-inner--card-table' over tenured
  ROOT-SET              — caller-supplied mutator roots
  FROM-SPACE-OBJECTS-FN — `(lambda (addr) → plist)' for nursery
  TENURED-OBJECTS-FN    — `(lambda () → ALIST)' for tenured side
                          (consumed by `scan-remembered-set')

Returns a stats plist:
  (:copied-count N
   :promoted-count N
   :age-saturated-count N
   :promotion-candidates ((TO-ADDR . (FAMILY . SIZE)) ...)
   :tenured-addresses (TENURED-ADDR ...)
   :cross-gen-roots N
   :card-table-cleared t
   :elapsed-ms INT
   :kind :minor)

The age-saturated count tracks objects whose age field had already
hit 63 *before* this cycle — they're not re-promoted (already in
tenured; if they're seen here it's because the simulator placed them
back in nursery, an ERT-only fixture path).  In production this stays
at 0."
  (unless (nelisp-gc-inner--semispace-p semi)
    (signal 'wrong-type-argument (list 'nelisp-gc-inner--semispace-p semi)))
  (let* ((start (float-time))
         ;; Step 1: minor GC + write-barrier-extended root set.
         (minor-stats (nelisp-gc-inner-run-minor-gc-with-barrier
                       semi card-table root-set
                       from-space-objects-fn tenured-objects-fn))
         (to-objects (plist-get minor-stats :to-space-objects))
         ;; Step 2: candidate collection + age accounting.
         (candidates (and to-objects
                          (nelisp-gc-inner-collect-promotion-candidates
                           to-objects)))
         (saturated 0)
         (promoted-addrs nil)
         (promoted 0))
    ;; Step 3: promote each candidate.
    (when (and to-objects tenured)
      (dolist (cand candidates)
        (let* ((addr (car cand))
               (family (cadr cand))
               (size   (cddr cand))
               (cell (gethash addr to-objects)))
          (when cell
            (let ((age (nelisp-gc-inner--header-age
                        (or (plist-get cell :header) 0))))
              (when (>= age nelisp-gc-inner--header-age-mask)
                (cl-incf saturated)))
            (let ((new-addr (nelisp-gc-inner-promote-to-tenured
                             semi tenured to-objects addr family size)))
              (push new-addr promoted-addrs)
              (cl-incf promoted))))))
    (append (list :kind :minor
                  :promoted-count promoted
                  :age-saturated-count saturated
                  :promotion-candidates candidates
                  :tenured-addresses (nreverse promoted-addrs)
                  :elapsed-ms (round (* 1000.0 (- (float-time) start))))
            minor-stats)))


;;;; §12.5 Young-root-limited major mode (Doc 30 v2 §0 default) ----

(defun nelisp-gc-inner-run-major-young-root-limited
    (card-table semi root-set heap-regions
                tenured-objects-fn &optional finalizer-table)
  "Run a young-root-limited major cycle (Doc 30 v2 §0 default mode).

Algorithm (Doc 30 v2 §0 BLOCKER 1 mitigation):
  1. `scan-remembered-set' (T26) — extract cross-generation pointers
     from CARD-TABLE's dirty cards.
  2. Construct LIMITED-ROOTS = ROOT-SET ∪ cross-gen-pointers.  This
     skips the static-globals / cold-tenured root frontier that a
     `:major-full' cycle would walk; the trade-off is that cold
     tenured garbage isn't reclaimed this cycle (waiting for the next
     `:major-full' opt-in).
  3. `run-mark-phase' (T17) with LIMITED-ROOTS.
  4. *Limited sweep*: build a HEAP-REGIONS subset filtered to *only
     the regions whose addresses fall on dirty cards*.  This is the
     core 50ms-pause optimization — we never visit cold tenured pages.
  5. `run-sweep-phase' (T21) on the filtered region set.
  6. `clear-card-table' to reset for the next cycle.

Pause-time target: 50ms (vs. 500ms-1s for `run-full-cycle').

Arguments:
  CARD-TABLE         — `nelisp-gc-inner--card-table' over tenured
  SEMI               — Cheney semispace (used to decode nursery boundary)
  ROOT-SET           — mutator roots
  HEAP-REGIONS       — Doc 29 §1.4 region descriptors
  TENURED-OBJECTS-FN — `(lambda () → ALIST)' for remembered-set scan
  FINALIZER-TABLE    — optional, forwarded to `run-sweep-phase'

Returns a stats plist:
  (:kind :major-young-root-limited
   :pause-ms INT                    ;; full-pipeline elapsed
   :marked-count N                  ;; from mark phase
   :swept-count N                   ;; from limited sweep
   :promoted-count N                ;; (always 0 here; promotion is in :minor)
   :cross-gen-roots N               ;; remembered-set yield
   :card-table-cleared t)

The `:promoted-count' field is always 0 in this mode — promotion is
the minor-GC's responsibility.  We keep the field for symmetry with
`run-minor-gc-with-promotion' so callers can treat the two stat
plists uniformly."
  (unless (nelisp-gc-inner--card-table-p card-table)
    (signal 'wrong-type-argument
            (list 'nelisp-gc-inner--card-table-p card-table)))
  (unless (nelisp-gc-inner--semispace-p semi)
    (signal 'wrong-type-argument (list 'nelisp-gc-inner--semispace-p semi)))
  (unless (functionp tenured-objects-fn)
    (signal 'wrong-type-argument (list 'functionp tenured-objects-fn)))
  (let* ((start (float-time))
         (nursery-region (nelisp-gc-inner--semispace-from-space semi))
         ;; Step 1: scan remembered set.
         (rem-set (nelisp-gc-inner-scan-remembered-set
                   card-table nursery-region tenured-objects-fn))
         (cross-gen-ptrs (apply #'append (mapcar #'cdr rem-set)))
         ;; Step 2: limited root set.
         (limited-roots (cl-remove-duplicates
                         (append root-set cross-gen-ptrs)
                         :test #'eql :from-end t))
         ;; Step 3: mark phase.
         (mark-stats (nelisp-gc-inner-run-mark-phase
                      limited-roots heap-regions))
         ;; Step 4: filter HEAP-REGIONS to only those that overlap a
         ;; dirty card.  A region overlaps a dirty card when *any* of
         ;; its `[:start :end)' interval intersects a 4KB page whose
         ;; bit is set in `dirty-bits'.  The card-table covers exactly
         ;; one tenured region; non-tenured regions (nursery) are
         ;; always retained because the minor GC may have left
         ;; unscanned residue (defensive — the doc spec says "tenured
         ;; only" but we don't accidentally prune nursery cells if a
         ;; caller passes a mixed list).
         (limited-regions
          (nelisp-gc-inner--filter-regions-by-dirty
           card-table heap-regions))
         ;; Step 5: sweep on filtered set.
         (sweep-stats (nelisp-gc-inner-run-sweep-phase
                       limited-regions finalizer-table)))
    ;; Step 6: clear card table.
    (nelisp-gc-inner-clear-card-table card-table)
    (list :kind :major-young-root-limited
          :pause-ms          (round (* 1000.0 (- (float-time) start)))
          :marked-count      (or (plist-get mark-stats :marked-count) 0)
          :swept-count       (or (plist-get sweep-stats :swept-count) 0)
          :promoted-count    0
          :cross-gen-roots   (length cross-gen-ptrs)
          :card-table-cleared t
          :mark              mark-stats
          :sweep             sweep-stats
          :limited-region-count (length limited-regions))))

(defun nelisp-gc-inner--filter-regions-by-dirty (card-table regions)
  "Return REGIONS filtered to those overlapping a dirty card on CARD-TABLE.

A region with generation `:nursery' is *unconditionally* retained
(the minor-GC pipeline may have produced freshly-copied objects we
still want to sweep on a corner-case false-mark, and pruning would
risk silent corruption).  Tenured regions are retained iff any
4KB-aligned page within `[:start :end)' is dirty.

Used by `run-major-young-root-limited' to scope the sweep set to
just the dirty pages; the cold-page elision is what gives the mode
its 50ms pause-time target."
  (let ((bv     (nelisp-gc-inner--card-table-dirty-bits card-table))
        (size   (nelisp-gc-inner--card-table-card-size  card-table))
        (cstart (nelisp-gc-inner--card-table-region-start card-table))
        (cend   (nelisp-gc-inner--card-table-region-end   card-table)))
    (cl-remove-if-not
     (lambda (region)
       (cond
        ((eq (plist-get region :generation) :nursery) t)
        (t
         (let* ((rstart (plist-get region :start))
                (rend   (plist-get region :end))
                ;; Compute the [overlap-start, overlap-end) interval
                ;; between the region and the card-table's coverage.
                (lo (max rstart cstart))
                (hi (min rend   cend)))
           (and (< lo hi)
                ;; Walk the cards in [lo, hi) and stop at the first
                ;; dirty bit.
                (let ((found nil)
                      (card-lo (/ (- lo cstart) size))
                      (card-hi (/ (- hi cstart 1) size)))
                  (cl-loop for i from card-lo to card-hi
                           when (and (>= i 0)
                                     (< i (length bv))
                                     (aref bv i))
                           do (setq found t) and return nil)
                  found))))))
     regions)))


;;;; §12.6 Cycle dispatch v2 (Phase 7.3.5) -------------------------

(defun nelisp-gc-inner-run-cycle-v2 (cycle-kind &rest args)
  "Phase 7.3.5 cycle dispatch — extends T24's `run-cycle' with promotion.

CYCLE-KIND is one of:
  :minor — `(:minor SEMI TENURED CARD-TABLE ROOT-SET
            FROM-SPACE-OBJECTS-FN TENURED-OBJECTS-FN)'.  Forwards to
            `run-minor-gc-with-promotion'.  When TENURED is nil the
            promotion step is skipped (the call degrades to a plain
            minor GC); this matches Doc 30 v2 §3.3 \"promotion is
            opt-in\" before tenured initialization completes.
  :major-young-root-limited — `(:major-young-root-limited CARD-TABLE
            SEMI ROOT-SET HEAP-REGIONS TENURED-OBJECTS-FN
            [FINALIZER-TABLE])'.  Forwards to
            `run-major-young-root-limited'.  This is the default major
            mode (`nelisp-gc-major-mode' = `young-root-limited',
            user signoff 2026-04-25).
  :major-full — `(:major-full ROOT-SET HEAP-REGIONS [FINALIZER-TABLE])'.
            Forwards to `run-full-cycle' (T21 SHIPPED).  Opt-out major
            mode for batch / `gc-stress' bench / explicit `(let
            ((nelisp-gc-major-mode \\='full)) ...)' contexts.

Returns the wrapped result plist; `(plist-get RESULT :kind)' gives
the demultiplex key."
  (cond
   ((eq cycle-kind :minor)
    (let ((semi      (nth 0 args))
          (tenured   (nth 1 args))
          (card-table (nth 2 args))
          (root-set  (nth 3 args))
          (from-fn   (nth 4 args))
          (tenured-fn (nth 5 args)))
      (nelisp-gc-inner-run-minor-gc-with-promotion
       semi tenured card-table root-set from-fn tenured-fn)))
   ((eq cycle-kind :major-young-root-limited)
    (let ((card-table  (nth 0 args))
          (semi        (nth 1 args))
          (root-set    (nth 2 args))
          (heap-regions (nth 3 args))
          (tenured-fn  (nth 4 args))
          (finalizer-table (nth 5 args)))
      (nelisp-gc-inner-run-major-young-root-limited
       card-table semi root-set heap-regions tenured-fn finalizer-table)))
   ((eq cycle-kind :major-full)
    (let ((root-set     (nth 0 args))
          (heap-regions (nth 1 args))
          (finalizer-table (nth 2 args)))
      (let ((res (nelisp-gc-inner-run-full-cycle
                  root-set heap-regions finalizer-table)))
        (cons :kind (cons :major-full res)))))
   (t
    (signal 'wrong-type-argument
            (list 'nelisp-gc-inner--cycle-kind-v2-p cycle-kind)))))


(provide 'nelisp-gc-inner)
;;; nelisp-gc-inner.el ends here
