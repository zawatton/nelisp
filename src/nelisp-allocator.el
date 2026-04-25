;;; nelisp-allocator.el --- Phase 7.2.1 bump allocator nursery + heap-region registry  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 7.2.1 — bump allocator nursery (single generation, MVP).
;;
;; Design refs (LOCKED):
;;   - docs/design/29-phase7.2-allocator.org v2 LOCKED 2026-04-25
;;     §1.4 heap-region registry v1 contract (provider = Phase 7.2,
;;       consumer = Phase 7.3 GC)
;;     §2.1 allocator algorithm = bump (nursery) + free-list (tenured)
;;     §2.2 heap layout = 2-generation (nursery 4 MB / tenured 64 MB)
;;     §2.5 object family table (5 families: cons-pool / closure-pool /
;;       string-span / vector-span / large-object)
;;     §3.1 sub-phase 7.2.1 = bump allocator nursery only (+8 ERT)
;;     §6.7 generational correctness interim invariant
;;   - docs/design/30-phase7.3-gc-inner.org v2 LOCKED 2026-04-25
;;     §2.13 object header layout (64-bit packed:
;;       [63] mark-bit / [62] forwarding-bit / [61:56] age 6-bit /
;;       [55:48] family-tag 8-bit / [47:0] size-or-new-addr 48-bit)
;;
;; Scope (Phase 7.2.1 only):
;;   - heap-region registry v1 schema + provider API
;;   - bump allocator nursery (default 4 MB, defcustom)
;;   - per-family alloc helpers (cons / closure / string-span /
;;     vector-span / large-object)
;;   - object header write (Doc 30 v2 §2.13 layout)
;;   - nursery overflow callback stub (Phase 7.3 minor GC trigger)
;;
;; Out of scope (Phase 7.2.2 / 7.2.3):
;;   - tenured generation + free-list + promotion (7.2.2)
;;   - per-type pool + bulk API + alloc-stats (7.2.3)
;;   - real mmap (Phase 7.5 FFI integration)
;;   - write barrier + remembered-set (Phase 7.3 minor GC)
;;
;; Module convention:
;;   - `nelisp-allocator-' = public API
;;   - `nelisp-allocator--' = private helper / state
;;   - errors derive from `nelisp-allocator-error'

;;; Code:

(require 'cl-lib)

;;; Customization ----------------------------------------------------

(defgroup nelisp-allocator nil
  "Phase 7.2 NeLisp allocator (bump nursery + free-list tenured)."
  :group 'nelisp
  :prefix "nelisp-allocator-")

(defcustom nelisp-allocator-nursery-size (* 4 1024 1024)
  "Nursery size in bytes.

Doc 29 §2.2 推奨 A: 2-generation default with nursery = 4 MB.
Tunable per workload.  Phase 7.2.1 MVP allocates a single nursery
region; Phase 7.2.2 will add the tenured generation.

Must be a positive multiple of `nelisp-allocator-page-size' (4096
bytes).  Non-multiples are rejected at `nelisp-allocator-init-nursery'
time so the bump pointer always lands inside the registered region."
  :type 'integer
  :group 'nelisp-allocator)

;;; Constants --------------------------------------------------------

(defconst nelisp-heap-region-version 1
  "Doc 29 §1.4 heap-region registry contract version.

Phase 7.2 allocator is the *provider* of this contract; Phase 7.3 GC
(Doc 30) is the *consumer*.  Bumped on any breaking change to the
`nelisp-heap-region' schema; Phase 7.3 must refuse to consume any
region whose `version' field it does not understand.

Independent versioning from `nelisp-cc-runtime-gc-metadata-version'
(Doc 28 §2.9, Phase 7.1 producer).  See Doc 29 §1.4 for the rationale
behind splitting the two contracts.")

(defconst nelisp-allocator-page-size 4096
  "OS native page size (4 KiB).
Doc 29 §2.3 推奨 A — Linux/macOS/Windows native page size, used for
nursery alignment and overflow detection.")

(defconst nelisp-allocator-large-object-threshold (* 4 1024)
  "Object family selection threshold (Doc 29 §2.5 v2 selection rules).

  size <= 32 byte (fixed)   → cons-pool / closure-pool
  32 byte < size <= 4 KiB   → string-span / vector-span (size-classed)
  size > 4 KiB              → large-object (dedicated mmap region)")

(defconst nelisp-allocator--family-tag-alist
  '((cons-pool    . #x01)
    (closure-pool . #x02)
    (string-span  . #x03)
    (vector-span  . #x04)
    (large-object . #x05))
  "Family symbol → 8-bit family-tag (Doc 30 v2 §2.13 [55:48]).
Phase 7.3 GC reads this from the object header; bumping any value
here is an ABI break — guarded by `nelisp-heap-region-version'.")

(defconst nelisp-allocator--alignment 8
  "Minimum allocation alignment in bytes.
Doc 30 v2 §2.13 packs the header into one 64-bit word, which forces
8-byte alignment of every allocated address.  Family-specific
helpers may demand stricter alignment (e.g. 16-byte for cons-pool)
on top of this floor.")

(defconst nelisp-allocator--cons-size 16
  "Cons cell size: 8-byte car + 8-byte cdr.")

(defconst nelisp-allocator--closure-header-size 32
  "Closure header size before per-slot upvalue storage.")

(defconst nelisp-allocator--span-header-size 8
  "string-span / vector-span header size (= 1 word, Doc 30 v2 §2.13).")

(defconst nelisp-allocator--span-size-classes
  '(8 16 32 64 128 256 512 1024 2048 4096)
  "Size classes for string-span and vector-span families.

Round-up classes from Emacs alloc.c precedent (Doc 29 §2.5 推奨 A,
§9.2 noted as Phase 7.5 tuning candidate).  4 KiB ceiling matches
`nelisp-allocator-large-object-threshold' so the largest size-class
exactly fills a span before the large-object path takes over.")

;;; Errors -----------------------------------------------------------

(define-error 'nelisp-allocator-error
  "NeLisp Phase 7.2 allocator error")

(define-error 'nelisp-allocator-overflow
  "Nursery exhausted; minor GC trigger required (Phase 7.3 stub)"
  'nelisp-allocator-error)

(define-error 'nelisp-allocator-bad-config
  "Invalid allocator configuration"
  'nelisp-allocator-error)

(define-error 'nelisp-allocator-unknown-family
  "Unknown object family (Doc 29 §2.5 table)"
  'nelisp-allocator-error)

;;; Heap region registry (Doc 29 §1.4) -------------------------------

(cl-defstruct (nelisp-heap-region
               (:constructor nelisp-heap-region-make)
               (:copier nil))
  "Heap region registry v1 entry.

Doc 29 §1.4 LOCKED contract.  Phase 7.2 allocator is the producer;
Phase 7.3 GC (Doc 30) is the consumer.

Slots:
  ID         — sequential uint32 assigned by `nelisp-allocator-register-region'.
               Stable for the region's lifetime; recycled only after
               an explicit `nelisp-allocator--reset-region-table' (test
               harness only — never called in production).
  START      — region base address (unsigned).  Phase 7.2.1 simulates
               this with a deterministic counter so ERT predictions are
               stable; Phase 7.5 will swap it for the real `mmap'
               return value.
  END        — region end address (exclusive).  END - START = region
               byte size.
  GENERATION — `nursery' or `tenured'.  Phase 7.2.1 only emits
               `nursery'; `tenured' arrives in Phase 7.2.2.
  FAMILY     — one of cons-pool / closure-pool / string-span /
               vector-span / large-object (Doc 29 §2.5 v2 table).
  VERSION    — `nelisp-heap-region-version' snapshotted at registration
               so a future v2 reader can refuse v1 entries (and vice
               versa) at consume time."
  (id 0 :read-only t)
  (start 0)
  (end 0)
  (generation 'nursery :read-only t)
  (family 'cons-pool :read-only t)
  (version nelisp-heap-region-version :read-only t))

(defvar nelisp-allocator--region-table (make-hash-table :test 'eql)
  "Region id → `nelisp-heap-region'.

Doc 29 §1.4 provider-side storage.  Hash table (test \\='eql) so id
lookup is O(1) for both Phase 7.3 root scanning and ERT fixtures.

Reset only via `nelisp-allocator--reset-region-table' (test fixture)
or process exit; production allocator code only adds and looks up.")

(defvar nelisp-allocator--next-region-id 0
  "Monotonic sequential uint32 source for `nelisp-heap-region.id'.

Reset only by `nelisp-allocator--reset-region-table'.  Wrap-around
beyond UINT32_MAX is not handled in Phase 7.2.1 — the LOC budget
lands at ~600 LOC and 4 billion regions / process is well past the
operational range.  A wrap-around guard is a Phase 8+ concern when
long-running daemons start filing the request.")

(defvar nelisp-allocator--next-sim-address #x10000000
  "Simulated next mmap address.

Phase 7.2.1 ships a *simulator* allocator: real `mmap' lands in
Phase 7.5 once the FFI bridge is ready.  We model the kernel's
returned address with a monotonic counter starting at a non-zero
base so ERTs can distinguish unallocated (= 0) from allocated at
the bottom of the simulated address space.

The counter advances by `region-size' rounded up to a 4 KiB page
boundary so simulator addresses share the same alignment property
real `mmap' returns would.")

(defun nelisp-allocator--reset-region-table ()
  "Reset the region registry to the clean post-load state.

ERT fixtures call this to isolate per-test state.  Production code
must never call it — tearing down the registry mid-process strands
every live region pointer in Phase 7.3's view."
  (clrhash nelisp-allocator--region-table)
  (setq nelisp-allocator--next-region-id 0)
  (setq nelisp-allocator--next-sim-address #x10000000))

(defun nelisp-allocator--alloc-sim-address (size)
  "Reserve SIZE bytes of simulated address space and return the base.

Round SIZE up to `nelisp-allocator-page-size' so the next call
returns a page-aligned address — mirrors what `mmap' guarantees."
  (let* ((page nelisp-allocator-page-size)
         (rounded (* (/ (+ size (1- page)) page) page))
         (base nelisp-allocator--next-sim-address))
    (setq nelisp-allocator--next-sim-address (+ base rounded))
    base))

(defun nelisp-allocator-register-region (region)
  "Register REGION (a `nelisp-heap-region') in the global table.

REGION's `id' slot must already match a fresh
`nelisp-allocator--next-region-id' value — `register-region' does
not allocate the id itself so callers stay in control of when the
counter advances (e.g. for atomic batch registration).  Returns the
registered region's id."
  (unless (nelisp-heap-region-p region)
    (signal 'nelisp-allocator-error
            (list "register-region: not a nelisp-heap-region" region)))
  (let ((id (nelisp-heap-region-id region)))
    (when (gethash id nelisp-allocator--region-table)
      (signal 'nelisp-allocator-error
              (list "register-region: id already registered" id)))
    (puthash id region nelisp-allocator--region-table)
    id))

(defun nelisp-allocator-lookup-region (id)
  "Look up the `nelisp-heap-region' with ID.  Return nil if absent."
  (gethash id nelisp-allocator--region-table))

(defun nelisp-allocator-region-table-snapshot ()
  "Return a list of all currently registered regions (Doc 29 §1.4 contract).

Phase 7.3 GC consumes this to seed the heap walker.  The returned
list is freshly allocated; callers may mutate it freely.  Order is
ascending by id so consumers can rely on registration order without
threading a sort through the producer."
  (let (out)
    (maphash (lambda (_id region) (push region out))
             nelisp-allocator--region-table)
    (sort out (lambda (a b)
                (< (nelisp-heap-region-id a)
                   (nelisp-heap-region-id b))))))

(defun nelisp-allocator--make-region (size generation family)
  "Allocate a fresh region of SIZE bytes for GENERATION/FAMILY.

Returns the registered `nelisp-heap-region' (id already assigned and
table entry already inserted).  SIZE is rounded up to the nearest
page; the `start' .. `end' interval reflects the rounded total."
  (unless (assq family nelisp-allocator--family-tag-alist)
    (signal 'nelisp-allocator-unknown-family (list family)))
  (let* ((id (prog1 nelisp-allocator--next-region-id
               (cl-incf nelisp-allocator--next-region-id)))
         (page nelisp-allocator-page-size)
         (rounded (* (/ (+ size (1- page)) page) page))
         (start (nelisp-allocator--alloc-sim-address rounded))
         (region (nelisp-heap-region-make
                  :id id
                  :start start
                  :end (+ start rounded)
                  :generation generation
                  :family family
                  :version nelisp-heap-region-version)))
    (nelisp-allocator-register-region region)
    region))

;;; Object header (Doc 30 v2 §2.13) ---------------------------------

;; Layout (LSB-first description for clarity, but bit numbers below
;; are MSB-indexed to match Doc 30 v2 §2.13 LOCKED):
;;
;;   [63]    mark-bit            (init 0)
;;   [62]    forwarding-bit      (init 0)
;;   [61:56] age (6 bits)        (init 0)
;;   [55:48] family-tag (8 bits)
;;   [47:0]  size (forwarding-bit=0) or new-addr (forwarding-bit=1)
;;
;; Phase 7.2.1 only writes the header (mark/forwarding/age = 0,
;; family-tag set, size = SIZE).  Phase 7.3 GC will mutate the
;; mark-bit and (Cheney) forwarding-bit + new-addr.

(defconst nelisp-allocator--header-size-mask
  ;; 48-bit mask = (1 << 48) - 1 = #xFFFFFFFFFFFF
  #xFFFFFFFFFFFF
  "Mask for the 48-bit size / new-addr field of the object header.")

(defconst nelisp-allocator--header-family-shift 48
  "Bit position of the family-tag field within the 64-bit header.")

(defconst nelisp-allocator--header-family-mask #xFF
  "Mask applied after `nelisp-allocator--header-family-shift' to
recover the 8-bit family-tag.")

(defconst nelisp-allocator--header-age-shift 56
  "Bit position of the age field within the 64-bit header.")

(defconst nelisp-allocator--header-age-mask #x3F
  "6-bit mask for the age field.")

(defconst nelisp-allocator--header-forwarding-shift 62
  "Bit position of the forwarding-bit within the 64-bit header.")

(defconst nelisp-allocator--header-mark-shift 63
  "Bit position of the mark-bit within the 64-bit header.")

(defun nelisp-allocator--family-tag (family)
  "Return the 8-bit numeric tag for FAMILY symbol."
  (or (cdr (assq family nelisp-allocator--family-tag-alist))
      (signal 'nelisp-allocator-unknown-family (list family))))

(defun nelisp-allocator-pack-header (family size &optional age forwarding mark)
  "Pack a 64-bit object header per Doc 30 v2 §2.13 LOCKED layout.

FAMILY is one of the symbols in `nelisp-allocator--family-tag-alist'.
SIZE is the object byte size (forwarded=nil) or the forwarded new-
address (when FORWARDING is non-nil); must fit in 48 bits.

AGE defaults to 0 (fresh allocation).  FORWARDING and MARK default
to 0 (initial allocation state).  Phase 7.2.1 hot path always passes
nil/nil/nil so the `cl-incf'-style fast path lands in 1 logical
multiplication-free op."
  (when (or (< size 0) (> size nelisp-allocator--header-size-mask))
    (signal 'nelisp-allocator-error
            (list "header size out of 48-bit range" size)))
  (let* ((age* (or age 0))
         (forwarding* (if forwarding 1 0))
         (mark* (if mark 1 0))
         (family-tag (nelisp-allocator--family-tag family)))
    (when (or (< age* 0) (> age* nelisp-allocator--header-age-mask))
      (signal 'nelisp-allocator-error
              (list "header age out of 6-bit range" age*)))
    (logior
     (ash mark* nelisp-allocator--header-mark-shift)
     (ash forwarding* nelisp-allocator--header-forwarding-shift)
     (ash age* nelisp-allocator--header-age-shift)
     (ash family-tag nelisp-allocator--header-family-shift)
     (logand size nelisp-allocator--header-size-mask))))

(defun nelisp-allocator-header-family-tag (header)
  "Return the family-tag (numeric 8-bit) decoded from HEADER."
  (logand (ash header (- nelisp-allocator--header-family-shift))
          nelisp-allocator--header-family-mask))

(defun nelisp-allocator-header-family (header)
  "Return the family symbol decoded from HEADER, or nil if unknown."
  (let ((tag (nelisp-allocator-header-family-tag header)))
    (car (cl-find tag nelisp-allocator--family-tag-alist :key #'cdr))))

(defun nelisp-allocator-header-size (header)
  "Return the size (or forwarded new-addr if forwarding-bit set)
field decoded from HEADER (48 bits)."
  (logand header nelisp-allocator--header-size-mask))

(defun nelisp-allocator-header-age (header)
  "Return the 6-bit age decoded from HEADER."
  (logand (ash header (- nelisp-allocator--header-age-shift))
          nelisp-allocator--header-age-mask))

(defun nelisp-allocator-header-mark-bit (header)
  "Return the mark-bit (0 or 1) of HEADER."
  (logand (ash header (- nelisp-allocator--header-mark-shift)) 1))

(defun nelisp-allocator-header-forwarding-bit (header)
  "Return the forwarding-bit (0 or 1) of HEADER."
  (logand (ash header (- nelisp-allocator--header-forwarding-shift)) 1))

;;; Nursery (bump allocator) ----------------------------------------

(cl-defstruct (nelisp-allocator--nursery
               (:constructor nelisp-allocator--nursery-make)
               (:copier nil))
  "Bump-pointer nursery state.

Doc 29 §2.1 推奨 A — bump (nursery) hot path.  Hot allocation = single
pointer increment (`free' = `free' + size); overflow falls into the
slow path which calls `overflow-callback' (Phase 7.3 minor GC trigger
in production, an abort signal in Phase 7.2.1 MVP).

Slots:
  REGION              — `nelisp-heap-region' for this nursery (id /
                        start / end registered).
  START               — bump-pointer base = region.start.  Cached so
                        the hot path avoids the struct accessor.
  FREE                — current bump pointer.  Initially = START.
  LIMIT               — region end = region.end.  Allocation succeeds
                        iff free + size <= limit.
  OVERFLOW-CALLBACK   — function called with (NURSERY SIZE FAMILY)
                        when an alloc would step past LIMIT.  May
                        return non-nil to retry (after a real Phase
                        7.3 minor GC reclaims space) or signal to
                        abort.  Default = signal `nelisp-allocator-overflow'."
  (region nil)
  (start 0)
  (free 0)
  (limit 0)
  (overflow-callback nil))

(defvar nelisp-allocator--current-nursery nil
  "Process-wide active nursery.

Phase 7.2.1 has a single global nursery (multi-thread per-thread
arenas land in Phase 7.5+, Doc 29 §2.9).  `nelisp-allocator-init-
nursery' replaces this binding; tests use `let'-binding to isolate.")

(defun nelisp-allocator--default-overflow (nursery size family)
  "Default overflow callback: signal `nelisp-allocator-overflow'.

Phase 7.3 will swap this for `nelisp-gc-minor' which evacuates live
roots into tenured then resets `free' = `start'.  Phase 7.2.1 ERT
asserts the callback is invoked with (NURSERY SIZE FAMILY) before
the signal raises."
  (signal 'nelisp-allocator-overflow
          (list "nursery exhausted"
                :requested size
                :family family
                :free (nelisp-allocator--nursery-free nursery)
                :limit (nelisp-allocator--nursery-limit nursery))))

(defun nelisp-allocator-init-nursery (&optional size overflow-callback)
  "Initialise a fresh nursery and make it the current one.

SIZE bytes (default `nelisp-allocator-nursery-size') are reserved
via the simulator address allocator and registered as a heap region
with generation = `nursery', family = `cons-pool' (the dominant
short-lived family — sub-allocations of other families share the
same nursery in Phase 7.2.1; Phase 7.2.3 introduces per-family pool
splitting).

OVERFLOW-CALLBACK overrides the default
`nelisp-allocator--default-overflow'.  When supplied, it is called
with (NURSERY SIZE FAMILY) on every overflow; non-nil return values
are interpreted as retry-the-alloc (= a real Phase 7.3 minor GC
freed enough space).

Returns the new nursery struct; also stored in
`nelisp-allocator--current-nursery'.  Resets the region table and
sim-address counter, so production code must call this exactly once
at startup."
  (let* ((nursery-size (or size nelisp-allocator-nursery-size)))
    (unless (and (integerp nursery-size)
                 (> nursery-size 0)
                 (zerop (% nursery-size nelisp-allocator-page-size)))
      (signal 'nelisp-allocator-bad-config
              (list "nursery size must be a positive page-multiple"
                    nursery-size nelisp-allocator-page-size)))
    (nelisp-allocator--reset-region-table)
    (let* ((region (nelisp-allocator--make-region
                    nursery-size 'nursery 'cons-pool))
           (nursery (nelisp-allocator--nursery-make
                     :region region
                     :start (nelisp-heap-region-start region)
                     :free  (nelisp-heap-region-start region)
                     :limit (nelisp-heap-region-end region)
                     :overflow-callback
                     (or overflow-callback
                         #'nelisp-allocator--default-overflow))))
      (setq nelisp-allocator--current-nursery nursery)
      nursery)))

(defun nelisp-allocator--align-up (n alignment)
  "Round N up to the nearest multiple of ALIGNMENT."
  (let ((rem (% n alignment)))
    (if (zerop rem) n (+ n (- alignment rem)))))

(defun nelisp-allocator-nursery-alloc (nursery family size)
  "Allocate SIZE bytes from NURSERY for FAMILY.

SIZE is the *payload* size (header is added on top by family helpers
that need one).  Address is rounded up to
`nelisp-allocator--alignment' (8-byte) so headers stay word-aligned.

On success, returns the allocated base address (integer).  On
overflow, the nursery's overflow callback is invoked.  If the
callback returns non-nil the allocation is retried once (the callback
having reclaimed space by running a minor GC); if it returns nil the
default overflow signal raises."
  (unless (nelisp-allocator--nursery-p nursery)
    (signal 'nelisp-allocator-error (list "not a nursery" nursery)))
  (unless (assq family nelisp-allocator--family-tag-alist)
    (signal 'nelisp-allocator-unknown-family (list family)))
  (when (or (not (integerp size)) (<= size 0))
    (signal 'nelisp-allocator-error (list "size must be positive" size)))
  (let* ((aligned (nelisp-allocator--align-up
                   size nelisp-allocator--alignment))
         (free  (nelisp-allocator--nursery-free nursery))
         (limit (nelisp-allocator--nursery-limit nursery))
         (next  (+ free aligned)))
    (if (<= next limit)
        (progn
          (setf (nelisp-allocator--nursery-free nursery) next)
          free)
      ;; Slow path: overflow.
      (let* ((cb (or (nelisp-allocator--nursery-overflow-callback nursery)
                     #'nelisp-allocator--default-overflow))
             (retry (funcall cb nursery aligned family)))
        (if retry
            ;; Callback claims it freed space — try once more.
            (let* ((free2  (nelisp-allocator--nursery-free nursery))
                   (next2  (+ free2 aligned)))
              (if (<= next2 (nelisp-allocator--nursery-limit nursery))
                  (progn
                    (setf (nelisp-allocator--nursery-free nursery) next2)
                    free2)
                (signal 'nelisp-allocator-overflow
                        (list "callback returned non-nil but space still
short"
                              :requested aligned :family family))))
          (signal 'nelisp-allocator-overflow
                  (list "callback returned nil, allocation refused"
                        :requested aligned :family family)))))))

;;; Per-family allocators (Doc 29 §2.5 v2 table) --------------------

(defun nelisp-allocator-alloc-cons (nursery)
  "Allocate a cons cell (16 byte = car + cdr) from NURSERY.

Doc 29 §2.5: family = cons-pool, fixed 16-byte layout.  The 64-bit
header is *not* prepended in Phase 7.2.1 — cons cells use the
NeLisp 3-bit low-tag pointer scheme (Doc 29 §2.4) which encodes the
type without a per-object header.  Phase 7.3 mark-bit lives in a
side bitmap (Doc 30 §2.4).

Returns the cons cell's base address."
  (nelisp-allocator-nursery-alloc nursery 'cons-pool
                                  nelisp-allocator--cons-size))

(defun nelisp-allocator-alloc-closure (nursery slots)
  "Allocate a closure with SLOTS upvalues from NURSERY.

Layout: 32-byte header + 8 * SLOTS bytes for upvalue slots.

The 32-byte header includes the 8-byte Doc 30 v2 §2.13 packed
header plus 24 bytes of closure-specific bookkeeping (function ptr,
arity, captured-env ptr).  Phase 7.2.1 reserves the bytes; Phase
7.1 native compiler / Phase 7.4 coding fill them in.  Returns the
closure's base address."
  (when (or (not (integerp slots)) (< slots 0))
    (signal 'nelisp-allocator-error
            (list "closure slots must be non-negative" slots)))
  (let* ((total (+ nelisp-allocator--closure-header-size (* 8 slots))))
    (nelisp-allocator-nursery-alloc nursery 'closure-pool total)))

(defun nelisp-allocator--span-size-class (payload-size)
  "Return the smallest size-class >= PAYLOAD-SIZE.

Used by string-span and vector-span to round payloads up to one of
`nelisp-allocator--span-size-classes'.  Signals if PAYLOAD-SIZE
exceeds the largest class — caller should have routed to
large-object first."
  (or (cl-find-if (lambda (sz) (>= sz payload-size))
                  nelisp-allocator--span-size-classes)
      (signal 'nelisp-allocator-error
              (list "span payload exceeds largest size-class"
                    payload-size
                    (car (last nelisp-allocator--span-size-classes))))))

(defun nelisp-allocator-alloc-string-span (nursery payload-size)
  "Allocate a string-span (header + size-classed payload).

Doc 29 §2.5 size-classed span — round PAYLOAD-SIZE up to
`nelisp-allocator--span-size-classes' and prepend an 8-byte header
(Doc 30 v2 §2.13 packed).  PAYLOAD-SIZE must be > 32 byte and
<= `nelisp-allocator-large-object-threshold'; caller should have
routed via `nelisp-allocator-alloc-large-object' for sizes above
the threshold (the boundary check enforces this).

Returns the string-span's base address (header location)."
  (when (or (not (integerp payload-size)) (<= payload-size 0))
    (signal 'nelisp-allocator-error
            (list "string-span payload-size must be positive" payload-size)))
  (when (> payload-size nelisp-allocator-large-object-threshold)
    (signal 'nelisp-allocator-error
            (list "string-span payload exceeds large-object threshold"
                  payload-size
                  nelisp-allocator-large-object-threshold)))
  (let* ((sz-class (nelisp-allocator--span-size-class payload-size))
         (total (+ nelisp-allocator--span-header-size sz-class)))
    (nelisp-allocator-nursery-alloc nursery 'string-span total)))

(defun nelisp-allocator-alloc-vector-span (nursery payload-size)
  "Allocate a vector-span (header + size-classed payload).

Same shape as `nelisp-allocator-alloc-string-span' but tagged
`vector-span' so Phase 7.3 GC can distinguish payload semantics
(byte-typed vs slot-typed) without an extra type field."
  (when (or (not (integerp payload-size)) (<= payload-size 0))
    (signal 'nelisp-allocator-error
            (list "vector-span payload-size must be positive" payload-size)))
  (when (> payload-size nelisp-allocator-large-object-threshold)
    (signal 'nelisp-allocator-error
            (list "vector-span payload exceeds large-object threshold"
                  payload-size
                  nelisp-allocator-large-object-threshold)))
  (let* ((sz-class (nelisp-allocator--span-size-class payload-size))
         (total (+ nelisp-allocator--span-header-size sz-class)))
    (nelisp-allocator-nursery-alloc nursery 'vector-span total)))

(defun nelisp-allocator-alloc-large-object (_nursery payload-size)
  "Allocate a large object (>4 KiB) via a dedicated mmap region.

Doc 29 §2.5: large-object family bypasses the nursery — every large
object lives in its own freshly-`mmap'-ed region for two reasons:

  1. nursery 4 MB default would be exhausted by a handful of large
     payloads, defeating the generational hypothesis.
  2. Phase 7.3 GC reclaims large objects via `munmap' rather than
     copying, so giving each its own region simplifies the unmap.

PAYLOAD-SIZE must be strictly greater than
`nelisp-allocator-large-object-threshold' (= 4 KiB) — smaller payloads
should use string-span / vector-span.

Returns the large-object's base address.  The NURSERY argument is
accepted for API symmetry with the other family allocators (and so
Phase 7.5 thread-arenas can pin per-arena large-object metadata
without changing call sites) but is not currently consulted."
  (when (or (not (integerp payload-size))
            (<= payload-size nelisp-allocator-large-object-threshold))
    (signal 'nelisp-allocator-error
            (list "large-object requires payload-size > 4 KiB"
                  payload-size
                  nelisp-allocator-large-object-threshold)))
  (let* ((total (+ nelisp-allocator--span-header-size payload-size))
         (region (nelisp-allocator--make-region
                  total 'nursery 'large-object)))
    (nelisp-heap-region-start region)))

(provide 'nelisp-allocator)
;;; nelisp-allocator.el ends here
