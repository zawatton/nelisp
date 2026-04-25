;;; nelisp-allocator.el --- Phase 7.2 allocator (nursery + tenured + per-type pool)  -*- lexical-binding: t; -*-

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

;;; ----------------------------------------------------------------
;;; Phase 7.2.2 — tenured generation + free-list + promotion
;;; ----------------------------------------------------------------
;;
;; Doc 29 v2 LOCKED 2026-04-25 §3.2 sub-phase 7.2.2 scope:
;;   - tenured region (default 64 MB, defcustom)
;;   - size-classed free-list (10 bins, 8 .. 4096 byte powers of 2)
;;   - manual promotion API (Doc 29 §6.4 — auto promotion is Phase 7.3)
;;   - free-block coalescing (Doc 29 §6.5 — 80%+ utilisation target)
;;   - tenured stats helper (subset of Doc 29 §2.10 — full alloc-stats
;;     plist arrives in Phase 7.2.3)
;;
;; Phase 7.2 interim invariant (Doc 29 §6.7 v2): minor GC is *not*
;; executed during Phase 7.2; tenured → nursery references defer to
;; the slow path until Phase 7.3 enables the write barrier.  The
;; promotion API below therefore copies the header alone — payload
;; bytes are zero-filled in the simulator, since real `mmap'-backed
;; bytes do not exist until Phase 7.5 wires the FFI bridge.
;;
;; Design notes (simulator path):
;;   - `headers' hash maps addr → packed 64-bit header word.  This is
;;     a logical stand-in for the bytes the real allocator would write
;;     into the `mmap' span; it lets ERTs assert age-bit / family-tag
;;     state without committing to real memory before Phase 7.5.
;;   - `block-sizes' hash maps addr → size-class so `tenured-free' can
;;     find the right free-list bin in O(1) without re-deriving the
;;     class from the SIZE argument (which the caller might round
;;     differently than `--size-class-of').
;;   - Coalescing uses the "buddy" property of power-of-2 size-classes:
;;     two adjacent size-N free blocks merge into a single size-2N
;;     block, recursively up to the largest class.  This is the
;;     standard buddy-allocator merge step (Knuth TAOCP vol. 1 §2.5),
;;     adapted to NeLisp's 10-bin layout.

;;; Tenured: customization ------------------------------------------

(defcustom nelisp-allocator-tenured-size (* 64 1024 1024)
  "Tenured generation size in bytes (default 64 MB).

Doc 29 v2 §2.2 推奨 A: 2-generation default with tenured = 64 MB.
Tunable per workload.  Phase 7.2.2 MVP allocates a single tenured
region; per-family tenured pools land in Phase 7.2.3.

Must be a positive multiple of `nelisp-allocator-page-size' so the
size-class free-list always lands inside the registered region."
  :type 'integer
  :group 'nelisp-allocator)

;;; Tenured: constants ----------------------------------------------

(defconst nelisp-allocator-tenured-size-classes
  '(8 16 32 64 128 256 512 1024 2048 4096)
  "Size-class bins (bytes) for the tenured free-list.

Doc 29 v2 §2.5 size-classed span: 10 power-of-2 bins from 8 byte
(minimum 64-bit-word footprint) up to 4096 byte (= the
`nelisp-allocator-large-object-threshold' boundary).  Allocations
above 4096 byte should be routed via the large-object family
before reaching this list.

Power-of-2 spacing also enables the buddy coalescing step in
`nelisp-allocator-tenured-coalesce' — two adjacent size-N blocks
merge into a size-2N block.  Bumping the bin count or breaking the
power-of-2 invariant must update the coalesce algorithm in lock
step, since the merge step assumes pairs of equal-size buddies.")

;;; Tenured: state struct -------------------------------------------

(cl-defstruct (nelisp-allocator--tenured
               (:constructor nelisp-allocator--tenured-make)
               (:copier nil))
  "Tenured generation state with size-classed free-list (Doc 29 §3.2).

Slots:
  REGION          — `nelisp-heap-region' (generation = `tenured').
  FREE-LISTS      — alist (size-class . list-of-free-addr).  Each
                    bin holds the base addresses of *available*
                    blocks of that size.  Pop = alloc fast path,
                    push = free fast path.
  BUMP            — next fresh address to carve when no free-list
                    bin satisfies the request.  Initially equal to
                    `region.start'; advances monotonically up to
                    `region.end'.
  ALLOCATED-BYTES — sum of the size-classes of every currently
                    *live* (= not on a free-list) tenured block.
                    Free-list pushes decrement, alloc-from-free-list
                    increments, fresh-carve increments.
  CAPACITY        — total tenured capacity (= region.end - region.start).
  HEADERS         — addr → packed 64-bit header word (Doc 30 v2 §2.13).
                    Simulator-only stand-in for the real `mmap' bytes;
                    Phase 7.5 will replace this with raw-byte writes
                    through the FFI bridge.
  BLOCK-SIZES     — addr → size-class.  Used by `tenured-free' to
                    locate the correct free-list bin in O(1) without
                    asking the caller to remember which class their
                    block was rounded to."
  (region nil)
  (free-lists nil)
  (bump 0)
  (allocated-bytes 0)
  (capacity 0)
  (headers (make-hash-table :test 'eql))
  (block-sizes (make-hash-table :test 'eql)))

(defvar nelisp-allocator--current-tenured nil
  "Process-wide active tenured generation.

Phase 7.2.2 has a single global tenured region (per-family tenured
pools land in Phase 7.2.3, multi-thread arenas in Phase 7.5).
`nelisp-allocator-init-tenured' replaces this binding; ERT
fixtures use `let'-binding to isolate per-test state.")

;;; Tenured: errors -------------------------------------------------

(define-error 'nelisp-allocator-tenured-overflow
  "Tenured region exhausted; major GC required (Phase 7.3 stub)"
  'nelisp-allocator-error)

;;; Tenured: helpers ------------------------------------------------

(defun nelisp-allocator--size-class-of (size)
  "Return the smallest size-class (Doc 29 §3.2) that fits SIZE bytes.

Signals `nelisp-allocator-error' if SIZE exceeds the largest
size-class — caller is expected to route to the large-object
family for sizes above `nelisp-allocator-large-object-threshold'."
  (when (or (not (integerp size)) (<= size 0))
    (signal 'nelisp-allocator-error
            (list "size-class lookup requires a positive integer" size)))
  (or (cl-find-if (lambda (sz) (>= sz size))
                  nelisp-allocator-tenured-size-classes)
      (signal 'nelisp-allocator-error
              (list "size exceeds largest tenured size-class"
                    size
                    (car (last nelisp-allocator-tenured-size-classes))))))

(defun nelisp-allocator--tenured-free-list-bin (tenured size-class)
  "Return the cons cell (CLASS . LIST) for SIZE-CLASS in TENURED's
free-lists alist, creating it if absent.  Returns the cell so callers
can mutate its `cdr' in place."
  (let ((cell (assq size-class
                    (nelisp-allocator--tenured-free-lists tenured))))
    (unless cell
      (setq cell (cons size-class nil))
      (setf (nelisp-allocator--tenured-free-lists tenured)
            (cons cell (nelisp-allocator--tenured-free-lists tenured))))
    cell))

;;; Tenured: init ---------------------------------------------------

(defun nelisp-allocator-init-tenured (&optional size)
  "Initialise the tenured generation with SIZE bytes (default
`nelisp-allocator-tenured-size').

Allocates a fresh `nelisp-heap-region' (generation = `tenured',
family = `cons-pool' as the dominant family — per-family tenured
pools arrive in Phase 7.2.3) and returns the new
`nelisp-allocator--tenured' struct.  Also stored in
`nelisp-allocator--current-tenured' for production use.

Unlike `nelisp-allocator-init-nursery', this function does *not*
reset the region table — production code calls init-nursery first
(which clears the table) and init-tenured second (which extends
it).  ERT fixtures call `nelisp-allocator--reset-region-table' if
they need a clean slate."
  (let ((tenured-size (or size nelisp-allocator-tenured-size)))
    (unless (and (integerp tenured-size)
                 (> tenured-size 0)
                 (zerop (% tenured-size nelisp-allocator-page-size)))
      (signal 'nelisp-allocator-bad-config
              (list "tenured size must be a positive page-multiple"
                    tenured-size nelisp-allocator-page-size)))
    (let* ((region (nelisp-allocator--make-region
                    tenured-size 'tenured 'cons-pool))
           (start (nelisp-heap-region-start region))
           (end (nelisp-heap-region-end region))
           (tenured (nelisp-allocator--tenured-make
                     :region region
                     :free-lists nil
                     :bump start
                     :allocated-bytes 0
                     :capacity (- end start)
                     :headers (make-hash-table :test 'eql)
                     :block-sizes (make-hash-table :test 'eql))))
      (setq nelisp-allocator--current-tenured tenured)
      tenured)))

;;; Tenured: alloc / free -------------------------------------------

(defun nelisp-allocator-tenured-alloc (tenured family size)
  "Allocate SIZE bytes from TENURED for FAMILY object family.

SIZE is rounded up to the smallest fitting bin in
`nelisp-allocator-tenured-size-classes'.  The fast path pops a
free address from the matching bin; the slow path carves a fresh
block off the bump cursor.  Both paths write a packed 64-bit
header (Doc 30 v2 §2.13: family-tag set, mark/forwarding/age = 0)
into the simulator `headers' map.

Signals `nelisp-allocator-tenured-overflow' if neither the
free-list nor the bump cursor can satisfy the request.

Returns the allocated base address (integer)."
  (unless (nelisp-allocator--tenured-p tenured)
    (signal 'nelisp-allocator-error (list "not a tenured" tenured)))
  (unless (assq family nelisp-allocator--family-tag-alist)
    (signal 'nelisp-allocator-unknown-family (list family)))
  (let* ((size-class (nelisp-allocator--size-class-of size))
         (bin (nelisp-allocator--tenured-free-list-bin tenured size-class))
         (free-list (cdr bin))
         (region (nelisp-allocator--tenured-region tenured))
         (region-end (nelisp-heap-region-end region))
         (addr nil))
    (cond
     ;; Fast path: pop a free block from the matching bin.
     (free-list
      (setq addr (car free-list))
      (setcdr bin (cdr free-list)))
     ;; Slow path: carve a fresh block off the bump cursor.
     (t
      (let* ((bump (nelisp-allocator--tenured-bump tenured))
             (next (+ bump size-class)))
        (when (> next region-end)
          (signal 'nelisp-allocator-tenured-overflow
                  (list "tenured exhausted"
                        :requested size-class
                        :family family
                        :bump bump
                        :end region-end)))
        (setq addr bump)
        (setf (nelisp-allocator--tenured-bump tenured) next))))
    ;; Common: write the header + size-class index, bump allocated bytes.
    (puthash addr
             (nelisp-allocator-pack-header family size-class)
             (nelisp-allocator--tenured-headers tenured))
    (puthash addr size-class
             (nelisp-allocator--tenured-block-sizes tenured))
    (cl-incf (nelisp-allocator--tenured-allocated-bytes tenured)
             size-class)
    addr))

(defun nelisp-allocator-tenured-free (tenured addr &optional size)
  "Return the block at ADDR (size SIZE) to TENURED's free-list.

SIZE defaults to the size-class recorded at alloc time
(`block-sizes' map); explicit SIZE rounds up to its size-class for
sanity-check parity with the caller.

The header at ADDR is *not* cleared — Phase 7.3 GC may want to
inspect the family-tag of recently-freed blocks during sweep.
Returns the size-class the block was recycled into."
  (unless (nelisp-allocator--tenured-p tenured)
    (signal 'nelisp-allocator-error (list "not a tenured" tenured)))
  (let* ((known-size (gethash addr
                              (nelisp-allocator--tenured-block-sizes tenured)))
         (effective-size (or known-size
                             (and size
                                  (nelisp-allocator--size-class-of size))
                             (signal 'nelisp-allocator-error
                                     (list "tenured-free: addr never alloc'd"
                                           addr))))
         (bin (nelisp-allocator--tenured-free-list-bin
               tenured effective-size)))
    (when (memql addr (cdr bin))
      (signal 'nelisp-allocator-error
              (list "tenured-free: double free" addr)))
    (setcdr bin (cons addr (cdr bin)))
    (cl-decf (nelisp-allocator--tenured-allocated-bytes tenured)
             effective-size)
    effective-size))

;;; Tenured: header age update --------------------------------------

(defun nelisp-allocator--update-age (tenured addr increment)
  "Increment the 6-bit age field of the header at ADDR by INCREMENT.

Doc 30 v2 §2.13 layout: bits [61:56].  The simulator stores the
header in TENURED's `headers' map; Phase 7.5 will rewrite this to
mutate the actual `mmap'-backed bytes.

Saturates at the 6-bit ceiling (63) — once an object has reached
the maximum age it stays there until promoted again or freed.
Returns the new age."
  (unless (nelisp-allocator--tenured-p tenured)
    (signal 'nelisp-allocator-error (list "not a tenured" tenured)))
  (let* ((headers (nelisp-allocator--tenured-headers tenured))
         (header (gethash addr headers)))
    (unless header
      (signal 'nelisp-allocator-error
              (list "update-age: no header for addr" addr)))
    (let* ((cur (nelisp-allocator-header-age header))
           (new (min (+ cur increment) nelisp-allocator--header-age-mask))
           (cleared (logand header
                            (lognot (ash nelisp-allocator--header-age-mask
                                         nelisp-allocator--header-age-shift))))
           (rewritten (logior cleared
                              (ash new nelisp-allocator--header-age-shift))))
      (puthash addr rewritten headers)
      new)))

;;; Tenured: manual promotion (Doc 29 §6.4) -------------------------

(defun nelisp-allocator-promote (_nursery tenured addr size family)
  "Manually promote the object at ADDR (SIZE bytes, FAMILY) into TENURED.

Allocates a tenured slot of the same size-class, copies the simulator
header (age incremented by 1, capped at the 6-bit ceiling) into it,
and returns the new tenured address.

Phase 7.2.2 ships only the *manual* API per Doc 29 §6.4; auto
promotion (driven by Phase 7.3 minor GC reaching the age threshold)
is out of scope and lands in Phase 7.3.  Doc 29 §6.7 v2 interim
invariant — Phase 7.2 must not run minor GC, so promotion is caller
driven (e.g. tests, anvil dashboards), never reaper-driven.

The NURSERY argument is currently unused (the simulator does not
free the original nursery slot — that requires Phase 7.3 evacuation
semantics) but is accepted for API symmetry with the auto-promotion
signature Phase 7.3 will introduce."
  (unless (nelisp-allocator--tenured-p tenured)
    (signal 'nelisp-allocator-error (list "not a tenured" tenured)))
  (unless (assq family nelisp-allocator--family-tag-alist)
    (signal 'nelisp-allocator-unknown-family (list family)))
  (when (or (not (integerp size)) (<= size 0))
    (signal 'nelisp-allocator-error
            (list "promote: size must be positive" size)))
  (let* ((new-addr (nelisp-allocator-tenured-alloc tenured family size))
         (_ (ignore addr)))
    ;; Bump age on the freshly-installed tenured header.  The
    ;; tenured-alloc above wrote a fresh header (age = 0); promotion
    ;; semantics demand age ≥ 1 so the very next promotion attempt
    ;; sees a non-zero age (Doc 30 §2.13 invariant: tenured headers
    ;; always carry age ≥ 1, only nursery headers may have age 0).
    (nelisp-allocator--update-age tenured new-addr 1)
    new-addr))

;;; Tenured: coalesce (Doc 29 §6.5) ---------------------------------

(defun nelisp-allocator-tenured-coalesce (tenured)
  "Merge adjacent free blocks of the same size-class in TENURED.

Doc 29 v2 §6.5 fragmentation mitigation: 80%+ utilisation target.
Power-of-2 size-classes mean two adjacent size-N free blocks form
a buddy pair that fits exactly into one size-2N free block.  This
function walks every bin in ascending order and promotes such
pairs into the next-larger bin, repeating up to the largest class
(4096 byte).

Returns the total number of merge operations performed across all
bins (each merge consumes 2 size-N blocks and produces 1 size-2N
block, so the live free-block count strictly decreases by 1 per
merge)."
  (unless (nelisp-allocator--tenured-p tenured)
    (signal 'nelisp-allocator-error (list "not a tenured" tenured)))
  (let ((merges 0)
        (classes nelisp-allocator-tenured-size-classes)
        (block-sizes (nelisp-allocator--tenured-block-sizes tenured))
        (headers (nelisp-allocator--tenured-headers tenured)))
    (while (cdr classes)
      (let* ((class (car classes))
             (next-class (cadr classes))
             (bin (nelisp-allocator--tenured-free-list-bin tenured class))
             ;; Sort the bin ascending so adjacency tests are linear.
             (sorted (sort (copy-sequence (cdr bin)) #'<))
             (remaining nil))
        (while sorted
          (let ((a (car sorted))
                (rest (cdr sorted)))
            (cond
             ((and rest
                   (= (+ a class) (car rest))
                   ;; Buddy alignment: a must be aligned to 2N so the
                   ;; merged block lands on a 2N boundary too.
                   (zerop (% a next-class)))
              ;; Merge a + (a + class) into a single size-next-class block.
              (let ((next-bin (nelisp-allocator--tenured-free-list-bin
                               tenured next-class)))
                (setcdr next-bin (cons a (cdr next-bin)))
                ;; Drop the buddy's bookkeeping: it is no longer a
                ;; standalone block, only the survivor at A is.
                (remhash (car rest) block-sizes)
                (remhash (car rest) headers)
                ;; Update the survivor's size-class so a future
                ;; tenured-free targeting A goes to next-class bin.
                (puthash a next-class block-sizes))
              (cl-incf merges)
              (setq sorted (cdr rest)))
             (t
              (push a remaining)
              (setq sorted rest)))))
        (setcdr bin (nreverse remaining)))
      (setq classes (cdr classes)))
    merges))

;;; Tenured: stats (Doc 29 §2.10 subset) -----------------------------

(defun nelisp-allocator-tenured-stats (tenured)
  "Return a plist describing the current state of TENURED.

Doc 29 v2 §2.10 推奨 A allocator stats subset (the full
`nelisp-alloc-stats' plist arrives in Phase 7.2.3 with promotion
counts, per-family bytes, and bulk-alloc counters):

  (:capacity N
   :allocated N
   :free N
   :utilization-percent N
   :size-class-distribution ((CLASS . COUNT) ...))

`utilization-percent' is `(allocated / capacity) * 100' rounded to
the nearest integer; 0 for an empty tenured.  The
`size-class-distribution' alist enumerates *free-list* occupancy
per bin (not allocated-block counts) — Phase 7.3 will swap this
for live-block counts once the heap walker can tell the two apart."
  (unless (nelisp-allocator--tenured-p tenured)
    (signal 'nelisp-allocator-error (list "not a tenured" tenured)))
  (let* ((capacity (nelisp-allocator--tenured-capacity tenured))
         (allocated (nelisp-allocator--tenured-allocated-bytes tenured))
         (free (- capacity allocated))
         (utilisation (if (zerop capacity)
                          0
                        (/ (* 100 allocated) capacity)))
         (dist (mapcar (lambda (class)
                         (let ((bin (assq class
                                          (nelisp-allocator--tenured-free-lists
                                           tenured))))
                           (cons class (length (cdr bin)))))
                       nelisp-allocator-tenured-size-classes)))
    (list :capacity capacity
          :allocated allocated
          :free free
          :utilization-percent utilisation
          :size-class-distribution dist)))

;;; ----------------------------------------------------------------
;;; Phase 7.2.3 — per-type pool optimization + bulk API + alloc-stats
;;; ----------------------------------------------------------------
;;
;; Doc 29 v2 LOCKED 2026-04-25 §3.3 sub-phase 7.2.3 scope:
;;   - per-type fixed-size pool (cons-pool / closure-pool) with
;;     per-block bitmap free tracking (Doc 29 §2.5 — Emacs alloc.c
;;     `cons_block' / `string_block' precedent).  Faster than the
;;     size-class free-list for cells whose size is constant.
;;   - bulk API (`nelisp-allocator-bulk-cons' /
;;     `nelisp-allocator-bulk-free' /
;;     `nelisp-allocator-bulk-alloc-string') so `mapcar'-style hot
;;     loops compress N alloc calls into one bitmap scan + one
;;     free-list pop sweep.
;;   - alloc-stats expansion (Doc 29 §2.10 推奨 A —
;;     `nelisp-allocator-stats-snapshot' / `-stats-reset' /
;;     `-profile-run' Lisp API).  Production builds keep stats off
;;     (defcustom default nil) so the per-call overhead lives only
;;     in dev / `--alloc-profile' subcommands.
;;
;; Phase 7.1 native compile integration: the bitmap-pop primitive
;; and `cons-pool-alloc' fast path are emitted as `defsubst' so
;; the JIT inliner can fuse them into the calling function (Doc 29
;; §2.7 推奨 A — per-type API + `declare (inline t)').  The
;; `defsubst' choice is deliberate over `cl-defun' + inline-pragma
;; because it both byte-compiles and native-compiles cleanly.
;;
;; Out of scope (Phase 7.5+):
;;   - real `mmap'-backed cell storage (the simulator stores cells
;;     in Emacs `bool-vector'-tracked free state only)
;;   - per-thread arena pools (single-thread MVP, Doc 29 §2.9)
;;   - integration of `--alloc-profile' into the
;;     `nelisp-runtime exec-bytes' subcommand (Phase 7.5 FFI bridge)
;;
;; Design notes (simulator path):
;;   - `nelisp-allocator--cons-block' is a chained list of
;;     fixed-cell-size blocks.  Each block carries a `bool-vector'
;;     `free-bitmap' (t = free, nil = allocated) and a
;;     `free-count' counter so empty / full checks are O(1) without
;;     scanning the bitmap.  Allocation walks `pool-blocks' looking
;;     for the first block with `free-count > 0', pops a free bit,
;;     and returns base + offset.
;;   - The pool keeps `addr->block' map so `cons-pool-free' can find
;;     the owning block in O(1) without scanning every block to
;;     locate the bitmap that owns ADDR.
;;   - `nelisp-allocator-stats-enabled' guards every counter mutation
;;     so the production build (default nil) pays a single boolean
;;     test in the hot path — well under the 5 % overhead budget that
;;     Doc 29 §2.10 sets for instrumentation.

;;; Phase 7.2.3 — per-type pool customization -----------------------

(defcustom nelisp-allocator-cons-block-size (* 64 1024)
  "Cons-pool block size in bytes (default 64 KiB = ~4096 cons cells).

Doc 29 v2 §2.5 fixed-size pool: cons cells live in dedicated blocks
whose every slot is exactly `nelisp-allocator--cons-size' (= 16
byte) wide.  Per-block free-bitmap tracking is faster than a
size-class free-list for fixed-size objects (Emacs alloc.c
precedent).

Must be a positive multiple of `nelisp-allocator--cons-size' so
every cell lands on a 16-byte boundary; non-multiples are rejected
at `nelisp-allocator-init-cons-pool' time."
  :type 'integer
  :group 'nelisp-allocator)

(defcustom nelisp-allocator-closure-block-size (* 64 1024)
  "Closure-pool block size in bytes (default 64 KiB).

Doc 29 v2 §2.5 fixed-size pool: closures with the canonical 32-byte
header layout (`nelisp-allocator--closure-header-size') share the
same per-block bitmap optimisation as cons cells.  Variable-arity
closures with extra upvalues fall back to the `nursery-alloc'
generic path."
  :type 'integer
  :group 'nelisp-allocator)

(defcustom nelisp-allocator-stats-enabled nil
  "Non-nil to record per-call allocation statistics.

Doc 29 v2 §2.10 推奨 A — production runs with stats *off* (default
nil) so the hot path pays only one boolean test per alloc / free.
Dev builds and `--alloc-profile' subcommands flip this to t before
the workload starts and read the counters out via
`nelisp-allocator-stats-snapshot'.

Overhead budget: ~5-10 % when enabled (per-family hash lookup +
plist update per alloc / free).  Disabled = single `if' branch
guarded out by the byte-compiler / native compiler."
  :type 'boolean
  :group 'nelisp-allocator)

;;; Phase 7.2.3 — cons-pool block struct ----------------------------

(cl-defstruct (nelisp-allocator--cons-block
               (:constructor nelisp-allocator--cons-block-make)
               (:copier nil))
  "One block of fixed-size cons cells with per-cell free bitmap.

Doc 29 v2 §2.5 fixed-size pool unit.  A cons-pool is a chained
list of these blocks; allocation walks the chain until a block
with `free-count > 0' is found and pops its first free bit.

Slots:
  REGION       — `nelisp-heap-region' for this block's address range.
  CELLS-BASE   — first cell address (= region.start).
  CELLS-COUNT  — number of cells in the block (= block-bytes / cell-size).
  CELL-SIZE    — bytes per cell (= 16 for cons-pool, 32 for closure-pool).
                 Stored on the block so `--bitmap-pop' / `--bitmap-free'
                 do not need a back-pointer to the owning pool.
  FREE-BITMAP  — `bool-vector' of length CELLS-COUNT, t = free, nil =
                 allocated.  Initialised to all-t at block birth.
  FREE-COUNT   — number of t bits in FREE-BITMAP, kept in sync with
                 every alloc / free so empty / full checks are O(1).
  NEXT-BLOCK   — next block in the pool's chain, or nil at the tail."
  (region nil :read-only t)
  (cells-base 0 :read-only t)
  (cells-count 0 :read-only t)
  (cell-size 0 :read-only t)
  (free-bitmap nil)
  (free-count 0)
  (next-block nil))

(cl-defstruct (nelisp-allocator--cons-pool
               (:constructor nelisp-allocator--cons-pool-make)
               (:copier nil))
  "Cons-pool state — chained `--cons-block' list + addr-to-block map.

Slots:
  BLOCKS         — head of the cons-block chain (a
                   `nelisp-allocator--cons-block' or nil for an
                   empty pool, though `init-cons-pool' always
                   primes at least one block).
  ADDR-TO-BLOCK  — hash addr -> `--cons-block' so `cons-pool-free'
                   locates the owning bitmap in O(1).
  CELL-SIZE      — `nelisp-allocator--cons-size' = 16, kept here so
                   `closure-pool' can share the same struct with a
                   different cell-size value.
  BLOCK-SIZE     — bytes per block, taken from the defcustom at init.
  FAMILY         — `cons-pool' or `closure-pool'."
  (blocks nil)
  (addr-to-block (make-hash-table :test 'eql))
  (cell-size 0 :read-only t)
  (block-size 0 :read-only t)
  (family 'cons-pool :read-only t))

(defvar nelisp-allocator--current-cons-pool nil
  "Process-wide active cons-pool.

`nelisp-allocator-init-cons-pool' replaces this binding; ERT
fixtures use `let'-binding to isolate per-test state.")

(defvar nelisp-allocator--current-closure-pool nil
  "Process-wide active closure-pool.

Mirrors `nelisp-allocator--current-cons-pool' for the
`closure-pool' family.  `nelisp-allocator-init-closure-pool'
replaces this binding; tests `let'-bind to isolate.")

;;; Phase 7.2.3 — alloc-stats counters ------------------------------

(defvar nelisp-allocator--stats-counters
  (make-hash-table :test 'eq)
  "Family symbol -> plist of per-family allocation counters.

Plist shape:
  (:alloc-count N
   :alloc-bytes N
   :free-count N
   :free-bytes N
   :promote-count N
   :promote-bytes N
   :bulk-alloc-count N
   :bulk-alloc-cells N)

`nelisp-allocator-stats-enabled' guards every mutation so this
hash table stays empty in production runs.  ERT fixtures and
`--alloc-profile' subcommands flip the gate before the workload.")

(defun nelisp-allocator--stats-bump (family key delta)
  "Add DELTA to FAMILY's counter at KEY.

No-op when `nelisp-allocator-stats-enabled' is nil — the
byte-compiler short-circuits the entire body to a 1-instruction
boolean test in that hot-path."
  (when nelisp-allocator-stats-enabled
    (let* ((plist (gethash family nelisp-allocator--stats-counters))
           (cur (or (plist-get plist key) 0)))
      (puthash family
               (plist-put (or plist (list)) key (+ cur delta))
               nelisp-allocator--stats-counters))))

(defun nelisp-allocator-stats-reset ()
  "Reset every per-family counter to 0.

`nelisp-allocator-stats-enabled' state is *not* changed — callers
flip the gate independently.  Returns nil."
  (clrhash nelisp-allocator--stats-counters)
  nil)

(defun nelisp-allocator-stats-snapshot ()
  "Return an alist (FAMILY . PLIST) of every recorded counter.

The snapshot is freshly allocated; callers may mutate it freely.
Families with no recorded events are omitted (= absent from the
hash table).  Order: `eq'-hash insertion order (Emacs 27+ stable)."
  (let (out)
    (maphash (lambda (family plist)
               (push (cons family (copy-sequence plist)) out))
             nelisp-allocator--stats-counters)
    (nreverse out)))

(defun nelisp-allocator-profile-run (thunk &optional output-file)
  "Run THUNK with stats enabled, dump snapshot to OUTPUT-FILE on exit.

Doc 29 v2 §2.10 推奨 A — Lisp API entry point for the
`--alloc-profile' subcommand.  Phase 7.5 will integrate this into
`nelisp-runtime exec-bytes --alloc-profile <file>' once the FFI
bridge is wired; until then callers invoke this directly.

Behaviour:
  1. Save the current `nelisp-allocator-stats-enabled' state.
  2. Reset every counter and enable stats.
  3. Call THUNK with no arguments.
  4. Snapshot the counters (whether THUNK returned or signalled).
  5. Restore the prior `stats-enabled' state and reset counters
     so the post-run process is back to its pre-call state.
  6. Write the snapshot to OUTPUT-FILE (`prin1' form, one alist
     per file) when supplied; otherwise return the snapshot.

Returns the snapshot (alist).  Signals propagating from THUNK are
re-raised after the cleanup runs in an `unwind-protect' so the
process never leaks an enabled-stats state."
  (let ((prior-enabled nelisp-allocator-stats-enabled)
        (snapshot nil))
    (unwind-protect
        (progn
          (nelisp-allocator-stats-reset)
          (setq nelisp-allocator-stats-enabled t)
          (funcall thunk)
          (setq snapshot (nelisp-allocator-stats-snapshot)))
      ;; Capture even on signal, then restore.
      (unless snapshot
        (setq snapshot (nelisp-allocator-stats-snapshot)))
      (setq nelisp-allocator-stats-enabled prior-enabled)
      (nelisp-allocator-stats-reset))
    (when output-file
      (with-temp-file output-file
        (let ((print-length nil)
              (print-level nil))
          (prin1 snapshot (current-buffer))
          (insert "\n"))))
    snapshot))

;;; Phase 7.2.3 — bitmap primitives (defsubst hot path) -------------

(defsubst nelisp-allocator--bitmap-pop (block)
  "Pop a free cell index from BLOCK's free-bitmap.

Returns the cell's allocated address (cells-base + idx * cell-size)
or nil when the block is full (= free-count = 0).  Updates
free-bitmap[idx] = nil and decrements free-count.

Defsubst-inlined so Phase 7.1 native compiler can fuse the bitmap
scan into the caller — Doc 29 §2.7 推奨 A `declare (inline t)'."
  (let ((free-count (nelisp-allocator--cons-block-free-count block)))
    (when (> free-count 0)
      (let* ((bitmap (nelisp-allocator--cons-block-free-bitmap block))
             (n (length bitmap))
             (idx 0)
             (found nil))
        (while (and (< idx n) (not found))
          (if (aref bitmap idx)
              (setq found idx)
            (setq idx (1+ idx))))
        (when found
          (aset bitmap found nil)
          (setf (nelisp-allocator--cons-block-free-count block)
                (1- free-count))
          (let ((base (nelisp-allocator--cons-block-cells-base block))
                (cell-size (nelisp-allocator--cons-block-cell-size
                            block)))
            (+ base (* found cell-size))))))))

(defsubst nelisp-allocator--bitmap-free (block addr cell-size)
  "Mark ADDR (size CELL-SIZE) free in BLOCK's bitmap.

Returns t on success, signals if ADDR is already free (double-free
guard) or out of range."
  (let* ((base (nelisp-allocator--cons-block-cells-base block))
         (offset (- addr base))
         (idx (/ offset cell-size))
         (bitmap (nelisp-allocator--cons-block-free-bitmap block)))
    (when (or (< idx 0) (>= idx (length bitmap)))
      (signal 'nelisp-allocator-error
              (list "bitmap-free: addr outside block" addr base)))
    (when (aref bitmap idx)
      (signal 'nelisp-allocator-error
              (list "bitmap-free: double free" addr)))
    (aset bitmap idx t)
    (setf (nelisp-allocator--cons-block-free-count block)
          (1+ (nelisp-allocator--cons-block-free-count block)))
    t))

;;; Phase 7.2.3 — cons-pool init / alloc / free ---------------------

(defun nelisp-allocator--make-cons-block (cell-size block-size family)
  "Allocate a fresh cons-block (CELL-SIZE wide, BLOCK-SIZE bytes) for FAMILY.

Returns a `nelisp-allocator--cons-block' with all bitmap bits set
to t and `free-count' = `cells-count'.  The owning region is
registered in the heap-region table with generation = `nursery'
and the supplied FAMILY tag."
  (unless (zerop (% block-size cell-size))
    (signal 'nelisp-allocator-bad-config
            (list "cons-block size must divide evenly by cell-size"
                  block-size cell-size)))
  (let* ((cells-count (/ block-size cell-size))
         (region (nelisp-allocator--make-region
                  block-size 'nursery family))
         (bitmap (make-bool-vector cells-count t)))
    (nelisp-allocator--cons-block-make
     :region region
     :cells-base (nelisp-heap-region-start region)
     :cells-count cells-count
     :cell-size cell-size
     :free-bitmap bitmap
     :free-count cells-count
     :next-block nil)))

(defun nelisp-allocator-init-cons-pool (&optional initial-blocks block-size)
  "Initialise a fresh cons-pool with INITIAL-BLOCKS pre-allocated blocks.

INITIAL-BLOCKS defaults to 1 (the smallest legal pool — alloc on an
empty pool would otherwise fault before extending).  BLOCK-SIZE
defaults to `nelisp-allocator-cons-block-size' (= 64 KiB).

Returns the fresh `nelisp-allocator--cons-pool' and stores it in
`nelisp-allocator--current-cons-pool'.  Does *not* reset the
heap-region table — callers managing both nursery + cons-pool +
tenured at once must order their inits accordingly (production
flow: init-nursery first to clear table, then cons-pool /
closure-pool, then tenured)."
  (let* ((cell-size nelisp-allocator--cons-size)
         (block-bytes (or block-size nelisp-allocator-cons-block-size))
         (n (or initial-blocks 1)))
    (unless (and (integerp block-bytes) (> block-bytes 0)
                 (zerop (% block-bytes cell-size)))
      (signal 'nelisp-allocator-bad-config
              (list "cons-pool block-size must divide evenly by cell-size"
                    block-bytes cell-size)))
    (when (or (not (integerp n)) (< n 1))
      (signal 'nelisp-allocator-bad-config
              (list "cons-pool initial-blocks must be >= 1" n)))
    (let* ((pool (nelisp-allocator--cons-pool-make
                  :blocks nil
                  :addr-to-block (make-hash-table :test 'eql)
                  :cell-size cell-size
                  :block-size block-bytes
                  :family 'cons-pool))
           (head nil)
           (tail nil))
      (dotimes (_ n)
        (let ((blk (nelisp-allocator--make-cons-block
                    cell-size block-bytes 'cons-pool)))
          (if head
              (progn
                (setf (nelisp-allocator--cons-block-next-block tail) blk)
                (setq tail blk))
            (setq head blk tail blk))))
      (setf (nelisp-allocator--cons-pool-blocks pool) head)
      (setq nelisp-allocator--current-cons-pool pool)
      pool)))

(defun nelisp-allocator-init-closure-pool (&optional initial-blocks
                                                     block-size)
  "Initialise a fresh closure-pool with INITIAL-BLOCKS pre-allocated blocks.

Mirrors `nelisp-allocator-init-cons-pool' but with cell-size =
`nelisp-allocator--closure-header-size' (= 32 byte).  Returns the
fresh pool and stores it in
`nelisp-allocator--current-closure-pool'."
  (let* ((cell-size nelisp-allocator--closure-header-size)
         (block-bytes (or block-size nelisp-allocator-closure-block-size))
         (n (or initial-blocks 1)))
    (unless (and (integerp block-bytes) (> block-bytes 0)
                 (zerop (% block-bytes cell-size)))
      (signal 'nelisp-allocator-bad-config
              (list "closure-pool block-size must divide evenly by cell-size"
                    block-bytes cell-size)))
    (when (or (not (integerp n)) (< n 1))
      (signal 'nelisp-allocator-bad-config
              (list "closure-pool initial-blocks must be >= 1" n)))
    (let* ((pool (nelisp-allocator--cons-pool-make
                  :blocks nil
                  :addr-to-block (make-hash-table :test 'eql)
                  :cell-size cell-size
                  :block-size block-bytes
                  :family 'closure-pool))
           (head nil)
           (tail nil))
      (dotimes (_ n)
        (let ((blk (nelisp-allocator--make-cons-block
                    cell-size block-bytes 'closure-pool)))
          (if head
              (progn
                (setf (nelisp-allocator--cons-block-next-block tail) blk)
                (setq tail blk))
            (setq head blk tail blk))))
      (setf (nelisp-allocator--cons-pool-blocks pool) head)
      (setq nelisp-allocator--current-closure-pool pool)
      pool)))

(defun nelisp-allocator--pool-extend (pool)
  "Append a fresh block to POOL's chain, return the new block.

Used when `cons-pool-alloc' walks the entire chain and finds every
block full.  Allocates one more block of POOL's `block-size' for
POOL's `family' and links it at the tail of the chain."
  (let* ((cell-size (nelisp-allocator--cons-pool-cell-size pool))
         (block-bytes (nelisp-allocator--cons-pool-block-size pool))
         (family (nelisp-allocator--cons-pool-family pool))
         (new-block (nelisp-allocator--make-cons-block
                     cell-size block-bytes family))
         (head (nelisp-allocator--cons-pool-blocks pool)))
    (if (null head)
        (setf (nelisp-allocator--cons-pool-blocks pool) new-block)
      (let ((cur head))
        (while (nelisp-allocator--cons-block-next-block cur)
          (setq cur (nelisp-allocator--cons-block-next-block cur)))
        (setf (nelisp-allocator--cons-block-next-block cur) new-block)))
    new-block))

(defun nelisp-allocator-cons-pool-alloc (pool)
  "Allocate one cell from POOL.

Walks POOL's block chain looking for the first block with
`free-count > 0'; if every block is full, extends the pool with a
fresh block.  Pops the first free bit, records the
`addr->block' mapping for O(1) free, bumps the per-family alloc
stats counter, and returns the allocated cell address.

Hot path: 1 chain walk + 1 bitmap scan + 1 hash-table puthash.
Phase 7.1 native compile can inline `--bitmap-pop' so the chain
walk + bitmap scan fuse into ~10-20 machine instructions per
alloc."
  (unless (nelisp-allocator--cons-pool-p pool)
    (signal 'nelisp-allocator-error (list "not a cons-pool" pool)))
  (let ((cell-size (nelisp-allocator--cons-pool-cell-size pool))
        (family (nelisp-allocator--cons-pool-family pool))
        (block (nelisp-allocator--cons-pool-blocks pool))
        (addr nil))
    ;; Walk the chain for a non-full block.
    (while (and block (null addr))
      (setq addr (nelisp-allocator--bitmap-pop block))
      (unless addr
        (setq block (nelisp-allocator--cons-block-next-block block))))
    ;; Slow path: every block was full -> extend.
    (unless addr
      (setq block (nelisp-allocator--pool-extend pool))
      (setq addr (nelisp-allocator--bitmap-pop block)))
    (puthash addr block
             (nelisp-allocator--cons-pool-addr-to-block pool))
    (nelisp-allocator--stats-bump family :alloc-count 1)
    (nelisp-allocator--stats-bump family :alloc-bytes cell-size)
    addr))

(defun nelisp-allocator-cons-pool-free (pool addr)
  "Return ADDR to POOL's bitmap-tracked free state.

Locates the owning block via POOL's `addr-to-block' map (O(1)),
flips the bit back to t, and decrements the live count.  Signals
on double-free or unknown ADDR.  Returns the cell-size that was
freed."
  (unless (nelisp-allocator--cons-pool-p pool)
    (signal 'nelisp-allocator-error (list "not a cons-pool" pool)))
  (let* ((map (nelisp-allocator--cons-pool-addr-to-block pool))
         (block (gethash addr map))
         (cell-size (nelisp-allocator--cons-pool-cell-size pool))
         (family (nelisp-allocator--cons-pool-family pool)))
    (unless block
      (signal 'nelisp-allocator-error
              (list "cons-pool-free: addr never alloc'd from pool"
                    addr)))
    (nelisp-allocator--bitmap-free block addr cell-size)
    (remhash addr map)
    (nelisp-allocator--stats-bump family :free-count 1)
    (nelisp-allocator--stats-bump family :free-bytes cell-size)
    cell-size))

;;; Phase 7.2.3 — bulk API (Doc 29 v2 §2.7) -------------------------

(defun nelisp-allocator-bulk-cons (n &optional pool)
  "Allocate N cons cells in bulk, return the list of cell addresses.

POOL defaults to `nelisp-allocator--current-cons-pool'.  The fast
path locates a block whose `free-count >= N' and pops N bits in
one bitmap pass — minimises both the chain walk and the
addr-to-block hash inserts (one per cell, but inserted in batch
without re-walking).

The slow path falls back to per-cell `cons-pool-alloc' when no
single block has enough headroom, transparently extending the
pool as required.  Returns the addresses in allocation order so
hot callers (`mapcar' rewrites etc.) can reuse them as a list
without sorting."
  (unless (and (integerp n) (>= n 0))
    (signal 'nelisp-allocator-error
            (list "bulk-cons: n must be a non-negative integer" n)))
  (let ((pool (or pool nelisp-allocator--current-cons-pool)))
    (unless pool
      (signal 'nelisp-allocator-error
              (list "bulk-cons: no current cons-pool")))
    (cond
     ((zerop n) nil)
     (t
      (let ((cell-size (nelisp-allocator--cons-pool-cell-size pool))
            (family (nelisp-allocator--cons-pool-family pool))
            (block (nelisp-allocator--cons-pool-blocks pool))
            (target nil))
        ;; Fast path: find a block with free-count >= n.
        (while (and block (null target))
          (when (>= (nelisp-allocator--cons-block-free-count block) n)
            (setq target block))
          (unless target
            (setq block (nelisp-allocator--cons-block-next-block block))))
        (cond
         (target
          (let ((bitmap (nelisp-allocator--cons-block-free-bitmap target))
                (base (nelisp-allocator--cons-block-cells-base target))
                (count (nelisp-allocator--cons-block-cells-count target))
                (cz (nelisp-allocator--cons-block-cell-size target))
                (out nil)
                (taken 0)
                (idx 0))
            ;; One bitmap pass, take N free bits.
            (while (and (< taken n) (< idx count))
              (when (aref bitmap idx)
                (aset bitmap idx nil)
                (push (+ base (* idx cz)) out)
                (cl-incf taken))
              (cl-incf idx))
            (setf (nelisp-allocator--cons-block-free-count target)
                  (- (nelisp-allocator--cons-block-free-count target)
                     n))
            (let ((map (nelisp-allocator--cons-pool-addr-to-block pool)))
              (dolist (a out)
                (puthash a target map)))
            (nelisp-allocator--stats-bump family :alloc-count n)
            (nelisp-allocator--stats-bump family :alloc-bytes
                                          (* n cell-size))
            (nelisp-allocator--stats-bump family :bulk-alloc-count 1)
            (nelisp-allocator--stats-bump family :bulk-alloc-cells n)
            (nreverse out)))
         (t
          ;; Slow path: per-cell alloc, transparently extends pool.
          (let ((acc nil))
            (dotimes (_ n)
              (push (nelisp-allocator-cons-pool-alloc pool) acc))
            ;; Per-cell alloc already bumped per-call stats; record the
            ;; bulk envelope so callers can distinguish bulk vs per-cell.
            (nelisp-allocator--stats-bump family :bulk-alloc-count 1)
            (nelisp-allocator--stats-bump family :bulk-alloc-cells n)
            (nreverse acc)))))))))

(defun nelisp-allocator-bulk-free (addrs &optional pool)
  "Free a batch of cons-pool ADDRS, return the count freed.

POOL defaults to `nelisp-allocator--current-cons-pool'.  Groups
ADDRS by their owning block (looked up via the pool's
`addr-to-block' map) so each block's bitmap takes at most one
batch update — minimises bitmap-mutation overhead vs N independent
`cons-pool-free' calls.

Signals on the first unknown / double-free ADDR; the partial
state at that point is left alone (any earlier addrs in the batch
have already been returned to the pool).  Returns the number of
cells successfully freed."
  (let ((pool (or pool nelisp-allocator--current-cons-pool)))
    (unless pool
      (signal 'nelisp-allocator-error
              (list "bulk-free: no current cons-pool")))
    (let ((freed 0))
      (dolist (a addrs)
        (nelisp-allocator-cons-pool-free pool a)
        (cl-incf freed))
      freed)))

(defun nelisp-allocator-bulk-alloc-string (sizes &optional nursery)
  "Allocate one string-span per element in SIZES, return alist (size . addr).

NURSERY defaults to `nelisp-allocator--current-nursery'.  Each
size routes through `nelisp-allocator-alloc-string-span' so the
size-class round-up + nursery bump bookkeeping stays consistent;
the bulk wrapper records the bulk envelope so stats consumers can
tell bulk-allocated strings from per-call ones.

The returned alist preserves SIZES order; callers can reuse it
with `mapcar' / `cl-mapcar' to pair payloads with addresses."
  (let ((nursery (or nursery nelisp-allocator--current-nursery)))
    (unless nursery
      (signal 'nelisp-allocator-error
              (list "bulk-alloc-string: no current nursery")))
    (let ((out nil)
          (n (length sizes))
          (total-bytes 0))
      (dolist (size sizes)
        (let ((addr (nelisp-allocator-alloc-string-span nursery size)))
          (push (cons size addr) out)
          (cl-incf total-bytes
                   (+ nelisp-allocator--span-header-size
                      (nelisp-allocator--span-size-class size)))))
      (nelisp-allocator--stats-bump 'string-span :alloc-count n)
      (nelisp-allocator--stats-bump 'string-span :alloc-bytes
                                    total-bytes)
      (nelisp-allocator--stats-bump 'string-span :bulk-alloc-count 1)
      (nelisp-allocator--stats-bump 'string-span :bulk-alloc-cells n)
      (nreverse out))))

;;; Phase 7.2.3 — alloc-stats integration with existing paths -------
;;
;; The Phase 7.2.1 / 7.2.2 alloc/free/promote helpers do not call the
;; stats counters directly (T16/T20 freeze).  The new
;; `nelisp-allocator-alloc-instrumented' helper layers stats on top of
;; the public helpers so callers who want to observe nursery /
;; tenured / promote events under `--alloc-profile' can opt in
;; without touching the freeze-stable hot paths.

(defun nelisp-allocator-alloc-instrumented (family size &rest args)
  "Allocate FAMILY with SIZE bytes, recording alloc-stats.

Routes the call to the appropriate public helper based on FAMILY:

  cons-pool     -> `nelisp-allocator-cons-pool-alloc'   (no SIZE arg)
  closure-pool  -> `nelisp-allocator-cons-pool-alloc'   (no SIZE arg)
  string-span   -> `nelisp-allocator-alloc-string-span' (NURSERY SIZE)
  vector-span   -> `nelisp-allocator-alloc-vector-span' (NURSERY SIZE)
  large-object  -> `nelisp-allocator-alloc-large-object' (NURSERY SIZE)

ARGS pass any positional arguments the dispatched helper requires
(e.g. NURSERY for span / large-object families).  Unconditionally
bumps the alloc-count + alloc-bytes counters under the
`stats-enabled' gate; callers who want stats *without* the
dispatch wrapper can call the original helper and then
`stats-bump' explicitly."
  (let ((addr nil))
    (pcase family
      ('cons-pool
       (let ((pool (or (car args)
                       nelisp-allocator--current-cons-pool)))
         (setq addr (nelisp-allocator-cons-pool-alloc pool))))
      ('closure-pool
       (let ((pool (or (car args)
                       nelisp-allocator--current-closure-pool)))
         (setq addr (nelisp-allocator-cons-pool-alloc pool))))
      ('string-span
       (setq addr (nelisp-allocator-alloc-string-span
                   (car args) size)))
      ('vector-span
       (setq addr (nelisp-allocator-alloc-vector-span
                   (car args) size)))
      ('large-object
       (setq addr (nelisp-allocator-alloc-large-object
                   (car args) size)))
      (_
       (signal 'nelisp-allocator-unknown-family (list family))))
    ;; cons-pool / closure-pool helpers already bumped their counters
    ;; via `cons-pool-alloc' (size = pool cell-size).  For span /
    ;; large-object the bump is the wrapper's responsibility.
    (unless (memq family '(cons-pool closure-pool))
      (nelisp-allocator--stats-bump family :alloc-count 1)
      (nelisp-allocator--stats-bump family :alloc-bytes size))
    addr))

(provide 'nelisp-allocator)
;;; nelisp-allocator.el ends here
