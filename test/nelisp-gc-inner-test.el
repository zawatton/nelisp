;;; nelisp-gc-inner-test.el --- Phase 7.3.1 mark phase ERTs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 7.3.1 MVP subset — wire decoder round-trip, tri-color invariant
;; on simulated heaps, root extraction from Doc 28 §2.9 metadata, and
;; finalizer queue enqueue/drain with the `:once' resurrection policy.
;; See docs/design/30-phase7.3-gc-inner.org §3.1.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-gc-inner)

;;; Helpers ----------------------------------------------------------

(defun nelisp-gc-inner-test--mk-region (id start end children-of)
  "Build a Doc 29 §1.4 region descriptor for tests."
  (list :region-id   id
        :start       start
        :end         end
        :generation  :nursery
        :family      :cons-pool
        :children-of children-of))

(defun nelisp-gc-inner-test--reset ()
  "Reset every piece of mutable state between ERTs."
  (nelisp-gc-inner-init-mark-state)
  (nelisp-gc-inner-finalizer-queue-reset))


;;; 1. Wire decoder round-trip --------------------------------------

(ert-deftest nelisp-gc-inner-decode-wire-roundtrip ()
  "encode-wire-for-test → decode-wire is identity on a canonical fixture."
  (nelisp-gc-inner-test--reset)
  (let* ((bv (make-bool-vector 4 nil))
         (_  (progn (aset bv 0 t) (aset bv 2 t)))
         (internal
          (list :gc-metadata-version 1
                :function-name 'fn-roundtrip
                :safe-points
                (list (list :id 0 :kind 'entry :pc-offset 0
                            :live-roots bv :frame-size 32
                            :frame-layout (list :size 32 :slots 4
                                                :base 0 :slot-table nil)))))
         (wire   (nelisp-gc-inner-encode-wire-for-test internal))
         (decoded (nelisp-gc-inner-decode-wire wire))
         (sp0     (car (plist-get decoded :safe-points))))
    (should (eq 1 (plist-get decoded :gc-metadata-version)))
    (should (eq 'fn-roundtrip (plist-get decoded :function-name)))
    (should (eq 0 (plist-get sp0 :id)))
    (should (eq 'entry (plist-get sp0 :kind)))
    (should (equal bv (plist-get sp0 :live-roots)))
    (should (eq 32 (plist-get sp0 :frame-size)))))

(ert-deftest nelisp-gc-inner-decode-wire-rejects-version-skew ()
  "Decoder signals on unknown metadata version (forward-compat guard)."
  (let ((wire (list :gc-metadata-version 999
                    :function-name 'x
                    :safe-points nil)))
    (should-error (nelisp-gc-inner-decode-wire wire))))


;;; 2. Mark phase on empty heap -------------------------------------

(ert-deftest nelisp-gc-inner-mark-phase-on-empty-heap-marks-zero ()
  "Empty root set + empty registry → 0 marked, 0 grey, invariant holds."
  (nelisp-gc-inner-test--reset)
  (let ((stats (nelisp-gc-inner-run-mark-phase nil nil)))
    (should (eq 0 (plist-get stats :marked-count)))
    (should (eq 0 (plist-get stats :grey-count)))
    (should (integerp (plist-get stats :elapsed-ms)))
    (should (>= (plist-get stats :elapsed-ms) 0))))


;;; 3. Tri-color invariant on a non-trivial graph -------------------

(ert-deftest nelisp-gc-inner-mark-phase-tri-color-invariant ()
  "After mark phase, no address remains :grey (work-list fully drained).
We build a tiny address graph 100→200→300 with one cycle (300→200) and
mark from root {100}; every reachable address must end up :black, every
unreachable must stay :white, and `:grey-count' must be 0."
  (nelisp-gc-inner-test--reset)
  (let* ((graph (let ((h (make-hash-table :test 'eql)))
                  (puthash 100 (list 200) h)
                  (puthash 200 (list 300) h)
                  (puthash 300 (list 200) h)  ; cycle back to 200
                  (puthash 400 nil           h)  ; unreachable leaf
                  h))
         (region (nelisp-gc-inner-test--mk-region
                  0 100 500
                  (lambda (a) (gethash a graph))))
         (stats  (nelisp-gc-inner-run-mark-phase '(100) (list region))))
    (should (eq 0 (plist-get stats :grey-count)))
    (should (eq :black (nelisp-gc-inner-mark-color 100)))
    (should (eq :black (nelisp-gc-inner-mark-color 200)))
    (should (eq :black (nelisp-gc-inner-mark-color 300)))
    ;; 400 was never enqueued by the walker, so it's implicit :white.
    (should (eq :white (nelisp-gc-inner-mark-color 400)))
    (should (eq 3 (plist-get stats :marked-count)))))


;;; 4. Root scan via Doc 28 §2.9 GC metadata ------------------------

(ert-deftest nelisp-gc-inner-collect-roots-from-gc-metadata ()
  "Collect-roots resolves live-roots bitmap through frame layout.
Two safe-points (one entry, one back-edge) on different frames each
contribute one address; the heap-region :start gets contributed too."
  (nelisp-gc-inner-test--reset)
  (let* ((bv0 (let ((v (make-bool-vector 2 nil))) (aset v 0 t) v))
         (bv1 (let ((v (make-bool-vector 2 nil))) (aset v 1 t) v))
         (meta-a
          (list :gc-metadata-version 1
                :function-name 'fa
                :safe-points
                (list (list :id 0 :kind 'entry :pc-offset 0
                            :live-roots bv0 :frame-size 16
                            :frame-layout (list :base 1000 :slots 2)))))
         (meta-b
          (list :gc-metadata-version 1
                :function-name 'fb
                :safe-points
                (list (list :id 0 :kind 'back-edge :pc-offset 12
                            :live-roots bv1 :frame-size 16
                            :frame-layout (list :base 2000 :slots 2)))))
         (region (nelisp-gc-inner-test--mk-region 0 500 600 #'ignore))
         (roots  (nelisp-gc-inner-collect-roots
                  (list meta-a meta-b) (list region))))
    ;; vid 0 at base 1000 → 1000; vid 1 at base 2000 → 2008; region :start 500.
    (should (memq 1000 roots))
    (should (memq 2008 roots))
    (should (memq 500  roots))
    ;; Dedup invariant: re-running must keep length identical.
    (should (= (length roots)
               (length (cl-remove-duplicates roots))))))


;;; 5. Finalizer queue enqueue + drain ------------------------------

(ert-deftest nelisp-gc-inner-finalizer-queue-enqueue-and-drain ()
  "Enqueue two finalizers, drain runs both with their ADDR argument,
counters update, and the `:once' policy blocks re-fire of the same
ADDR after drain."
  (nelisp-gc-inner-test--reset)
  (let* ((seen-addrs nil)
         (fn (lambda (a) (push a seen-addrs))))
    (should (= 1 (nelisp-gc-inner-enqueue-finalizer fn 111)))
    (should (= 2 (nelisp-gc-inner-enqueue-finalizer fn 222)))
    (should (= 2 (nelisp-gc-inner-finalizer-queue-length)))
    (let ((stats (nelisp-gc-inner-drain-finalizer-queue)))
      (should (eq 2 (plist-get stats :fired)))
      (should (eq 0 (plist-get stats :errors)))
      (should (eq 0 (plist-get stats :remaining))))
    ;; Both ADDRs appeared exactly once (order = head-to-tail).
    (should (equal (sort (copy-sequence seen-addrs) #'<)
                   '(111 222)))
    ;; :once policy — re-enqueue 111 is dropped.
    (should (= 0 (nelisp-gc-inner-enqueue-finalizer fn 111)))
    (should (= 0 (nelisp-gc-inner-finalizer-queue-length)))))


;;; 6. Sweep phase — per-pool free-list rebuild (Phase 7.3.2) -------

(defun nelisp-gc-inner-test--mk-region-with-objects
    (id start end family objects &optional children-of)
  "Build a Doc 29 §1.4 region descriptor that carries OBJECTS.
OBJECTS is the `(addr . size)' list returned to the sweep phase via
the `:objects' key.  CHILDREN-OF is an optional walker used by the
mark phase (defaults to a no-edges leaf walker so tests don't need
to wire one up when they only care about sweep)."
  (list :region-id   id
        :start       start
        :end         end
        :generation  :nursery
        :family      family
        :objects     objects
        :children-of (or children-of #'ignore)))

(ert-deftest nelisp-gc-inner-sweep-removes-white-objects ()
  "Whites land on the free-list, blacks do not — basic per-region sweep.
Address 100 is marked black (live root), 200 is left white (unreachable);
the post-sweep cons-pool free-list contains exactly (200 . 16)."
  (nelisp-gc-inner-test--reset)
  (let ((region (nelisp-gc-inner-test--mk-region-with-objects
                 0 100 1000 :cons-pool
                 '((100 . 16) (200 . 16)))))
    ;; Manually colour: 100 black (live), 200 default-white.
    (nelisp-gc-inner-set-color 100 :black)
    (let ((stats (nelisp-gc-inner-run-sweep-phase (list region))))
      (should (eq 1 (plist-get stats :swept-count)))
      (should (eq 16 (plist-get stats :freed-bytes)))
      (should (eq 1 (plist-get stats :live-count))))
    (let ((fl (nelisp-gc-inner-free-list-for-family :cons-pool)))
      (should (equal fl '((200 . 16))))
      ;; closure-pool was untouched by this sweep.
      (should (null (nelisp-gc-inner-free-list-for-family :closure-pool))))))

(ert-deftest nelisp-gc-inner-sweep-per-family-isolation ()
  "Sweeping cons-pool whites does not perturb closure-pool whites.
Two regions of different families both have whites; per-family free-
lists are populated independently and the per-family stats slice
reports the right family-keyed totals."
  (nelisp-gc-inner-test--reset)
  (let* ((cons-region (nelisp-gc-inner-test--mk-region-with-objects
                       0 100 1000 :cons-pool
                       '((100 . 16) (116 . 16))))
         (closure-region (nelisp-gc-inner-test--mk-region-with-objects
                          1 1000 2000 :closure-pool
                          '((1000 . 32) (1032 . 32))))
         ;; Mark only one of the cons addresses live, none of closures live.
         (_ (nelisp-gc-inner-set-color 100 :black))
         (stats (nelisp-gc-inner-run-sweep-phase
                 (list cons-region closure-region)))
         (per-fam (plist-get stats :per-family)))
    ;; cons-pool: one white (116), one black (100).
    (let ((c (cdr (assq :cons-pool per-fam))))
      (should (eq 1 (plist-get c :swept-count)))
      (should (eq 16 (plist-get c :freed-bytes)))
      (should (eq 1 (plist-get c :live-count))))
    ;; closure-pool: two whites, zero blacks.
    (let ((c (cdr (assq :closure-pool per-fam))))
      (should (eq 2 (plist-get c :swept-count)))
      (should (eq 64 (plist-get c :freed-bytes)))
      (should (eq 0 (plist-get c :live-count))))
    ;; Free-lists are family-isolated.
    (should (equal (nelisp-gc-inner-free-list-for-family :cons-pool)
                   '((116 . 16))))
    (let ((cfl (nelisp-gc-inner-free-list-for-family :closure-pool)))
      ;; Walker prepends as it scans, so order is reversed-input.
      (should (= 2 (length cfl)))
      (should (memq 1000 (mapcar #'car cfl)))
      (should (memq 1032 (mapcar #'car cfl))))))

(ert-deftest nelisp-gc-inner-sweep-frees-correct-bytes ()
  "Total :freed-bytes = sum of white object sizes across every region.
Three regions, mixed sizes (16/32/64), only some marked live; the
top-level :freed-bytes must match the white-only byte total."
  (nelisp-gc-inner-test--reset)
  (let* ((r1 (nelisp-gc-inner-test--mk-region-with-objects
              0 100 1000 :cons-pool
              '((100 . 16) (116 . 16) (132 . 16))))
         (r2 (nelisp-gc-inner-test--mk-region-with-objects
              1 1000 2000 :closure-pool
              '((1000 . 32) (1032 . 32))))
         (r3 (nelisp-gc-inner-test--mk-region-with-objects
              2 2000 3000 :vector-span
              '((2000 . 64))))
         ;; Whites: 116, 132, 1000, 1032, 2000.  Live: 100.
         (_ (nelisp-gc-inner-set-color 100 :black))
         (stats (nelisp-gc-inner-run-sweep-phase (list r1 r2 r3))))
    (should (eq (+ 16 16 32 32 64) (plist-get stats :freed-bytes)))
    (should (eq 5 (plist-get stats :swept-count)))
    (should (eq 1 (plist-get stats :live-count)))
    (should (eq 3 (plist-get stats :region-count)))))

(ert-deftest nelisp-gc-inner-sweep-large-object-special-case ()
  "Large-object family uses dedicated regions; sweep handles them like
any other family — the family-tag flows through `:per-family' entries."
  (nelisp-gc-inner-test--reset)
  (let* ((region (nelisp-gc-inner-test--mk-region-with-objects
                  0 #x100000 #x200000 :large-object
                  '((#x100000 . 8192))))
         ;; Default white (no mark call) → goes to free-list.
         (stats (nelisp-gc-inner-run-sweep-phase (list region)))
         (fl    (nelisp-gc-inner-free-list-for-family :large-object))
         (lp    (cdr (assq :large-object (plist-get stats :per-family)))))
    (should (eq 1 (plist-get lp :swept-count)))
    (should (eq 8192 (plist-get lp :freed-bytes)))
    (should (equal fl '((#x100000 . 8192))))
    ;; Other families remain empty per-family entries.
    (let ((c (cdr (assq :cons-pool (plist-get stats :per-family)))))
      (should (eq 0 (plist-get c :swept-count)))
      (should (eq 0 (plist-get c :freed-bytes))))))

(ert-deftest nelisp-gc-inner-finalizer-enqueued-on-white-with-registered ()
  "Whites with a registered finalizer push onto the finalizer queue;
the registered :black does not.  (Doc 30 §6.9 v2 BLOCKER 2 mitigation.)"
  (nelisp-gc-inner-test--reset)
  (let* ((calls 0)
         (fn (lambda (_a) (cl-incf calls)))
         (finalizer-table (list (cons 100 fn) (cons 200 fn)))
         (region (nelisp-gc-inner-test--mk-region-with-objects
                  0 100 1000 :cons-pool
                  '((100 . 16) (200 . 16))))
         ;; 100 = black (live), 200 = white (garbage).
         (_ (nelisp-gc-inner-set-color 100 :black))
         (stats (nelisp-gc-inner-run-sweep-phase
                 (list region) finalizer-table)))
    (should (eq 1 (plist-get stats :finalizers-queued)))
    (should (eq 1 (nelisp-gc-inner-finalizer-queue-length)))
    ;; Until drain, fn has not actually fired.
    (should (eq 0 calls))
    (let ((dstats (nelisp-gc-inner-drain-finalizer-queue)))
      (should (eq 1 (plist-get dstats :fired)))
      (should (eq 0 (plist-get dstats :errors))))
    (should (eq 1 calls))))

(ert-deftest nelisp-gc-inner-fragmentation-stats-reports-largest ()
  "Fragmentation stats find the largest free block + ratio is sensible.
Two same-size whites in cons-pool plus one giant white in vector-span:
:largest-free-block must equal the giant, ratio = giant / total."
  (nelisp-gc-inner-test--reset)
  (let* ((r-cons (nelisp-gc-inner-test--mk-region-with-objects
                  0 100 1000 :cons-pool
                  '((100 . 16) (200 . 16))))
         (r-vec  (nelisp-gc-inner-test--mk-region-with-objects
                  1 2000 3000 :vector-span
                  '((2000 . 1024))))
         (_     (nelisp-gc-inner-run-sweep-phase (list r-cons r-vec)))
         (frag  (nelisp-gc-inner-sweep-fragmentation-stats)))
    (should (eq 1024 (plist-get frag :largest-free-block)))
    (should (eq (+ 16 16 1024) (plist-get frag :total-free-bytes)))
    (should (eq 3 (plist-get frag :total-free-blocks)))
    (let ((ratio (plist-get frag :fragmentation-ratio)))
      (should (numberp ratio))
      (should (< 0.9 ratio))
      (should (<= ratio 1.0)))
    ;; Per-family slice spells out the largest within each family.
    (let ((cf (cdr (assq :cons-pool (plist-get frag :per-family))))
          (vf (cdr (assq :vector-span (plist-get frag :per-family)))))
      (should (eq 16 (plist-get cf :largest)))
      (should (eq 1024 (plist-get vf :largest))))))

(ert-deftest nelisp-gc-inner-full-cycle-mark-then-sweep ()
  "`run-full-cycle' chains mark + sweep + finalizer enqueue end-to-end.
Two-object cons-pool region: only one is reached from the root, so
sweep must report exactly one white and the live one must be black
in the post-mark state."
  (nelisp-gc-inner-test--reset)
  (let* ((graph (let ((h (make-hash-table :test 'eql)))
                  (puthash 100 nil h)  ; live leaf
                  (puthash 200 nil h)  ; unreachable leaf
                  h))
         (region (list :region-id 0 :start 100 :end 1000
                       :generation :nursery :family :cons-pool
                       :children-of (lambda (a) (gethash a graph))
                       :objects '((100 . 16) (200 . 16))))
         (out   (nelisp-gc-inner-run-full-cycle '(100) (list region))))
    (let ((mark (plist-get out :mark))
          (sweep (plist-get out :sweep)))
      (should (eq 0 (plist-get mark :grey-count)))
      (should (eq 1 (plist-get mark :marked-count)))
      (should (eq 1 (plist-get sweep :swept-count)))
      (should (eq 16 (plist-get sweep :freed-bytes)))
      (should (eq :black (nelisp-gc-inner-mark-color 100)))
      ;; 200 was popped *off* the free-list-feeding white set, but
      ;; sweep does not relabel; mark colour stays :white.
      (should (eq :white (nelisp-gc-inner-mark-color 200))))))

(ert-deftest nelisp-gc-inner-rebuild-free-list-empty-on-fully-marked ()
  "When every object in a region is :black, that family's free-list is
empty after sweep — no free-list entry leaks for live objects."
  (nelisp-gc-inner-test--reset)
  (let ((region (nelisp-gc-inner-test--mk-region-with-objects
                 0 100 1000 :cons-pool
                 '((100 . 16) (200 . 16) (300 . 16)))))
    (nelisp-gc-inner-set-color 100 :black)
    (nelisp-gc-inner-set-color 200 :black)
    (nelisp-gc-inner-set-color 300 :black)
    (let ((stats (nelisp-gc-inner-run-sweep-phase (list region))))
      (should (eq 0 (plist-get stats :swept-count)))
      (should (eq 0 (plist-get stats :freed-bytes)))
      (should (eq 3 (plist-get stats :live-count))))
    (should (null (nelisp-gc-inner-rebuild-free-list-for-family :cons-pool)))
    ;; All other families also empty.
    (dolist (fam '(:closure-pool :string-span :vector-span :large-object))
      (should (null (nelisp-gc-inner-rebuild-free-list-for-family fam))))))


;;; 7. Cheney semispace nursery copy (Phase 7.3.3, T24) -----------

(defun nelisp-gc-inner-test--mk-nursery-region (id start end)
  "Build a `:nursery' Doc 29 §1.4 region descriptor for Cheney tests."
  (list :region-id   id
        :start       start
        :end         end
        :generation  :nursery
        :family      :cons-pool
        :children-of #'ignore))

(defun nelisp-gc-inner-test--mk-from-space-fn (objects)
  "Return a `(addr → plist)' lookup over OBJECTS (alist of `(addr . plist)').
Plists carry `:header' / `:fields' / `:size'."
  (let ((tbl (make-hash-table :test 'eql)))
    (dolist (entry objects)
      (puthash (car entry) (cdr entry) tbl))
    (lambda (addr) (gethash addr tbl))))

(ert-deftest nelisp-gc-inner-init-semispace-allocates-pointers ()
  "init-semispace seeds scan-pointer = free-pointer = to-space :start."
  (nelisp-gc-inner-test--reset)
  (let* ((from (nelisp-gc-inner-test--mk-nursery-region 0 0     1000))
         (to   (nelisp-gc-inner-test--mk-nursery-region 1 10000 11000))
         (semi (nelisp-gc-inner-init-semispace from to)))
    (should (nelisp-gc-inner--semispace-p semi))
    (should (eq from (nelisp-gc-inner--semispace-from-space semi)))
    (should (eq to   (nelisp-gc-inner--semispace-to-space   semi)))
    (should (= 10000 (nelisp-gc-inner--semispace-scan-pointer semi)))
    (should (= 10000 (nelisp-gc-inner--semispace-free-pointer semi)))
    (should (zerop (hash-table-count
                    (nelisp-gc-inner--semispace-copied-objects semi))))))

(ert-deftest nelisp-gc-inner-forward-marks-bit-62 ()
  "`--forward' sets bit 62 of the header (forwarding overload, §2.13)."
  (let* ((header (nelisp-gc-inner--make-header 32 1 0))   ; size 32, fam 1, age 0
         (forwarded (nelisp-gc-inner--forward header 12345)))
    (should-not (nelisp-gc-inner--forwarded-p header))
    (should (nelisp-gc-inner--forwarded-p forwarded))
    ;; mark bit / age / family preserved across the forward.
    (should-not (nelisp-gc-inner--header-mark-bit-p forwarded))
    (should (= 0 (nelisp-gc-inner--header-age forwarded)))
    (should (= 1 (nelisp-gc-inner--header-family forwarded)))))

(ert-deftest nelisp-gc-inner-forwarded-addr-extract-correct ()
  "`--forwarded-addr' returns the bits 47:0 we encoded via `--forward'."
  (let* ((header (nelisp-gc-inner--make-header 64 2 1))
         (target #xDEADBEEF)
         (forwarded (nelisp-gc-inner--forward header target)))
    (should (= target (nelisp-gc-inner--forwarded-addr forwarded)))
    ;; The bit-level helper agrees with the simulator side-table:
    ;; encoding survives a round trip through the bit operations.
    (should (= target
               (logand forwarded nelisp-gc-inner--header-size-mask)))))

(ert-deftest nelisp-gc-inner-run-minor-gc-copies-reachable-only ()
  "Cheney minor GC copies live (root-reachable) objects only.

Three from-space objects: A (root-reachable), B (reachable from A),
C (unreachable garbage).  After minor GC, A + B must be in to-space
(forwarded) and C must remain only in from-space (not forwarded)."
  (nelisp-gc-inner-test--reset)
  (let* ((from   (nelisp-gc-inner-test--mk-nursery-region 0 100  500))
         (to     (nelisp-gc-inner-test--mk-nursery-region 1 1000 1500))
         (hdr    (nelisp-gc-inner--make-header 16 0 0))
         (a-addr 100) (b-addr 200) (c-addr 300)
         (objects
          (list
           (cons a-addr (list :header hdr :fields (list b-addr) :size 16))
           (cons b-addr (list :header hdr :fields nil           :size 16))
           (cons c-addr (list :header hdr :fields nil           :size 16))))
         (objs-fn (nelisp-gc-inner-test--mk-from-space-fn objects))
         (semi (nelisp-gc-inner-init-semispace from to))
         (stats (nelisp-gc-inner-run-minor-gc semi (list a-addr) objs-fn)))
    ;; A + B copied, C left behind.
    (should (eq 2 (plist-get stats :copied-count)))
    (should (eq 32 (plist-get stats :copied-bytes)))
    (should (nelisp-gc-inner--lookup-forwarded semi a-addr))
    (should (nelisp-gc-inner--lookup-forwarded semi b-addr))
    (should (null (nelisp-gc-inner--lookup-forwarded semi c-addr)))
    ;; Forwarded root list is non-empty + points into to-space range.
    (let ((roots (plist-get stats :forwarded-roots)))
      (should (= 1 (length roots)))
      (should (>= (car roots) 1000))
      (should (<  (car roots) 1500)))
    ;; Cheney termination: scan = free at end.
    (should (= (plist-get stats :scan-pointer)
               (plist-get stats :free-pointer)))))

(ert-deftest nelisp-gc-inner-run-minor-gc-flip-semispace ()
  "After minor GC + flip, what was to-space becomes the new from-space."
  (nelisp-gc-inner-test--reset)
  (let* ((from   (nelisp-gc-inner-test--mk-nursery-region 0 100  500))
         (to     (nelisp-gc-inner-test--mk-nursery-region 1 1000 1500))
         (hdr    (nelisp-gc-inner--make-header 16 0 0))
         (a-addr 100)
         (objects
          (list (cons a-addr (list :header hdr :fields nil :size 16))))
         (objs-fn (nelisp-gc-inner-test--mk-from-space-fn objects))
         (semi (nelisp-gc-inner-init-semispace from to)))
    (nelisp-gc-inner-run-minor-gc semi (list a-addr) objs-fn)
    (let ((from-before (nelisp-gc-inner--semispace-from-space semi))
          (to-before   (nelisp-gc-inner--semispace-to-space   semi)))
      (nelisp-gc-inner-flip-semispace semi)
      ;; Roles swapped.
      (should (eq to-before   (nelisp-gc-inner--semispace-from-space semi)))
      (should (eq from-before (nelisp-gc-inner--semispace-to-space   semi)))
      ;; Pointers reset to the new to-space (= old from-space) :start.
      (should (= (plist-get from-before :start)
                 (nelisp-gc-inner--semispace-scan-pointer semi)))
      (should (= (plist-get from-before :start)
                 (nelisp-gc-inner--semispace-free-pointer semi)))
      ;; Forwarding side-table cleared for the next cycle.
      (should (zerop (hash-table-count
                      (nelisp-gc-inner--semispace-copied-objects semi)))))))

(ert-deftest nelisp-gc-inner-promotion-candidate-age-threshold ()
  "Objects whose post-survival age >= threshold land in :promotion-candidates.

Threshold is `nelisp-gc-inner-promotion-age-threshold' (default 2).
Set up two objects: one with age 0 (post-increment age 1, NOT a
candidate), one with age 1 (post-increment 2, IS a candidate)."
  (nelisp-gc-inner-test--reset)
  (let* ((from   (nelisp-gc-inner-test--mk-nursery-region 0 100  500))
         (to     (nelisp-gc-inner-test--mk-nursery-region 1 1000 1500))
         (young  (nelisp-gc-inner--make-header 16 0 0))   ; age 0 → 1
         (mature (nelisp-gc-inner--make-header 16 0 1))   ; age 1 → 2 (>= 2)
         (young-addr  100)
         (mature-addr 200)
         (objects
          (list
           (cons young-addr  (list :header young  :fields nil :size 16))
           (cons mature-addr (list :header mature :fields nil :size 16))))
         (objs-fn (nelisp-gc-inner-test--mk-from-space-fn objects))
         (semi (nelisp-gc-inner-init-semispace from to))
         (stats (nelisp-gc-inner-run-minor-gc
                 semi (list young-addr mature-addr) objs-fn)))
    (let* ((candidates (plist-get stats :promotion-candidates))
           (mature-to (nelisp-gc-inner--lookup-forwarded semi mature-addr))
           (young-to  (nelisp-gc-inner--lookup-forwarded semi young-addr)))
      (should (member mature-to candidates))
      (should-not (member young-to candidates))
      ;; Sanity: the candidate's stored to-cell carries the bumped age.
      (let* ((to-objs (plist-get stats :to-space-objects))
             (mature-cell (gethash mature-to to-objs))
             (mature-hdr (plist-get mature-cell :header)))
        (should (>= (nelisp-gc-inner--header-age mature-hdr) 2))
        (should (nelisp-gc-inner--promotion-candidate-p mature-hdr))))))


(provide 'nelisp-gc-inner-test)
;;; nelisp-gc-inner-test.el ends here
