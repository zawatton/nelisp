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
contribute one address.  T65 (T51 codex critical #5): region.start is
NO LONGER a synthetic root (the prior implementation conflated
\"region table held alive by host\" with \"first heap object always
live\" — see `collect-roots' docstring for the GC-correctness
rationale)."
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
    ;; vid 0 at base 1000 → 1000; vid 1 at base 2000 → 2008.
    (should (memq 1000 roots))
    (should (memq 2008 roots))
    ;; T65: region :start (500) is NOT injected as a synthetic root.
    (should (null (memq 500 roots)))
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
         (to     (nelisp-gc-inner-test--mk-nursery-region 1 1000 1400))
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
         (to     (nelisp-gc-inner-test--mk-nursery-region 1 1000 1400))
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
         (to     (nelisp-gc-inner-test--mk-nursery-region 1 1000 1400))
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


;;; 8. Card-marking write barrier (Phase 7.3.4, T26) ---------------

(defun nelisp-gc-inner-test--mk-tenured-region (id start end)
  "Build a `:tenured' Doc 29 §1.4 region descriptor for card-table tests."
  (list :region-id   id
        :start       start
        :end         end
        :generation  :tenured
        :family      :cons-pool
        :children-of #'ignore))

(ert-deftest nelisp-gc-inner-init-card-table-clean ()
  "Fresh card table starts with every dirty bit clear (= nil) and
exposes a sensible card-count derived from region size / card size."
  (nelisp-gc-inner-test--reset)
  (let* ((tenured (nelisp-gc-inner-test--mk-tenured-region
                   ;; Small region: 16KB so we get exactly 4 cards
                   ;; of 4KB each — a tractable test fixture.
                   0 0 (* 16 1024)))
         (table (nelisp-gc-inner-init-card-table tenured)))
    (should (nelisp-gc-inner--card-table-p table))
    (should (= 4 (nelisp-gc-inner--card-table-card-count table)))
    (should (= nelisp-gc-inner-card-size
               (nelisp-gc-inner--card-table-card-size table)))
    (should (= 0  (nelisp-gc-inner--card-table-region-start table)))
    (should (= (* 16 1024)
               (nelisp-gc-inner--card-table-region-end table)))
    ;; All dirty bits clear.
    (let ((bv (nelisp-gc-inner--card-table-dirty-bits table)))
      (should (bool-vector-p bv))
      (should (= 4 (length bv)))
      (dotimes (i 4)
        (should-not (aref bv i))))
    ;; Stats agree.
    (let ((stats (nelisp-gc-inner-card-table-stats table)))
      (should (= 4 (plist-get stats :total-cards)))
      (should (= 0 (plist-get stats :dirty-cards)))
      (should (= 0.0 (plist-get stats :dirty-percent))))))

(ert-deftest nelisp-gc-inner-write-barrier-marks-dirty ()
  "Cross-generation write (tenured slot ← nursery pointer) dirties the
card whose page contains the tenured slot.  Other cards stay clean."
  (nelisp-gc-inner-test--reset)
  (let* ((nursery (nelisp-gc-inner-test--mk-nursery-region 0 #x10000 #x20000))
         (tenured (nelisp-gc-inner-test--mk-tenured-region
                   1 0 (* 16 1024)))                ; 4 cards
         (table   (nelisp-gc-inner-init-card-table tenured))
         ;; Write at byte 5000 (= card 1, since 5000 / 4096 = 1) of a
         ;; nursery-pointer (#x10100 is inside [#x10000, #x20000)).
         (slot-addr   5000)
         (nursery-ptr #x10100)
         (rc (nelisp-gc-inner-write-barrier
              table slot-addr nursery-ptr nursery)))
    ;; Barrier reports `dirtied'.
    (should (eq t rc))
    ;; Card 1 dirty; cards 0, 2, 3 clean.
    (let ((bv (nelisp-gc-inner--card-table-dirty-bits table)))
      (should-not (aref bv 0))
      (should      (aref bv 1))
      (should-not (aref bv 2))
      (should-not (aref bv 3)))
    ;; Stats: exactly one card dirty.
    (let ((stats (nelisp-gc-inner-card-table-stats table)))
      (should (= 1 (plist-get stats :dirty-cards)))
      (should (= 25.0 (plist-get stats :dirty-percent))))
    ;; clear-card-table puts every bit back to nil.
    (nelisp-gc-inner-clear-card-table table)
    (let ((bv2 (nelisp-gc-inner--card-table-dirty-bits table)))
      (dotimes (i 4)
        (should-not (aref bv2 i))))))

(ert-deftest nelisp-gc-inner-write-barrier-skips-intra-tenured ()
  "Intra-generation writes (tenured slot ← tenured pointer) must NOT
dirty any card.  The barrier checks the WRITTEN-VALUE against the
nursery boundary and only marks when the value points into nursery."
  (nelisp-gc-inner-test--reset)
  (let* ((nursery (nelisp-gc-inner-test--mk-nursery-region 0 #x10000 #x20000))
         (tenured (nelisp-gc-inner-test--mk-tenured-region 1 0 (* 16 1024)))
         (table   (nelisp-gc-inner-init-card-table tenured))
         ;; Tenured-to-tenured write: slot at 1000 (card 0), value 5000
         ;; (also tenured, NOT in nursery [#x10000, #x20000)).
         (rc (nelisp-gc-inner-write-barrier
              table 1000 5000 nursery)))
    ;; Barrier reports `not dirtied'.
    (should (null rc))
    ;; Every card stays clean.
    (let ((bv (nelisp-gc-inner--card-table-dirty-bits table)))
      (dotimes (i 4)
        (should-not (aref bv i))))
    ;; Out-of-range write: addr beyond region-end is silently ignored
    ;; (the barrier returns nil; no crash).  This keeps the mutator
    ;; able to write to addresses *outside* the card-table's covered
    ;; region without paying for a bounds check.
    (let ((rc2 (nelisp-gc-inner-write-barrier
                table (* 100 1024) #x10100 nursery)))
      (should (null rc2)))
    ;; Even a real cross-gen value dirties only the in-range write —
    ;; flip the slot so we can prove the barrier still works after
    ;; the no-op cases.
    (let ((rc3 (nelisp-gc-inner-write-barrier
                table 1000 #x10100 nursery)))
      (should (eq t rc3)))
    (let ((bv (nelisp-gc-inner--card-table-dirty-bits table)))
      (should      (aref bv 0))
      (should-not (aref bv 1))
      (should-not (aref bv 2))
      (should-not (aref bv 3)))))

(ert-deftest
    nelisp-gc-inner-scan-remembered-set-yields-cross-gen-pointers ()
  "Walk dirty cards → return per-card list of cross-generation
pointers.  Two dirty cards each carrying one tenured object whose
`:fields' contain a nursery pointer; the scanner must emit both
edges, dropping the (clean) third card entirely."
  (nelisp-gc-inner-test--reset)
  (let* ((nursery (nelisp-gc-inner-test--mk-nursery-region 0 #x10000 #x20000))
         (tenured (nelisp-gc-inner-test--mk-tenured-region 1 0 (* 16 1024)))
         (table   (nelisp-gc-inner-init-card-table tenured))
         ;; Three tenured objects, one per card boundary.  Card 0 obj
         ;; at addr 100, card 1 obj at addr 5000, card 2 obj at addr
         ;; 9000.  Cards 0 and 1 will be dirtied; card 2 stays clean.
         (objs (list
                ;; Card 0: tenured object at 100 with a nursery field.
                (cons 100  (list :fields (list #x10100)))
                ;; Card 1: tenured object at 5000 with a nursery field.
                (cons 5000 (list :fields (list #x10200)))
                ;; Card 2: tenured object at 9000 with a tenured-only
                ;; field (#x500 is *inside* tenured, not nursery — so
                ;; this card stays clean and the scanner skips it).
                (cons 9000 (list :fields (list #x500)))))
         (objs-fn (lambda () objs)))
    ;; Mutator records the cross-gen writes.
    (nelisp-gc-inner-write-barrier table 100  #x10100 nursery)
    (nelisp-gc-inner-write-barrier table 5000 #x10200 nursery)
    ;; Card 2 (object at 9000) was *not* a cross-gen write so the
    ;; barrier was never invoked there.
    (let* ((rem-set (nelisp-gc-inner-scan-remembered-set
                     table nursery objs-fn)))
      ;; Two dirty cards yielded entries; card 2 (clean) is absent.
      (should (= 2 (length rem-set)))
      (should (assoc 0 rem-set))
      (should (assoc 1 rem-set))
      (should-not (assoc 2 rem-set))
      ;; Each entry's pointer list contains its nursery edge.
      (should (member #x10100 (cdr (assoc 0 rem-set))))
      (should (member #x10200 (cdr (assoc 1 rem-set)))))
    ;; Sanity: a clean table after `clear' yields nil.
    (nelisp-gc-inner-clear-card-table table)
    (should (null (nelisp-gc-inner-scan-remembered-set
                   table nursery objs-fn)))))


;;; 9. Phase 7.3.5 promotion + age + young-root-limited (T29) -----

(require 'nelisp-allocator)

(defun nelisp-gc-inner-test--mk-large-tenured (id start end)
  "Build a `:tenured' region big enough for promotion sweep tests."
  (list :region-id   id
        :start       start
        :end         end
        :generation  :tenured
        :family      :cons-pool
        :children-of #'ignore))

(ert-deftest nelisp-gc-inner-promote-to-tenured-allocates-and-forwards ()
  "promote-to-tenured copies a candidate from to-space into TENURED,
removes the to-space entry, installs a tenured cell, and forwards
the original from-space addr → tenured addr on the semispace."
  (nelisp-gc-inner-test--reset)
  (nelisp-allocator--reset-region-table)
  (let* ((from   (nelisp-gc-inner-test--mk-nursery-region 0 100  500))
         (to     (nelisp-gc-inner-test--mk-nursery-region 1 1000 1400))
         (mature (nelisp-gc-inner--make-header 16 0 1)) ; age 1 → 2 post-bump
         (mature-from-addr 200)
         (objects
          (list
           (cons mature-from-addr
                 (list :header mature :fields nil :size 16))))
         (objs-fn (nelisp-gc-inner-test--mk-from-space-fn objects))
         (semi    (nelisp-gc-inner-init-semispace from to))
         (tenured (nelisp-allocator-init-tenured (* 64 1024)))
         (stats (nelisp-gc-inner-run-minor-gc
                 semi (list mature-from-addr) objs-fn))
         (to-objects (plist-get stats :to-space-objects))
         (mature-to-addr (nelisp-gc-inner--lookup-forwarded
                          semi mature-from-addr)))
    (should (integerp mature-to-addr))
    (should (gethash mature-to-addr to-objects))
    ;; Promote.
    (let ((tenured-addr
           (nelisp-gc-inner-promote-to-tenured
            semi tenured to-objects mature-to-addr :cons-pool 16)))
      (should (integerp tenured-addr))
      (should (>= tenured-addr 0))
      ;; Tenured entry installed.
      (should (gethash tenured-addr to-objects))
      (let ((tcell (gethash tenured-addr to-objects)))
        (should (eq t (plist-get tcell :tenured)))
        (should (= 16 (plist-get tcell :size)))
        ;; Fresh tenured header has age 0 (allocator semantics).
        (should (= 0 (nelisp-gc-inner--header-age
                      (plist-get tcell :header)))))
      ;; To-space entry removed (would otherwise double-age).
      (should-not (gethash mature-to-addr to-objects))
      ;; Forwarding lookup now points to tenured.
      (should (= tenured-addr
                 (nelisp-gc-inner--lookup-forwarded
                  semi mature-from-addr))))))

(ert-deftest nelisp-gc-inner-age-tick-increments-survivor-age ()
  "age-tick bumps the age field of every cell in the to-space hash."
  (nelisp-gc-inner-test--reset)
  (let ((tbl (make-hash-table :test 'eql)))
    ;; Two cells with ages 0 and 5; expected post-tick ages 1 and 6.
    (puthash 100 (list :addr 100
                       :header (nelisp-gc-inner--make-header 16 0 0)
                       :fields nil :size 16)
             tbl)
    (puthash 200 (list :addr 200
                       :header (nelisp-gc-inner--make-header 16 0 5)
                       :fields nil :size 16)
             tbl)
    (let ((stats (nelisp-gc-inner-age-tick tbl)))
      (should (= 2 (plist-get stats :total-count)))
      (should (= 2 (plist-get stats :bumped-count)))
      (should (= 0 (plist-get stats :saturated-count)))
      (should (= 1 (nelisp-gc-inner--header-age
                    (plist-get (gethash 100 tbl) :header))))
      (should (= 6 (nelisp-gc-inner--header-age
                    (plist-get (gethash 200 tbl) :header)))))))

(ert-deftest nelisp-gc-inner-age-tick-saturates-at-63 ()
  "age-tick clamps at the 6-bit ceiling (63) — already-saturated cells
report through `:saturated-count' and stay at 63."
  (nelisp-gc-inner-test--reset)
  (let ((tbl (make-hash-table :test 'eql)))
    ;; Cell at age 63 (saturated) + cell at age 62 (will saturate).
    (puthash 100 (list :addr 100
                       :header (nelisp-gc-inner--make-header 16 0 63)
                       :fields nil :size 16)
             tbl)
    (puthash 200 (list :addr 200
                       :header (nelisp-gc-inner--make-header 16 0 62)
                       :fields nil :size 16)
             tbl)
    (let ((stats (nelisp-gc-inner-age-tick tbl)))
      (should (= 2 (plist-get stats :total-count)))
      ;; The age-62 cell got bumped to 63 (still bumped, not saturated
      ;; *before* the call); the age-63 cell was already saturated.
      (should (= 1 (plist-get stats :bumped-count)))
      (should (= 1 (plist-get stats :saturated-count)))
      ;; Both end at 63 — the bump uses `--header-with-age' which
      ;; clamps internally.
      (should (= 63 (nelisp-gc-inner--header-age
                     (plist-get (gethash 100 tbl) :header))))
      (should (= 63 (nelisp-gc-inner--header-age
                     (plist-get (gethash 200 tbl) :header)))))
    ;; Second tick: both cells now saturated.
    (let ((stats2 (nelisp-gc-inner-age-tick tbl)))
      (should (= 0 (plist-get stats2 :bumped-count)))
      (should (= 2 (plist-get stats2 :saturated-count))))))

(ert-deftest nelisp-gc-inner-major-young-root-limited-respects-card-dirty ()
  "`run-major-young-root-limited' filters HEAP-REGIONS to only those
overlapping a dirty card — clean tenured pages skip the sweep."
  (nelisp-gc-inner-test--reset)
  (let* ((nursery (nelisp-gc-inner-test--mk-nursery-region 0 #x10000 #x20000))
         ;; Two tenured regions, both 16KB (= 4 cards each), but the
         ;; card-table only covers tenured-A (the simulator wires one
         ;; card-table per tenured region).
         (tenured-a (nelisp-gc-inner-test--mk-large-tenured 1 0 (* 16 1024)))
         (tenured-b (nelisp-gc-inner-test--mk-large-tenured 2 (* 32 1024)
                                                            (* 48 1024)))
         (table   (nelisp-gc-inner-init-card-table tenured-a))
         (semi (nelisp-gc-inner-init-semispace
                nursery
                (nelisp-gc-inner-test--mk-nursery-region 99 #x30000 #x40000))))
    ;; Dirty exactly one card on tenured-A.
    (nelisp-gc-inner-write-barrier table 5000 #x10100 nursery)
    (let* ((tenured-objs-fn (lambda () nil))
           (stats (nelisp-gc-inner-run-major-young-root-limited
                   table semi nil
                   (list tenured-a tenured-b)
                   tenured-objs-fn)))
      (should (eq :major-young-root-limited (plist-get stats :kind)))
      (should (eq t (plist-get stats :card-table-cleared)))
      ;; tenured-B has no dirty cards on this card-table → filtered out.
      ;; tenured-A had a dirty card → retained.  So filter-region count
      ;; must be 1 (just tenured-A).
      (should (= 1 (plist-get stats :limited-region-count)))
      ;; Pause-time field is integer ms (smoke check, no real budget).
      (should (integerp (plist-get stats :pause-ms)))
      ;; Card table cleared.
      (let ((bv (nelisp-gc-inner--card-table-dirty-bits table)))
        (dotimes (i (length bv))
          (should-not (aref bv i)))))))

(ert-deftest nelisp-gc-inner-promotion-policy-threshold-2-promotes-survivors ()
  "End-to-end: minor GC + age policy + threshold check + promote.
A nursery survivor with age 1 (post-copy bumps to 2) becomes a
promotion candidate and is moved into TENURED via
`run-minor-gc-with-promotion'.  A young (age 0) sibling stays in
to-space.  The candidate's :fields survive the round trip."
  (nelisp-gc-inner-test--reset)
  (nelisp-allocator--reset-region-table)
  (let* ((from   (nelisp-gc-inner-test--mk-nursery-region 0 100  500))
         (to     (nelisp-gc-inner-test--mk-nursery-region 1 1000 1400))
         (tenured-region (nelisp-gc-inner-test--mk-large-tenured
                          2 (* 64 1024) (* 128 1024)))
         (card-table (nelisp-gc-inner-init-card-table tenured-region))
         (young-hdr  (nelisp-gc-inner--make-header 16 0 0))
         (mature-hdr (nelisp-gc-inner--make-header 16 0 1)) ; → age 2
         (young-from-addr  100)
         (mature-from-addr 200)
         (objects
          (list
           (cons young-from-addr  (list :header young-hdr  :fields nil :size 16))
           (cons mature-from-addr (list :header mature-hdr :fields nil :size 16))))
         (objs-fn (nelisp-gc-inner-test--mk-from-space-fn objects))
         (semi (nelisp-gc-inner-init-semispace from to))
         (tenured (nelisp-allocator-init-tenured (* 64 1024)))
         (tenured-objects-fn (lambda () nil))
         (stats (nelisp-gc-inner-run-minor-gc-with-promotion
                 semi tenured card-table
                 (list young-from-addr mature-from-addr)
                 objs-fn tenured-objects-fn)))
    (should (eq :minor (plist-get stats :kind)))
    (should (= 2 (plist-get stats :copied-count)))
    ;; Exactly 1 promotion candidate (mature) — young is age-1 post-copy
    ;; (< threshold 2).
    (should (= 1 (plist-get stats :promoted-count)))
    (let ((cands (plist-get stats :promotion-candidates))
          (tenured-addrs (plist-get stats :tenured-addresses)))
      (should (= 1 (length cands)))
      (should (= 1 (length tenured-addrs)))
      ;; The candidate descriptor carries (FAMILY . SIZE).
      (let ((cand (car cands)))
        (should (eq :cons-pool (cadr cand)))
        (should (= 16 (cddr cand))))
      ;; Forwarding now points the original from-addr to tenured.
      (let* ((tenured-addr (car tenured-addrs))
             (forward-target (nelisp-gc-inner--lookup-forwarded
                              semi mature-from-addr)))
        (should (= tenured-addr forward-target))))
    ;; The tenured allocator reports the new allocation.
    (should (> (nelisp-allocator--tenured-allocated-bytes tenured) 0))
    ;; Card table cleared (Phase 7.3.5 invariant from
    ;; run-minor-gc-with-barrier wrapper).
    (should (eq t (plist-get stats :card-table-cleared)))))



;;; 14. Phase 7.3.6 — scheduler + safe-point + finalizer executor ---

(ert-deftest nelisp-gc-inner-scheduler-triggers-minor-on-nursery-full ()
  "scheduler-on-allocation accumulates bytes; once it crosses the
nursery threshold the *next* allocation returns `'minor', and the
counter does *not* reset until the GC dispatch path resets it.

Coverage matrix:
  - sub-threshold alloc → `'none'
  - cumulative crossing → `'minor', minor-trigger-count++
  - tenured 80%+ utilization → `'major', major-trigger-count++
  - minor takes precedence over major when both fire"
  (nelisp-gc-inner-test--reset)
  (nelisp-allocator--reset-region-table)
  (nelisp-gc-inner-scheduler-reset)
  ;; Drop the threshold to a tiny value so we can exercise crossing
  ;; without actually allocating 4MB worth of fixtures.
  (let ((nelisp-gc-inner-nursery-trigger-bytes 1024)
        (nelisp-gc-inner-tenured-trigger-percent 80))
    ;; First alloc 512 bytes — below threshold, no GC.
    (should (eq 'none (nelisp-gc-inner-scheduler-on-allocation 512 nil)))
    (should (= 512 nelisp-gc-inner--bytes-since-minor))
    (should (= 0 nelisp-gc-inner--minor-trigger-count))
    ;; Second alloc 600 bytes — total 1112, crosses 1024 → 'minor.
    (should (eq 'minor (nelisp-gc-inner-scheduler-on-allocation 600 nil)))
    (should (= 1 nelisp-gc-inner--minor-trigger-count))
    ;; Counter still 1112 — scheduler does not auto-reset; the
    ;; dispatcher (safe-point-poll) is responsible.  Reset manually
    ;; to test the major path next.
    (setq nelisp-gc-inner--bytes-since-minor 0)
    ;; Now wire a tenured at 80% utilization and check 'major fires.
    (let* ((tenured (nelisp-allocator-init-tenured (* 64 1024))))
      ;; Force allocated-bytes to slightly above 80% of capacity (= 80%
      ;; of 64KB).  Integer-math `pct = used * 100 / cap', so we set
      ;; used so that `(used * 100) >= cap * 80'.  Use ceiling form to
      ;; avoid truncation putting us at 79% when computing the fixture
      ;; from the other direction.
      (setf (nelisp-allocator--tenured-allocated-bytes tenured)
            (1+ (/ (* (nelisp-allocator--tenured-capacity tenured) 80)
                   100)))
      (should (eq 'major (nelisp-gc-inner-scheduler-on-allocation
                          16 tenured)))
      (should (= 1 nelisp-gc-inner--major-trigger-count))
      ;; Minor takes precedence: bump bytes-since-minor over the
      ;; nursery threshold while tenured is still 80%; expect 'minor.
      (setq nelisp-gc-inner--bytes-since-minor 0)
      (should (eq 'minor (nelisp-gc-inner-scheduler-on-allocation
                          2048 tenured)))
      (should (= 2 nelisp-gc-inner--minor-trigger-count)))))

(ert-deftest nelisp-gc-inner-finalizer-executor-drains-with-resurrection-once ()
  "drain-finalizer-queue-with-policy fires every queued thunk with
:once resurrection bookkeeping — a resurrected ADDR is recorded in
`--finalizer-fired' so a re-enqueue from inside the thunk is dropped
on the floor.

Coverage matrix:
  - all queued thunks fire (FIFO, no leaks)
  - one thunk signals; errors counter increments, drain continues
  - one thunk's ADDR resurrects; resurrected counter increments and
    re-enqueue of that ADDR is silently dropped
  - queue is empty post-drain (atomic swap invariant)"
  (nelisp-gc-inner-test--reset)
  (nelisp-gc-inner-scheduler-reset)
  (let* ((seen nil)
         (fn-ok (lambda (a) (push a seen)))
         (fn-err (lambda (_a) (signal 'arith-error nil)))
         (fn-resurrect (lambda (a) (push a seen)))
         ;; Resurrection check: ADDR 200 is "still reachable" post-call.
         (rcheck (lambda (a) (eql a 200))))
    (nelisp-gc-inner-enqueue-finalizer fn-ok 100)
    (nelisp-gc-inner-enqueue-finalizer fn-resurrect 200)
    (nelisp-gc-inner-enqueue-finalizer fn-err 300)
    (should (= 3 (nelisp-gc-inner-finalizer-queue-length)))
    (let ((stats (nelisp-gc-inner-drain-finalizer-queue-with-policy rcheck)))
      ;; All three FN bodies invoked.
      (should (eq 3 (plist-get stats :fired)))
      ;; Exactly one signalled.
      (should (eq 1 (plist-get stats :errors)))
      ;; Exactly one ADDR was reachable post-call.
      (should (eq 1 (plist-get stats :resurrected)))
      (should (eq 0 (plist-get stats :remaining))))
    ;; Both successful FNs ran, in FIFO order; the error FN didn't push.
    (should (equal (sort (copy-sequence seen) #'<) '(100 200)))
    ;; :once policy — re-enqueue 200 dedups silently.
    (should (= 0 (nelisp-gc-inner-enqueue-finalizer fn-ok 200)))
    (should (= 0 (nelisp-gc-inner-finalizer-queue-length)))
    ;; And re-enqueue of 100 (also fired) likewise dedups.
    (should (= 0 (nelisp-gc-inner-enqueue-finalizer fn-ok 100)))
    (should (= 0 (nelisp-gc-inner-finalizer-queue-length)))))

(ert-deftest nelisp-gc-inner-run-managed-cycle-end-to-end ()
  "run-managed-cycle integrates safe-point-poll + drain end-to-end.

Setup: nursery with one survivor, threshold dialled low so the
allocation-triggered scheduler fires `'minor' on next tick.  Wire a
finalizer-table entry for an unreachable nursery address (no longer
forwarded post-copy → eligible for queue + drain).

Expect:
  - poll plist reports `:gc-kind 'minor' and a non-nil cycle-result
  - drain plist reports `:fired ≥ 0' (queue may be empty in this
    minor-only path; key invariant is shape correctness)
  - bytes-since-minor reset to 0 after dispatch"
  (nelisp-gc-inner-test--reset)
  (nelisp-allocator--reset-region-table)
  (nelisp-gc-inner-scheduler-reset)
  (let* ((from   (nelisp-gc-inner-test--mk-nursery-region 0 100  500))
         (to     (nelisp-gc-inner-test--mk-nursery-region 1 1000 1400))
         (tenured-region (nelisp-gc-inner-test--mk-large-tenured
                          2 (* 64 1024) (* 128 1024)))
         (card-table (nelisp-gc-inner-init-card-table tenured-region))
         (live-hdr  (nelisp-gc-inner--make-header 16 0 0))
         (live-from-addr 100)
         (objects
          (list
           (cons live-from-addr (list :header live-hdr :fields nil :size 16))))
         (objs-fn (nelisp-gc-inner-test--mk-from-space-fn objects))
         (semi (nelisp-gc-inner-init-semispace from to))
         (tenured (nelisp-allocator-init-tenured (* 64 1024)))
         (tenured-objects-fn (lambda () nil))
         ;; Seed the scheduler so a tick will trigger 'minor.
         (nelisp-gc-inner-nursery-trigger-bytes 1024))
    (setq nelisp-gc-inner--bytes-since-minor 2048)
    ;; Also enqueue a finalizer so the drain path has something to do.
    (let* ((fired-addrs nil)
           (fin-fn (lambda (a) (push a fired-addrs))))
      (nelisp-gc-inner-enqueue-finalizer fin-fn 999)
      (let* ((heap-regions (list from to tenured-region))
             (result
              (nelisp-gc-inner-run-managed-cycle
               semi card-table tenured (list live-from-addr)
               objs-fn tenured-objects-fn heap-regions
               nil ;; finalizer-table — not used by the minor path
               nil ;; resurrection-check-fn
               )))
        (let ((poll (plist-get result :poll))
              (drain (plist-get result :drain)))
          ;; Poll dispatched 'minor.
          (should (eq 'minor (plist-get poll :gc-kind)))
          (should (plist-get poll :cycle-result))
          ;; Bytes counter reset post-dispatch.
          (should (eq 0 (plist-get poll :bytes-since-minor-after)))
          ;; Drain executed our enqueued finalizer.
          (should (eq 1 (plist-get drain :fired)))
          (should (eq 0 (plist-get drain :errors)))
          (should (eq 0 (plist-get drain :remaining))))
        ;; Top-level shape: elapsed + version field.
        (should (integerp (plist-get result :elapsed-ms)))
        (should (eq 1 (plist-get result :phase-7-3-6-version))))
      ;; The fin-fn body actually ran (post-mutator drain).
      (should (equal '(999) fired-addrs)))))


(provide 'nelisp-gc-inner-test)
;;; nelisp-gc-inner-test.el ends here
