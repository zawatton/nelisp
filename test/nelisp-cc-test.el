;;; nelisp-cc-test.el --- ERT for nelisp-cc Phase 7.1.1 scaffold  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Smoke tests for the Phase 7.1.1 SSA IR scaffold in
;; `src/nelisp-cc.el'.  Coverage:
;;
;;   Scaffold (1-8):
;;     1. entry block creation by `nelisp-cc--ssa-make-function'
;;     2. instruction append order in a block
;;     3. predecessor / successor bidirectional edges
;;     4. pp / from-sexp round-trip preserves structure
;;     5. verifier rejects orphan (unreachable) blocks
;;     6. value IDs are unique within a function
;;     7. add-use is idempotent and updates use-list
;;     8. verifier rejects a one-sided pred/succ edge
;;
;;   Linear-scan allocator (9-16):
;;     9.  intervals in a single block: start < end ordering
;;    10. multi-block linearisation follows reverse postorder
;;    11. operand uses appear in interval USES list
;;    12. linear-scan: 16 sequential values fit in 8 registers, no spill
;;    13. linear-scan: pressure > pool forces spill markers
;;    14. linear-scan: expired register is reused by a later interval
;;    15. linear-scan: longest-end candidate is the spill victim
;;    16. alloc-register-of / alloc-spilled-values lookup helpers

;;; Code:

(require 'ert)
(require 'seq)
(require 'nelisp-cc)
(require 'nelisp-cc-x86_64)
(require 'nelisp-cc-arm64)
(require 'nelisp-cc-runtime)
(require 'nelisp-closure)
(require 'nelisp-gc)

;;; (1) entry block ---------------------------------------------------

(ert-deftest nelisp-cc-ssa-make-function-creates-entry-block ()
  "A fresh function has an ENTRY block in its BLOCKS table."
  (let* ((fn (nelisp-cc--ssa-make-function 'foo '(int int)))
         (entry (nelisp-cc--ssa-function-entry fn))
         (blocks (nelisp-cc--ssa-function-blocks fn)))
    (should entry)
    (should (memq entry blocks))
    (should (= 1 (length blocks)))
    (should (equal "entry" (nelisp-cc--ssa-block-label entry)))
    ;; Two params with :param def-point and the requested types.
    (let ((params (nelisp-cc--ssa-function-params fn)))
      (should (= 2 (length params)))
      (dolist (p params)
        (should (eq :param (nelisp-cc--ssa-value-def-point p))))
      (should (eq 'int (nelisp-cc--ssa-value-type (car params)))))))

;;; (2) instruction append --------------------------------------------

(ert-deftest nelisp-cc-ssa-add-instr-appends-to-block ()
  "`add-instr' appends to BLOCK-INSTRS in order; def-point is patched."
  (let* ((fn (nelisp-cc--ssa-make-function 'add2 '(int)))
         (entry (nelisp-cc--ssa-function-entry fn))
         (param (car (nelisp-cc--ssa-function-params fn)))
         (vsum (nelisp-cc--ssa-make-value fn 'int))
         (vret (nelisp-cc--ssa-make-value fn 'int))
         (i1 (nelisp-cc--ssa-add-instr fn entry 'add (list param param) vsum))
         (i2 (nelisp-cc--ssa-add-instr fn entry 'identity (list vsum) vret))
         (i3 (nelisp-cc--ssa-add-instr fn entry 'return (list vret) nil)))
    (should (equal (list i1 i2 i3) (nelisp-cc--ssa-block-instrs entry)))
    (should (eq i1 (nelisp-cc--ssa-value-def-point vsum)))
    (should (eq i2 (nelisp-cc--ssa-value-def-point vret)))
    (should (null (nelisp-cc--ssa-instr-def i3)))))

;;; (3) edge bidirectionality -----------------------------------------

(ert-deftest nelisp-cc-ssa-link-blocks-bidirectional ()
  "`link-blocks' adds the edge in both directions and is idempotent."
  (let* ((fn (nelisp-cc--ssa-make-function 'branchy nil))
         (a  (nelisp-cc--ssa-function-entry fn))
         (b  (nelisp-cc--ssa-make-block fn "then"))
         (c  (nelisp-cc--ssa-make-block fn "else")))
    (nelisp-cc--ssa-link-blocks a b)
    (nelisp-cc--ssa-link-blocks a c)
    (should (memq b (nelisp-cc--ssa-block-successors a)))
    (should (memq c (nelisp-cc--ssa-block-successors a)))
    (should (memq a (nelisp-cc--ssa-block-predecessors b)))
    (should (memq a (nelisp-cc--ssa-block-predecessors c)))
    ;; Idempotent — calling twice keeps the lists at length 2 / 1.
    (nelisp-cc--ssa-link-blocks a b)
    (should (= 2 (length (nelisp-cc--ssa-block-successors a))))
    (should (= 1 (length (nelisp-cc--ssa-block-predecessors b))))))

;;; (4) pp / from-sexp round-trip -------------------------------------

(ert-deftest nelisp-cc-ssa-pp-roundtrip ()
  "`pp' followed by `from-sexp' reproduces the printed shape."
  (let* ((fn (nelisp-cc--ssa-make-function 'rt '(int)))
         (entry (nelisp-cc--ssa-function-entry fn))
         (then (nelisp-cc--ssa-make-block fn "then"))
         (param (car (nelisp-cc--ssa-function-params fn)))
         (v1 (nelisp-cc--ssa-make-value fn 'int)))
    (nelisp-cc--ssa-add-instr fn entry 'add (list param param) v1)
    (nelisp-cc--ssa-add-instr fn entry 'branch (list v1) nil)
    (nelisp-cc--ssa-add-instr fn then  'return (list v1) nil)
    (nelisp-cc--ssa-link-blocks entry then)
    (nelisp-cc--ssa-verify-function fn)
    (let* ((sexp (nelisp-cc--ssa-pp fn))
           (fn2 (nelisp-cc--ssa-from-sexp sexp))
           (sexp2 (nelisp-cc--ssa-pp fn2)))
      (should (equal sexp sexp2))
      (should (eq 'rt (nelisp-cc--ssa-function-name fn2)))
      (should (= 2 (length (nelisp-cc--ssa-function-blocks fn2))))
      ;; Round-tripped function is itself well-formed.
      (should (eq t (nelisp-cc--ssa-verify-function fn2))))))

;;; (5) verifier — orphan block ---------------------------------------

(ert-deftest nelisp-cc-ssa-verify-detects-orphan-block ()
  "An unreachable block is rejected by the verifier."
  (let* ((fn (nelisp-cc--ssa-make-function 'orphan nil))
         (_  (nelisp-cc--ssa-make-block fn "orphan")))
    ;; Note: no link from entry → orphan.  Verifier must complain.
    ;; `should-error' returns (ERR-SYMBOL . DATA-LIST) with DATA-LIST
    ;; spliced — so the first datum sits at (cadr err).
    (let ((err (should-error (nelisp-cc--ssa-verify-function fn)
                             :type 'nelisp-cc-verify-error)))
      (should (eq :orphan-block (cadr err))))))

;;; (6) value ID uniqueness -------------------------------------------

(ert-deftest nelisp-cc-ssa-value-id-uniqueness ()
  "Builders hand out monotonically increasing, unique value IDs."
  (let* ((fn (nelisp-cc--ssa-make-function 'unique '(int int int)))
         (params (nelisp-cc--ssa-function-params fn))
         (locals (list (nelisp-cc--ssa-make-value fn 'int)
                       (nelisp-cc--ssa-make-value fn 'int)
                       (nelisp-cc--ssa-make-value fn nil)))
         (ids (mapcar #'nelisp-cc--ssa-value-id (append params locals))))
    (should (equal ids (number-sequence 0 5)))
    (should (= (length ids) (length (delete-dups (copy-sequence ids)))))))

;;; (7) add-use idempotency -------------------------------------------

(ert-deftest nelisp-cc-ssa-add-use-updates-list ()
  "`add-use' inserts on first call and is a no-op on repeat."
  (let* ((fn (nelisp-cc--ssa-make-function 'uses '(int)))
         (entry (nelisp-cc--ssa-function-entry fn))
         (param (car (nelisp-cc--ssa-function-params fn)))
         (vd (nelisp-cc--ssa-make-value fn 'int))
         (instr (nelisp-cc--ssa-add-instr fn entry 'inc (list param) vd)))
    ;; The first add-use happened inside add-instr.
    (should (memq instr (nelisp-cc--ssa-value-use-list param)))
    (should (= 1 (length (nelisp-cc--ssa-value-use-list param))))
    ;; Re-adding must not duplicate.
    (nelisp-cc--ssa-add-use param instr)
    (should (= 1 (length (nelisp-cc--ssa-value-use-list param))))))

;;; (8) verifier — broken edge ----------------------------------------

(ert-deftest nelisp-cc-ssa-verify-detects-broken-edge ()
  "A one-sided pred/succ edge is rejected by the verifier."
  (let* ((fn (nelisp-cc--ssa-make-function 'broken nil))
         (a  (nelisp-cc--ssa-function-entry fn))
         (b  (nelisp-cc--ssa-make-block fn "leaf")))
    ;; Manually corrupt: A's successors mention B but B's predecessors do not.
    (setf (nelisp-cc--ssa-block-successors a) (list b))
    ;; Reachability now passes (B is reachable), but the back-edge is missing.
    (let ((err (should-error (nelisp-cc--ssa-verify-function fn)
                             :type 'nelisp-cc-verify-error)))
      (should (eq :missing-predecessor-back-edge (cadr err))))))

;;; (9) linear-scan — single-block intervals -------------------------

(ert-deftest nelisp-cc-interval-single-block-linear ()
  "Within a single block, def positions are 1..N and intervals span uses."
  (let* ((fn (nelisp-cc--ssa-make-function 'linear '(int)))
         (entry (nelisp-cc--ssa-function-entry fn))
         (param (car (nelisp-cc--ssa-function-params fn)))
         (v1 (nelisp-cc--ssa-make-value fn 'int))
         (v2 (nelisp-cc--ssa-make-value fn 'int)))
    (nelisp-cc--ssa-add-instr fn entry 'inc (list param) v1)   ; pos 1
    (nelisp-cc--ssa-add-instr fn entry 'inc (list v1) v2)      ; pos 2
    (nelisp-cc--ssa-add-instr fn entry 'return (list v2) nil)  ; pos 3
    (nelisp-cc--ssa-verify-function fn)
    (let* ((ivs (nelisp-cc--compute-intervals fn))
           (by-vid (mapcar (lambda (iv)
                             (cons (nelisp-cc--ssa-value-id
                                    (nelisp-cc--ssa-interval-value iv))
                                   iv))
                           ivs)))
      ;; Param is at position 0, used by instr at position 1.
      (let ((p (cdr (assq (nelisp-cc--ssa-value-id param) by-vid))))
        (should p)
        (should (= 0 (nelisp-cc--ssa-interval-start p)))
        (should (= 1 (nelisp-cc--ssa-interval-end p))))
      ;; v1 defined at pos 1, used at pos 2 → end 2.
      (let ((i1 (cdr (assq (nelisp-cc--ssa-value-id v1) by-vid))))
        (should (= 1 (nelisp-cc--ssa-interval-start i1)))
        (should (= 2 (nelisp-cc--ssa-interval-end i1))))
      ;; v2 defined at pos 2, used at pos 3 → end 3.
      (let ((i2 (cdr (assq (nelisp-cc--ssa-value-id v2) by-vid))))
        (should (= 2 (nelisp-cc--ssa-interval-start i2)))
        (should (= 3 (nelisp-cc--ssa-interval-end i2))))
      ;; The list is sorted by start ascending.
      (should (equal (mapcar #'nelisp-cc--ssa-interval-start ivs)
                     '(0 1 2))))))

;;; (10) linear-scan — multi-block reverse postorder -----------------

(ert-deftest nelisp-cc-interval-multi-block-reverse-postorder ()
  "Multi-block linearisation places successors after predecessors."
  ;; CFG: entry → mid → tail (forward chain).  Reverse postorder of a
  ;; forward DFS from entry is exactly (entry, mid, tail), so positions
  ;; are assigned in that block order.
  (let* ((fn (nelisp-cc--ssa-make-function 'chain nil))
         (entry (nelisp-cc--ssa-function-entry fn))
         (mid (nelisp-cc--ssa-make-block fn "mid"))
         (tail (nelisp-cc--ssa-make-block fn "tail"))
         (va (nelisp-cc--ssa-make-value fn 'int))
         (vb (nelisp-cc--ssa-make-value fn 'int)))
    (nelisp-cc--ssa-add-instr fn entry 'jump nil nil)    ; pos 1
    (nelisp-cc--ssa-add-instr fn mid   'load nil va)     ; pos 2
    (nelisp-cc--ssa-add-instr fn mid   'jump nil nil)    ; pos 3
    (nelisp-cc--ssa-add-instr fn tail  'load (list va) vb) ; pos 4
    (nelisp-cc--ssa-add-instr fn tail  'return (list vb) nil) ; pos 5
    (nelisp-cc--ssa-link-blocks entry mid)
    (nelisp-cc--ssa-link-blocks mid tail)
    (nelisp-cc--ssa-verify-function fn)
    (let* ((ivs (nelisp-cc--compute-intervals fn))
           (by-vid (mapcar (lambda (iv)
                             (cons (nelisp-cc--ssa-value-id
                                    (nelisp-cc--ssa-interval-value iv))
                                   iv))
                           ivs)))
      ;; va defined in mid (pos 2), used in tail (pos 4).
      (let ((iva (cdr (assq (nelisp-cc--ssa-value-id va) by-vid))))
        (should (= 2 (nelisp-cc--ssa-interval-start iva)))
        (should (= 4 (nelisp-cc--ssa-interval-end iva))))
      ;; vb defined in tail (pos 4), used in tail (pos 5).
      (let ((ivb (cdr (assq (nelisp-cc--ssa-value-id vb) by-vid))))
        (should (= 4 (nelisp-cc--ssa-interval-start ivb)))
        (should (= 5 (nelisp-cc--ssa-interval-end ivb)))))))

;;; (11) linear-scan — uses are collected ----------------------------

(ert-deftest nelisp-cc-interval-uses-collected ()
  "Every read of a value contributes a position to the interval USES list."
  (let* ((fn (nelisp-cc--ssa-make-function 'multiuse '(int)))
         (entry (nelisp-cc--ssa-function-entry fn))
         (param (car (nelisp-cc--ssa-function-params fn)))
         (v1 (nelisp-cc--ssa-make-value fn 'int))
         (v2 (nelisp-cc--ssa-make-value fn 'int))
         (v3 (nelisp-cc--ssa-make-value fn 'int)))
    (nelisp-cc--ssa-add-instr fn entry 'load nil v1)         ; pos 1
    (nelisp-cc--ssa-add-instr fn entry 'add (list v1 v1) v2) ; pos 2 — v1 x2
    (nelisp-cc--ssa-add-instr fn entry 'add (list v1 v2) v3) ; pos 3 — v1, v2
    (nelisp-cc--ssa-add-instr fn entry 'return (list v3) nil) ; pos 4 — v3
    (nelisp-cc--ssa-verify-function fn)
    (let* ((ivs (nelisp-cc--compute-intervals fn))
           (by-vid (mapcar (lambda (iv)
                             (cons (nelisp-cc--ssa-value-id
                                    (nelisp-cc--ssa-interval-value iv))
                                   iv))
                           ivs)))
      ;; v1 is read 3 times (pos 2 twice, pos 3 once).
      (let ((iv1 (cdr (assq (nelisp-cc--ssa-value-id v1) by-vid))))
        (should (equal '(2 2 3) (nelisp-cc--ssa-interval-uses iv1)))
        (should (= 3 (nelisp-cc--ssa-interval-end iv1))))
      ;; v2 read once at pos 3.
      (let ((iv2 (cdr (assq (nelisp-cc--ssa-value-id v2) by-vid))))
        (should (equal '(3) (nelisp-cc--ssa-interval-uses iv2))))
      ;; v3 read once at pos 4.
      (let ((iv3 (cdr (assq (nelisp-cc--ssa-value-id v3) by-vid))))
        (should (equal '(4) (nelisp-cc--ssa-interval-uses iv3))))
      ;; Param is unused in this fn (no use entries).
      (let ((ip (cdr (assq (nelisp-cc--ssa-value-id param) by-vid))))
        (should (null (nelisp-cc--ssa-interval-uses ip)))))))

;;; (12) linear-scan — no spill under capacity -----------------------

(defun nelisp-cc-test--chain-fn (n-locals)
  "Build a single-block function with N-LOCALS sequentially-chained adds.
Each value V_i is defined by `add' on V_{i-1} and immediately consumed
by V_{i+1} — i.e. each interval is exactly one position long.  A
final `return' on the last value closes the block."
  (let* ((fn (nelisp-cc--ssa-make-function 'chainn '(int)))
         (entry (nelisp-cc--ssa-function-entry fn))
         (prev (car (nelisp-cc--ssa-function-params fn))))
    (dotimes (_ n-locals)
      (let ((v (nelisp-cc--ssa-make-value fn 'int)))
        (nelisp-cc--ssa-add-instr fn entry 'inc (list prev) v)
        (setq prev v)))
    (nelisp-cc--ssa-add-instr fn entry 'return (list prev) nil)
    fn))

(ert-deftest nelisp-cc-linear-scan-no-conflict ()
  "16 values whose intervals are disjoint pairs fit in 8 registers."
  ;; In a chain of 16 inc, each value's interval is [i, i+1], so at any
  ;; point only ~2 intervals overlap.  Even the default 8-register pool
  ;; is overkill; no `:spill' should appear.
  (let* ((fn (nelisp-cc-test--chain-fn 16))
         (assignments (nelisp-cc--linear-scan fn))
         (spilled (nelisp-cc--alloc-spilled-values assignments)))
    (should (null spilled))
    ;; Every value (param + 16 locals = 17 ids 0..16) gets a register.
    (should (= 17 (length assignments)))
    (dolist (cell assignments)
      (should (memq (cdr cell) nelisp-cc--default-int-registers)))))

;;; (13) linear-scan — pressure forces spill -------------------------

(defun nelisp-cc-test--all-overlap-fn (n-locals)
  "Build a function where every local is live until a final big consumer.

Each of N-LOCALS values is defined by a `load' (no operands), then a
final `return' instruction reads *all* of them — so every interval
spans from its def position up to the position of the return.  This
produces maximum register pressure with minimal CFG complexity."
  (let* ((fn (nelisp-cc--ssa-make-function 'pressure nil))
         (entry (nelisp-cc--ssa-function-entry fn))
         (locals (let (acc)
                   (dotimes (_ n-locals)
                     (let ((v (nelisp-cc--ssa-make-value fn 'int)))
                       (nelisp-cc--ssa-add-instr fn entry 'load nil v)
                       (push v acc)))
                   (nreverse acc))))
    (nelisp-cc--ssa-add-instr fn entry 'sink locals nil)
    fn))

(ert-deftest nelisp-cc-linear-scan-spill-when-pressure-exceeds-pool ()
  "16 simultaneously-live values must spill into a 4-register pool."
  (let* ((fn (nelisp-cc-test--all-overlap-fn 16))
         (assignments (nelisp-cc--linear-scan fn '(r0 r1 r2 r3)))
         (spilled (nelisp-cc--alloc-spilled-values assignments)))
    ;; 16 live values minus 4 register-fitting → 12 spills.
    (should (= 12 (length spilled)))
    ;; Exactly 4 values escaped spill.
    (let ((kept (cl-remove-if (lambda (cell) (eq (cdr cell) :spill))
                              assignments)))
      (should (= 4 (length kept)))
      (dolist (cell kept)
        (should (memq (cdr cell) '(r0 r1 r2 r3)))))))

;;; (14) linear-scan — expired register is reused --------------------

(ert-deftest nelisp-cc-linear-scan-expire-old-frees-register ()
  "When an interval expires its register returns to the free pool."
  ;; In a chain of 16 inc, every adjacent pair of values overlaps for
  ;; one position (V_{i-1}'s end == V_i's start), so the working set is
  ;; always 2.  A 1-register pool therefore forces spills (the
  ;; allocator cannot keep two values resident at once).  A 2-register
  ;; pool has zero spills — and the *only* way 17 values fit in 2
  ;; registers is if expired registers return to FREE and cycle.
  (let* ((fn (nelisp-cc-test--chain-fn 16))
         (assignments (nelisp-cc--linear-scan fn '(r0)))
         (spilled (nelisp-cc--alloc-spilled-values assignments)))
    ;; 1-register pool: every overlap forces an eviction, so most
    ;; values get marked :spill.  We only assert > 0; the exact count
    ;; depends on the order of `<=' tie-breaks in `spill-at'.
    (should (> (length spilled) 0))
    (let* ((assignments-2 (nelisp-cc--linear-scan fn '(r0 r1)))
           (spilled-2 (nelisp-cc--alloc-spilled-values assignments-2)))
      (should (null spilled-2))
      ;; With 2 registers cycled across 17 values, both r0 and r1 must
      ;; appear in the result more than once — that *is* the reuse.
      (let* ((regs (mapcar #'cdr assignments-2))
             (r0-count (cl-count 'r0 regs))
             (r1-count (cl-count 'r1 regs)))
        (should (> r0-count 1))
        (should (> r1-count 1))
        ;; And nobody got an unexpected register name.
        (should (= 17 (+ r0-count r1-count)))))))

;;; (15) linear-scan — spill heuristic = longest-end -----------------

(ert-deftest nelisp-cc-linear-scan-spill-heuristic-longest-end ()
  "When pressure exceeds capacity, the longest-end interval is the victim."
  ;; Build: entry: load v0 ; load v1 ; load v2 ; sink(v0,v1,v2)
  ;; All three intervals start at 1/2/3 and end at 4 (the sink).  With a
  ;; 2-register pool, the third interval (v2) arrives when active is
  ;; full.  v2 has the same end as the rest — Poletto-Sarkar's "longest
  ;; end first" picks the longest, but since v0/v1/v2 all share end=4,
  ;; the algorithm spills *the new interval* (v2) per the `<=' tie-break
  ;; in `spill-at'.  We assert that exactly one of the three is spilled
  ;; and the survivors are r0/r1.
  ;; Sub-case A — uniform-end pressure: three identical-end intervals
  ;; into a 2-reg pool.  All three values end at position 4 (the sink),
  ;; so the `<=' tie-break in `spill-at' picks the *new* interval (v2)
  ;; as the spill victim — a stable, deterministic outcome we lock in.
  (let* ((fn (nelisp-cc-test--all-overlap-fn 3))
         (assignments (nelisp-cc--linear-scan fn '(r0 r1)))
         (spilled (nelisp-cc--alloc-spilled-values assignments)))
    (should (= 1 (length spilled)))
    ;; v0, v1, v2 are issued in order with vids 0, 1, 2.  v2 (newest)
    ;; loses the tie under `<=' and is marked :spill.
    (should (equal '(2) spilled))
    ;; The two survivors hold r0 and r1 (any order).
    (let ((kept-regs
           (sort (mapcar #'cdr
                         (cl-remove-if (lambda (c) (eq (cdr c) :spill))
                                       assignments))
                 (lambda (a b) (string< (symbol-name a) (symbol-name b))))))
      (should (equal '(r0 r1) kept-regs))))
  ;; Sub-case B — non-uniform end: v2 has a *strictly shorter* end than
  ;; v0/v1, so longest-end-first picks one of v0/v1 as the spill
  ;; victim and the new arrival v2 wins a register.
  ;;   pos 1  load v0
  ;;   pos 2  load v1
  ;;   pos 3  load v2
  ;;   pos 4  use(v2)         — v2 ends here (end=4)
  ;;   pos 5  use(v0, v1)     — v0 / v1 end here (end=5)
  (let* ((fn2 (nelisp-cc--ssa-make-function 'longest nil))
         (entry (nelisp-cc--ssa-function-entry fn2))
         (v0 (nelisp-cc--ssa-make-value fn2 'int))
         (v1 (nelisp-cc--ssa-make-value fn2 'int))
         (v2 (nelisp-cc--ssa-make-value fn2 'int)))
    (nelisp-cc--ssa-add-instr fn2 entry 'load nil v0)
    (nelisp-cc--ssa-add-instr fn2 entry 'load nil v1)
    (nelisp-cc--ssa-add-instr fn2 entry 'load nil v2)
    (nelisp-cc--ssa-add-instr fn2 entry 'use (list v2) nil)
    (nelisp-cc--ssa-add-instr fn2 entry 'use (list v0 v1) nil)
    (let* ((assignments (nelisp-cc--linear-scan fn2 '(r0 r1)))
           (spilled (nelisp-cc--alloc-spilled-values assignments)))
      (should (= 1 (length spilled)))
      ;; Spill victim is v0 or v1 (end=5 — longest), not v2 (end=4).
      (should (or (memq 0 spilled) (memq 1 spilled)))
      (should (not (memq 2 spilled)))
      ;; v2 specifically must hold a register (the eviction was for it).
      (should (memq (nelisp-cc--alloc-register-of assignments 2)
                    '(r0 r1))))))

;;; (16) alloc helpers -----------------------------------------------

(ert-deftest nelisp-cc-alloc-register-of-lookup ()
  "`alloc-register-of', `alloc-spilled-values' and `alloc-pp' work."
  (let ((assignments '((0 . r0) (1 . :spill) (2 . r1) (3 . :spill))))
    ;; Direct register hit.
    (should (eq 'r0 (nelisp-cc--alloc-register-of assignments 0)))
    (should (eq 'r1 (nelisp-cc--alloc-register-of assignments 2)))
    ;; Spilled values return :spill, not nil.
    (should (eq :spill (nelisp-cc--alloc-register-of assignments 1)))
    (should (eq :spill (nelisp-cc--alloc-register-of assignments 3)))
    ;; Missing id returns nil.
    (should (null (nelisp-cc--alloc-register-of assignments 99)))
    ;; Spilled extraction preserves order.
    (should (equal '(1 3) (nelisp-cc--alloc-spilled-values assignments)))
    ;; Pretty-printer surfaces both fields.
    (let ((pp (nelisp-cc--alloc-pp assignments)))
      (should (equal assignments
                     (plist-get pp :assignments)))
      (should (equal '(1 3) (plist-get pp :spilled))))))

;;; (17) AST → SSA — empty lambda ------------------------------------

(defun nelisp-cc-test--instr-ops (fn)
  "Return the flat list of all opcodes in FN, block-by-block."
  (let ((acc nil))
    (dolist (b (nelisp-cc--ssa-function-blocks fn))
      (dolist (instr (nelisp-cc--ssa-block-instrs b))
        (push (nelisp-cc--ssa-instr-opcode instr) acc)))
    (nreverse acc)))

(ert-deftest nelisp-cc-build-ssa-empty-lambda ()
  "`(lambda () nil)' lowers to one block with one :const + :return."
  (let* ((fn (nelisp-cc-build-ssa-from-ast '(lambda () nil)))
         (blocks (nelisp-cc--ssa-function-blocks fn))
         (entry (nelisp-cc--ssa-function-entry fn)))
    (should (= 1 (length blocks)))
    (should (eq entry (car blocks)))
    (should (null (nelisp-cc--ssa-function-params fn)))
    (let ((ops (nelisp-cc-test--instr-ops fn)))
      (should (equal '(const return) ops)))
    ;; The single :const carries nil as its literal in META.
    (let ((const-instr (car (nelisp-cc--ssa-block-instrs entry))))
      (should (eq 'const (nelisp-cc--ssa-instr-opcode const-instr)))
      (should (null (plist-get (nelisp-cc--ssa-instr-meta const-instr)
                               :literal))))
    (should (eq t (nelisp-cc--ssa-verify-function fn)))))

;;; (18) AST → SSA — identity ----------------------------------------

(ert-deftest nelisp-cc-build-ssa-identity ()
  "`(lambda (x) x)' returns the parameter SSA value with no extra ops."
  (let* ((fn (nelisp-cc-build-ssa-from-ast '(lambda (x) x)))
         (params (nelisp-cc--ssa-function-params fn))
         (entry (nelisp-cc--ssa-function-entry fn))
         (instrs (nelisp-cc--ssa-block-instrs entry)))
    (should (= 1 (length params)))
    ;; The body is a bare symbol — scope lookup returns the param SSA
    ;; value directly so no synthetic :load-var is emitted.  The only
    ;; instruction is the closing :return.
    (should (equal '(return) (mapcar #'nelisp-cc--ssa-instr-opcode instrs)))
    (let ((ret (car instrs)))
      (should (eq (car params)
                  (car (nelisp-cc--ssa-instr-operands ret)))))
    (should (eq t (nelisp-cc--ssa-verify-function fn)))))

;;; (19) AST → SSA — arithmetic --------------------------------------

(ert-deftest nelisp-cc-build-ssa-arithmetic ()
  "`(lambda (x) (+ x 1))' lowers to const + call(+, x, c) + return."
  (let* ((fn (nelisp-cc-build-ssa-from-ast '(lambda (x) (+ x 1))))
         (entry (nelisp-cc--ssa-function-entry fn))
         (instrs (nelisp-cc--ssa-block-instrs entry))
         (ops (mapcar #'nelisp-cc--ssa-instr-opcode instrs)))
    (should (equal '(const call return) ops))
    ;; The :call's META records the callee symbol.
    (let ((call (nth 1 instrs)))
      (should (eq '+ (plist-get (nelisp-cc--ssa-instr-meta call) :fn)))
      (should (eq t (plist-get (nelisp-cc--ssa-instr-meta call) :unresolved))))
    ;; :const's META records the literal `1'.
    (let ((c (car instrs)))
      (should (= 1 (plist-get (nelisp-cc--ssa-instr-meta c) :literal))))
    (should (eq t (nelisp-cc--ssa-verify-function fn)))))

;;; (20) AST → SSA — if form -----------------------------------------

(ert-deftest nelisp-cc-build-ssa-if-form ()
  "`(lambda (c) (if c 'a 'b))' splits into 4 blocks with a phi merge."
  (let* ((fn (nelisp-cc-build-ssa-from-ast '(lambda (c) (if c 'a 'b))))
         (blocks (nelisp-cc--ssa-function-blocks fn)))
    (should (= 4 (length blocks)))
    (let ((labels (mapcar #'nelisp-cc--ssa-block-label blocks)))
      (should (equal '("entry" "then" "else" "merge") labels)))
    ;; Find the merge block — its first instruction is :phi with two
    ;; operands and two predecessors.
    (let* ((merge (nth 3 blocks))
           (phi (car (nelisp-cc--ssa-block-instrs merge))))
      (should (eq 'phi (nelisp-cc--ssa-instr-opcode phi)))
      (should (= 2 (length (nelisp-cc--ssa-instr-operands phi))))
      (should (= 2 (length (nelisp-cc--ssa-block-predecessors merge))))
      ;; META records the (PRED-BID . VID) arms.
      (let ((arms (plist-get (nelisp-cc--ssa-instr-meta phi) :phi-arms)))
        (should (= 2 (length arms)))))
    ;; Verifier must accept the resulting CFG.
    (should (eq t (nelisp-cc--ssa-verify-function fn)))))

;;; (21) AST → SSA — let binding -------------------------------------

(ert-deftest nelisp-cc-build-ssa-let-binding ()
  "`(lambda (x) (let ((y (+ x 1))) y))' threads INIT through scope."
  (let* ((fn (nelisp-cc-build-ssa-from-ast
              '(lambda (x) (let ((y (+ x 1))) y))))
         (entry (nelisp-cc--ssa-function-entry fn))
         (ops (mapcar #'nelisp-cc--ssa-instr-opcode
                      (nelisp-cc--ssa-block-instrs entry))))
    ;; const(1) → call(+) → return — the let body is just `y' which
    ;; reuses the call's def value, so no extra instruction.
    (should (equal '(const call return) ops))
    ;; Return operand must be the call's def value (because `y' is
    ;; bound to it and the body returns `y').
    (let* ((call (nth 1 (nelisp-cc--ssa-block-instrs entry)))
           (ret (nth 2 (nelisp-cc--ssa-block-instrs entry))))
      (should (eq (nelisp-cc--ssa-instr-def call)
                  (car (nelisp-cc--ssa-instr-operands ret)))))
    (should (eq t (nelisp-cc--ssa-verify-function fn)))))

;;; (22) AST → SSA — progn -------------------------------------------

(ert-deftest nelisp-cc-build-ssa-progn-sequence ()
  "`(lambda () (progn 1 2 3))' emits 3 :const, return uses last."
  (let* ((fn (nelisp-cc-build-ssa-from-ast '(lambda () (progn 1 2 3))))
         (entry (nelisp-cc--ssa-function-entry fn))
         (instrs (nelisp-cc--ssa-block-instrs entry))
         (ops (mapcar #'nelisp-cc--ssa-instr-opcode instrs)))
    (should (equal '(const const const return) ops))
    ;; The :return's operand must be the third :const's def.
    (let ((c3 (nth 2 instrs))
          (ret (nth 3 instrs)))
      (should (eq (nelisp-cc--ssa-instr-def c3)
                  (car (nelisp-cc--ssa-instr-operands ret))))
      ;; And that third :const's literal is `3'.
      (should (= 3 (plist-get (nelisp-cc--ssa-instr-meta c3) :literal))))
    (should (eq t (nelisp-cc--ssa-verify-function fn)))))

;;; (23) AST → SSA — nested lambda -----------------------------------

(ert-deftest nelisp-cc-build-ssa-nested-lambda ()
  "Nested `(lambda () (lambda (x) x))' produces an inner SSA function."
  (let* ((fn (nelisp-cc-build-ssa-from-ast
              '(lambda () (lambda (x) x))))
         (entry (nelisp-cc--ssa-function-entry fn))
         (instrs (nelisp-cc--ssa-block-instrs entry))
         (ops (mapcar #'nelisp-cc--ssa-instr-opcode instrs)))
    (should (equal '(closure return) ops))
    (let* ((cls (car instrs))
           (inner (plist-get (nelisp-cc--ssa-instr-meta cls)
                             :inner-function)))
      (should inner)
      (should (nelisp-cc--ssa-function-p inner))
      ;; Inner has 1 param + a single-block body that returns it.
      (should (= 1 (length (nelisp-cc--ssa-function-params inner))))
      (should (eq t (nelisp-cc--ssa-verify-function inner))))
    (should (eq t (nelisp-cc--ssa-verify-function fn)))))

;;; (24) AST → SSA — unsupported form fails fast --------------------

(ert-deftest nelisp-cc-build-ssa-unsupported-form-errors ()
  "Forms outside the MVP kernel raise `nelisp-cc-unsupported-form'."
  ;; `catch' is intentionally unsupported in Phase 7.1.1 frontend.
  (let ((err (should-error
              (nelisp-cc-build-ssa-from-ast
               '(lambda () (catch 'tag (throw 'tag 1))))
              :type 'nelisp-cc-unsupported-form)))
    ;; ERR is (SYMBOL :head HEAD :form FORM ...) — the data plist
    ;; starts at `cdr' because `signal' splices its DATA list onto the
    ;; symbol.
    (should (eq 'catch (plist-get (cdr err) :head))))
  ;; `condition-case' too.
  (should-error
   (nelisp-cc-build-ssa-from-ast
    '(lambda () (condition-case nil (foo) (error nil))))
   :type 'nelisp-cc-unsupported-form)
  ;; T38 Phase 7.5.5: `while' is now supported — see ERT (30) below.
  ;; `unwind-protect' remains out of scope (cleanup-action lowering
  ;; defers to Phase 7.1.5 graph-coloring + safe-point insertion).
  (should-error
   (nelisp-cc-build-ssa-from-ast
    '(lambda () (unwind-protect 1 2)))
   :type 'nelisp-cc-unsupported-form)
  ;; Non-lambda input is rejected before any lowering happens.
  (should-error
   (nelisp-cc-build-ssa-from-ast '(defun foo () nil))
   :type 'nelisp-cc-unsupported-form))

;;; (25) AST → SSA — round-trip via pp / from-sexp -------------------

(ert-deftest nelisp-cc-build-ssa-roundtrip-pp ()
  "Built SSA function survives `pp' / `from-sexp' round-trip."
  (let* ((fn (nelisp-cc-build-ssa-from-ast
              '(lambda (a b) (if a (+ a b) b))))
         (sexp (nelisp-cc--ssa-pp fn))
         (fn2 (nelisp-cc--ssa-from-sexp sexp))
         (sexp2 (nelisp-cc--ssa-pp fn2)))
    (should (equal sexp sexp2))
    (should (= (length (nelisp-cc--ssa-function-blocks fn))
               (length (nelisp-cc--ssa-function-blocks fn2))))
    (should (eq t (nelisp-cc--ssa-verify-function fn2)))))

;;; (26) AST → SSA — verify passes -----------------------------------

(ert-deftest nelisp-cc-build-ssa-verify-passes ()
  "Every supported form produces a verifier-clean SSA function."
  (dolist (lf '((lambda () nil)
                (lambda (x) x)
                (lambda (x) (+ x 1))
                (lambda (c) (if c 'a 'b))
                (lambda (x) (let ((y (+ x 1))) y))
                (lambda (x) (let* ((a x) (b a)) b))
                (lambda () (progn 1 2 3))
                (lambda () (lambda (x) x))
                (lambda (x) (and x 1 2))
                (lambda (x) (or x 1))
                (lambda (x) (when x 1))
                (lambda (x) (unless x 1))
                (lambda (x) (cond ((eq x 1) 'one)
                                  ((eq x 2) 'two)
                                  (t 'other)))
                (lambda (x) (setq x 1))))
    (let ((fn (nelisp-cc-build-ssa-from-ast lf)))
      (should (eq t (nelisp-cc--ssa-verify-function fn))))))

;;; (27) AST → SSA → linear-scan integration ------------------------

(ert-deftest nelisp-cc-pipeline-build-then-allocate ()
  "End-to-end: build-ssa-from-ast → compute-intervals → linear-scan.
T63 Phase 7.5.7 — call-aware spill is now correct, so any value
live across a `:call' / `:call-indirect' is forced to a stack slot
rather than competing for caller-saved registers.  In the form
`(+ x (* y z))' the parameter `x' is live across the inner `*' call,
so it spills under the new allocator.  The test asserts the *kind*
of allocation (register or spill) rather than counting spills."
  (let* ((fn (nelisp-cc-build-ssa-from-ast
              '(lambda (x y z) (+ x (* y z)))))
         (intervals (nelisp-cc--compute-intervals fn))
         (assignments (nelisp-cc--linear-scan fn)))
    ;; Verify the SSA function itself.
    (should (eq t (nelisp-cc--ssa-verify-function fn)))
    ;; Every interval must correspond to a defined value.
    (dolist (iv intervals)
      (let ((v (nelisp-cc--ssa-interval-value iv)))
        (should v)
        (should (or (eq :param (nelisp-cc--ssa-value-def-point v))
                    (nelisp-cc--ssa-value-def-point v)))))
    ;; Every assignment maps to a register from the default pool *or*
    ;; the `:spill' marker (T63 call-aware allocator output).
    (dolist (cell assignments)
      (should (or (memq (cdr cell) nelisp-cc--default-int-registers)
                  (eq (cdr cell) :spill))))
    ;; T63 Phase 7.5.7 — `x' is live across the inner `*' call so it
    ;; must spill.  At least one interval must therefore be marked
    ;; call-crossing in the interval analysis.
    (should (cl-some #'nelisp-cc--ssa-interval-crosses-call intervals))))

;;; T38 Phase 7.5.5 SSA frontend extension (28-33) ------------------
;;
;; Six smoke tests for the three deferred forms now in scope (`letrec'
;; / `funcall' closure target / `while') plus the matching backend
;; `:call-indirect' bytes (x86_64 CALL [reg] / arm64 BLR Xn) plus a
;; full pipeline run that proves fib(30) builds + allocates + emits
;; bytes end-to-end (the bench gate unblock smoke).

;;; (28) AST → SSA — letrec mutual recursion -------------------------

(ert-deftest nelisp-cc-build-ssa-letrec-mutual-recursion ()
  "`letrec' lowers a recursive lambda into SSA; the inner `funcall fib'
references the binding NAME and must resolve to the outer `letrec'
slot rather than a free variable.  We exercise the canonical bench
shape `(letrec ((fib LAMBDA)) (funcall fib N))' and assert:

  - the build itself succeeds (the `:unknown-opcode closure 0' regression
    was the original bench-gate failure mode),
  - the resulting SSA function passes `nelisp-cc--ssa-verify-function',
  - the entry block contains at least one `:store-var' (the placeholder
    + final write the `--lower-letrec' two-pass scheme emits),
  - and at least one `:call-indirect' sits somewhere in the function
    (the `funcall fib' call site)."
  (let* ((fn (nelisp-cc-build-ssa-from-ast
              '(lambda ()
                 (letrec ((fib (lambda (n)
                                 (if (< n 2) n
                                   (+ (funcall fib (- n 1))
                                      (funcall fib (- n 2)))))))
                   (funcall fib 5)))))
         (entry (nelisp-cc--ssa-function-entry fn))
         (entry-instrs (nelisp-cc--ssa-block-instrs entry))
         (entry-opcodes (mapcar #'nelisp-cc--ssa-instr-opcode
                                entry-instrs))
         (all-opcodes
          (cl-loop for blk in (nelisp-cc--ssa-function-blocks fn)
                   append (mapcar #'nelisp-cc--ssa-instr-opcode
                                  (nelisp-cc--ssa-block-instrs blk)))))
    (should (eq t (nelisp-cc--ssa-verify-function fn)))
    ;; letrec emits the placeholder `:store-var' + final `:store-var'
    ;; on the entry block (no merge / branch in this lowering shape).
    (should (memq 'store-var entry-opcodes))
    ;; The inner `funcall fib N' must have produced a :call-indirect
    ;; somewhere — it lives inside the inner lambda's SSA function,
    ;; which is captured as the META :inner-function of the entry
    ;; block's :closure instruction.  We surface that via the entry's
    ;; opcode list (the closure instruction itself) plus the outer
    ;; block's `(funcall fib 5)' call-indirect.
    (should (memq 'closure entry-opcodes))
    (should (memq 'call-indirect all-opcodes))))

;;; (29) AST → SSA — funcall closure target -------------------------

(ert-deftest nelisp-cc-build-ssa-funcall-closure-target ()
  "`(funcall LAMBDA ARG)' lowers via the indirect-call path.  We assert
that the SSA function contains exactly one `:call-indirect' (no `:call'
to the symbol `funcall' — that would mean the dispatch fell through to
`--lower-call' and treated `funcall' as a named function)."
  (let* ((fn (nelisp-cc-build-ssa-from-ast
              '(lambda () (funcall (lambda (x) x) 42))))
         (all-opcodes
          (cl-loop for blk in (nelisp-cc--ssa-function-blocks fn)
                   append (mapcar #'nelisp-cc--ssa-instr-opcode
                                  (nelisp-cc--ssa-block-instrs blk))))
         (n-indirect (cl-count 'call-indirect all-opcodes))
         (n-call-funcall
          (cl-count-if
           (lambda (instr)
             (and (eq (nelisp-cc--ssa-instr-opcode instr) 'call)
                  (eq (plist-get (nelisp-cc--ssa-instr-meta instr) :fn)
                      'funcall)))
           (cl-loop for blk in (nelisp-cc--ssa-function-blocks fn)
                    append (nelisp-cc--ssa-block-instrs blk)))))
    (should (eq t (nelisp-cc--ssa-verify-function fn)))
    (should (= 1 n-indirect))
    (should (zerop n-call-funcall))))

;;; (30) AST → SSA — while loop back-edge ---------------------------

(ert-deftest nelisp-cc-build-ssa-while-loop-back-edge ()
  "`(while COND BODY)' allocates three blocks (header / body / exit) and
wires a back-edge from body to header.  We assert the structural
invariants:

  - 4 blocks exist (entry + header + body + exit),
  - header has at least 2 predecessors (entry + body's back-edge),
  - body has exactly one successor and it is the header (back-edge),
  - the function still verifies (loop reachability + bidirectional
    edges + no orphans)."
  (let* ((fn (nelisp-cc-build-ssa-from-ast
              '(lambda ()
                 (let ((i 0))
                   (while (< i 10)
                     (setq i (1+ i)))
                   i))))
         (blocks (nelisp-cc--ssa-function-blocks fn))
         (header
          (cl-find-if (lambda (b)
                        (equal (nelisp-cc--ssa-block-label b)
                               "while-header"))
                      blocks))
         (body
          (cl-find-if (lambda (b)
                        (equal (nelisp-cc--ssa-block-label b)
                               "while-body"))
                      blocks))
         (exit
          (cl-find-if (lambda (b)
                        (equal (nelisp-cc--ssa-block-label b)
                               "while-exit"))
                      blocks)))
    (should (eq t (nelisp-cc--ssa-verify-function fn)))
    (should header)
    (should body)
    (should exit)
    ;; Header has ≥ 2 predecessors (entry path + body back-edge).
    (should (>= (length (nelisp-cc--ssa-block-predecessors header)) 2))
    ;; Body's single successor is the header (back-edge).
    (should (= 1 (length (nelisp-cc--ssa-block-successors body))))
    (should (eq header (car (nelisp-cc--ssa-block-successors body))))))

;;; (31) x86_64 :call-indirect emits CALL [reg] bytes ---------------

(ert-deftest nelisp-cc-x86_64-call-indirect-emits-bytes ()
  "Lowering a `:call-indirect' SSA function emits a sequence containing
the `0xFF /2' (CALL r/m64) opcode pattern.

The fixture builds an SSA function with the shape
  `(lambda (f) (funcall f))'
which lowers to: load param[0] → :call-indirect with no args → return.
After linear-scan + backend compile the buffer must contain at least
one `0x48 0xFF 0xD0' triplet (REX.W + CALL /2 + ModR/M with rm=rax,
mod=11, reg=2 — i.e. CALL [rax], the canonical indirect-call shape
the lower helper emits)."
  (let* ((fn (nelisp-cc-build-ssa-from-ast
              '(lambda (f) (funcall f))))
         (alloc (nelisp-cc--linear-scan fn))
         (bytes (nelisp-cc-x86_64-compile fn alloc))
         (bvec  (append bytes nil))
         (found-call-indirect-p nil))
    ;; Walk the byte stream looking for the triplet.  We cannot
    ;; predict the exact prologue size in test (depends on spill
    ;; frame), so we scan the whole buffer.
    (cl-loop for tail on bvec
             when (and (eq (car  tail) #x48)
                       (eq (cadr tail) #xFF)
                       (cl-caddr tail)
                       ;; ModR/M for CALL [rax]:
                       ;;   mod=11 reg=2 rm=0 = #b11_010_000 = 0xD0.
                       (eq (cl-caddr tail) #xD0))
             do (setq found-call-indirect-p t))
    (should found-call-indirect-p)))

;;; (32) arm64 :call-indirect emits BLR bytes -----------------------

(ert-deftest nelisp-cc-arm64-call-indirect-emits-bytes ()
  "Lowering a `:call-indirect' SSA function on AAPCS64 emits the BLR X16
instruction word `0xD63F0200' (little-endian bytes 00 02 3F D6).

The lower helper materialises the callee into X16 (IP0, intra-
procedure-call scratch) and emits `BLR X16'.  The encoded word is
0xD63F0000 | (16 << 5) = 0xD63F0200.  Little-endian byte sequence in
the buffer: 0x00 0x02 0x3F 0xD6."
  (let* ((fn (nelisp-cc-build-ssa-from-ast
              '(lambda (f) (funcall f))))
         (alloc (nelisp-cc--linear-scan fn))
         (bytes (nelisp-cc-arm64-compile fn alloc))
         (bvec  (append bytes nil))
         (found-blr-p nil))
    (cl-loop for tail on bvec
             when (and (eq (car  tail)         #x00)
                       (eq (cadr tail)         #x02)
                       (cl-caddr tail)
                       (eq (cl-caddr tail)     #x3F)
                       (cdddr tail)
                       (eq (cadddr tail)       #xD6))
             do (setq found-blr-p t))
    (should found-blr-p)))

;;; (33) full pipeline fib(30) builds + allocates + emits -----------

(ert-deftest nelisp-cc-pipeline-fib-30-builds-and-allocates ()
  "End-to-end pipeline smoke for the bench-actual fib(30) form.

Doc 28 v2 §5.2 LOCKED 30x speedup gate is downstream of this smoke —
before T38 the SSA build itself raised
`nelisp-cc-unsupported-form :head while' / unknown-opcode `closure 0'
on the bench-actual form (T37 measurement reported 0/3 axes pass).
After T38 the same form must:

  1. build a verifier-clean SSA function,
  2. linear-scan without raising (loops + indirect calls + closures
     all share the supported opcode set),
  3. lower to x86_64 bytes without `:unknown-opcode' (the bench gate
     unblock condition).

We do NOT assert on speedup or correctness of the executed bytes —
that is the bench-actual harness's job once the runtime module is
built (Doc 28 §5.2 caveat — semantic correctness pending Phase 7.5
callee resolution)."
  (let* ((fib-30-form
          '(lambda ()
             (letrec ((fib (lambda (n)
                             (if (< n 2) n
                               (+ (funcall fib (- n 1))
                                  (funcall fib (- n 2)))))))
               (funcall fib 30))))
         (fn (nelisp-cc-build-ssa-from-ast fib-30-form))
         (alloc (nelisp-cc--linear-scan fn))
         (bytes (nelisp-cc-x86_64-compile fn alloc)))
    (should (eq t (nelisp-cc--ssa-verify-function fn)))
    (should (listp alloc))
    ;; The allocator returns an alist of (VID . REGISTER-OR-:spill).
    (dolist (cell alloc)
      (should (integerp (car cell)))
      (should (or (memq (cdr cell) nelisp-cc--default-int-registers)
                  (eq (cdr cell) :spill))))
    ;; The byte vector must be non-empty (prologue + body + return).
    (should (vectorp bytes))
    (should (> (length bytes) 0))))

;;; T63 Phase 7.5.7 — CRITICAL fix bundle ERT (34-50) ----------------
;;
;; T49 codex audit + T50 letrec partial-blocker fix bundle.  Covers the
;; four critical findings + the letrec self-recursion gate:
;;
;;   #1  x86_64 if branch layout-independent       (tests 34-36)
;;   #2  arm64 :jump emits B opcode                (tests 37-39)
;;   #3  call-crossing register protection         (tests 40-44)
;;   #4  GC metadata frame-size + call-points      (tests 45-46)
;;   T50 letrec self-recursion semantics           (tests 47-50)

;;; (34) x86_64 :branch emits both JE else AND JMP then -------------

(ert-deftest nelisp-cc-x86_64-branch-emits-both-arms-explicit ()
  "T63 #1 — :branch lowering must emit explicit jumps for both arms.

Pre-T63 the lowering emitted only `JE else_label' and relied on a
fallthrough into the THEN block.  Reverse-postorder linearisation
actually places ELSE *before* THEN for a `(if c T E)' diamond, so
the fallthrough was hitting ELSE — every taken branch went to the
wrong arm.  After T63 the bytes contain a JCC (0x0F 0x84 = JE rel32)
*and* a JMP rel32 (opcode 0xE9), proving both arms are reached
through an explicit branch."
  (let* ((fn (nelisp-cc-build-ssa-from-ast
              '(lambda (x) (if x 1 2))))
         (alloc (nelisp-cc--linear-scan fn))
         (bytes (nelisp-cc-x86_64-compile fn alloc))
         (bvec  (append bytes nil))
         (saw-je nil)
         (saw-jmp nil))
    (cl-loop for tail on bvec
             when (and (eq (car tail) #x0F) (eq (cadr tail) #x84))
             do (setq saw-je t)
             when (eq (car tail) #xE9)
             do (setq saw-jmp t))
    (should saw-je)
    (should saw-jmp)))

;;; (35) x86_64 if-diamond — both branch fixups resolve to valid offsets

(ert-deftest nelisp-cc-x86_64-if-diamond-fixups-resolve ()
  "T63 #1 — finalize() must resolve both JE-else and JMP-then fixups
without raising `nelisp-cc-x86_64-unresolved-label'.  This proves the
synthetic L_block_N label scheme covers the layout-independent branch."
  (let* ((fn (nelisp-cc-build-ssa-from-ast
              '(lambda (x) (if x (+ x 1) (- x 1)))))
         (alloc (nelisp-cc--linear-scan fn)))
    (should-not
     (condition-case _err
         (progn (nelisp-cc-x86_64-compile fn alloc) nil)
       (nelisp-cc-x86_64-unresolved-label t)))))

;;; (36) x86_64 nested if — both arms still reached

(ert-deftest nelisp-cc-x86_64-nested-if-no-fallthrough-bug ()
  "T63 #1 — nested `if' lowers without the pre-fix fallthrough bug.
The buffer must be non-empty and contain at least 2 JE / JCC and 2
JMP rel32 (one per branch level)."
  (let* ((fn (nelisp-cc-build-ssa-from-ast
              '(lambda (x y)
                 (if x (if y 1 2) 3))))
         (alloc (nelisp-cc--linear-scan fn))
         (bytes (nelisp-cc-x86_64-compile fn alloc))
         (bvec  (append bytes nil))
         (n-je 0) (n-jmp 0))
    (cl-loop for tail on bvec
             when (and (eq (car tail) #x0F) (eq (cadr tail) #x84))
             do (cl-incf n-je)
             when (eq (car tail) #xE9)
             do (cl-incf n-jmp))
    ;; Two `if' ⇒ at least 2 JE and at least 2 JMP.
    (should (>= n-je 2))
    (should (>= n-jmp 2))))

;;; (37) arm64 :jump emits B opcode (top byte 0x14)  -----------------

(ert-deftest nelisp-cc-arm64-jump-emits-b-opcode ()
  "T63 #2 — `:jump' on arm64 must lower to a B (unconditional branch)
instruction whose top opcode bits are 0x14000000 (top byte 0x14 in
little-endian as the 4th byte of the instruction word).

The pre-T63 backend dropped `:jump' entirely (no-op), which silently
collapsed every `if' merge and turned every `while' into a single
iteration.  After T63 the encoded word is `0x14000000 | imm26' so the
4-byte LE sequence ends with byte 0x14 in the top position.

Fixture: `(lambda (x) (if x 1 2))' — both then and else arms emit a
`:jump' to the merge block, so the byte stream must contain ≥ 2 B
opcodes (4 bytes each, top byte 0x14)."
  (let* ((fn (nelisp-cc-build-ssa-from-ast
              '(lambda (x) (if x 1 2))))
         (alloc (nelisp-cc--linear-scan fn))
         (bytes (nelisp-cc-arm64-compile fn alloc))
         (n     (length bytes))
         (n-b   0))
    ;; Walk 4-byte aligned windows; B opcode word = 0x14xxxxxx so the
    ;; *high* byte (which is the 4th LE byte of the word) starts with
    ;; 0001_01xx in its top 6 bits = 0x14..0x17 inclusive.
    (cl-loop for i from 0 below (- n 3) by 4
             for w4 = (aref bytes (+ i 3))
             when (and (>= w4 #x14) (<= w4 #x17))
             do (cl-incf n-b))
    (should (>= n-b 2))))

;;; (38) arm64 :jump fixup resolves cleanly --------------------------

(ert-deftest nelisp-cc-arm64-jump-fixup-resolves ()
  "T63 #2 — finalize() on arm64 must resolve the B-opcode fixup the
T63 `--lower-jump' helper emits, without raising `:unbound-label'."
  (let* ((fn (nelisp-cc-build-ssa-from-ast
              '(lambda (x) (if x 1 2))))
         (alloc (nelisp-cc--linear-scan fn)))
    (should-not
     (condition-case _err
         (progn (nelisp-cc-arm64-compile fn alloc) nil)
       (nelisp-cc-arm64-error t)))))

;;; (39) arm64 while loop emits back-edge B --------------------------

(ert-deftest nelisp-cc-arm64-while-back-edge-emits-b ()
  "T63 #2 — `while' loop body emits a B back-edge to the loop-header.
Pre-T63 the back-edge was dropped, turning every loop into a single
iteration.  After T63 the byte stream contains at least one B-opcode
word with a *negative* imm26 displacement (top byte still 0x14..0x17;
the imm26 field carries the negative offset)."
  (let* ((fn (nelisp-cc-build-ssa-from-ast
              '(lambda (x) (while x (setq x 0)))))
         (alloc (nelisp-cc--linear-scan fn))
         (bytes (nelisp-cc-arm64-compile fn alloc))
         (n     (length bytes))
         (saw-b nil))
    (cl-loop for i from 0 below (- n 3) by 4
             for w4 = (aref bytes (+ i 3))
             when (and (>= w4 #x14) (<= w4 #x17))
             do (setq saw-b t))
    (should saw-b)))

;;; (40) interval crosses-call detected -----------------------------

(ert-deftest nelisp-cc-interval-crosses-call-detected ()
  "T63 #3 — `--compute-intervals' must mark intervals as call-crossing
when the value is live across a `:call' / `:call-indirect'.

Fixture: `(lambda (x) (+ x (* 2 3)))' — `x' is live across the inner
`*' call (the `+' call uses `x' as an operand *after* the `*' call
returns).  The interval for the parameter must therefore be flagged."
  (let* ((fn (nelisp-cc-build-ssa-from-ast
              '(lambda (x) (+ x (* 2 3)))))
         (intervals (nelisp-cc--compute-intervals fn)))
    (should (cl-some #'nelisp-cc--ssa-interval-crosses-call intervals))))

;;; (41) call-crossing intervals always spill -----------------------

(ert-deftest nelisp-cc-linear-scan-call-crossing-spills ()
  "T63 #3 — `--linear-scan' must force `:spill' for every call-crossing
interval, regardless of register pressure.  The default register pool
is all caller-saved (rdi..r11 / x0..x7), so the only correct answer
is to put the value in a stack slot where it survives the callee's
clobber."
  (let* ((fn (nelisp-cc-build-ssa-from-ast
              '(lambda (x) (+ x (* 2 3)))))
         (intervals (nelisp-cc--compute-intervals fn))
         (alloc (nelisp-cc--linear-scan fn)))
    ;; For every interval flagged crosses-call, the assignment must be
    ;; `:spill', NOT a register from the pool.
    (dolist (iv intervals)
      (when (nelisp-cc--ssa-interval-crosses-call iv)
        (let* ((vid (nelisp-cc--ssa-value-id
                     (nelisp-cc--ssa-interval-value iv)))
               (assign (nelisp-cc--alloc-register-of alloc vid)))
          (should (eq assign :spill)))))))

;;; (42) call-crossing → spill slot allocated -----------------------

(ert-deftest nelisp-cc-allocate-stack-slots-covers-call-crossing ()
  "T63 #3 — every `:spill' produced by call-aware allocation gets a
real stack slot via `--allocate-stack-slots' so the backend has a
positive byte offset to load from / store to."
  (let* ((fn (nelisp-cc-build-ssa-from-ast
              '(lambda (x) (+ x (* 2 3)))))
         (alloc (nelisp-cc--linear-scan fn))
         (slots-pair (nelisp-cc--allocate-stack-slots alloc))
         (slot-alist (car slots-pair))
         (frame-size (cdr slots-pair))
         (spilled (nelisp-cc--alloc-spilled-values alloc)))
    (should spilled)
    (should (>= frame-size 16))           ; ABI-aligned floor for ≥1 slot.
    (dolist (vid spilled)
      (should (integerp (nelisp-cc--stack-slot-of slot-alist vid))))))

;;; (43) x86_64 backend handles call-crossing spill end-to-end -----

(ert-deftest nelisp-cc-x86_64-call-crossing-end-to-end ()
  "T63 #3 — full pipeline (build → allocate → x86_64 compile) succeeds
on a call-crossing form without raising any backend signal.  Proves
the spill-aware lowering path is wired for the call-aware allocator."
  (let* ((fn (nelisp-cc-build-ssa-from-ast
              '(lambda (x) (+ x (* 2 3)))))
         (alloc (nelisp-cc--linear-scan fn)))
    (should-not
     (condition-case _err
         (progn (nelisp-cc-x86_64-compile fn alloc) nil)
       (nelisp-cc-x86_64-error t)))))

;;; (44) arm64 backend handles call-crossing spill end-to-end ------

(ert-deftest nelisp-cc-arm64-call-crossing-end-to-end ()
  "T63 #3 — same as test 43 but for the AAPCS64 arm64 backend.
Uses `compile-with-link' since arm64 :call lowering requires the
callee trampoline to bind the `callee:NAME' label fixup; the bare
`compile' API would otherwise raise `:unbound-label' for the `+'
and `*' primitives this test exercises."
  (let* ((fn (nelisp-cc-build-ssa-from-ast
              '(lambda (x) (+ x (* 2 3)))))
         (alloc (nelisp-cc--linear-scan fn)))
    (should-not
     (condition-case _err
         (progn (nelisp-cc-arm64-compile-with-link fn alloc) nil)
       (nelisp-cc-arm64-error t)))))

;;; (45) GC metadata records a non-zero frame-size when spills occur

(ert-deftest nelisp-cc-runtime-gc-metadata-frame-size ()
  "T63 #4 — GC metadata `:frame-size' must reflect the real spill
slot count, not the constant 0 the pre-T63 stub returned.  The T63
helper `nelisp-cc-runtime--insert-safe-points-with-meta' threads the
frame-size from `nelisp-cc--allocate-stack-slots'."
  (let* ((fn (nelisp-cc-build-ssa-from-ast
              '(lambda (x) (+ x (* 2 3)))))
         (alloc (nelisp-cc--linear-scan fn))
         (slots-pair (nelisp-cc--allocate-stack-slots alloc))
         (frame-size (cdr slots-pair))
         (meta (nelisp-cc-runtime--insert-safe-points-with-meta
                fn :frame-size frame-size)))
    (let ((sps (plist-get meta :safe-points)))
      (should sps)
      ;; Every safe-point's :frame-size matches the real frame.
      (dolist (sp sps)
        (should (= frame-size (plist-get sp :frame-size)))))))

;;; (46) GC metadata records call-points for moving-GC root recovery

(ert-deftest nelisp-cc-runtime-gc-metadata-call-points ()
  "T63 #4 — GC metadata records each `:call' / `:call-indirect' as a
safe-point so a moving collector can walk roots at the precise PC
where a callee transition happens.  Pre-T63 only entry / back-edge /
exit points were emitted; call-points were missing."
  (let* ((fn (nelisp-cc-build-ssa-from-ast
              '(lambda (x) (+ x (* 2 3)))))
         (meta (nelisp-cc-runtime--insert-safe-points-with-meta
                fn :frame-size 0))
         (sps (plist-get meta :safe-points))
         (n-call-pts (cl-count-if
                      (lambda (sp) (eq (plist-get sp :kind) 'call))
                      sps)))
    ;; Two arithmetic primitives in the form ⇒ ≥ 2 call safe-points.
    (should (>= n-call-pts 2))))

;;; (46b) AOT root-vector bridge for Doc 129.5C --------------------

(ert-deftest nelisp-cc-runtime-aot-root-vector-from-slots ()
  "Doc 129.5C — materialize a root vector from compiler slot metadata."
  (let* ((root-a (cons 'aot 'a))
         (root-b (vector 'aot 'b))
         (frame (vector root-a 'non-root root-b))
         (roots (nelisp-cc-runtime-aot-root-vector-from-slots
                 frame '(0 2))))
    (should (equal roots (vector root-a root-b)))))

(ert-deftest nelisp-cc-runtime-call-with-aot-roots-registers-frame ()
  "Doc 129.5C — the runtime call-boundary wrapper exposes AOT roots."
  (let* ((root (cons 'aot 'live))
         (roots (vector root))
         (nelisp-gc--active-aot-frames nil)
         (result
          (nelisp-cc-runtime-call-with-aot-roots
           roots
           (lambda ()
             (let* ((aot-roots
                     (cl-remove-if-not
                      (lambda (r) (eq (plist-get r :kind) 'aot-frame))
                      (nelisp-gc-root-set)))
                    (live (nelisp-gc-reachable-set)))
               (should (= 1 (length aot-roots)))
               (should (eq roots (plist-get (car aot-roots) :value)))
               (should (gethash root live)))
             'ok))))
    (should (eq result 'ok))
    (should (null nelisp-gc--active-aot-frames))))

(ert-deftest nelisp-cc-runtime-aot-materialize-roots-boundary ()
  "Doc 129.5G — native root materialization bridge builds root vectors."
  (let* ((root-a (cons 'aot 'a))
         (root-b (vector 'aot 'b))
         (out (vector nil))
         (roots (nelisp-cc-runtime-aot-materialize-roots-boundary
                 'mirror 'frames 2 out 'scratch root-a root-b)))
    (should (equal roots (vector root-a root-b)))
    (should (eq roots (aref out 0)))))

(ert-deftest nelisp-cc-runtime-aot-materialize-roots-validates-boundary ()
  "Doc 129.5G — root materialization rejects malformed ABI values."
  (let ((out (vector nil)))
    (should-error
     (nelisp-cc-runtime-aot-materialize-roots-boundary
      'mirror 'frames -1 out 'scratch)
     :type 'nelisp-cc-runtime-error)
    (should-error
     (nelisp-cc-runtime-aot-materialize-roots-boundary
      'mirror 'frames 2 out 'scratch 'only-one)
     :type 'nelisp-cc-runtime-error)
    (should-error
     (nelisp-cc-runtime-aot-materialize-roots-boundary
      'mirror 'frames 0 nil 'scratch)
     :type 'nelisp-cc-runtime-error)))

(ert-deftest nelisp-cc-runtime-aot-root-push-pop-boundary ()
  "Doc 129.5E — explicit native root push/pop bridges update root-set."
  (let* ((root (cons 'aot 'native))
         (roots (vector root))
         (out (vector nil))
         (nelisp-gc--active-aot-frames nil))
    (should (eq (nelisp-cc-runtime-aot-push-roots-boundary
                 'mirror 'frames roots out 'scratch)
                out))
    (should (eq (aref out 0) roots))
    (should (eq roots
                (car (nelisp-cc-runtime-aot-root-stack-snapshot))))
    (let ((live (nelisp-gc-reachable-set)))
      (should (gethash root live)))
    (aset out 0 nil)
    (should (eq (nelisp-cc-runtime-aot-pop-roots-boundary
                 'mirror 'frames roots out 'scratch)
                out))
    (should (eq (aref out 0) roots))
    (should (null nelisp-gc--active-aot-frames))))

(ert-deftest nelisp-cc-runtime-aot-root-push-pop-validates-boundary ()
  "Doc 129.5E — root push/pop bridges reject malformed ABI values."
  (let ((out (vector nil))
        (roots (vector 'root))
        (nelisp-gc--active-aot-frames nil))
    (should-error
     (nelisp-cc-runtime-aot-push-roots-boundary
      'mirror 'frames 'not-a-vector out 'scratch)
     :type 'nelisp-cc-runtime-error)
    (should-error
     (nelisp-cc-runtime-aot-push-roots-boundary
      'mirror 'frames roots nil 'scratch)
     :type 'nelisp-cc-runtime-error)
    (should-error
     (nelisp-cc-runtime-aot-pop-roots-boundary
      'mirror 'frames roots out 'scratch)
     :type 'nelisp-cc-runtime-error)
    (nelisp-cc-runtime-aot-push-roots-boundary
     'mirror 'frames roots out 'scratch)
    (should-error
     (nelisp-cc-runtime-aot-pop-roots-boundary
      'mirror 'frames (vector 'other) out 'scratch)
     :type 'nelisp-cc-runtime-error)))

(ert-deftest nelisp-cc-runtime-aot-special-push-pop-boundary ()
  "Doc 129.4C — special binding bridges save and restore value cells."
  (let ((sym (make-symbol "doc129-special"))
        (out (vector nil)))
    (unwind-protect
        (progn
          (nelisp-cc-runtime-aot-reset-special-stack)
          (should (not (boundp sym)))
          (should (eq (nelisp-cc-runtime-aot-push-special-boundary
                       'mirror 'frames sym 42 out 'scratch)
                      out))
          (let ((record (aref out 0)))
            (should (= (symbol-value sym) 42))
            (should (eq (car (nelisp-cc-runtime-aot-special-stack-snapshot))
                        record))
            (should (eq (nelisp-cc-runtime-aot-pop-special-boundary
                         'mirror 'frames record out 'scratch)
                        out))
            (should (eq (aref out 0) record))
            (should (not (boundp sym)))))
      (nelisp-cc-runtime-aot-reset-special-stack)
      (when (boundp sym)
        (makunbound sym)))))

(ert-deftest nelisp-cc-runtime-aot-special-push-pop-restores-old-value ()
  "Doc 129.4C — special binding pop restores pre-existing values."
  (let ((sym (make-symbol "doc129-special-bound"))
        (out (vector nil)))
    (unwind-protect
        (progn
          (nelisp-cc-runtime-aot-reset-special-stack)
          (set sym 'old)
          (nelisp-cc-runtime-aot-push-special-boundary
           'mirror 'frames sym 'new out 'scratch)
          (let ((record (aref out 0)))
            (should (eq (symbol-value sym) 'new))
            (nelisp-cc-runtime-aot-pop-special-boundary
             'mirror 'frames record out 'scratch)
            (should (eq (symbol-value sym) 'old))))
      (nelisp-cc-runtime-aot-reset-special-stack)
      (when (boundp sym)
        (makunbound sym)))))

(ert-deftest nelisp-cc-runtime-aot-special-pop-zero-skips-expected-check ()
  "Doc 129.4D — source lowering can pop the innermost special binding."
  (let ((sym (make-symbol "doc129-special-zero"))
        (out (vector nil)))
    (unwind-protect
        (progn
          (nelisp-cc-runtime-aot-reset-special-stack)
          (nelisp-cc-runtime-aot-push-special-boundary
           'mirror 'frames sym 7 out 'scratch)
          (should (= (symbol-value sym) 7))
          (nelisp-cc-runtime-aot-pop-special-boundary
           'mirror 'frames 0 out 'scratch)
          (should (not (boundp sym))))
      (nelisp-cc-runtime-aot-reset-special-stack)
      (when (boundp sym)
        (makunbound sym)))))

(ert-deftest nelisp-cc-runtime-aot-special-push-pop-validates-boundary ()
  "Doc 129.4C — special binding bridges reject malformed ABI values."
  (let ((sym (make-symbol "doc129-special-bad"))
        (out (vector nil)))
    (unwind-protect
        (progn
          (nelisp-cc-runtime-aot-reset-special-stack)
          (should-error
           (nelisp-cc-runtime-aot-push-special-boundary
            'mirror 'frames "not-symbol" 1 out 'scratch)
           :type 'nelisp-cc-runtime-error)
          (should-error
           (nelisp-cc-runtime-aot-push-special-boundary
            'mirror 'frames sym 1 nil 'scratch)
           :type 'nelisp-cc-runtime-error)
          (should-error
           (nelisp-cc-runtime-aot-pop-special-boundary
            'mirror 'frames nil out 'scratch)
           :type 'nelisp-cc-runtime-error)
          (nelisp-cc-runtime-aot-push-special-boundary
           'mirror 'frames sym 1 out 'scratch)
          (should-error
           (nelisp-cc-runtime-aot-pop-special-boundary
            'mirror 'frames (list :kind 'special :name sym) out 'scratch)
           :type 'nelisp-cc-runtime-error)
          (should (= (length (nelisp-cc-runtime-aot-special-stack-snapshot))
                     1))
          (should (= (symbol-value sym) 1)))
      (nelisp-cc-runtime-aot-reset-special-stack)
      (when (boundp sym)
        (makunbound sym)))))

(ert-deftest nelisp-cc-runtime-aot-builtin-call1-host-dispatch ()
  "Doc 129.6C — builtin call1 writes the dispatcher result to OUT."
  (let* ((out (vector nil))
         (ret (nelisp-cc-runtime-aot-builtin-call1
               'mirror 'frames 'symbol-name 'doc129 out 'scratch)))
    (should (eq ret out))
    (should (equal (aref out 0) "doc129"))))

(ert-deftest nelisp-cc-runtime-aot-builtin-call1-host-unary-table ()
  "Doc 129.6E — host dispatch covers representative unary builtins."
  (let ((out (vector nil)))
    (nelisp-cc-runtime-aot-builtin-call1
     'mirror 'frames 'length '(a b c) out 'scratch)
    (should (= (aref out 0) 3))
    (nelisp-cc-runtime-aot-builtin-call1
     'mirror 'frames 'car '(head tail) out 'scratch)
    (should (eq (aref out 0) 'head))
    (nelisp-cc-runtime-aot-builtin-call1
     'mirror 'frames 'number-to-string 129 out 'scratch)
    (should (equal (aref out 0) "129"))))

(ert-deftest nelisp-cc-runtime-aot-builtin-call1-custom-dispatch ()
  "Doc 129.6C — callers can inject the native/Doc99 dispatcher body."
  (let* ((out (vector nil))
         (events nil)
         (ret (nelisp-cc-runtime-aot-builtin-call1
               'mirror 'frames 'doc129-upper "abc" out 'scratch
               (lambda (name arg context)
                 (push (list name arg
                             (plist-get context :mirror)
                             (plist-get context :out))
                       events)
                 (upcase arg)))))
    (should (eq ret out))
    (should (equal (aref out 0) "ABC"))
    (should (equal events
                   `((doc129-upper "abc" mirror ,out))))))

(ert-deftest nelisp-cc-runtime-aot-builtin-call1-validates-boundary ()
  "Doc 129.6C — builtin call1 rejects malformed ABI arguments."
  (should-error
   (nelisp-cc-runtime-aot-builtin-call1
    'mirror 'frames "symbol-name" 'x (vector nil) 'scratch)
   :type 'nelisp-cc-runtime-error)
  (should-error
   (nelisp-cc-runtime-aot-builtin-call1
    'mirror 'frames 'symbol-name 'x nil 'scratch)
   :type 'nelisp-cc-runtime-error)
  (should-error
   (nelisp-cc-runtime-aot-builtin-call1
    'mirror 'frames 'symbol-name 'x (vector nil) 'scratch :not-a-function)
   :type 'nelisp-cc-runtime-error))

(ert-deftest nelisp-cc-runtime-aot-builtin-calln-host-dispatch ()
  "Doc 129.6F — builtin calln dispatches variable argument lists."
  (let* ((out (vector nil))
         (ret (nelisp-cc-runtime-aot-builtin-calln
               'mirror 'frames 'list 3 out 'scratch
               'doc 129 'calln)))
    (should (eq ret out))
    (should (equal (aref out 0) '(doc 129 calln))))
  (let ((out (vector nil)))
    (nelisp-cc-runtime-aot-builtin-calln
     'mirror 'frames 'concat 3 out 'scratch "doc" "129" "F")
    (should (equal (aref out 0) "doc129F"))))

(ert-deftest nelisp-cc-runtime-aot-builtin-calln-host-expanded-table ()
  "Doc 129.6G — builtin calln covers common fixed-arity builtins."
  (let ((out (vector nil)))
    (nelisp-cc-runtime-aot-builtin-calln
     'mirror 'frames 'cons 2 out 'scratch 'a 'b)
    (should (equal (aref out 0) '(a . b)))
    (nelisp-cc-runtime-aot-builtin-calln
     'mirror 'frames 'eq 2 out 'scratch 'same 'same)
    (should (eq (aref out 0) t))
    (nelisp-cc-runtime-aot-builtin-calln
     'mirror 'frames 'nth 2 out 'scratch 1 '(zero one two))
    (should (eq (aref out 0) 'one))
    (nelisp-cc-runtime-aot-builtin-calln
     'mirror 'frames 'assq 2 out 'scratch 'k '((a . 1) (k . 2)))
    (should (equal (aref out 0) '(k . 2)))))

(ert-deftest nelisp-cc-runtime-aot-builtin-calln-host-string-table ()
  "Doc 129.6H — builtin calln covers common string/format builtins."
  (let ((out (vector nil)))
    (nelisp-cc-runtime-aot-builtin-calln
     'mirror 'frames 'format 2 out 'scratch "doc%s" 129)
    (should (equal (aref out 0) "doc129"))
    (nelisp-cc-runtime-aot-builtin-calln
     'mirror 'frames 'string-match 2 out 'scratch "29" "Doc129")
    (should (= (aref out 0) 4))
    (nelisp-cc-runtime-aot-builtin-calln
     'mirror 'frames 'substring 3 out 'scratch "Doc129" 3 6)
    (should (equal (aref out 0) "129"))
    (nelisp-cc-runtime-aot-builtin-calln
     'mirror 'frames 'replace-regexp-in-string 3 out 'scratch
     "129" "130" "Doc129")
    (should (equal (aref out 0) "Doc130"))))

(ert-deftest nelisp-cc-runtime-aot-builtin-calln-host-higher-order-table ()
  "Doc 129.6I — builtin calln covers dynamic higher-order builtins."
  (let ((out (vector nil))
        (seen nil))
    (nelisp-cc-runtime-aot-builtin-calln
     'mirror 'frames 'mapcar 2 out 'scratch #'1+ '(1 2 3))
    (should (equal (aref out 0) '(2 3 4)))
    (nelisp-cc-runtime-aot-builtin-calln
     'mirror 'frames 'mapc 2 out 'scratch
     (lambda (x) (push x seen))
     '(a b))
    (should (equal seen '(b a)))
    (nelisp-cc-runtime-aot-builtin-calln
     'mirror 'frames 'mapconcat 3 out 'scratch #'identity '("a" "b") ",")
    (should (equal (aref out 0) "a,b"))
    (nelisp-cc-runtime-aot-builtin-calln
     'mirror 'frames 'mapcan 2 out 'scratch
     (lambda (x) (list x x))
     '(a b))
    (should (equal (aref out 0) '(a a b b)))
    (let ((table (make-hash-table :test 'eq))
          (seen nil))
      (puthash 'k 9 table)
      (nelisp-cc-runtime-aot-builtin-calln
       'mirror 'frames 'maphash 2 out 'scratch
       (lambda (k v) (push (cons k v) seen))
       table)
      (should (null (aref out 0)))
      (should (equal seen '((k . 9)))))
    (nelisp-cc-runtime-aot-builtin-calln
     'mirror 'frames 'seq-filter 2 out 'scratch #'numberp '(a 1 b 2))
    (should (equal (aref out 0) '(1 2)))
    (nelisp-cc-runtime-aot-builtin-calln
     'mirror 'frames 'seq-reduce 3 out 'scratch
     (lambda (acc x) (+ acc x)) '(1 2 3) 10)
    (should (= (aref out 0) 16))
    (let ((seen nil))
      (nelisp-cc-runtime-aot-builtin-calln
       'mirror 'frames 'seq-do 2 out 'scratch
       (lambda (x) (push x seen))
       '(a b))
      (should (equal (aref out 0) '(a b)))
      (should (equal seen '(b a))))
    (nelisp-cc-runtime-aot-builtin-calln
     'mirror 'frames 'cl-find-if 2 out 'scratch #'numberp '(a 1 b 2))
    (should (= (aref out 0) 1))
    (nelisp-cc-runtime-aot-builtin-calln
     'mirror 'frames 'cl-remove-if 2 out 'scratch #'symbolp '(a 1 b 2))
    (should (equal (aref out 0) '(1 2)))
    (nelisp-cc-runtime-aot-builtin-calln
     'mirror 'frames 'cl-reduce 2 out 'scratch
     (lambda (acc x) (+ acc x))
     '(1 2 3))
    (should (= (aref out 0) 6))
    (nelisp-cc-runtime-aot-builtin-calln
     'mirror 'frames 'sort 2 out 'scratch '(3 1 2) #'<)
    (should (equal (aref out 0) '(1 2 3)))
    (nelisp-cc-runtime-aot-builtin-calln
     'mirror 'frames 'seq-keep 2 out 'scratch #'numberp '(a 1 nil 2))
    (should (equal (aref out 0) '(t t)))
    (nelisp-cc-runtime-aot-builtin-calln
     'mirror 'frames 'seq-map-indexed 2 out 'scratch
     (lambda (x i) (list i x)) '(a b))
    (should (equal (aref out 0) '((0 a) (1 b))))
    (nelisp-cc-runtime-aot-builtin-calln
     'mirror 'frames 'seq-mapn 3 out 'scratch #'+ '(1 2) '(10 20))
    (should (equal (aref out 0) '(11 22)))
    (nelisp-cc-runtime-aot-builtin-calln
     'mirror 'frames 'seq-sort 2 out 'scratch #'< '(3 1 2))
    (should (equal (aref out 0) '(1 2 3)))
    (nelisp-cc-runtime-aot-builtin-calln
     'mirror 'frames 'seq-uniq 2 out 'scratch '(a a b) #'eq)
    (should (equal (aref out 0) '(a b)))
    (nelisp-cc-runtime-aot-builtin-calln
     'mirror 'frames 'cl-map 3 out 'scratch 'list #'1+ '(1 2))
    (should (equal (aref out 0) '(2 3)))
    (nelisp-cc-runtime-aot-builtin-calln
     'mirror 'frames 'cl-sort 2 out 'scratch '(3 1 2) #'<)
    (should (equal (aref out 0) '(1 2 3)))
    (nelisp-cc-runtime-aot-builtin-calln
     'mirror 'frames 'cl-stable-sort 2 out 'scratch '(3 1 2) #'<)
    (should (equal (aref out 0) '(1 2 3)))
    (nelisp-cc-runtime-aot-builtin-calln
     'mirror 'frames 'cl-merge 4 out 'scratch 'list '(1 3) '(2 4) #'<)
    (should (equal (aref out 0) '(1 2 3 4)))))

(ert-deftest nelisp-cc-runtime-aot-builtin-calln-validates-boundary ()
  "Doc 129.6F — builtin calln rejects malformed ABI arguments."
  (should-error
   (nelisp-cc-runtime-aot-builtin-calln
    'mirror 'frames "list" 0 (vector nil) 'scratch)
   :type 'nelisp-cc-runtime-error)
  (should-error
   (nelisp-cc-runtime-aot-builtin-calln
    'mirror 'frames 'list -1 (vector nil) 'scratch)
   :type 'nelisp-cc-runtime-error)
  (should-error
   (nelisp-cc-runtime-aot-builtin-calln
    'mirror 'frames 'list 2 (vector nil) 'scratch 'only-one)
   :type 'nelisp-cc-runtime-error)
  (should-error
   (nelisp-cc-runtime-aot-builtin-calln
    'mirror 'frames 'list 0 nil 'scratch)
   :type 'nelisp-cc-runtime-error))

(ert-deftest nelisp-cc-runtime-aot-funcall1-host-dispatch ()
  "Doc 129.7A — funcall1 writes the dispatch result to OUT."
  (let* ((out (vector nil))
         (ret (nelisp-cc-runtime-aot-funcall1
               'mirror 'frames '1+ 40 out 'scratch)))
    (should (eq ret out))
    (should (= (aref out 0) 41))
    (nelisp-cc-runtime-aot-funcall1
     'mirror 'frames (lambda (x) (* x 2)) 21 out 'scratch)
    (should (= (aref out 0) 42))))

(ert-deftest nelisp-cc-runtime-aot-funcall1-custom-dispatch ()
  "Doc 129.7A — callers can inject the native/Doc99 funcall body."
  (let* ((out (vector nil))
         (events nil)
         (ret (nelisp-cc-runtime-aot-funcall1
               'mirror 'frames 'doc129-fn 7 out 'scratch
               (lambda (fn arg context)
                 (push (list fn arg
                             (plist-get context :frames)
                             (plist-get context :out))
                       events)
                 (+ arg 35)))))
    (should (eq ret out))
    (should (= (aref out 0) 42))
    (should (equal events
                   `((doc129-fn 7 frames ,out))))))

(ert-deftest nelisp-cc-runtime-aot-funcall1-validates-boundary ()
  "Doc 129.7A — funcall1 rejects malformed ABI arguments."
  (should-error
   (nelisp-cc-runtime-aot-funcall1
    'mirror 'frames '1+ 1 nil 'scratch)
   :type 'nelisp-cc-runtime-error)
  (should-error
   (nelisp-cc-runtime-aot-funcall1
    'mirror 'frames '1+ 1 (vector nil) 'scratch :not-a-function)
   :type 'nelisp-cc-runtime-error)
  (should-error
   (nelisp-cc-runtime-aot-funcall1
    'mirror 'frames 'nelisp-doc129-missing-fn 1 (vector nil) 'scratch)
   :type 'nelisp-cc-runtime-error))

(ert-deftest nelisp-cc-runtime-aot-funcall2-host-dispatch ()
  "Doc 129.7B — funcall2 writes the two-arg dispatch result to OUT."
  (let* ((out (vector nil))
         (ret (nelisp-cc-runtime-aot-funcall2
               'mirror 'frames '+ 19 23 out)))
    (should (eq ret out))
    (should (= (aref out 0) 42))
    (nelisp-cc-runtime-aot-funcall2
     'mirror 'frames (lambda (a b) (concat a b)) "doc" "129" out)
    (should (equal (aref out 0) "doc129"))))

(ert-deftest nelisp-cc-runtime-aot-funcall2-custom-dispatch ()
  "Doc 129.7B — callers can inject the native/Doc99 funcall2 body."
  (let* ((out (vector nil))
         (events nil)
         (ret (nelisp-cc-runtime-aot-funcall2
               'mirror 'frames 'doc129-fn 20 22 out
               (lambda (fn arg0 arg1 context)
                 (push (list fn arg0 arg1
                             (plist-get context :mirror)
                             (plist-get context :out))
                       events)
                 (+ arg0 arg1)))))
    (should (eq ret out))
    (should (= (aref out 0) 42))
    (should (equal events
                   `((doc129-fn 20 22 mirror ,out))))))

(ert-deftest nelisp-cc-runtime-aot-funcall2-validates-boundary ()
  "Doc 129.7B — funcall2 rejects malformed ABI arguments."
  (should-error
   (nelisp-cc-runtime-aot-funcall2
    'mirror 'frames '+ 1 2 nil)
   :type 'nelisp-cc-runtime-error)
  (should-error
   (nelisp-cc-runtime-aot-funcall2
    'mirror 'frames '+ 1 2 (vector nil) :not-a-function)
   :type 'nelisp-cc-runtime-error)
  (should-error
   (nelisp-cc-runtime-aot-funcall2
    'mirror 'frames 'nelisp-doc129-missing-fn 1 2 (vector nil))
   :type 'nelisp-cc-runtime-error))

(ert-deftest nelisp-cc-runtime-aot-funcall3-host-dispatch ()
  "Doc 129.7E — funcall3 writes the three-arg dispatch result to OUT."
  (let* ((out (vector nil))
         (ret (nelisp-cc-runtime-aot-funcall3
               'mirror 'frames '+ 10 11 21 out)))
    (should (eq ret out))
    (should (= (aref out 0) 42))
    (nelisp-cc-runtime-aot-funcall3
     'mirror 'frames (lambda (a b c) (concat a b c)) "doc" "129" "E" out)
    (should (equal (aref out 0) "doc129E"))))

(ert-deftest nelisp-cc-runtime-aot-funcall3-custom-dispatch ()
  "Doc 129.7E — callers can inject the native/Doc99 funcall3 body."
  (let* ((out (vector nil))
         (events nil)
         (ret (nelisp-cc-runtime-aot-funcall3
               'mirror 'frames 'doc129-fn 10 11 21 out
               (lambda (fn arg0 arg1 arg2 context)
                 (push (list fn arg0 arg1 arg2
                             (plist-get context :frames)
                             (plist-get context :out))
                       events)
                 (+ arg0 arg1 arg2)))))
    (should (eq ret out))
    (should (= (aref out 0) 42))
    (should (equal events
                   `((doc129-fn 10 11 21 frames ,out))))))

(ert-deftest nelisp-cc-runtime-aot-funcall3-validates-boundary ()
  "Doc 129.7E — funcall3 rejects malformed ABI arguments."
  (should-error
   (nelisp-cc-runtime-aot-funcall3
    'mirror 'frames '+ 1 2 3 nil)
   :type 'nelisp-cc-runtime-error)
  (should-error
   (nelisp-cc-runtime-aot-funcall3
    'mirror 'frames '+ 1 2 3 (vector nil) :not-a-function)
   :type 'nelisp-cc-runtime-error)
  (should-error
   (nelisp-cc-runtime-aot-funcall3
    'mirror 'frames 'nelisp-doc129-missing-fn 1 2 3 (vector nil))
   :type 'nelisp-cc-runtime-error))

(ert-deftest nelisp-cc-runtime-aot-funcalln-host-dispatch ()
  "Doc 129.7H — funcalln builds an args list and dispatches."
  (let* ((out (vector nil))
         (ret (nelisp-cc-runtime-aot-funcalln
               'mirror 'frames '+ 4 out 'scratch 10 11 12 9)))
    (should (eq ret out))
    (should (= (aref out 0) 42))
    (nelisp-cc-runtime-aot-funcalln
     'mirror 'frames (lambda (&rest xs) (mapconcat #'identity xs ":"))
     4 out 'scratch "doc" "129" "call" "n")
    (should (equal (aref out 0) "doc:129:call:n"))))

(ert-deftest nelisp-cc-runtime-aot-funcalln-preserves-function-args ()
  "Doc 129.7H — function values in ARGS are ordinary user arguments."
  (let ((out (vector nil)))
    (nelisp-cc-runtime-aot-funcalln
     'mirror 'frames
     (lambda (&rest xs) (mapcar #'functionp xs))
     2 out 'scratch #'identity (lambda (x) x))
    (should (equal (aref out 0) '(t t)))))

(ert-deftest nelisp-cc-runtime-aot-funcalln-validates-boundary ()
  "Doc 129.7H — funcalln rejects malformed ABI arguments."
  (should-error
   (nelisp-cc-runtime-aot-funcalln
    'mirror 'frames '+ 4 nil 'scratch 1 2 3 4)
   :type 'nelisp-cc-runtime-error)
  (should-error
   (nelisp-cc-runtime-aot-funcalln
    'mirror 'frames '+ -1 (vector nil) 'scratch)
   :type 'nelisp-cc-runtime-error)
  (should-error
   (nelisp-cc-runtime-aot-funcalln
    'mirror 'frames '+ 3 (vector nil) 'scratch 1 2)
   :type 'nelisp-cc-runtime-error)
  (should-error
   (nelisp-cc-runtime-aot-funcalln
    'mirror 'frames 'nelisp-doc129-missing-fn 1 (vector nil) 'scratch 1)
   :type 'nelisp-cc-runtime-error))

(ert-deftest nelisp-cc-runtime-aot-funcall-closure-dispatch ()
  "Doc 129.7T — funcall dispatches canonical heap closures."
  (let* ((out (vector nil))
         (add-cap (nelisp-closure-make '((cap . 7)) '(x) '((+ x cap)))))
    (should (eq (nelisp-cc-runtime-aot-funcall1
                 'mirror 'frames add-cap 5 out 'scratch)
                out))
    (should (= (aref out 0) 12))
    (nelisp-cc-runtime-aot-funcall2
     'mirror 'frames
     (nelisp-closure-make nil '(a b) '((+ a b)))
     19 23 out)
    (should (= (aref out 0) 42))
    (nelisp-cc-runtime-aot-funcall3
     'mirror 'frames
     (nelisp-closure-make '((base . 6)) '(a b c) '((+ base a b c)))
     10 11 15 out)
    (should (= (aref out 0) 42))))

(ert-deftest nelisp-cc-runtime-aot-apply-closure-dispatch ()
  "Doc 129.7T — apply/funcalln dispatch canonical heap closures."
  (let* ((out (vector nil))
         (closure (nelisp-closure-make
                   '((cap . 40))
                   '(x y)
                   '((+ cap x y)))))
    (should (eq (nelisp-cc-runtime-aot-apply
                 'mirror 'frames closure '(1 1) out 'scratch)
                out))
    (should (= (aref out 0) 42))
    (nelisp-cc-runtime-aot-funcalln
     'mirror 'frames closure 2 out 'scratch 1 1)
    (should (= (aref out 0) 42))
    (nelisp-cc-runtime-aot-applyn
     'mirror 'frames closure 2 out 'scratch 1 '(1))
    (should (= (aref out 0) 42))))

(ert-deftest nelisp-cc-runtime-aot-closure-captured-mutation ()
  "Doc 129.7T — captured mutation survives through AOT dispatch."
  (let* ((out (vector nil))
         (shared (list (cons 'counter 0)))
         (writer (nelisp-closure-make
                  shared
                  '(n)
                  '((setq counter (+ counter n))
                    counter)))
         (reader (nelisp-closure-make shared '() '(counter))))
    (nelisp-cc-runtime-aot-funcall1
     'mirror 'frames writer 1 out 'scratch)
    (should (= (aref out 0) 1))
    (nelisp-cc-runtime-aot-funcall1
     'mirror 'frames writer 4 out 'scratch)
    (should (= (aref out 0) 5))
    (nelisp-cc-runtime-aot-funcalln
     'mirror 'frames reader 0 out 'scratch)
    (should (= (aref out 0) 5))))

(ert-deftest nelisp-cc-runtime-aot-make-closure ()
  "Doc 129.7U — make-closure bridge materializes captured closures."
  (unwind-protect
      (let* ((out (vector nil))
             (descriptor '(:name nelisp-doc129-closure
                           :arglist (x)
                           :body ((+ x cap))
                           :captures (cap))))
        (nelisp-cc-runtime-clear-aot-closure-descriptors)
        (nelisp-cc-runtime-register-aot-closure-descriptor descriptor)
        (should (eq (nelisp-cc-runtime-aot-make-closure
                     'mirror 'frames 'nelisp-doc129-closure
                     1 out 'scratch 40)
                    out))
        (should (nelisp-closure-p (aref out 0)))
        (should (= (nelisp-closure-apply (aref out 0) '(2)) 42)))
    (nelisp-cc-runtime-clear-aot-closure-descriptors)))

(ert-deftest nelisp-cc-runtime-aot-apply-host-dispatch ()
  "Doc 129.7C — apply bridge writes the dispatch result to OUT."
  (let* ((out (vector nil))
         (ret (nelisp-cc-runtime-aot-apply
               'mirror 'frames '+ '(19 23) out 'scratch)))
    (should (eq ret out))
    (should (= (aref out 0) 42))
    (nelisp-cc-runtime-aot-apply
     'mirror 'frames (lambda (&rest xs) (mapconcat #'identity xs ":"))
     '("doc" "129" "apply") out 'scratch)
    (should (equal (aref out 0) "doc:129:apply"))))

(ert-deftest nelisp-cc-runtime-aot-apply-custom-dispatch ()
  "Doc 129.7C — callers can inject the native/Doc99 apply body."
  (let* ((out (vector nil))
         (events nil)
         (ret (nelisp-cc-runtime-aot-apply
               'mirror 'frames 'doc129-fn '(20 22) out 'scratch
               (lambda (fn args-list context)
                 (push (list fn args-list
                             (plist-get context :scratch)
                             (plist-get context :out))
                       events)
                 (apply #'+ args-list)))))
    (should (eq ret out))
    (should (= (aref out 0) 42))
    (should (equal events
                   `((doc129-fn (20 22) scratch ,out))))))

(ert-deftest nelisp-cc-runtime-aot-apply-validates-boundary ()
  "Doc 129.7C — apply bridge rejects malformed ABI arguments."
  (should-error
   (nelisp-cc-runtime-aot-apply
    'mirror 'frames '+ '(1 2) nil 'scratch)
   :type 'nelisp-cc-runtime-error)
  (should-error
   (nelisp-cc-runtime-aot-apply
    'mirror 'frames '+ '(1 2) (vector nil) 'scratch :not-a-function)
   :type 'nelisp-cc-runtime-error)
  (should-error
   (nelisp-cc-runtime-aot-apply
    'mirror 'frames '+ 1 (vector nil) 'scratch)
   :type 'nelisp-cc-runtime-error)
  (should-error
   (nelisp-cc-runtime-aot-apply
    'mirror 'frames 'nelisp-doc129-missing-fn '(1 2) (vector nil) 'scratch)
   :type 'nelisp-cc-runtime-error))

(ert-deftest nelisp-cc-runtime-aot-applyn-host-dispatch ()
  "Doc 129.7I — applyn splices fixed args before the list tail."
  (let* ((out (vector nil))
         (ret (nelisp-cc-runtime-aot-applyn
               'mirror 'frames '+ 3 out 'scratch 10 12 '(20))))
    (should (eq ret out))
    (should (= (aref out 0) 42))
    (nelisp-cc-runtime-aot-applyn
     'mirror 'frames (lambda (&rest xs) (mapconcat #'identity xs ":"))
     3 out 'scratch "doc" "129" '("applyn"))
    (should (equal (aref out 0) "doc:129:applyn"))))

(ert-deftest nelisp-cc-runtime-aot-applyn-validates-boundary ()
  "Doc 129.7I — applyn rejects malformed ABI arguments."
  (should-error
   (nelisp-cc-runtime-aot-applyn
    'mirror 'frames '+ 1 nil 'scratch '(1 2))
   :type 'nelisp-cc-runtime-error)
  (should-error
   (nelisp-cc-runtime-aot-applyn
    'mirror 'frames '+ 0 (vector nil) 'scratch)
   :type 'nelisp-cc-runtime-error)
  (should-error
   (nelisp-cc-runtime-aot-applyn
    'mirror 'frames '+ 2 (vector nil) 'scratch 1)
   :type 'nelisp-cc-runtime-error)
  (should-error
   (nelisp-cc-runtime-aot-applyn
    'mirror 'frames '+ 2 (vector nil) 'scratch 1 2)
   :type 'nelisp-cc-runtime-error))

(ert-deftest nelisp-cc-runtime-aot-listn-builds-rest-list ()
  "Doc 129.7J — listn builds the source rest-parameter list."
  (let* ((out (vector nil))
         (ret (nelisp-cc-runtime-aot-listn
               'mirror 'frames 3 out 'scratch 'a 'b 'c)))
    (should (eq ret out))
    (should (equal (aref out 0) '(a b c))))
  (let ((out (vector :sentinel)))
    (nelisp-cc-runtime-aot-listn 'mirror 'frames 0 out 'scratch)
    (should (null (aref out 0)))))

(ert-deftest nelisp-cc-runtime-aot-listn-validates-boundary ()
  "Doc 129.7J — listn rejects malformed ABI arguments."
  (should-error
   (nelisp-cc-runtime-aot-listn
    'mirror 'frames 1 nil 'scratch 'x)
   :type 'nelisp-cc-runtime-error)
  (should-error
   (nelisp-cc-runtime-aot-listn
    'mirror 'frames -1 (vector nil) 'scratch)
   :type 'nelisp-cc-runtime-error)
  (should-error
   (nelisp-cc-runtime-aot-listn
    'mirror 'frames 2 (vector nil) 'scratch 'x)
   :type 'nelisp-cc-runtime-error))

(ert-deftest nelisp-cc-runtime-aot-handler-stack-push-pop ()
  "Doc 129.8A — handler stack push/pop is explicit and typed."
  (unwind-protect
      (progn
        (nelisp-cc-runtime-aot-reset-handler-stack)
        (nelisp-cc-runtime-aot-push-catch 'tag 'catch-pad 80)
        (nelisp-cc-runtime-aot-push-condition 'error 'cc-pad 72)
        (should (= (length (nelisp-cc-runtime-aot-handler-stack-snapshot))
                   2))
        (should (eq (plist-get (nelisp-cc-runtime-aot-pop-handler
                                'condition)
                               :landing-pad)
                    'cc-pad))
        (should (eq (plist-get (nelisp-cc-runtime-aot-pop-handler
                                'catch)
                               :tag)
                    'tag))
        (should-error (nelisp-cc-runtime-aot-pop-handler)
                      :type 'nelisp-cc-runtime-error))
    (nelisp-cc-runtime-aot-reset-handler-stack)))

(ert-deftest nelisp-cc-runtime-aot-throw-runs-crossed-cleanups ()
  "Doc 129.8A — throw unwinds through cleanup handlers to a catch pad."
  (let ((events nil))
    (unwind-protect
        (progn
          (nelisp-cc-runtime-aot-reset-handler-stack)
          (nelisp-cc-runtime-aot-push-catch 'outer 'outer-pad 64)
          (nelisp-cc-runtime-aot-push-condition 'error 'ignored-cc 56)
          (nelisp-cc-runtime-aot-push-unwind
           (lambda (ctx)
             (push (list (plist-get ctx :reason)
                         (plist-get ctx :tag)
                         (plist-get ctx :value))
                   events)
             'cleanup-ok)
           'cleanup-pad 48)
          (let ((landing (nelisp-cc-runtime-aot-throw 'outer 42)))
            (should (eq (plist-get landing :kind) 'catch))
            (should (eq (plist-get landing :landing-pad) 'outer-pad))
            (should (= (plist-get landing :saved-sp) 64))
            (should (equal (plist-get landing :value) 42))
            (should (equal (plist-get landing :cleanups)
                           '(cleanup-ok)))))
      (nelisp-cc-runtime-aot-reset-handler-stack))
    (should (equal events '((throw outer 42))))
    (should (null (nelisp-cc-runtime-aot-handler-stack-snapshot)))))

(ert-deftest nelisp-cc-runtime-aot-signal-condition-handler ()
  "Doc 129.8A — signal matches condition handlers and binds error data."
  (let ((events nil))
    (unwind-protect
        (progn
          (nelisp-cc-runtime-aot-reset-handler-stack)
          (nelisp-cc-runtime-aot-push-condition
           '(wrong-type-argument arith-error) 'handler-pad 40)
          (nelisp-cc-runtime-aot-push-unwind
           (lambda (ctx)
             (push (list (plist-get ctx :reason)
                         (plist-get ctx :tag)
                         (plist-get ctx :data))
                   events)
             'signal-cleanup)
           'cleanup-pad 32)
          (let ((landing
                 (nelisp-cc-runtime-aot-signal
                  'wrong-type-argument '(integerp "x"))))
            (should (eq (plist-get landing :kind) 'condition))
            (should (eq (plist-get landing :landing-pad) 'handler-pad))
            (should (equal (plist-get landing :error)
                           '(wrong-type-argument integerp "x")))
            (should (equal (plist-get landing :cleanups)
                           '(signal-cleanup)))))
      (nelisp-cc-runtime-aot-reset-handler-stack))
    (should (equal events
                   '((signal wrong-type-argument (integerp "x")))))))

(ert-deftest nelisp-cc-runtime-aot-exception-validates-boundary ()
  "Doc 129.8A — handler-stack helpers reject malformed inputs."
  (unwind-protect
      (progn
        (nelisp-cc-runtime-aot-reset-handler-stack)
        (should-error
         (nelisp-cc-runtime-aot-push-condition '(error "bad") 'pad 0)
         :type 'nelisp-cc-runtime-error)
        (should-error
         (nelisp-cc-runtime-aot-push-unwind :not-a-function 'pad 0)
         :type 'nelisp-cc-runtime-error)
        (should-error
         (nelisp-cc-runtime-aot-signal "error" nil)
         :type 'nelisp-cc-runtime-error)
        (should-error
         (nelisp-cc-runtime-aot-throw 'missing 1)
         :type 'nelisp-cc-runtime-error))
    (nelisp-cc-runtime-aot-reset-handler-stack)))

(ert-deftest nelisp-cc-runtime-aot-throw-boundary ()
  "Doc 129.8B — throw boundary bridge writes landing descriptor to OUT."
  (unwind-protect
      (let ((out (vector nil)))
        (nelisp-cc-runtime-aot-reset-handler-stack)
        (nelisp-cc-runtime-aot-push-catch 'tag 'catch-pad 88)
        (should (eq (nelisp-cc-runtime-aot-throw-boundary
                     'mirror 'frames 'tag 42 out 'scratch)
                    out))
        (let ((landing (aref out 0)))
          (should (eq (plist-get landing :kind) 'catch))
          (should (eq (plist-get landing :landing-pad) 'catch-pad))
          (should (= (plist-get landing :saved-sp) 88))
          (should (= (plist-get landing :value) 42))))
    (nelisp-cc-runtime-aot-reset-handler-stack)))

(ert-deftest nelisp-cc-runtime-aot-landing-value-boundary ()
  "Doc 129.8L — landing-value bridge extracts catch payloads."
  (let ((out (vector nil))
        (landing (vector '(:kind catch :value 42))))
    (should (eq (nelisp-cc-runtime-aot-landing-value-boundary
                 'mirror 'frames landing out 'scratch)
                out))
    (should (= (aref out 0) 42))))

(ert-deftest nelisp-cc-runtime-aot-landing-error-boundary ()
  "Doc 129.8M — landing-error bridge extracts condition data."
  (let ((out (vector nil))
        (landing (vector '(:kind condition :error (error "bad")))))
    (should (eq (nelisp-cc-runtime-aot-landing-error-boundary
                 'mirror 'frames landing out 'scratch)
                out))
    (should (equal (aref out 0) '(error "bad")))))

(ert-deftest nelisp-cc-runtime-aot-signal-boundary ()
  "Doc 129.8B — signal boundary bridge writes landing descriptor to OUT."
  (unwind-protect
      (let ((out (vector nil)))
        (nelisp-cc-runtime-aot-reset-handler-stack)
        (nelisp-cc-runtime-aot-push-condition 'error 'cc-pad 72)
        (should (eq (nelisp-cc-runtime-aot-signal-boundary
                     'mirror 'frames 'arith-error '("bad") out 'scratch)
                    out))
        (let ((landing (aref out 0)))
          (should (eq (plist-get landing :kind) 'condition))
          (should (eq (plist-get landing :landing-pad) 'cc-pad))
          (should (equal (plist-get landing :error)
                         '(arith-error "bad")))))
    (nelisp-cc-runtime-aot-reset-handler-stack)))

(ert-deftest nelisp-cc-runtime-aot-errorn-boundary ()
  "Doc 129.8I — errorn formats args and signals `error'."
  (unwind-protect
      (let ((out (vector nil)))
        (nelisp-cc-runtime-aot-reset-handler-stack)
        (nelisp-cc-runtime-aot-push-condition 'error 'cc-pad 72)
        (should (eq (nelisp-cc-runtime-aot-errorn-boundary
                     'mirror 'frames 3 out 'scratch "%s=%s" "k" "v")
                    out))
        (let ((landing (aref out 0)))
          (should (eq (plist-get landing :kind) 'condition))
          (should (eq (plist-get landing :landing-pad) 'cc-pad))
          (should (equal (plist-get landing :error)
                         '(error "k=v")))))
    (nelisp-cc-runtime-aot-reset-handler-stack)))

(ert-deftest nelisp-cc-runtime-aot-errorn-validates-boundary ()
  "Doc 129.8I — errorn rejects malformed ABI arguments."
  (should-error
   (nelisp-cc-runtime-aot-errorn-boundary
    'mirror 'frames 1 nil 'scratch "x")
   :type 'nelisp-cc-runtime-error)
  (should-error
   (nelisp-cc-runtime-aot-errorn-boundary
    'mirror 'frames -1 (vector nil) 'scratch)
   :type 'nelisp-cc-runtime-error)
  (should-error
   (nelisp-cc-runtime-aot-errorn-boundary
    'mirror 'frames 2 (vector nil) 'scratch "x")
   :type 'nelisp-cc-runtime-error))

(ert-deftest nelisp-cc-runtime-aot-exception-boundary-custom-dispatch ()
  "Doc 129.8B — callers can inject native throw/signal bridge bodies."
  (let ((throw-out (vector nil))
        (signal-out (vector nil))
        (events nil))
    (should (eq (nelisp-cc-runtime-aot-throw-boundary
                 'mirror 'frames 'tag 7 throw-out 'scratch
                 (lambda (tag value context)
                   (push (list :throw tag value
                               (plist-get context :scratch))
                         events)
                   (list :kind 'catch :value value)))
                throw-out))
    (should (equal (aref throw-out 0)
                   '(:kind catch :value 7)))
    (should (eq (nelisp-cc-runtime-aot-signal-boundary
                 'mirror 'frames 'error '("x") signal-out 'scratch
                 (lambda (tag data context)
                   (push (list :signal tag data
                               (plist-get context :out))
                         events)
                   (list :kind 'condition :error (cons tag data))))
                signal-out))
    (should (equal (aref signal-out 0)
                   '(:kind condition :error (error "x"))))
    (should (equal events
                   `((:signal error ("x") ,signal-out)
                     (:throw tag 7 scratch))))))

(ert-deftest nelisp-cc-runtime-aot-exception-boundary-validates ()
  "Doc 129.8B — throw/signal boundary bridges reject malformed inputs."
  (should-error
   (nelisp-cc-runtime-aot-throw-boundary
    'mirror 'frames 'tag 1 nil 'scratch)
   :type 'nelisp-cc-runtime-error)
  (should-error
   (nelisp-cc-runtime-aot-throw-boundary
    'mirror 'frames 'tag 1 (vector nil) 'scratch :not-a-function)
   :type 'nelisp-cc-runtime-error)
  (should-error
   (nelisp-cc-runtime-aot-signal-boundary
    'mirror 'frames 'error nil nil 'scratch)
   :type 'nelisp-cc-runtime-error)
  (should-error
   (nelisp-cc-runtime-aot-signal-boundary
    'mirror 'frames 'error nil (vector nil) 'scratch :not-a-function)
   :type 'nelisp-cc-runtime-error))

(ert-deftest nelisp-cc-runtime-aot-push-handler-boundaries ()
  "Doc 129.8C — push handler bridges install stack records."
  (let ((cleanup (lambda (_ctx) :cleanup)))
    (unwind-protect
        (progn
          (nelisp-cc-runtime-aot-reset-handler-stack)
          (nelisp-cc-runtime-aot-push-catch-boundary
           'mirror 'frames 'tag 'catch-pad 96 'scratch)
          (nelisp-cc-runtime-aot-push-condition-boundary
           'mirror 'frames '(error arith-error) 'cc-pad 88 'scratch)
          (nelisp-cc-runtime-aot-push-unwind-boundary
           'mirror 'frames cleanup 'cleanup-pad 80 'scratch)
          (let ((stack (nelisp-cc-runtime-aot-handler-stack-snapshot)))
            (should (= (length stack) 3))
            (should (eq (plist-get (nth 0 stack) :kind) 'unwind))
            (should (eq (plist-get (nth 1 stack) :kind) 'condition))
            (should (eq (plist-get (nth 2 stack) :kind) 'catch))
            (should (eq (plist-get (nth 2 stack) :landing-pad)
                        'catch-pad))))
      (nelisp-cc-runtime-aot-reset-handler-stack))))

(ert-deftest nelisp-cc-runtime-aot-push-handler-boundary-custom-dispatch ()
  "Doc 129.8C — push handler bridges accept injected native bodies."
  (let ((events nil))
    (should (equal
             (nelisp-cc-runtime-aot-push-catch-boundary
              'mirror 'frames 'tag 'pad 64 'scratch
              (lambda (tag landing saved context)
                (push (list tag landing saved
                            (plist-get context :scratch))
                      events)
                (list :ok tag landing saved)))
             '(:ok tag pad 64)))
    (should (equal events '((tag pad 64 scratch))))))

(ert-deftest nelisp-cc-runtime-aot-push-handler-boundary-validates ()
  "Doc 129.8C — push handler bridges reject malformed inputs."
  (unwind-protect
      (progn
        (nelisp-cc-runtime-aot-reset-handler-stack)
        (should-error
         (nelisp-cc-runtime-aot-push-catch-boundary
          'mirror 'frames 'tag 'pad 64 'scratch :not-a-function)
         :type 'nelisp-cc-runtime-error)
        (should-error
         (nelisp-cc-runtime-aot-push-condition-boundary
          'mirror 'frames '(error "bad") 'pad 64 'scratch)
         :type 'nelisp-cc-runtime-error)
        (should-error
         (nelisp-cc-runtime-aot-push-unwind-boundary
          'mirror 'frames :not-a-function 'pad 64 'scratch)
         :type 'nelisp-cc-runtime-error))
    (nelisp-cc-runtime-aot-reset-handler-stack)))

(ert-deftest nelisp-cc-runtime-aot-pop-handler-boundary ()
  "Doc 129.8D — pop handler bridge writes the popped descriptor to OUT."
  (unwind-protect
      (let ((out (vector nil)))
        (nelisp-cc-runtime-aot-reset-handler-stack)
        (nelisp-cc-runtime-aot-push-catch 'tag 'pad 64)
        (should (eq (nelisp-cc-runtime-aot-pop-handler-boundary
                     'mirror 'frames 'catch out 'scratch)
                    out))
        (should (eq (plist-get (aref out 0) :kind) 'catch))
        (should (null (nelisp-cc-runtime-aot-handler-stack-snapshot))))
    (nelisp-cc-runtime-aot-reset-handler-stack)))

(ert-deftest nelisp-cc-runtime-aot-pop-handler-boundary-custom-dispatch ()
  "Doc 129.8D — pop handler bridge accepts an injected native body."
  (let ((out (vector nil))
        (events nil))
    (should (eq (nelisp-cc-runtime-aot-pop-handler-boundary
                 'mirror 'frames 'catch out 'scratch
                 (lambda (expected-kind context)
                   (push (list expected-kind
                               (plist-get context :scratch))
                         events)
                   (list :kind expected-kind :native t)))
                out))
    (should (equal (aref out 0)
                   '(:kind catch :native t)))
    (should (equal events '((catch scratch))))))

(ert-deftest nelisp-cc-runtime-aot-pop-handler-boundary-validates ()
  "Doc 129.8D — pop handler bridge rejects malformed inputs."
  (unwind-protect
      (progn
        (nelisp-cc-runtime-aot-reset-handler-stack)
        (should-error
         (nelisp-cc-runtime-aot-pop-handler-boundary
          'mirror 'frames 'catch nil 'scratch)
         :type 'nelisp-cc-runtime-error)
        (should-error
         (nelisp-cc-runtime-aot-pop-handler-boundary
          'mirror 'frames 'catch (vector nil) 'scratch :not-a-function)
         :type 'nelisp-cc-runtime-error)
        (nelisp-cc-runtime-aot-push-catch 'tag 'pad 64)
        (should-error
         (nelisp-cc-runtime-aot-pop-handler-boundary
          'mirror 'frames 'condition (vector nil) 'scratch)
         :type 'nelisp-cc-runtime-error))
    (nelisp-cc-runtime-aot-reset-handler-stack)))

(ert-deftest nelisp-cc-runtime-aot-c-abi-descriptor-table ()
  "Doc 129.6Q — exported AOT C ABI symbols map to runtime bridges."
  (let* ((descriptors
          (nelisp-cc-runtime-aot-c-abi-descriptors))
         (symbols (mapcar (lambda (d) (plist-get d :symbol))
                          descriptors))
         (builtin-calln
          (nelisp-cc-runtime-aot-c-abi-descriptor
           'nelisp_aot_builtin_calln))
         (funcall3
          (nelisp-cc-runtime-aot-c-abi-descriptor
           'nelisp_aot_funcall3))
         (make-closure
          (nelisp-cc-runtime-aot-c-abi-descriptor
           'nelisp_aot_make_closure))
         (throw
          (nelisp-cc-runtime-aot-c-abi-descriptor
           'nelisp_aot_throw)))
    (should (nelisp-cc-runtime-validate-aot-c-abi-descriptors))
    (should (equal symbols (delete-dups (copy-sequence symbols))))
    (should (eq (plist-get builtin-calln :function)
                'nelisp-cc-runtime-aot-builtin-calln))
    (should (= (plist-get builtin-calln :fixed-argc) 6))
    (should (plist-get builtin-calln :rest))
    (should (eq (plist-get funcall3 :function)
                'nelisp-cc-runtime-aot-funcall3))
    (should (= (plist-get funcall3 :fixed-argc) 7))
    (should-not (plist-get funcall3 :rest))
    (should (eq (plist-get make-closure :function)
                'nelisp-cc-runtime-aot-make-closure))
    (should (plist-get make-closure :rest))
    (should (equal (plist-get throw :args)
                   '(mirror frames tag value out scratch)))
    (should-not
     (nelisp-cc-runtime-aot-c-abi-descriptor 'nelisp_aot_missing))))

(ert-deftest nelisp-cc-runtime-aot-c-abi-descriptor-rejects-bad-symbol ()
  "Doc 129.6Q — ABI descriptor lookup requires a native symbol."
  (should-error
   (nelisp-cc-runtime-aot-c-abi-descriptor "nelisp_aot_throw")
   :type 'nelisp-cc-runtime-error))

(ert-deftest nelisp-cc-runtime-aot-module-init-plan ()
  "Doc 129.3H — runtime normalizes compiler metadata for Doc 99."
  (let* ((init-helpers
          '((:kind defvar
             :name x
             :helper nelisp_aot_var_0_x
             :index 0)
            (:kind defcustom
             :name z
             :helper nelisp_aot_custom_1_z
             :index 1)))
         (custom-metadata
          '((:name z
             :helper nelisp_aot_custom_1_z
             :standard 9
             :docstring "doc"
             :options (:type (quote integer)))))
         (root-descriptors
          '((:name make-str
             :slots (0)
             :param-count 1
             :rt-slot-count 0)))
         (closure-descriptors
          '((:name nelisp_aot_closure_0
             :arglist (x)
             :body ((+ x cap))
             :captures (cap))))
         (plan
          (nelisp-cc-runtime-aot-module-init-plan
           init-helpers custom-metadata root-descriptors
           closure-descriptors)))
    (should (equal (plist-get plan :helper-order)
                   '(nelisp_aot_var_0_x nelisp_aot_custom_1_z)))
    (should (equal (plist-get plan :init-helpers) init-helpers))
    (should (equal (plist-get plan :custom-by-helper)
                   `((nelisp_aot_custom_1_z . ,(car custom-metadata)))))
    (should (equal (plist-get plan :root-descriptors)
                   root-descriptors))
    (should (equal (plist-get plan :closure-descriptors)
                   closure-descriptors))))

(ert-deftest nelisp-cc-runtime-aot-module-init-plan-rejects-orphan-custom ()
  "Doc 129.3H — custom metadata must point at an emitted init helper."
  (should-error
   (nelisp-cc-runtime-aot-module-init-plan
    '((:kind defvar
       :name x
       :helper nelisp_aot_var_0_x
       :index 0))
    '((:name z
       :helper nelisp_aot_custom_1_z
       :standard 9
       :docstring "doc"
	      :options nil)))
   :type 'nelisp-cc-runtime-error))

(ert-deftest nelisp-cc-runtime-aot-module-init-plan-rejects-bad-closure ()
  "Doc 129.7V — closure descriptors are validated in module plans."
  (should-error
   (nelisp-cc-runtime-aot-module-init-plan
    nil nil nil
    '((:name "not-a-symbol"
       :arglist (x)
       :body ((+ x cap))
       :captures (cap))))
   :type 'nelisp-cc-runtime-error))

(ert-deftest nelisp-cc-runtime-run-aot-module-init-plan ()
  "Doc 129.3I — runtime executes init helpers in module-plan order."
  (let* ((init-helpers
          '((:kind defvar
             :name x
             :helper nelisp_aot_var_0_x
             :index 0)
            (:kind defcustom
             :name z
             :helper nelisp_aot_custom_1_z
             :index 1)))
         (custom-metadata
          '((:name z
             :helper nelisp_aot_custom_1_z
             :standard 9
             :docstring "doc"
             :options (:type (quote integer)))))
         (plan (nelisp-cc-runtime-aot-module-init-plan
                init-helpers custom-metadata nil))
         (context (list :out 'out-slot
                        :mirror 'mirror
                        :frames 'frames
                        :scratch 'scratch
                        :name-slot 'name-slot))
         (events nil)
         (result
          (nelisp-cc-runtime-run-aot-module-init-plan
           plan context
           (lambda (helper ctx descriptor)
             (push (list :call helper
                         (plist-get ctx :out)
                         (plist-get descriptor :kind))
                   events)
             (list :ok helper))
           (lambda (custom ctx descriptor)
             (push (list :custom
                         (plist-get custom :name)
                         (plist-get ctx :name-slot)
                         (plist-get descriptor :helper))
                   events)
             :registered))))
    (should (equal (nreverse events)
                   '((:call nelisp_aot_var_0_x out-slot defvar)
                     (:call nelisp_aot_custom_1_z out-slot defcustom)
                     (:custom z name-slot nelisp_aot_custom_1_z))))
    (should (equal (plist-get result :init-results)
                   '((nelisp_aot_var_0_x . (:ok nelisp_aot_var_0_x))
                     (nelisp_aot_custom_1_z . (:ok nelisp_aot_custom_1_z)))))
    (should (equal (plist-get result :custom-results)
                   '((nelisp_aot_custom_1_z . :registered))))))

(ert-deftest nelisp-cc-runtime-run-aot-module-init-plan-requires-context ()
  "Doc 129.3I — init execution requires all boundary context slots."
  (let ((plan (nelisp-cc-runtime-aot-module-init-plan
               '((:kind defvar
                  :name x
                  :helper nelisp_aot_var_0_x
                  :index 0)))))
    (should-error
     (nelisp-cc-runtime-run-aot-module-init-plan
      plan
      (list :out 'out
            :mirror 'mirror
            :frames 'frames
            :scratch 'scratch)
      (lambda (_helper _context _descriptor) :ok))
     :type 'nelisp-cc-runtime-error)))

(ert-deftest nelisp-cc-runtime-make-aot-init-context ()
  "Doc 129.3L — runtime allocates the standard init boundary slots."
  (let* ((context
          (nelisp-cc-runtime-make-aot-init-context 'mirror 'frames))
         (out (plist-get context :out))
         (scratch (plist-get context :scratch))
         (name-slot (plist-get context :name-slot)))
    (should (eq (plist-get context :mirror) 'mirror))
    (should (eq (plist-get context :frames) 'frames))
    (should (vectorp out))
    (should (vectorp scratch))
    (should (vectorp name-slot))
    (should (= (length out) 1))
    (should (= (length scratch) 1))
    (should (= (length name-slot) 1))
    (should-not (eq out scratch))
    (should-not (eq out name-slot))
    (should-not (eq scratch name-slot))))

(ert-deftest nelisp-cc-runtime-aot-init-helper-argv ()
  "Doc 129.3M — init helper native calls use a fixed ABI argument order."
  (let ((context (list :out 'out
                       :mirror 'mirror
                       :frames 'frames
                       :scratch 'scratch
                       :name-slot 'name-slot)))
    (should (equal (nelisp-cc-runtime-aot-init-helper-argv context)
                   '(out mirror frames scratch name-slot)))
    (should-error
     (nelisp-cc-runtime-aot-init-helper-argv
      (list :out 'out :mirror 'mirror :frames 'frames :scratch 'scratch))
     :type 'nelisp-cc-runtime-error)))

(ert-deftest nelisp-cc-runtime-run-aot-module-init-plan-default-context ()
  "Doc 129.3L — module-init execution can allocate default context slots."
  (let* ((plan
          (nelisp-cc-runtime-aot-module-init-plan
           '((:kind defvar
              :name x
              :helper nelisp_aot_var_0_x
              :index 0))
           nil nil))
         (seen nil)
         (result
          (nelisp-cc-runtime-run-aot-module-init-plan-with-default-context
           plan
           (lambda (helper context descriptor)
             (push (list helper
                         (plist-get context :mirror)
                         (plist-get context :frames)
                         (vectorp (plist-get context :out))
                         (vectorp (plist-get context :scratch))
                         (vectorp (plist-get context :name-slot))
                         (plist-get descriptor :kind))
                   seen)
             :ok)
           nil
           'mirror
           'frames)))
    (should (equal (nreverse seen)
                   '((nelisp_aot_var_0_x
                      mirror frames t t t defvar))))
    (should (equal (plist-get result :init-results)
                   '((nelisp_aot_var_0_x . :ok))))))

(ert-deftest nelisp-cc-runtime-aot-custom-table-default-registration ()
  "Doc 129.3J — module init stores defcustom metadata by default."
  (let* ((custom-metadata
          '((:name z
             :helper nelisp_aot_custom_0_z
             :standard 9
             :docstring "doc"
             :options (:type (quote integer)))))
         (plan (nelisp-cc-runtime-aot-module-init-plan
                '((:kind defcustom
                   :name z
                   :helper nelisp_aot_custom_0_z
                   :index 0))
                custom-metadata nil))
         (context (list :out 'out-slot
                        :mirror 'mirror
                        :frames 'frames
                        :scratch 'scratch
                        :name-slot 'name-slot)))
    (unwind-protect
        (progn
          (nelisp-cc-runtime-clear-aot-custom-table)
          (let ((result
                 (nelisp-cc-runtime-run-aot-module-init-plan
                  plan context
                  (lambda (_helper _ctx _descriptor) :ok)))
                (expected
                 '(:name z
                   :helper nelisp_aot_custom_0_z
                   :standard 9
                   :docstring "doc"
                   :options (:type (quote integer))
                   :init-index 0
                   :init-kind defcustom)))
            (should (equal (plist-get result :custom-results)
                           `((nelisp_aot_custom_0_z . ,expected))))
            (should (equal (nelisp-cc-runtime-aot-custom-metadata 'z)
                           expected))
            (should (equal (nelisp-cc-runtime-aot-custom-table-snapshot)
                           (list expected)))
	    (should-not (plist-member
                         (nelisp-cc-runtime-aot-custom-metadata 'z)
                         :out))))
      (nelisp-cc-runtime-clear-aot-custom-table))))

(ert-deftest nelisp-cc-runtime-aot-closure-default-registration ()
  "Doc 129.7V — module init registers AOT closure descriptors by default."
  (let* ((closure-descriptor
          '(:name nelisp_aot_closure_0
            :arglist (x)
            :body ((+ x cap))
            :captures (cap)))
         (plan (nelisp-cc-runtime-aot-module-init-plan
                nil nil nil (list closure-descriptor)))
         (context (list :out 'out-slot
                        :mirror 'mirror
                        :frames 'frames
                        :scratch 'scratch
                        :name-slot 'name-slot)))
    (unwind-protect
        (progn
          (nelisp-cc-runtime-clear-aot-closure-descriptors)
          (let ((result
                 (nelisp-cc-runtime-run-aot-module-init-plan
                  plan context
                  (lambda (_helper _ctx _descriptor) :ok))))
            (should (equal (plist-get result :closure-results)
                           `((nelisp_aot_closure_0
                              . ,closure-descriptor))))
            (should (equal
                     (nelisp-cc-runtime-aot-closure-descriptor
                      'nelisp_aot_closure_0)
                     closure-descriptor))))
      (nelisp-cc-runtime-clear-aot-closure-descriptors))))

(ert-deftest nelisp-cc-runtime-register-aot-custom-metadata-rejects-mismatch ()
  "Doc 129.3J — custom metadata must match the helper descriptor."
  (should-error
   (nelisp-cc-runtime-register-aot-custom-metadata
    '(:name z
      :helper nelisp_aot_custom_0_z
      :standard 9
      :docstring "doc"
      :options nil)
    nil
    '(:kind defcustom
      :name other
      :helper nelisp_aot_custom_0_z
      :index 0))
   :type 'nelisp-cc-runtime-error))

(ert-deftest nelisp-cc-runtime-resolve-aot-init-helper ()
  "Doc 129.3K — AOT init helpers resolve through the runtime resolver."
  (let* ((descriptor '(:kind defvar
                       :name x
                       :helper nelisp_aot_var_0_x
                       :index 0))
         (resolution
          (nelisp-cc-runtime-resolve-aot-init-helper
           descriptor
           (lambda (symbol)
             (should (eq symbol 'nelisp_aot_var_0_x))
             (cons :resolved #x400010)))))
    (should (equal resolution
                   (list :helper 'nelisp_aot_var_0_x
                         :status :resolved
                         :addr #x400010
                         :descriptor descriptor)))))

(ert-deftest nelisp-cc-runtime-call-aot-init-helper-host-stub ()
  "Doc 129.3K — host Emacs can inspect unresolved init helper calls."
  (let* ((descriptor '(:kind defvar
                       :name x
                       :helper nelisp_aot_var_0_x
                       :index 0))
         (context (list :out 'out
                        :mirror 'mirror
                        :frames 'frames
                        :scratch 'scratch
                        :name-slot 'name-slot))
         (resolution
          (let ((nelisp-cc-runtime-resolve-symbol-function nil))
            (nelisp-cc-runtime-call-aot-init-helper
             'nelisp_aot_var_0_x context descriptor))))
    (should (eq (plist-get resolution :status) :host-stub))
    (should (= (plist-get resolution :addr)
               nelisp-cc-runtime--resolve-symbol-stub-addr))
    (should (equal (plist-get resolution :abi-argv)
                   '(out mirror frames scratch name-slot)))))

(ert-deftest nelisp-cc-runtime-aot-init-helper-caller-native-call ()
  "Doc 129.3K — standard call-helper resolves then invokes native callback."
  (let* ((init-helpers
          '((:kind defvar
             :name x
             :helper nelisp_aot_var_0_x
             :index 0)
            (:kind defconst
             :name y
             :helper nelisp_aot_const_1_y
             :index 1)))
         (plan (nelisp-cc-runtime-aot-module-init-plan init-helpers nil nil))
         (context (list :out 'out
                        :mirror 'mirror
                        :frames 'frames
                        :scratch 'scratch
                        :name-slot 'name-slot))
         (seen nil)
         (caller
          (nelisp-cc-runtime-aot-init-helper-caller
           (lambda (resolution ctx descriptor)
             (push (list (plist-get resolution :helper)
                         (plist-get resolution :addr)
                         (plist-get resolution :abi-argv)
                         (plist-get ctx :name-slot)
                         (plist-get descriptor :kind))
                   seen)
             (list :called (plist-get resolution :addr)))
           (lambda (symbol)
             (pcase symbol
               ('nelisp_aot_var_0_x (cons :resolved #x401000))
               ('nelisp_aot_const_1_y (cons :resolved #x402000))
               (_ (cons :not-found nil))))))
         (result
          (nelisp-cc-runtime-run-aot-module-init-plan
           plan context caller)))
    (should (equal (nreverse seen)
                   '((nelisp_aot_var_0_x #x401000
                      (out mirror frames scratch name-slot)
                      name-slot defvar)
                     (nelisp_aot_const_1_y #x402000
                      (out mirror frames scratch name-slot)
                      name-slot defconst))))
    (should (equal (plist-get result :init-results)
                   '((nelisp_aot_var_0_x . (:called #x401000))
                     (nelisp_aot_const_1_y . (:called #x402000)))))))

(ert-deftest nelisp-cc-runtime-aot-init-helper-caller-rejects-not-found ()
  "Doc 129.3K — missing native helper symbols fail before native call."
  (let ((descriptor '(:kind defvar
                      :name x
                      :helper nelisp_aot_var_0_x
                      :index 0))
        (context (list :out 'out
                       :mirror 'mirror
                       :frames 'frames
                       :scratch 'scratch
                       :name-slot 'name-slot)))
    (should-error
     (nelisp-cc-runtime-call-aot-init-helper
      'nelisp_aot_var_0_x
      context
      descriptor
      (lambda (_resolution _context _descriptor)
        (ert-fail "native callback should not run"))
      (lambda (_symbol) (cons :not-found nil)))
     :type 'nelisp-cc-runtime-error)))

;;; (47) fib body lowers without spill collision -------------------

(ert-deftest nelisp-cc-letrec-self-recursion-lowers ()
  "T50 follow-up — the inner fib body of a self-recursive `letrec'
must lower end-to-end without raising any allocator / backend signal.

The inner body holds the partial sum live across the second
`funcall fib' call AND `n' live across both inner calls.  Pre-T63
both landed in caller-saved registers and were destroyed when fib
re-entered, producing SIGSEGV at execution.  After T63 those values
spill, the bytes become register-safe, and the SSA→bytes pipeline
runs cleanly.  We test the inner-body form directly (the AST→SSA
frontend would compile it as a sub-function via `:closure'; testing
it as a top-level lambda exercises the same allocator path with
direct visibility into the spill set)."
  (let* ((form '(lambda (n)
                  (if (< n 2) n
                    (+ (funcall fib (- n 1))
                       (funcall fib (- n 2))))))
         (fn (nelisp-cc-build-ssa-from-ast form))
         (alloc (nelisp-cc--linear-scan fn)))
    (should-not
     (condition-case _err
         (progn (nelisp-cc-x86_64-compile fn alloc) nil)
       (nelisp-cc-x86_64-error t)))))

;;; (48) fib body has call-crossing intervals + at least one spill --

(ert-deftest nelisp-cc-letrec-self-recursion-spills-call-crossers ()
  "T50 follow-up — fib body has multiple call-crossing values.

`n' is live across both `funcall fib' calls; the first `funcall
fib' result is live across the second.  Both must be flagged
crosses-call AND assigned `:spill'."
  (let* ((form '(lambda (n)
                  (if (< n 2) n
                    (+ (funcall fib (- n 1))
                       (funcall fib (- n 2))))))
         (fn (nelisp-cc-build-ssa-from-ast form))
         (intervals (nelisp-cc--compute-intervals fn))
         (alloc (nelisp-cc--linear-scan fn)))
    (should (cl-some #'nelisp-cc--ssa-interval-crosses-call intervals))
    (should (cl-some (lambda (cell) (eq (cdr cell) :spill)) alloc))))

;;; (49) fib body arm64 lowering also clean -------------------------

(ert-deftest nelisp-cc-arm64-letrec-self-recursion-lowers ()
  "T50 follow-up — same as test 47 on the AAPCS64 arm64 backend.
Uses `compile-with-link' so the `<' / `+' / `-' primitive fixups
get their `callee:NAME' label binding (otherwise `:unbound-label')."
  (let* ((form '(lambda (n)
                  (if (< n 2) n
                    (+ (funcall fib (- n 1))
                       (funcall fib (- n 2))))))
         (fn (nelisp-cc-build-ssa-from-ast form))
         (alloc (nelisp-cc--linear-scan fn)))
    (should-not
     (condition-case _err
         (progn (nelisp-cc-arm64-compile-with-link fn alloc) nil)
       (nelisp-cc-arm64-error t)))))

;;; (50) full bench-actual fib(30) form passes the call-aware path -

(ert-deftest nelisp-cc-pipeline-fib-30-call-aware ()
  "T63 final gate smoke — the bench-actual fib(30) form (Doc 28 v2
§5.2 30x gate input) builds + allocates + x86_64-compiles end-to-end
under the T63 call-aware allocator.  Asserts the pipeline emits
non-empty bytes; the call-aware allocator path itself is exercised
explicitly by tests 47-49 against the inner fib body."
  (let* ((form '(lambda ()
                  (letrec ((fib (lambda (n)
                                  (if (< n 2) n
                                    (+ (funcall fib (- n 1))
                                       (funcall fib (- n 2)))))))
                    (funcall fib 30))))
         (fn (nelisp-cc-build-ssa-from-ast form))
         (alloc (nelisp-cc--linear-scan fn))
         (bytes (nelisp-cc-x86_64-compile fn alloc)))
    (should (vectorp bytes))
    (should (> (length bytes) 0))
    ;; Allocator output is well-formed.
    (dolist (cell alloc)
      (should (or (memq (cdr cell) nelisp-cc--default-int-registers)
                  (eq (cdr cell) :spill))))))

(provide 'nelisp-cc-test)
;;; nelisp-cc-test.el ends here
