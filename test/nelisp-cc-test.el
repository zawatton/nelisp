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
(require 'nelisp-cc)

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
  ;; `while' is also out of MVP scope (loop linearisation deferred).
  (should-error
   (nelisp-cc-build-ssa-from-ast
    '(lambda () (while t)))
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
  "End-to-end: build-ssa-from-ast → compute-intervals → linear-scan."
  (let* ((fn (nelisp-cc-build-ssa-from-ast
              '(lambda (x y z) (+ x (* y z)))))
         (intervals (nelisp-cc--compute-intervals fn))
         (assignments (nelisp-cc--linear-scan fn))
         (spilled (nelisp-cc--alloc-spilled-values assignments)))
    ;; Verify the SSA function itself.
    (should (eq t (nelisp-cc--ssa-verify-function fn)))
    ;; Every interval must correspond to a defined value.
    (dolist (iv intervals)
      (let ((v (nelisp-cc--ssa-interval-value iv)))
        (should v)
        (should (or (eq :param (nelisp-cc--ssa-value-def-point v))
                    (nelisp-cc--ssa-value-def-point v)))))
    ;; With three params + two `:call' results = 5 intervals; 8-reg
    ;; pool fits all without spill.
    (should (null spilled))
    ;; Every assignment maps to a register from the default pool.
    (dolist (cell assignments)
      (should (memq (cdr cell) nelisp-cc--default-int-registers)))))

(provide 'nelisp-cc-test)
;;; nelisp-cc-test.el ends here
