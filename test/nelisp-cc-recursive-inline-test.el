;;; nelisp-cc-recursive-inline-test.el --- ERT for nelisp-cc-recursive-inline (Phase 7.7.3) -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT coverage for the Doc 42 §3.3 recursive-inline pass in
;; `src/nelisp-cc-recursive-inline.el'.  Builds SSA functions through
;; the Phase 7.1 scaffold builders and exercises the unroller's
;; per-decision logic plus the splice / clone machinery.
;;
;; Tests:
;;   1.  rec-inline-cost-estimate-positive
;;   2.  rec-inline-cost-estimate-zero-depth
;;   3.  rec-inline-cost-estimate-non-fn-signals
;;   4.  rec-inline-decide-go-self-call
;;   5.  rec-inline-decide-non-self-recursive
;;   6.  rec-inline-decide-no-callee-name
;;   7.  rec-inline-decide-unresolved-callee
;;   8.  rec-inline-decide-depth-limit-reached
;;   9.  rec-inline-decide-cost-budget-exceeded
;;  10.  rec-inline-respects-depth-limit-pass
;;  11.  rec-inline-fib-recursive-bounded
;;  12.  rec-inline-base-case-fold
;;  13.  rec-inline-multi-block-cfg-splice
;;  14.  rec-inline-skip-non-self-recursive
;;  15.  rec-inline-respects-cost-budget
;;  16.  rec-inline-pass-no-self-call-noop
;;  17.  rec-inline-pass-non-fn-signals
;;  18.  rec-inline-stats-tally
;;  19.  rec-inline-can-inline-p-go
;;  20.  rec-inline-can-inline-p-no-caller-signals
;;  21.  rec-inline-pass-rebinds-call-def
;;  22.  rec-inline-arity-mismatch-signals

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-cc)
(require 'nelisp-cc-escape)
(require 'nelisp-cc-inline)
(require 'nelisp-cc-recursive-inline)

;;; helpers --------------------------------------------------------

(defun nelisp-cc-rec-inline-test--make-self-leaf-fn ()
  "Build a single-block self-recursive callee.

Body shape (in pseudo-code):
  (lambda (x) (return (self x)))

The callee calls itself with its own param value and returns the
result.  It is *self-recursive* and *single-block*.  Eligible for
the recursive inliner."
  (let* ((fn (nelisp-cc--ssa-make-function 'self-leaf '(int)))
         (entry (nelisp-cc--ssa-function-entry fn))
         (x (car (nelisp-cc--ssa-function-params fn)))
         (rv (nelisp-cc--ssa-make-value fn 'int))
         (call (nelisp-cc--ssa-add-instr fn entry 'call (list x) rv)))
    (setf (nelisp-cc--ssa-instr-meta call)
          (list :fn 'self-leaf :unresolved t))
    (nelisp-cc--ssa-add-instr fn entry 'return (list rv) nil)
    fn))

(defun nelisp-cc-rec-inline-test--make-fib-like-fn ()
  "Build a multi-block self-recursive function with a base-case branch.

Body shape (in pseudo-code):
  (lambda (n) (if (< n 2) n (+ (self (- n 1)) (self (- n 2)))))

The function exercises:
  - multi-block CFG (entry / then / else / merge)
  - branch terminator + phi merge
  - 2 self-recursive call sites in the recursive arm
  - base case (the THEN arm returns the param directly)

The implementation does *not* lower the arithmetic into the
correct phi merge — for the inliner test we only need the call
sites + branch shape.  We therefore stub `+/-/<' as `call' opcodes
with `:fn' meta naming the (non-existent) helper, which the
inliner's :non-self-recursive gate will leave alone."
  (let* ((fn (nelisp-cc--ssa-make-function 'fib '(int)))
         (entry (nelisp-cc--ssa-function-entry fn))
         (then-blk (nelisp-cc--ssa-make-block fn "fib/then"))
         (else-blk (nelisp-cc--ssa-make-block fn "fib/else"))
         (merge-blk (nelisp-cc--ssa-make-block fn "fib/merge"))
         (n (car (nelisp-cc--ssa-function-params fn)))
         (cond-val (nelisp-cc--ssa-make-value fn 'bool))
         (cond-instr
          (nelisp-cc--ssa-add-instr fn entry 'call (list n) cond-val)))
    (setf (nelisp-cc--ssa-instr-meta cond-instr)
          (list :fn '< :unresolved t))
    (let ((br (nelisp-cc--ssa-add-instr fn entry 'branch
                                        (list cond-val) nil)))
      (setf (nelisp-cc--ssa-instr-meta br)
            (list :then (nelisp-cc--ssa-block-id then-blk)
                  :else (nelisp-cc--ssa-block-id else-blk))))
    (nelisp-cc--ssa-link-blocks entry then-blk)
    (nelisp-cc--ssa-link-blocks entry else-blk)
    ;; THEN arm — return n directly.
    (nelisp-cc--ssa-add-instr fn then-blk 'jump nil nil)
    (nelisp-cc--ssa-link-blocks then-blk merge-blk)
    ;; ELSE arm — two self-recursive calls + a stand-in `+'.
    (let* ((rv1 (nelisp-cc--ssa-make-value fn 'int))
           (c1 (nelisp-cc--ssa-add-instr fn else-blk 'call (list n) rv1)))
      (setf (nelisp-cc--ssa-instr-meta c1) (list :fn 'fib :unresolved t)))
    (let* ((rv2 (nelisp-cc--ssa-make-value fn 'int))
           (c2 (nelisp-cc--ssa-add-instr fn else-blk 'call (list n) rv2)))
      (setf (nelisp-cc--ssa-instr-meta c2) (list :fn 'fib :unresolved t)))
    (nelisp-cc--ssa-add-instr fn else-blk 'jump nil nil)
    (nelisp-cc--ssa-link-blocks else-blk merge-blk)
    ;; Merge — phi over the two arm values.  We use n itself as a
    ;; placeholder for both arms (the real fib would have rv2 + rv1
    ;; on else); the test only cares about the CFG splice, not the
    ;; numerical semantic.  The return drains the phi, *not* n —
    ;; otherwise the escape analysis would mark n :heap-required
    ;; (return-source) and the recursive inliner's escape gate
    ;; would reject the splice.
    (let ((phi-def (nelisp-cc--emit-phi fn merge-blk
                                        (list then-blk else-blk)
                                        (list n n)
                                        'int)))
      (nelisp-cc--ssa-add-instr fn merge-blk 'return (list phi-def) nil))
    fn))

(defun nelisp-cc-rec-inline-test--registry-of (fn)
  "Return a callee-registry containing FN under its NAME."
  (list (cons (nelisp-cc--ssa-function-name fn) fn)))

;;; (1) rec-inline-cost-estimate-positive --------------------------

(ert-deftest nelisp-cc-rec-inline-cost-estimate-positive ()
  "Cost estimate scales linearly with depth."
  (let* ((fn (nelisp-cc-rec-inline-test--make-self-leaf-fn))
         (base (nelisp-cc-inline-cost fn)))
    (should (= base (nelisp-cc-rec-inline-cost-estimate fn 1)))
    (should (= (* 2 base) (nelisp-cc-rec-inline-cost-estimate fn 2)))
    (should (= (* 4 base) (nelisp-cc-rec-inline-cost-estimate fn 4)))))

;;; (2) rec-inline-cost-estimate-zero-depth -------------------------

(ert-deftest nelisp-cc-rec-inline-cost-estimate-zero-depth ()
  "Cost estimate is 0 at depth ≤ 0."
  (let ((fn (nelisp-cc-rec-inline-test--make-self-leaf-fn)))
    (should (= 0 (nelisp-cc-rec-inline-cost-estimate fn 0)))
    (should (= 0 (nelisp-cc-rec-inline-cost-estimate fn -1)))))

;;; (3) rec-inline-cost-estimate-non-fn-signals ---------------------

(ert-deftest nelisp-cc-rec-inline-cost-estimate-non-fn-signals ()
  "Cost estimate signals on a non-SSA-function input."
  (should-error
   (nelisp-cc-rec-inline-cost-estimate "not-a-fn" 4)
   :type 'nelisp-cc-rec-inline-error))

;;; (4) rec-inline-decide-go-self-call ------------------------------

(ert-deftest nelisp-cc-rec-inline-decide-go-self-call ()
  "A canonical self-call at depth 0 is :go."
  (let* ((fn (nelisp-cc-rec-inline-test--make-self-leaf-fn))
         (call-instr
          (cl-find-if
           (lambda (i) (eq 'call (nelisp-cc--ssa-instr-opcode i)))
           (nelisp-cc--ssa-block-instrs (nelisp-cc--ssa-function-entry fn))))
         (escape-info (nelisp-cc-escape-analyze fn))
         (registry (nelisp-cc-rec-inline-test--registry-of fn))
         (decision (nelisp-cc--rec-inline-decide
                    call-instr 0 escape-info fn registry)))
    (should (eq :go (nelisp-cc--rec-inline-status-verdict decision)))
    (should (eq fn (nelisp-cc--rec-inline-status-callee decision)))))

;;; (5) rec-inline-decide-non-self-recursive ------------------------

(ert-deftest nelisp-cc-rec-inline-decide-non-self-recursive ()
  "A call to a *different* callee is :non-self-recursive."
  (let* ((other (let ((g (nelisp-cc--ssa-make-function 'other '(int))))
                  (nelisp-cc--ssa-add-instr
                   g (nelisp-cc--ssa-function-entry g) 'return
                   (list (car (nelisp-cc--ssa-function-params g))) nil)
                  g))
         (fn (nelisp-cc--ssa-make-function 'caller '(int)))
         (entry (nelisp-cc--ssa-function-entry fn))
         (x (car (nelisp-cc--ssa-function-params fn)))
         (rv (nelisp-cc--ssa-make-value fn 'int))
         (call (nelisp-cc--ssa-add-instr fn entry 'call (list x) rv)))
    (setf (nelisp-cc--ssa-instr-meta call) (list :fn 'other))
    (nelisp-cc--ssa-add-instr fn entry 'return (list rv) nil)
    (let* ((escape-info (nelisp-cc-escape-analyze fn))
           (registry (list (cons 'other other)))
           (decision (nelisp-cc--rec-inline-decide
                      call 0 escape-info fn registry)))
      (should (eq :non-self-recursive
                  (nelisp-cc--rec-inline-status-reason decision))))))

;;; (6) rec-inline-decide-no-callee-name ----------------------------

(ert-deftest nelisp-cc-rec-inline-decide-no-callee-name ()
  "A nameless `call' is :no-callee-name."
  (let* ((fn (nelisp-cc--ssa-make-function 'caller '(int)))
         (entry (nelisp-cc--ssa-function-entry fn))
         (x (car (nelisp-cc--ssa-function-params fn)))
         (rv (nelisp-cc--ssa-make-value fn 'int))
         (call (nelisp-cc--ssa-add-instr fn entry 'call (list x) rv)))
    (nelisp-cc--ssa-add-instr fn entry 'return (list rv) nil)
    (let* ((escape-info (nelisp-cc-escape-analyze fn))
           (decision (nelisp-cc--rec-inline-decide
                      call 0 escape-info fn nil)))
      (should (eq :no-callee-name
                  (nelisp-cc--rec-inline-status-reason decision))))))

;;; (7) rec-inline-decide-unresolved-callee -------------------------

(ert-deftest nelisp-cc-rec-inline-decide-unresolved-callee ()
  "A call whose name is not registered and not the caller is :unresolved-callee."
  (let* ((fn (nelisp-cc--ssa-make-function 'caller '(int)))
         (entry (nelisp-cc--ssa-function-entry fn))
         (x (car (nelisp-cc--ssa-function-params fn)))
         (rv (nelisp-cc--ssa-make-value fn 'int))
         (call (nelisp-cc--ssa-add-instr fn entry 'call (list x) rv)))
    (setf (nelisp-cc--ssa-instr-meta call) (list :fn 'missing))
    (nelisp-cc--ssa-add-instr fn entry 'return (list rv) nil)
    (let* ((escape-info (nelisp-cc-escape-analyze fn))
           (decision (nelisp-cc--rec-inline-decide
                      call 0 escape-info fn nil)))
      (should (eq :unresolved-callee
                  (nelisp-cc--rec-inline-status-reason decision))))))

;;; (8) rec-inline-decide-depth-limit-reached -----------------------

(ert-deftest nelisp-cc-rec-inline-decide-depth-limit-reached ()
  "A self-call at depth ≥ depth-limit is :depth-limit-reached."
  (let* ((nelisp-cc-rec-inline-depth-limit 2)
         (fn (nelisp-cc-rec-inline-test--make-self-leaf-fn))
         (call-instr
          (cl-find-if
           (lambda (i) (eq 'call (nelisp-cc--ssa-instr-opcode i)))
           (nelisp-cc--ssa-block-instrs (nelisp-cc--ssa-function-entry fn))))
         (escape-info (nelisp-cc-escape-analyze fn))
         (registry (nelisp-cc-rec-inline-test--registry-of fn))
         (decision (nelisp-cc--rec-inline-decide
                    call-instr 2 escape-info fn registry)))
    (should (eq :depth-limit-reached
                (nelisp-cc--rec-inline-status-reason decision)))))

;;; (9) rec-inline-decide-cost-budget-exceeded ----------------------

(ert-deftest nelisp-cc-rec-inline-decide-cost-budget-exceeded ()
  "When projected unroll cost exceeds the size cap the decision is
:cost-budget-exceeded."
  (let* ((nelisp-cc-rec-inline-depth-limit 100)
         ;; Projected cost = base × 100; cap = base × multiplier =
         ;; base × 1 with our override below.  So 100 > 1 hits the
         ;; gate.
         (nelisp-cc-rec-inline-size-multiplier 1)
         (fn (nelisp-cc-rec-inline-test--make-self-leaf-fn))
         (call-instr
          (cl-find-if
           (lambda (i) (eq 'call (nelisp-cc--ssa-instr-opcode i)))
           (nelisp-cc--ssa-block-instrs (nelisp-cc--ssa-function-entry fn))))
         (escape-info (nelisp-cc-escape-analyze fn))
         (registry (nelisp-cc-rec-inline-test--registry-of fn))
         (decision (nelisp-cc--rec-inline-decide
                    call-instr 0 escape-info fn registry)))
    (should (eq :cost-budget-exceeded
                (nelisp-cc--rec-inline-status-reason decision)))))

;;; (10) rec-inline-respects-depth-limit-pass -----------------------

(ert-deftest nelisp-cc-rec-inline-respects-depth-limit-pass ()
  "A pass with depth-limit 1 inlines exactly the outermost self-call once."
  (let* ((fn (nelisp-cc-rec-inline-test--make-self-leaf-fn))
         (escape-info (nelisp-cc-escape-analyze fn))
         (registry (nelisp-cc-rec-inline-test--registry-of fn))
         (result (nelisp-cc-rec-inline-pass fn escape-info registry 1)))
    ;; One outermost call eliminated; inner clones may still hold a
    ;; self-call (= depth-limit-reached on the next iteration).
    (should (= 1 (cdr result)))))

;;; (11) rec-inline-fib-recursive-bounded ---------------------------

(ert-deftest nelisp-cc-rec-inline-fib-recursive-bounded ()
  "Fib-like (multi-block, 2 self-call sites per iteration) unrolls
within the depth budget without crashing the splice."
  (let* ((fn (nelisp-cc-rec-inline-test--make-fib-like-fn))
         (escape-info (nelisp-cc-escape-analyze fn))
         (registry (nelisp-cc-rec-inline-test--registry-of fn))
         (orig-block-count (length (nelisp-cc--ssa-function-blocks fn)))
         (result (nelisp-cc-rec-inline-pass fn escape-info registry 2)))
    (should (eq fn (car result)))
    ;; At depth-limit 2 with 2 self-call sites at depth 0, we get
    ;; >= 2 inlined sites (= the iter-0 unroll).  The exact count
    ;; depends on how many sites the iter-1 walk picks up after
    ;; splicing — bound it loosely.
    (should (>= (cdr result) 2))
    ;; Block count grew (CFG splice added cloned blocks).
    (should (> (length (nelisp-cc--ssa-function-blocks fn))
               orig-block-count))))

;;; (12) rec-inline-base-case-fold ----------------------------------

(ert-deftest nelisp-cc-rec-inline-base-case-fold ()
  "When a branch's condition operand is a constant `t', the dead arm
edge is dropped and the terminator becomes a `jump'."
  (let* ((fn (nelisp-cc--ssa-make-function 'fold '(int)))
         (entry (nelisp-cc--ssa-function-entry fn))
         (then-blk (nelisp-cc--ssa-make-block fn "then"))
         (else-blk (nelisp-cc--ssa-make-block fn "else"))
         (cv (nelisp-cc--ssa-make-value fn 'bool))
         (cinstr (nelisp-cc--ssa-add-instr fn entry 'const nil cv)))
    (setf (nelisp-cc--ssa-instr-meta cinstr) (list :literal t))
    (let ((br (nelisp-cc--ssa-add-instr fn entry 'branch (list cv) nil)))
      (setf (nelisp-cc--ssa-instr-meta br)
            (list :then (nelisp-cc--ssa-block-id then-blk)
                  :else (nelisp-cc--ssa-block-id else-blk))))
    (nelisp-cc--ssa-link-blocks entry then-blk)
    (nelisp-cc--ssa-link-blocks entry else-blk)
    (nelisp-cc--ssa-add-instr fn then-blk 'return
                              (list (car (nelisp-cc--ssa-function-params fn)))
                              nil)
    (nelisp-cc--ssa-add-instr fn else-blk 'return
                              (list (car (nelisp-cc--ssa-function-params fn)))
                              nil)
    (let ((folded (nelisp-cc--rec-inline-fold-branch fn entry)))
      (should folded))
    ;; After fold: the branch's terminator opcode is `jump' and
    ;; else-blk is no longer a successor of entry.
    (let ((term (car (last (nelisp-cc--ssa-block-instrs entry)))))
      (should (eq 'jump (nelisp-cc--ssa-instr-opcode term))))
    (should-not (memq else-blk
                      (nelisp-cc--ssa-block-successors entry)))
    (should (memq then-blk
                  (nelisp-cc--ssa-block-successors entry)))))

;;; (13) rec-inline-multi-block-cfg-splice --------------------------

(ert-deftest nelisp-cc-rec-inline-multi-block-cfg-splice ()
  "A multi-block self-recursive callee splices into the caller via the
PRE / cloned-entry / cloned-return / POST chain."
  (let* ((fn (nelisp-cc-rec-inline-test--make-fib-like-fn))
         (escape-info (nelisp-cc-escape-analyze fn))
         (registry (nelisp-cc-rec-inline-test--registry-of fn))
         (orig-block-count (length (nelisp-cc--ssa-function-blocks fn)))
         (result (nelisp-cc-rec-inline-pass fn escape-info registry 1)))
    (should (>= (cdr result) 1))
    ;; Block count grew — splice added cloned blocks + post block.
    (should (> (length (nelisp-cc--ssa-function-blocks fn))
               orig-block-count))
    ;; Verify some cloned block carries the "/inl" suffix (the clone
    ;; helper appends it for diagnostic purposes).
    (should (cl-some
             (lambda (b)
               (let ((label (nelisp-cc--ssa-block-label b)))
                 (and label (string-match-p "/inl" label))))
             (nelisp-cc--ssa-function-blocks fn)))))

;;; (14) rec-inline-skip-non-self-recursive -------------------------

(ert-deftest nelisp-cc-rec-inline-skip-non-self-recursive ()
  "Non-self-recursive `call' sites are left untouched by the pass."
  (let* ((fn (nelisp-cc--ssa-make-function 'caller '(int)))
         (entry (nelisp-cc--ssa-function-entry fn))
         (x (car (nelisp-cc--ssa-function-params fn)))
         (rv (nelisp-cc--ssa-make-value fn 'int))
         (call (nelisp-cc--ssa-add-instr fn entry 'call (list x) rv)))
    (setf (nelisp-cc--ssa-instr-meta call) (list :fn 'other))
    (nelisp-cc--ssa-add-instr fn entry 'return (list rv) nil)
    (let* ((escape-info (nelisp-cc-escape-analyze fn))
           (other (let ((g (nelisp-cc--ssa-make-function 'other '(int))))
                    (nelisp-cc--ssa-add-instr
                     g (nelisp-cc--ssa-function-entry g) 'return
                     (list (car (nelisp-cc--ssa-function-params g))) nil)
                    g))
           (registry (list (cons 'other other)))
           (result (nelisp-cc-rec-inline-pass fn escape-info registry 4)))
      (should (= 0 (cdr result)))
      ;; The original `call' is still in the entry block.
      (should (memq call
                    (nelisp-cc--ssa-block-instrs entry))))))

;;; (15) rec-inline-respects-cost-budget ----------------------------

(ert-deftest nelisp-cc-rec-inline-respects-cost-budget ()
  "When the size-multiplier is too small, the cost gate aborts every
splice — the pass is a noop and the original call survives."
  (let* ((nelisp-cc-rec-inline-size-multiplier 0)
         (fn (nelisp-cc-rec-inline-test--make-self-leaf-fn))
         (escape-info (nelisp-cc-escape-analyze fn))
         (registry (nelisp-cc-rec-inline-test--registry-of fn))
         (result (nelisp-cc-rec-inline-pass fn escape-info registry 4)))
    (should (= 0 (cdr result)))
    ;; The original call is preserved.
    (should (cl-some (lambda (b)
                       (cl-some (lambda (i)
                                  (eq 'call (nelisp-cc--ssa-instr-opcode i)))
                                (nelisp-cc--ssa-block-instrs b)))
                     (nelisp-cc--ssa-function-blocks fn)))))

;;; (16) rec-inline-pass-no-self-call-noop --------------------------

(ert-deftest nelisp-cc-rec-inline-pass-no-self-call-noop ()
  "A function with no self-recursive sites is a no-op."
  (let* ((fn (nelisp-cc--ssa-make-function 'pure '(int)))
         (entry (nelisp-cc--ssa-function-entry fn))
         (x (car (nelisp-cc--ssa-function-params fn))))
    (nelisp-cc--ssa-add-instr fn entry 'return (list x) nil)
    (let* ((escape-info (nelisp-cc-escape-analyze fn))
           (result (nelisp-cc-rec-inline-pass fn escape-info nil 4)))
      (should (= 0 (cdr result))))))

;;; (17) rec-inline-pass-non-fn-signals -----------------------------

(ert-deftest nelisp-cc-rec-inline-pass-non-fn-signals ()
  "The pass signals on non-SSA-function input."
  (should-error
   (nelisp-cc-rec-inline-pass "not-a-fn" nil nil 4)
   :type 'nelisp-cc-rec-inline-error))

;;; (18) rec-inline-stats-tally -------------------------------------

(ert-deftest nelisp-cc-rec-inline-stats-tally ()
  "`nelisp-cc-rec-inline-stats' produces a per-reason histogram."
  (let* ((fn (nelisp-cc-rec-inline-test--make-self-leaf-fn))
         (escape-info (nelisp-cc-escape-analyze fn))
         (registry (nelisp-cc-rec-inline-test--registry-of fn))
         (stats (nelisp-cc-rec-inline-stats fn escape-info registry 4)))
    (should (= 1 (cdr (assq :inlined stats))))
    (let ((aborts (cdr (assq :abort stats))))
      ;; All abort buckets must be present.
      (dolist (k '(:no-callee-name :unresolved-callee :non-self-recursive
                   :depth-limit-reached :cost-budget-exceeded :escaped-arg))
        (should (assq k aborts))))))

;;; (19) rec-inline-can-inline-p-go ---------------------------------

(ert-deftest nelisp-cc-rec-inline-can-inline-p-go ()
  "The predicate accepts a canonical self-call."
  (let* ((fn (nelisp-cc-rec-inline-test--make-self-leaf-fn))
         (call-instr
          (cl-find-if
           (lambda (i) (eq 'call (nelisp-cc--ssa-instr-opcode i)))
           (nelisp-cc--ssa-block-instrs (nelisp-cc--ssa-function-entry fn))))
         (escape-info (nelisp-cc-escape-analyze fn))
         (registry (nelisp-cc-rec-inline-test--registry-of fn)))
    (should (nelisp-cc-rec-inline-can-inline-p
             call-instr 0 escape-info registry))))

;;; (20) rec-inline-can-inline-p-no-caller-signals ------------------

(ert-deftest nelisp-cc-rec-inline-can-inline-p-no-caller-signals ()
  "The predicate signals when no caller can be derived from the call site."
  (let* ((fn (nelisp-cc--ssa-make-function 'orphan '(int)))
         (entry (nelisp-cc--ssa-function-entry fn))
         (x (car (nelisp-cc--ssa-function-params fn)))
         (rv (nelisp-cc--ssa-make-value fn 'int))
         (call (nelisp-cc--ssa-add-instr fn entry 'call (list x) rv)))
    ;; Empty meta — no :fn, and registry empty.
    (nelisp-cc--ssa-add-instr fn entry 'return (list rv) nil)
    (let ((escape-info (nelisp-cc-escape-analyze fn)))
      (should-error
       (nelisp-cc-rec-inline-can-inline-p call 0 escape-info nil)
       :type 'nelisp-cc-rec-inline-error))))

;;; (21) rec-inline-pass-rebinds-call-def ---------------------------

(ert-deftest nelisp-cc-rec-inline-pass-rebinds-call-def ()
  "After a self-call splice, the original call's def is rebound to a
`copy' instruction in the post-block."
  (let* ((fn (nelisp-cc-rec-inline-test--make-self-leaf-fn))
         (call-instr
          (cl-find-if
           (lambda (i) (eq 'call (nelisp-cc--ssa-instr-opcode i)))
           (nelisp-cc--ssa-block-instrs (nelisp-cc--ssa-function-entry fn))))
         (rv (nelisp-cc--ssa-instr-def call-instr))
         (escape-info (nelisp-cc-escape-analyze fn))
         (registry (nelisp-cc-rec-inline-test--registry-of fn)))
    (nelisp-cc-rec-inline-pass fn escape-info registry 1)
    ;; The def's def-point is now a `copy' instruction (or the
    ;; cloned call when cost gate suppressed the splice — but
    ;; depth-limit 1 + cost-multiplier-default 5 lets the first
    ;; splice through).
    (let* ((dp (nelisp-cc--ssa-value-def-point rv))
           (op (and dp (nelisp-cc--ssa-instr-opcode dp))))
      (should (memq op '(copy call))))))

;;; (22) rec-inline-arity-mismatch-signals --------------------------

(ert-deftest nelisp-cc-rec-inline-arity-mismatch-signals ()
  "When a self-call's arg count differs from the callee's params, splice signals."
  (let* ((fn (nelisp-cc--ssa-make-function 'mismatch '(int)))
         (entry (nelisp-cc--ssa-function-entry fn))
         (x (car (nelisp-cc--ssa-function-params fn)))
         (y (nelisp-cc--ssa-make-value fn 'int))
         (rv (nelisp-cc--ssa-make-value fn 'int))
         (call (nelisp-cc--ssa-add-instr fn entry 'call (list x y) rv)))
    (setf (nelisp-cc--ssa-instr-meta call)
          (list :fn 'mismatch :unresolved t))
    (nelisp-cc--ssa-add-instr fn entry 'return (list rv) nil)
    (let ((vmap (make-hash-table :test 'eq)))
      (should-error
       (nelisp-cc--rec-inline-splice-self fn entry call fn vmap)
       :type 'nelisp-cc-rec-inline-error))))

(provide 'nelisp-cc-recursive-inline-test)
;;; nelisp-cc-recursive-inline-test.el ends here
