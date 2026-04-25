;;; nelisp-cc-escape-test.el --- ERT for nelisp-cc-escape (Phase 7.7.1) -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT coverage for the Doc 42 §3.1 escape-analysis prototype in
;; `src/nelisp-cc-escape.el'.  The harness drives the SSA scaffold
;; from `src/nelisp-cc.el' directly (no frontend lowering needed) so
;; the tests stay focused on the analysis lattice and the source-set
;; recognition logic.
;;
;; Tests:
;;   1.  pure-local value never escapes
;;   2.  value flowing into `return' escapes (:returned)
;;   3.  value captured by `closure' escapes (:captured)
;;   4.  value passed to opaque `call' escapes (:passed)
;;   5.  value passed to `call-indirect' escapes (:passed)
;;   6.  value `store-var'-d escapes (:stored)
;;   7.  transitive propagation through `copy' op
;;   8.  transitive propagation through `phi' node
;;   9.  parameter that flows directly into `return' escapes
;;  10.  `nelisp-cc-escape-info-can-stack-allocate-p' refuses
;;       :heap-required values
;;  11.  `nelisp-cc-escape-info-can-inline-p' refuses :heap-required
;;       values
;;  12.  conservative flag toggle changes the :unknown verdict
;;  13.  `nelisp-cc-escape-info-stats' tallies all 3 classes
;;  14.  the analysis is non-mutating w.r.t. the input function
;;  15.  multiple escape sources joined into one value record one reason

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-cc)
(require 'nelisp-cc-escape)

;;; helpers --------------------------------------------------------

(defun nelisp-cc-escape-test--make-pure-local-fn ()
  "Build `(lambda (x) (let ((y (add x x))) y))` in raw SSA.
Returns the function.  Y is a pure local with no escape source."
  (let* ((fn (nelisp-cc--ssa-make-function 'pure-local '(int)))
         (entry (nelisp-cc--ssa-function-entry fn))
         (x (car (nelisp-cc--ssa-function-params fn)))
         (y (nelisp-cc--ssa-make-value fn 'int)))
    (nelisp-cc--ssa-add-instr fn entry 'add (list x x) y)
    ;; No return — purposely leave Y dangling so the only "use" is the
    ;; add itself; that means Y has no escape source and should be
    ;; verdicted :stack-only.
    (cl-values fn x y)))

(defun nelisp-cc-escape-test--make-return-fn ()
  "Build `(lambda (x) x)` — X flows directly into return."
  (let* ((fn (nelisp-cc--ssa-make-function 'returner '(int)))
         (entry (nelisp-cc--ssa-function-entry fn))
         (x (car (nelisp-cc--ssa-function-params fn))))
    (nelisp-cc--ssa-add-instr fn entry 'return (list x) nil)
    (cl-values fn x)))

;;; (1) pure local --------------------------------------------------

(ert-deftest nelisp-cc-escape-pure-local-no-escape ()
  "A value with no escape-source use stays :stack-only."
  (cl-multiple-value-bind (fn _x y)
      (nelisp-cc-escape-test--make-pure-local-fn)
    (let ((info (nelisp-cc-escape-analyze fn)))
      (should (eq :stack-only
                  (nelisp-cc-escape-info-class-of y info)))
      (should (nelisp-cc-escape-info-can-stack-allocate-p y info))
      (should (nelisp-cc-escape-info-can-inline-p y info)))))

;;; (2) return escapes ---------------------------------------------

(ert-deftest nelisp-cc-escape-returned-value-escapes ()
  "An operand of `return' is verdicted :heap-required with reason :returned."
  (let* ((fn (nelisp-cc--ssa-make-function 'returns-sum '(int)))
         (entry (nelisp-cc--ssa-function-entry fn))
         (x (car (nelisp-cc--ssa-function-params fn)))
         (sum (nelisp-cc--ssa-make-value fn 'int)))
    (nelisp-cc--ssa-add-instr fn entry 'add (list x x) sum)
    (nelisp-cc--ssa-add-instr fn entry 'return (list sum) nil)
    (let ((info (nelisp-cc-escape-analyze fn)))
      (should (eq :heap-required
                  (nelisp-cc-escape-info-class-of sum info)))
      (should (eq :returned
                  (nelisp-cc-escape-info-reason-of sum info)))
      (should-not (nelisp-cc-escape-info-can-stack-allocate-p sum info)))))

;;; (3) closure capture -------------------------------------------

(ert-deftest nelisp-cc-escape-captured-by-closure-escapes ()
  "An operand of `closure' is verdicted :heap-required with reason :captured."
  (let* ((fn (nelisp-cc--ssa-make-function 'capturer '(int)))
         (entry (nelisp-cc--ssa-function-entry fn))
         (x (car (nelisp-cc--ssa-function-params fn)))
         (clo (nelisp-cc--ssa-make-value fn 'closure)))
    (nelisp-cc--ssa-add-instr fn entry 'closure (list x) clo)
    (nelisp-cc--ssa-add-instr fn entry 'return (list clo) nil)
    (let ((info (nelisp-cc-escape-analyze fn)))
      (should (eq :heap-required
                  (nelisp-cc-escape-info-class-of x info)))
      (should (eq :captured
                  (nelisp-cc-escape-info-reason-of x info))))))

;;; (4) opaque call escape ---------------------------------------

(ert-deftest nelisp-cc-escape-passed-to-opaque-function-escapes ()
  "An operand of `call' is verdicted :heap-required with reason :passed."
  (let* ((fn (nelisp-cc--ssa-make-function 'caller '(int)))
         (entry (nelisp-cc--ssa-function-entry fn))
         (x (car (nelisp-cc--ssa-function-params fn)))
         (rv (nelisp-cc--ssa-make-value fn 'int)))
    (nelisp-cc--ssa-add-instr fn entry 'call (list x) rv)
    (nelisp-cc--ssa-add-instr fn entry 'return (list rv) nil)
    (let ((info (nelisp-cc-escape-analyze fn)))
      (should (eq :heap-required
                  (nelisp-cc-escape-info-class-of x info)))
      (should (eq :passed
                  (nelisp-cc-escape-info-reason-of x info))))))

;;; (5) call-indirect escape -------------------------------------

(ert-deftest nelisp-cc-escape-call-indirect-escapes ()
  "An operand of `call-indirect' is also verdicted :heap-required."
  (let* ((fn (nelisp-cc--ssa-make-function 'icaller '(int int)))
         (entry (nelisp-cc--ssa-function-entry fn))
         (params (nelisp-cc--ssa-function-params fn))
         (callee (car params))
         (arg    (cadr params))
         (rv (nelisp-cc--ssa-make-value fn 'int)))
    (nelisp-cc--ssa-add-instr fn entry 'call-indirect (list callee arg) rv)
    (nelisp-cc--ssa-add-instr fn entry 'return (list rv) nil)
    (let ((info (nelisp-cc-escape-analyze fn)))
      (should (eq :heap-required
                  (nelisp-cc-escape-info-class-of arg info)))
      (should (eq :heap-required
                  (nelisp-cc-escape-info-class-of callee info))))))

;;; (6) store-var escape ----------------------------------------

(ert-deftest nelisp-cc-escape-stored-in-cell-escapes ()
  "An operand of `store-var' is verdicted :heap-required with reason :stored."
  (let* ((fn (nelisp-cc--ssa-make-function 'mutator '(int)))
         (entry (nelisp-cc--ssa-function-entry fn))
         (x (car (nelisp-cc--ssa-function-params fn))))
    ;; store-var takes the value being written; nelisp-cc emits it as
    ;; (store-var VALUE) with no def.  See nelisp-cc.el:1407.
    (nelisp-cc--ssa-add-instr fn entry 'store-var (list x) nil)
    (let ((info (nelisp-cc-escape-analyze fn)))
      (should (eq :heap-required
                  (nelisp-cc-escape-info-class-of x info)))
      (should (eq :stored
                  (nelisp-cc-escape-info-reason-of x info))))))

;;; (7) transitive through copy --------------------------------

(ert-deftest nelisp-cc-escape-transitive-through-copy ()
  "A value reached only via a `copy' chain still escapes when its sink does."
  (let* ((fn (nelisp-cc--ssa-make-function 'copy-chain '(int)))
         (entry (nelisp-cc--ssa-function-entry fn))
         (x (car (nelisp-cc--ssa-function-params fn)))
         (mid (nelisp-cc--ssa-make-value fn 'int)))
    (nelisp-cc--ssa-add-instr fn entry 'copy (list x) mid)
    (nelisp-cc--ssa-add-instr fn entry 'return (list mid) nil)
    (let ((info (nelisp-cc-escape-analyze fn)))
      ;; mid is the direct return operand → escape.
      (should (eq :heap-required (nelisp-cc-escape-info-class-of mid info)))
      ;; x defines mid (via copy) → also escapes by propagation.
      (should (eq :heap-required (nelisp-cc-escape-info-class-of x info))))))

;;; (8) transitive through phi --------------------------------

(ert-deftest nelisp-cc-escape-transitive-through-phi ()
  "A phi join whose result escapes propagates to all its operands."
  (let* ((fn (nelisp-cc--ssa-make-function 'phi-join '(int int)))
         (entry (nelisp-cc--ssa-function-entry fn))
         (a (car (nelisp-cc--ssa-function-params fn)))
         (b (cadr (nelisp-cc--ssa-function-params fn)))
         (joined (nelisp-cc--ssa-make-value fn 'int)))
    (nelisp-cc--ssa-add-instr fn entry 'phi (list a b) joined)
    (nelisp-cc--ssa-add-instr fn entry 'return (list joined) nil)
    (let ((info (nelisp-cc-escape-analyze fn)))
      (should (eq :heap-required (nelisp-cc-escape-info-class-of joined info)))
      (should (eq :heap-required (nelisp-cc-escape-info-class-of a info)))
      (should (eq :heap-required (nelisp-cc-escape-info-class-of b info))))))

;;; (9) param flowing directly into return -----------------

(ert-deftest nelisp-cc-escape-param-flows-into-return ()
  "A parameter that is the only operand of `return' escapes too."
  (cl-multiple-value-bind (fn x)
      (nelisp-cc-escape-test--make-return-fn)
    (let ((info (nelisp-cc-escape-analyze fn)))
      (should (eq :heap-required (nelisp-cc-escape-info-class-of x info)))
      (should (eq :returned (nelisp-cc-escape-info-reason-of x info))))))

;;; (10) can-stack-allocate-p refuses heap -----------------

(ert-deftest nelisp-cc-escape-can-stack-allocate-p-refuses-heap ()
  "`can-stack-allocate-p' is nil for :heap-required values."
  (let* ((fn (nelisp-cc--ssa-make-function 'returns-x '(int)))
         (entry (nelisp-cc--ssa-function-entry fn))
         (x (car (nelisp-cc--ssa-function-params fn))))
    (nelisp-cc--ssa-add-instr fn entry 'return (list x) nil)
    (let ((info (nelisp-cc-escape-analyze fn)))
      (should-not (nelisp-cc-escape-info-can-stack-allocate-p x info)))))

;;; (11) can-inline-p refuses heap ------------------------

(ert-deftest nelisp-cc-escape-can-inline-p-refuses-heap ()
  "`can-inline-p' is nil for :heap-required values."
  (let* ((fn (nelisp-cc--ssa-make-function 'returns-x2 '(int)))
         (entry (nelisp-cc--ssa-function-entry fn))
         (x (car (nelisp-cc--ssa-function-params fn))))
    (nelisp-cc--ssa-add-instr fn entry 'return (list x) nil)
    (let ((info (nelisp-cc-escape-analyze fn)))
      (should-not (nelisp-cc-escape-info-can-inline-p x info)))))

;;; (12) conservative flag for :unknown -------------------

(ert-deftest nelisp-cc-escape-conservative-flag-affects-unknown ()
  "Toggling `nelisp-cc-escape-conservative' flips the :unknown gate."
  ;; Build an SSA where one operand's def-point is foreign — that
  ;; triggers the :unknown branch in
  ;; `nelisp-cc--escape-mark-unknown-operands'.
  (let* ((fn (nelisp-cc--ssa-make-function 'with-foreign '(int)))
         (entry (nelisp-cc--ssa-function-entry fn))
         (x (car (nelisp-cc--ssa-function-params fn)))
         ;; A value whose def-point is set to an instruction that
         ;; *does not* belong to FN — simulates a dangling reference.
         (foreign-val (nelisp-cc--ssa-value-make :id 9999 :type 'int))
         (foreign-instr (nelisp-cc--ssa-instr-make
                         :id 9999 :opcode 'add
                         :operands nil :def foreign-val))
         (sum (nelisp-cc--ssa-make-value fn 'int)))
    (setf (nelisp-cc--ssa-value-def-point foreign-val) foreign-instr)
    ;; Use foreign-val as an operand of an in-FN instruction.
    (nelisp-cc--ssa-add-instr fn entry 'add (list x foreign-val) sum)
    (let ((info (nelisp-cc-escape-analyze fn)))
      (should (eq :unknown
                  (nelisp-cc-escape-info-class-of foreign-val info)))
      ;; Conservative t (default) → can-stack-allocate-p says no.
      (let ((nelisp-cc-escape-conservative t))
        (should-not (nelisp-cc-escape-info-can-stack-allocate-p
                     foreign-val info)))
      ;; Conservative nil → :unknown is accepted.
      (let ((nelisp-cc-escape-conservative nil))
        (should (nelisp-cc-escape-info-can-stack-allocate-p
                 foreign-val info))))))

;;; (13) stats ----------------------------------------

(ert-deftest nelisp-cc-escape-info-stats-counts-classes ()
  "`nelisp-cc-escape-info-stats' tallies every class."
  (let* ((fn (nelisp-cc--ssa-make-function 'mixed '(int int)))
         (entry (nelisp-cc--ssa-function-entry fn))
         (a (car (nelisp-cc--ssa-function-params fn)))
         (b (cadr (nelisp-cc--ssa-function-params fn)))
         (sum (nelisp-cc--ssa-make-value fn 'int)))
    (nelisp-cc--ssa-add-instr fn entry 'add (list a b) sum)
    (nelisp-cc--ssa-add-instr fn entry 'return (list sum) nil)
    (let* ((info (nelisp-cc-escape-analyze fn))
           (stats (nelisp-cc-escape-info-stats info)))
      ;; sum, a, b all escape via transitive return propagation.
      (should (= 3 (cdr (assq :heap-required stats))))
      (should (= 0 (cdr (assq :stack-only stats))))
      (should (= 0 (cdr (assq :unknown stats)))))))

;;; (14) non-mutating ---------------------------------

(ert-deftest nelisp-cc-escape-analyze-is-non-mutating ()
  "`nelisp-cc-escape-analyze' must not mutate the input SSA function."
  (let* ((fn (nelisp-cc--ssa-make-function 'observe '(int)))
         (entry (nelisp-cc--ssa-function-entry fn))
         (x (car (nelisp-cc--ssa-function-params fn))))
    (nelisp-cc--ssa-add-instr fn entry 'return (list x) nil)
    ;; Snapshot the externally-visible state before the analysis.
    (let* ((blocks-before (length (nelisp-cc--ssa-function-blocks fn)))
           (instrs-before (length (nelisp-cc--ssa-block-instrs entry)))
           (next-vid-before (nelisp-cc--ssa-function-next-value-id fn))
           (next-iid-before (nelisp-cc--ssa-function-next-instr-id fn))
           (next-bid-before (nelisp-cc--ssa-function-next-block-id fn))
           (use-list-before (length (nelisp-cc--ssa-value-use-list x))))
      (nelisp-cc-escape-analyze fn)
      (should (= blocks-before
                 (length (nelisp-cc--ssa-function-blocks fn))))
      (should (= instrs-before
                 (length (nelisp-cc--ssa-block-instrs entry))))
      (should (= next-vid-before
                 (nelisp-cc--ssa-function-next-value-id fn)))
      (should (= next-iid-before
                 (nelisp-cc--ssa-function-next-instr-id fn)))
      (should (= next-bid-before
                 (nelisp-cc--ssa-function-next-block-id fn)))
      (should (= use-list-before
                 (length (nelisp-cc--ssa-value-use-list x))))
      ;; Verifier still passes — strongest non-mutation evidence.
      (should (eq t (nelisp-cc--ssa-verify-function fn))))))

;;; (15) reason-first-wins for joined sources ----------

(ert-deftest nelisp-cc-escape-reason-first-wins ()
  "When a value escapes through multiple sources only the first reason sticks."
  (let* ((fn (nelisp-cc--ssa-make-function 'multi-sink '(int)))
         (entry (nelisp-cc--ssa-function-entry fn))
         (x (car (nelisp-cc--ssa-function-params fn)))
         (clo (nelisp-cc--ssa-make-value fn 'closure)))
    ;; First sink: closure capture.  Then return both clo and x — x
    ;; reaches *two* escape sources (closure + transitive return).
    (nelisp-cc--ssa-add-instr fn entry 'closure (list x) clo)
    (nelisp-cc--ssa-add-instr fn entry 'return (list clo) nil)
    (let ((info (nelisp-cc-escape-analyze fn)))
      (should (eq :heap-required (nelisp-cc-escape-info-class-of x info)))
      ;; The first sink we walk in pass 1 is `closure' (block-instr
      ;; order), so the recorded reason for X is :captured.  This
      ;; pins the documented "first-wins" reason policy.
      (should (eq :captured (nelisp-cc-escape-info-reason-of x info))))))

(provide 'nelisp-cc-escape-test)
;;; nelisp-cc-escape-test.el ends here
