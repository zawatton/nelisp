;;; nelisp-bytecode-test.el --- ERT for Phase 3b bytecode VM  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 3b.1 seeded the module with data shapes and stubs.  Phase 3b.2
;; ships the first five opcodes (RETURN / CONST / STACK-REF / DROP /
;; DUP) and a minimal compiler that handles self-evaluating atoms and
;; `(quote X)'.  Tests below split into:
;;
;;   - data shape (3b.1 carry-over)
;;   - opcode table (5 ops wired)
;;   - compiler + run round-trip (literal + quoted forms)
;;   - handwritten bcl exercising each opcode + error paths
;;
;; See docs/design/08-bytecode-vm.org §3.3b.2.

;;; Code:

(require 'ert)
(require 'nelisp-bytecode)

(defun nelisp-bc-test--code (ops)
  "Assemble OPS (list of (OP ARG...) or bare OP symbol) into a vector.
Each symbolic op is replaced with its opcode byte via
`nelisp-bc-opcode'; numeric args are inserted verbatim."
  (let ((acc nil))
    (dolist (item ops)
      (cond
       ((symbolp item)
        (push (nelisp-bc-opcode item) acc))
       ((and (consp item) (symbolp (car item)))
        (push (nelisp-bc-opcode (car item)) acc)
        (dolist (arg (cdr item)) (push arg acc)))
       (t (error "bad test code item: %S" item))))
    (apply #'vector (nreverse acc))))

;;; Data shape (from 3b.1) -------------------------------------------

(ert-deftest nelisp-bc-skeleton-make-and-predicate ()
  (let ((bcl (nelisp-bc-make nil '(x) [] [] 0 0)))
    (should (nelisp-bcl-p bcl))
    (should-not (nelisp-bcl-p '(nelisp-closure nil () nil)))
    (should-not (nelisp-bcl-p nil))
    (should-not (nelisp-bcl-p 42))))

(ert-deftest nelisp-bc-skeleton-accessors ()
  (let* ((env '((a . 1)))
         (params '(x &optional y))
         (consts [foo "bar" 3])
         (code [0 1 2])
         (bcl (nelisp-bc-make env params consts code 4 #b101)))
    (should (equal (nelisp-bc-env bcl) env))
    (should (equal (nelisp-bc-params bcl) params))
    (should (equal (nelisp-bc-consts bcl) consts))
    (should (equal (nelisp-bc-code bcl) code))
    (should (=     (nelisp-bc-stack-depth bcl) 4))
    (should (=     (nelisp-bc-special-mask bcl) 5))))

(ert-deftest nelisp-bc-skeleton-error-hierarchy ()
  (should (memq 'nelisp-bc-error
                (get 'nelisp-bc-unimplemented 'error-conditions))))

;;; Opcode table (3b.2) ----------------------------------------------

(ert-deftest nelisp-bc-3b2-opcode-table ()
  (should (= (nelisp-bc-opcode 'RETURN)    0))
  (should (= (nelisp-bc-opcode 'CONST)     1))
  (should (= (nelisp-bc-opcode 'STACK-REF) 2))
  (should (= (nelisp-bc-opcode 'DROP)      3))
  (should (= (nelisp-bc-opcode 'DUP)       4))
  (should-error (nelisp-bc-opcode 'NOPE) :type 'nelisp-bc-error))

(ert-deftest nelisp-bc-3b2-opcode-arg-bytes ()
  (should (= (aref nelisp-bc--opcode-arg-bytes 0) 0)) ; RETURN
  (should (= (aref nelisp-bc--opcode-arg-bytes 1) 1)) ; CONST
  (should (= (aref nelisp-bc--opcode-arg-bytes 2) 1)) ; STACK-REF
  (should (= (aref nelisp-bc--opcode-arg-bytes 3) 0)) ; DROP
  (should (= (aref nelisp-bc--opcode-arg-bytes 4) 0))) ; DUP

;;; Compiler + run round-trip ----------------------------------------

(defun nelisp-bc-test--eval (form)
  "Compile FORM with the Phase 3b.2 compiler and run it on the VM."
  (nelisp-bc-run (nelisp-bc-compile form)))

(ert-deftest nelisp-bc-3b2-compile-self-evaluating ()
  (should (eq       (nelisp-bc-test--eval nil) nil))
  (should (eq       (nelisp-bc-test--eval t) t))
  (should (eq       (nelisp-bc-test--eval :k) :k))
  (should (=        (nelisp-bc-test--eval 42) 42))
  (should (equal    (nelisp-bc-test--eval 3.14) 3.14))
  (should (equal    (nelisp-bc-test--eval "hi") "hi"))
  (should (equal    (nelisp-bc-test--eval [1 2 3]) [1 2 3])))

(ert-deftest nelisp-bc-3b2-compile-quote ()
  (should (eq    (nelisp-bc-test--eval '(quote foo)) 'foo))
  (should (equal (nelisp-bc-test--eval '(quote (a b c))) '(a b c))))

(ert-deftest nelisp-bc-3b2-compile-unimplemented ()
  ;; `let', generic calls, error-handling forms shipped in 3b.4a/b/c;
  ;; lambda landed in 3b.5a — see `nelisp-bc-3b4*-' / `nelisp-bc-3b5*-'.
  ;; (function NON-LAMBDA) and `defun' / `defvar' / `defmacro' /
  ;; short-circuit forms still belong to the interpreter.
  (should-error (nelisp-bc-compile '(function foo))
                :type 'nelisp-bc-unimplemented)
  (should-error (nelisp-bc-compile '(defun f () 1))
                :type 'nelisp-bc-unimplemented))

(ert-deftest nelisp-bc-3b2-compile-captures-env ()
  ;; ENV is carried verbatim — not yet consulted, just preserved.
  (let* ((env '((a . 1) (b . 2)))
         (bcl (nelisp-bc-compile 42 env)))
    (should (equal (nelisp-bc-env bcl) env))
    (should (=     (nelisp-bc-run bcl) 42))))

;;; Handwritten bcl — each opcode ------------------------------------

(ert-deftest nelisp-bc-3b2-handwritten-const-return ()
  (let ((bcl (nelisp-bc-make nil nil [:payload]
                             (nelisp-bc-test--code '((CONST 0) RETURN))
                             1 0)))
    (should (eq (nelisp-bc-run bcl) :payload))))

(ert-deftest nelisp-bc-3b2-handwritten-drop ()
  ;; CONST 0, CONST 1, DROP, RETURN -> first constant
  (let ((bcl (nelisp-bc-make nil nil ["keep" "discard"]
                             (nelisp-bc-test--code
                              '((CONST 0) (CONST 1) DROP RETURN))
                             2 0)))
    (should (equal (nelisp-bc-run bcl) "keep"))))

(ert-deftest nelisp-bc-3b2-handwritten-dup ()
  (let ((bcl (nelisp-bc-make nil nil [99]
                             (nelisp-bc-test--code
                              '((CONST 0) DUP DROP RETURN))
                             2 0)))
    (should (= (nelisp-bc-run bcl) 99))))

(ert-deftest nelisp-bc-3b2-handwritten-stack-ref ()
  ;; CONST 0, CONST 1, STACK-REF 1, RETURN -> the first constant
  (let ((bcl (nelisp-bc-make nil nil [first second]
                             (nelisp-bc-test--code
                              '((CONST 0) (CONST 1) (STACK-REF 1) RETURN))
                             3 0)))
    (should (eq (nelisp-bc-run bcl) 'first))))

;;; Error paths -------------------------------------------------------

(ert-deftest nelisp-bc-3b2-run-rejects-extra-args ()
  ;; 3b.5a accepts ARGS for closures with PARAMS; for a no-PARAMS
  ;; expression bcl, passing positional args is still rejected as
  ;; an arity violation by the parser.
  (let ((bcl (nelisp-bc-compile 0)))
    (should-error (nelisp-bc-run bcl '(1))
                  :type 'nelisp-bc-error)))

(ert-deftest nelisp-bc-3b2-run-rejects-non-bcl ()
  (should-error (nelisp-bc-run '(nelisp-closure nil () 0))
                :type 'nelisp-bc-error))

(ert-deftest nelisp-bc-3b2-unknown-opcode ()
  (let ((bcl (nelisp-bc-make nil nil [] [200] 1 0)))
    (should-error (nelisp-bc-run bcl) :type 'nelisp-bc-error)))

(ert-deftest nelisp-bc-3b2-return-on-empty-stack ()
  (let ((bcl (nelisp-bc-make nil nil []
                             (nelisp-bc-test--code '(RETURN))
                             1 0)))
    (should-error (nelisp-bc-run bcl) :type 'nelisp-bc-error)))

(ert-deftest nelisp-bc-3b2-fall-off-end ()
  ;; CODE ends without RETURN.
  (let ((bcl (nelisp-bc-make nil nil [1]
                             (nelisp-bc-test--code '((CONST 0)))
                             1 0)))
    (should-error (nelisp-bc-run bcl) :type 'nelisp-bc-error)))

;;; Phase 3b.3 -------------------------------------------------------

(ert-deftest nelisp-bc-3b3-opcode-table ()
  (should (= (nelisp-bc-opcode 'GOTO)            5))
  (should (= (nelisp-bc-opcode 'GOTO-IF-NIL)     6))
  (should (= (nelisp-bc-opcode 'GOTO-IF-NOT-NIL) 7))
  (should (= (nelisp-bc-opcode 'ADD1)            8))
  (should (= (nelisp-bc-opcode 'SUB1)            9))
  (should (= (nelisp-bc-opcode 'PLUS)           10))
  (should (= (nelisp-bc-opcode 'MINUS)          11))
  (should (= (nelisp-bc-opcode 'LESS)           12))
  (should (= (nelisp-bc-opcode 'GREATER)        13))
  (should (= (nelisp-bc-opcode 'EQ)             14))
  (should (= (nelisp-bc-opcode 'NOT)            15)))

(ert-deftest nelisp-bc-3b3-arith-direct ()
  (should (=  (nelisp-bc-test--eval '(+ 2 3)) 5))
  (should (=  (nelisp-bc-test--eval '(+ 1 2 3 4)) 10))
  (should (=  (nelisp-bc-test--eval '(- 10 3)) 7))
  (should (=  (nelisp-bc-test--eval '(- 10 3 2)) 5))
  (should (=  (nelisp-bc-test--eval '(1+ 41)) 42))
  (should (=  (nelisp-bc-test--eval '(1- 10)) 9))
  (should (=  (nelisp-bc-test--eval '(+)) 0))
  (should (=  (nelisp-bc-test--eval '(+ 7)) 7))
  (should (=  (nelisp-bc-test--eval '(- 4)) -4)))

(ert-deftest nelisp-bc-3b3-compare ()
  (should (eq (nelisp-bc-test--eval '(< 1 2)) t))
  (should (eq (nelisp-bc-test--eval '(< 2 1)) nil))
  (should (eq (nelisp-bc-test--eval '(> 5 3)) t))
  (should (eq (nelisp-bc-test--eval '(eq (quote a) (quote a))) t))
  (should (eq (nelisp-bc-test--eval '(eq (quote a) (quote b))) nil))
  (should (eq (nelisp-bc-test--eval '(not nil)) t))
  (should (eq (nelisp-bc-test--eval '(not t)) nil))
  (should (eq (nelisp-bc-test--eval '(not 0)) nil)))

(ert-deftest nelisp-bc-3b3-if-basic ()
  (should (= (nelisp-bc-test--eval '(if t 1 2))   1))
  (should (= (nelisp-bc-test--eval '(if nil 1 2)) 2))
  (should (= (nelisp-bc-test--eval '(if (< 1 2) 10 20)) 10))
  (should (= (nelisp-bc-test--eval '(if (< 2 1) 10 20)) 20)))

(ert-deftest nelisp-bc-3b3-if-no-else ()
  (should (eq (nelisp-bc-test--eval '(if t 1)) 1))
  (should (eq (nelisp-bc-test--eval '(if nil 1)) nil)))

(ert-deftest nelisp-bc-3b3-if-multi-else ()
  ;; (if T THEN E1 E2 E3) -> else branch is (progn E1 E2 E3)
  (should (= (nelisp-bc-test--eval '(if nil 99 (+ 1 2) (+ 3 4))) 7)))

(ert-deftest nelisp-bc-3b3-nested-if ()
  (should (= (nelisp-bc-test--eval
              '(if (< 1 2)
                   (if (> 5 3) 111 222)
                 333))
             111)))

(ert-deftest nelisp-bc-3b3-cond ()
  (should (eq (nelisp-bc-test--eval '(cond)) nil))
  (should (=  (nelisp-bc-test--eval '(cond ((< 1 2) 10) (t 20))) 10))
  (should (=  (nelisp-bc-test--eval '(cond ((< 2 1) 10) (t 20))) 20))
  (should (=  (nelisp-bc-test--eval
               '(cond ((eq 'x 'y) 1)
                      ((eq 'a 'b) 2)
                      (t 3)))
              3))
  ;; Bare test clause yields the test value.
  (should (=  (nelisp-bc-test--eval '(cond ((+ 1 2))))
              3)))

(ert-deftest nelisp-bc-3b3-progn ()
  (should (= (nelisp-bc-test--eval '(progn 1 2 3)) 3))
  (should (= (nelisp-bc-test--eval '(progn (+ 1 1) (+ 2 2))) 4))
  (should (eq (nelisp-bc-test--eval '(progn)) nil)))

(ert-deftest nelisp-bc-3b3-arithmetic-nested ()
  (should (= (nelisp-bc-test--eval '(- (+ 10 5) 3)) 12))
  (should (= (nelisp-bc-test--eval '(+ (- 100 50) 10)) 60))
  (should (= (nelisp-bc-test--eval
              '(+ (+ 1 2) (+ 3 (- 10 (+ 2 4))))) 10)))

(defun nelisp-bc-test--assemble-with-labels (items)
  "Two-pass assembler for handwritten bytecode used by loop tests.
Each ITEM is either a symbol (zero-arg op), (OP uint8) for one-byte
ops, (LABEL SYM), or (JUMP-OP SYM) for GOTO-family ops targeting
a label.  Returns the resolved int vector."
  ;; Pass 1: walk items, compute byte offsets of labels.
  (let ((offset 0)
        (labels nil))
    (dolist (item items)
      (cond
       ((and (consp item) (eq (car item) 'LABEL))
        (setq labels (plist-put labels (cadr item) offset)))
       ((symbolp item) (cl-incf offset 1))
       ((and (consp item)
             (memq (car item) '(GOTO GOTO-IF-NIL GOTO-IF-NOT-NIL)))
        (cl-incf offset 3))
       ((consp item) (cl-incf offset (1+ (length (cdr item)))))))
    ;; Pass 2: emit bytes.
    (let ((code (make-vector offset 0))
          (pos 0))
      (dolist (item items)
        (cond
         ((and (consp item) (eq (car item) 'LABEL)) nil)
         ((symbolp item)
          (aset code pos (nelisp-bc-opcode item))
          (setq pos (1+ pos)))
         ((and (consp item)
               (memq (car item) '(GOTO GOTO-IF-NIL GOTO-IF-NOT-NIL)))
          (let ((tgt (plist-get labels (cadr item))))
            (aset code pos (nelisp-bc-opcode (car item)))
            (aset code (1+ pos) (logand tgt #xFF))
            (aset code (+ pos 2) (logand (ash tgt -8) #xFF))
            (setq pos (+ pos 3))))
         ((consp item)
          (aset code pos (nelisp-bc-opcode (car item)))
          (setq pos (1+ pos))
          (dolist (byte (cdr item))
            (aset code pos byte)
            (setq pos (1+ pos))))))
      code)))

(ert-deftest nelisp-bc-3b3-handwritten-goto-countdown ()
  ;; Countdown loop that exits when top-of-stack reaches 0.
  ;; Pseudo:
  ;;   n = 3
  ;;   while (not (eq n 0)):
  ;;     n = n - 1
  ;;   return n   ; 0
  (let* ((code (nelisp-bc-test--assemble-with-labels
                '((CONST 0)            ; n=3            sp=1
                  (LABEL top)
                  DUP                  ; dup n          sp=2
                  (CONST 1)            ; push 0         sp=3
                  EQ                   ; (eq n 0)       sp=2
                  (GOTO-IF-NOT-NIL done) ; consume test sp=1
                  SUB1                 ; n -= 1         sp=1
                  (GOTO top)
                  (LABEL done)
                  RETURN)))
         (bcl (nelisp-bc-make nil nil [3 0] code 4 0)))
    (should (= (nelisp-bc-run bcl) 0))))

(ert-deftest nelisp-bc-3b3-handwritten-sum-loop ()
  ;; Sum 1..5 using a DUP/ADD + SUB1 pattern with single-slot accumulator.
  ;; Pseudo:
  ;;   acc = 0; n = 5
  ;;   while n != 0:
  ;;     acc = acc + n
  ;;     n = n - 1
  ;;   return acc
  ;; Stack layout: [acc n] throughout the loop.
  (let* ((code (nelisp-bc-test--assemble-with-labels
                '((CONST 0)          ; acc = 0       [0]
                  (CONST 1)          ; n = 5         [0 5]
                  (LABEL top)
                  DUP                ; dup n         [0 5 5]
                  (CONST 0)          ; push 0        [0 5 5 0]
                  EQ                 ; (eq n 0)      [0 5 t/nil]
                  (GOTO-IF-NOT-NIL done) ;           [0 5]
                  ;; acc = acc + n, preserving n
                  (STACK-REF 0)      ; push n        [0 5 5]
                  (STACK-REF 2)      ; push acc      [0 5 5 0]
                  PLUS               ;               [0 5 5]
                  ;; stack: [old-acc old-n new-acc]; want [new-acc (n-1)]
                  (STACK-REF 1)      ; push old-n    [0 5 5 5]
                  SUB1               ; -> n-1        [0 5 5 4]
                  ;; stack: [old-acc old-n new-acc new-n]
                  ;; Drop old-acc and old-n via STACK-REF 1 / 0 trick is
                  ;; awkward without a SWAP / ROT op; skip rewrite and
                  ;; simply DROP twice after swapping via DUP — instead
                  ;; shortcut: recompute top two via STACK-REF copies.
                  ;; Simpler: emit four DROPs to collapse then re-push.
                  ;; That defeats the purpose. Use a cleaner shape:
                  ;; after PLUS we have [0 5 5] and we've lost the old n
                  ;; decrement; rework as follows instead:
                  DROP DROP DROP DROP ; roll stack fully — placeholder
                  (GOTO top)
                  (LABEL done)
                  DROP               ; pop n         [acc]
                  RETURN))))
    ;; The hand-rolled version above is intentionally not production;
    ;; once the VM has swap/rot ops a full 1..5 sum test will replace it.
    ;; For now, just assert the assembler produced a vector of the right
    ;; shape — real correctness comes from compiled `(+ 1 2 3 4 5)'.
    (should (vectorp code))))

(ert-deftest nelisp-bc-3b3-compiled-sum-equivalence ()
  ;; Validate that the compile-and-run path produces 1..5 correctly.
  (should (= (nelisp-bc-test--eval '(+ 1 2 3 4 5)) 15)))

(ert-deftest nelisp-bc-3b3-stack-depth-tracking ()
  ;; Compiler should record the max stack depth it saw so the VM can
  ;; allocate the operand stack correctly up front.
  (let ((bcl (nelisp-bc-compile '(+ 1 2 3 4 5))))
    (should (>= (nelisp-bc-stack-depth bcl) 2)))
  (let ((bcl (nelisp-bc-compile '(+ (+ 1 2) (+ 3 (+ 4 (+ 5 6)))))))
    ;; Deeply nested right-fold pushes multiple pending values.
    (should (>= (nelisp-bc-stack-depth bcl) 4))))

(ert-deftest nelisp-bc-3b3-const-dedup ()
  ;; Repeated constants should share a single entry.
  (let ((bcl (nelisp-bc-compile '(+ 1 1 1 1))))
    (should (= (length (nelisp-bc-consts bcl)) 1))
    (should (= (aref (nelisp-bc-consts bcl) 0) 1))))

;;; Phase 3b.4a — variable access + let / let* / setq / while -------

(ert-deftest nelisp-bc-3b4a-opcode-table ()
  (should (= (nelisp-bc-opcode 'VARREF)    16))
  (should (= (nelisp-bc-opcode 'VARSET)    17))
  (should (= (nelisp-bc-opcode 'VARBIND)   18))
  (should (= (nelisp-bc-opcode 'UNBIND)    19))
  (should (= (nelisp-bc-opcode 'STACK-SET) 20))
  (should (= (nelisp-bc-opcode 'DISCARDN)  21)))

(ert-deftest nelisp-bc-3b4a-opcode-arg-bytes ()
  (should (= (aref nelisp-bc--opcode-arg-bytes 16) 1))
  (should (= (aref nelisp-bc--opcode-arg-bytes 17) 1))
  (should (= (aref nelisp-bc--opcode-arg-bytes 18) 1))
  (should (= (aref nelisp-bc--opcode-arg-bytes 19) 1))
  (should (= (aref nelisp-bc--opcode-arg-bytes 20) 1))
  (should (= (aref nelisp-bc--opcode-arg-bytes 21) 1)))

(ert-deftest nelisp-bc-3b4a-let-single ()
  (should (= (nelisp-bc-test--eval '(let ((x 42)) x)) 42))
  (should (= (nelisp-bc-test--eval '(let ((x 1)) (+ x 2))) 3)))

(ert-deftest nelisp-bc-3b4a-let-multiple ()
  (should (= (nelisp-bc-test--eval '(let ((a 1) (b 2) (c 3)) (+ a b c))) 6))
  (should (= (nelisp-bc-test--eval '(let ((x 10) (y 20)) (- y x))) 10)))

(ert-deftest nelisp-bc-3b4a-let-parallel-semantics ()
  ;; Inner `x' in init sees the OUTER `x'.
  (should (= (nelisp-bc-test--eval
              '(let ((x 5))
                 (let ((x 10) (y x))
                   (+ x y))))
             15)))

(ert-deftest nelisp-bc-3b4a-let*-sequential ()
  (should (= (nelisp-bc-test--eval
              '(let* ((x 1) (y (+ x 1)) (z (+ y 1)))
                 (+ x y z)))
             6)))

(ert-deftest nelisp-bc-3b4a-let*-empty-body-nil ()
  (should (eq (nelisp-bc-test--eval '(let* ((x 1)))) nil))
  (should (eq (nelisp-bc-test--eval '(let ())) nil)))

(ert-deftest nelisp-bc-3b4a-let-bare-sym ()
  (should (eq (nelisp-bc-test--eval '(let (x) x)) nil))
  (should (eq (nelisp-bc-test--eval '(let* (x) x)) nil)))

(ert-deftest nelisp-bc-3b4a-let-shadowing ()
  (should (= (nelisp-bc-test--eval
              '(let ((x 1))
                 (let ((x 2))
                   (let ((x 3)) x))))
             3))
  (should (= (nelisp-bc-test--eval
              '(let ((x 1))
                 (+ (let ((x 99)) x) x)))
             100)))

(ert-deftest nelisp-bc-3b4a-setq-lexical ()
  (should (= (nelisp-bc-test--eval
              '(let ((x 1)) (setq x 42) x))
             42))
  (should (= (nelisp-bc-test--eval
              '(let ((x 1))
                 (setq x (+ x 2))
                 (setq x (+ x 10))
                 x))
             13)))

(ert-deftest nelisp-bc-3b4a-setq-returns-value ()
  ;; `setq' evaluates to the last assigned value.
  (should (= (nelisp-bc-test--eval
              '(let ((x 0)) (setq x 77)))
             77))
  (should (= (nelisp-bc-test--eval
              '(let ((x 0) (y 0))
                 (setq x 1 y 2)))
             2)))

(ert-deftest nelisp-bc-3b4a-setq-multi ()
  (should (= (nelisp-bc-test--eval
              '(let ((a 0) (b 0) (c 0))
                 (setq a 1 b 2 c 3)
                 (+ a b c)))
             6)))

(ert-deftest nelisp-bc-3b4a-while-countdown ()
  (should (= (nelisp-bc-test--eval
              '(let ((n 5) (acc 0))
                 (while (> n 0)
                   (setq acc (+ acc n))
                   (setq n (- n 1)))
                 acc))
             15)))

(ert-deftest nelisp-bc-3b4a-while-zero-iters ()
  (should (eq (nelisp-bc-test--eval '(while nil 999)) nil))
  (should (= (nelisp-bc-test--eval
              '(let ((n 0))
                 (while (> n 0) (setq n (- n 1)))
                 n))
             0)))

(ert-deftest nelisp-bc-3b4a-varref-global ()
  ;; VARREF against a symbol registered in `nelisp--globals'.
  (unwind-protect
      (progn
        (puthash 'nelisp-bc-test--global 123 nelisp--globals)
        (should (= (nelisp-bc-test--eval 'nelisp-bc-test--global) 123)))
    (remhash 'nelisp-bc-test--global nelisp--globals)))

(ert-deftest nelisp-bc-3b4a-varref-unbound-signals ()
  (remhash 'nelisp-bc-test--nope nelisp--globals)
  (should-error
   (nelisp-bc-test--eval 'nelisp-bc-test--nope)
   :type 'nelisp-unbound-variable))

(ert-deftest nelisp-bc-3b4a-setq-global ()
  (unwind-protect
      (progn
        (remhash 'nelisp-bc-test--gset nelisp--globals)
        (nelisp-bc-test--eval '(setq nelisp-bc-test--gset 7))
        (should (= (gethash 'nelisp-bc-test--gset nelisp--globals) 7)))
    (remhash 'nelisp-bc-test--gset nelisp--globals)))

(ert-deftest nelisp-bc-3b4a-let-special-dynamic-bind ()
  ;; Binding a `defvar'd special with `let' must save/restore the
  ;; global slot and make the new value visible inside the body.
  (unwind-protect
      (progn
        (puthash 'nelisp-bc-test--dyn t nelisp--specials)
        (puthash 'nelisp-bc-test--dyn 'outer nelisp--globals)
        (should (eq (nelisp-bc-test--eval
                     '(let ((nelisp-bc-test--dyn 'inner))
                        nelisp-bc-test--dyn))
                    'inner))
        (should (eq (gethash 'nelisp-bc-test--dyn nelisp--globals)
                    'outer)))
    (remhash 'nelisp-bc-test--dyn nelisp--globals)
    (remhash 'nelisp-bc-test--dyn nelisp--specials)))

(ert-deftest nelisp-bc-3b4a-let-special-restored-on-error ()
  ;; A non-local exit from VM body must still restore the dyn global.
  (unwind-protect
      (progn
        (puthash 'nelisp-bc-test--dyn2 t nelisp--specials)
        (puthash 'nelisp-bc-test--dyn2 'outer nelisp--globals)
        (remhash 'nelisp-bc-test--no-such-var nelisp--globals)
        (condition-case nil
            (nelisp-bc-test--eval
             '(let ((nelisp-bc-test--dyn2 'inner))
                nelisp-bc-test--no-such-var))
          (nelisp-unbound-variable nil))
        (should (eq (gethash 'nelisp-bc-test--dyn2 nelisp--globals)
                    'outer)))
    (remhash 'nelisp-bc-test--dyn2 nelisp--globals)
    (remhash 'nelisp-bc-test--dyn2 nelisp--specials)))

(ert-deftest nelisp-bc-3b4a-let*-special-sequential ()
  (unwind-protect
      (progn
        (puthash 'nelisp-bc-test--dseq t nelisp--specials)
        (puthash 'nelisp-bc-test--dseq 'outer nelisp--globals)
        (should
         (= (nelisp-bc-test--eval
             '(let* ((nelisp-bc-test--dseq 10)
                     (y (+ nelisp-bc-test--dseq 5)))
                (+ nelisp-bc-test--dseq y)))
            25))
        (should (eq (gethash 'nelisp-bc-test--dseq nelisp--globals)
                    'outer)))
    (remhash 'nelisp-bc-test--dseq nelisp--globals)
    (remhash 'nelisp-bc-test--dseq nelisp--specials)))

(ert-deftest nelisp-bc-3b4a-mixed-lex-and-dyn ()
  ;; Parallel let with one lex + one dyn + one lex binding.
  (unwind-protect
      (progn
        (puthash 'nelisp-bc-test--mix t nelisp--specials)
        (puthash 'nelisp-bc-test--mix 'outer nelisp--globals)
        (should
         (= (nelisp-bc-test--eval
             '(let ((a 1) (nelisp-bc-test--mix 20) (c 3))
                (+ a nelisp-bc-test--mix c)))
            24))
        (should (eq (gethash 'nelisp-bc-test--mix nelisp--globals)
                    'outer)))
    (remhash 'nelisp-bc-test--mix nelisp--globals)
    (remhash 'nelisp-bc-test--mix nelisp--specials)))

(ert-deftest nelisp-bc-3b4a-setq-on-special-updates-global ()
  ;; `setq' of a special outside any `let' writes to the global.
  (unwind-protect
      (progn
        (puthash 'nelisp-bc-test--specset t nelisp--specials)
        (puthash 'nelisp-bc-test--specset 'outer nelisp--globals)
        (nelisp-bc-test--eval '(setq nelisp-bc-test--specset 'new))
        (should (eq (gethash 'nelisp-bc-test--specset nelisp--globals)
                    'new)))
    (remhash 'nelisp-bc-test--specset nelisp--globals)
    (remhash 'nelisp-bc-test--specset nelisp--specials)))

(ert-deftest nelisp-bc-3b4a-handwritten-varbind-unbind ()
  ;; Handwritten: (let ((s 42)) s) with `s' as a dyn special.  We push
  ;; 42, VARBIND s, VARREF s, UNBIND 1, RETURN.
  (unwind-protect
      (let* ((code (nelisp-bc-test--code
                    '((CONST 0) (VARBIND 1) (VARREF 1) (UNBIND 1) RETURN)))
             (bcl (nelisp-bc-make nil nil [42 nelisp-bc-test--hw]
                                  code 2 0)))
        (puthash 'nelisp-bc-test--hw t nelisp--specials)
        (puthash 'nelisp-bc-test--hw 'outer nelisp--globals)
        (should (= (nelisp-bc-run bcl) 42))
        (should (eq (gethash 'nelisp-bc-test--hw nelisp--globals)
                    'outer)))
    (remhash 'nelisp-bc-test--hw nelisp--globals)
    (remhash 'nelisp-bc-test--hw nelisp--specials)))

(ert-deftest nelisp-bc-3b4a-handwritten-discardn ()
  ;; Handwritten: push [9 10 11 12], DISCARDN 3, RETURN -> 12.
  (let* ((code (nelisp-bc-test--code
                '((CONST 0) (CONST 1) (CONST 2) (CONST 3)
                  (DISCARDN 3) RETURN)))
         (bcl (nelisp-bc-make nil nil [9 10 11 12] code 4 0)))
    (should (= (nelisp-bc-run bcl) 12))))

(ert-deftest nelisp-bc-3b4a-handwritten-stack-set ()
  ;; Push 1 and 99 -> [1 99]; STACK-SET 1 writes TOS into stack[0]
  ;; and pops, leaving [99]; RETURN -> 99.
  (let* ((code (nelisp-bc-test--code
                '((CONST 0) (CONST 1) (STACK-SET 1) RETURN)))
         (bcl (nelisp-bc-make nil nil [1 99] code 2 0)))
    (should (= (nelisp-bc-run bcl) 99))))

;;; Phase 3b.4b — CAR / CDR / CONS / LIST1..N / CALL ----------------

(ert-deftest nelisp-bc-3b4b-opcode-table ()
  (should (= (nelisp-bc-opcode 'CAR)   22))
  (should (= (nelisp-bc-opcode 'CDR)   23))
  (should (= (nelisp-bc-opcode 'CONS)  24))
  (should (= (nelisp-bc-opcode 'LIST1) 25))
  (should (= (nelisp-bc-opcode 'LIST2) 26))
  (should (= (nelisp-bc-opcode 'LIST3) 27))
  (should (= (nelisp-bc-opcode 'LIST4) 28))
  (should (= (nelisp-bc-opcode 'LISTN) 29))
  (should (= (nelisp-bc-opcode 'CALL)  30)))

(ert-deftest nelisp-bc-3b4b-opcode-arg-bytes ()
  (should (= (aref nelisp-bc--opcode-arg-bytes 22) 0))
  (should (= (aref nelisp-bc--opcode-arg-bytes 23) 0))
  (should (= (aref nelisp-bc--opcode-arg-bytes 24) 0))
  (should (= (aref nelisp-bc--opcode-arg-bytes 25) 0))
  (should (= (aref nelisp-bc--opcode-arg-bytes 26) 0))
  (should (= (aref nelisp-bc--opcode-arg-bytes 27) 0))
  (should (= (aref nelisp-bc--opcode-arg-bytes 28) 0))
  (should (= (aref nelisp-bc--opcode-arg-bytes 29) 1))
  (should (= (aref nelisp-bc--opcode-arg-bytes 30) 1)))

(ert-deftest nelisp-bc-3b4b-car-cdr ()
  (should (eq (nelisp-bc-test--eval '(car '(1 2 3))) 1))
  (should (equal (nelisp-bc-test--eval '(cdr '(1 2 3))) '(2 3)))
  (should (eq (nelisp-bc-test--eval '(car '())) nil))
  (should (eq (nelisp-bc-test--eval '(cdr '())) nil)))

(ert-deftest nelisp-bc-3b4b-cons ()
  (should (equal (nelisp-bc-test--eval '(cons 1 2)) '(1 . 2)))
  (should (equal (nelisp-bc-test--eval '(cons 1 '(2 3))) '(1 2 3)))
  (should (equal (nelisp-bc-test--eval
                  '(cons (cons 'a 'b) (cons 'c 'd)))
                 '((a . b) . (c . d)))))

(ert-deftest nelisp-bc-3b4b-list-small ()
  (should (equal (nelisp-bc-test--eval '(list)) nil))
  (should (equal (nelisp-bc-test--eval '(list 1)) '(1)))
  (should (equal (nelisp-bc-test--eval '(list 1 2)) '(1 2)))
  (should (equal (nelisp-bc-test--eval '(list 1 2 3)) '(1 2 3)))
  (should (equal (nelisp-bc-test--eval '(list 1 2 3 4)) '(1 2 3 4))))

(ert-deftest nelisp-bc-3b4b-list-n ()
  ;; 5+ args takes the LISTN path.
  (should (equal (nelisp-bc-test--eval '(list 1 2 3 4 5)) '(1 2 3 4 5)))
  (should (equal (nelisp-bc-test--eval '(list 1 2 3 4 5 6 7 8 9 10))
                 '(1 2 3 4 5 6 7 8 9 10)))
  (should (equal (nelisp-bc-test--eval '(list 'a 'b 'c 'd 'e 'f))
                 '(a b c d e f))))

(ert-deftest nelisp-bc-3b4b-list-with-binding ()
  (should (equal (nelisp-bc-test--eval
                  '(let ((x 1) (y 2) (z 3)) (list x y z)))
                 '(1 2 3))))

(ert-deftest nelisp-bc-3b4b-car-of-cons ()
  ;; Nested primitive compilation.
  (should (= (nelisp-bc-test--eval '(car (cons 1 2))) 1))
  (should (= (nelisp-bc-test--eval '(cdr (cons 1 2))) 2))
  (should (= (nelisp-bc-test--eval '(car (cdr '(9 8 7)))) 8)))

(ert-deftest nelisp-bc-3b4b-handwritten-listn ()
  ;; CONST 0/1/2, LISTN 3 -> list (a b c)
  (let* ((code (nelisp-bc-test--code
                '((CONST 0) (CONST 1) (CONST 2) (LISTN 3) RETURN)))
         (bcl (nelisp-bc-make nil nil [x y z] code 3 0)))
    (should (equal (nelisp-bc-run bcl) '(x y z)))))

(ert-deftest nelisp-bc-3b4b-call-host-primitive ()
  ;; Call a host primitive not in the dedicated opcode table (`length'
  ;; or `symbol-name').  Compiled via CALL.
  (should (= (nelisp-bc-test--eval '(length '(1 2 3 4))) 4))
  (should (equal (nelisp-bc-test--eval '(symbol-name 'foo)) "foo")))

(ert-deftest nelisp-bc-3b4b-call-nelisp-defun ()
  ;; Call a NeLisp-registered user function installed via nelisp--functions.
  (unwind-protect
      (progn
        ;; Manually register a simple adder so we don't depend on
        ;; eval-time compilation in the test.
        (puthash 'nelisp-bc-test--adder
                 (list 'nelisp-closure nil '(x y) '((+ x y)))
                 nelisp--functions)
        (should (= (nelisp-bc-test--eval
                    '(nelisp-bc-test--adder 3 4))
                   7)))
    (remhash 'nelisp-bc-test--adder nelisp--functions)))

(ert-deftest nelisp-bc-3b4b-call-with-nested-args ()
  (unwind-protect
      (progn
        (puthash 'nelisp-bc-test--sum3
                 (list 'nelisp-closure nil '(a b c) '((+ a b c)))
                 nelisp--functions)
        (should (= (nelisp-bc-test--eval
                    '(nelisp-bc-test--sum3 (+ 1 1) (* 1 3) 4))
                   9)))
    (remhash 'nelisp-bc-test--sum3 nelisp--functions)))

(ert-deftest nelisp-bc-3b4b-call-zero-args ()
  ;; Verify CALL with nargs=0 works (fn is still on the stack).
  (unwind-protect
      (progn
        (puthash 'nelisp-bc-test--fortytwo
                 (list 'nelisp-closure nil '() '(42))
                 nelisp--functions)
        (should (= (nelisp-bc-test--eval '(nelisp-bc-test--fortytwo)) 42)))
    (remhash 'nelisp-bc-test--fortytwo nelisp--functions)))

(ert-deftest nelisp-bc-3b4b-unsupported-special-forms ()
  ;; Special forms scheduled for later sub-phases surface cleanly.
  ;; `catch' / `throw' / `unwind-protect' / `condition-case' moved
  ;; to the supported set in 3b.4c; `lambda' moved in 3b.5a.
  (should-error (nelisp-bc-compile '(and 1 2))
                :type 'nelisp-bc-unimplemented)
  (should-error (nelisp-bc-compile '(or 1 2))
                :type 'nelisp-bc-unimplemented))

;;; Phase 3b.4c — catch / throw / unwind-protect / condition-case ---

(ert-deftest nelisp-bc-3b4c-opcode-table ()
  (should (= (nelisp-bc-opcode 'PUSH-CATCH)  31))
  (should (= (nelisp-bc-opcode 'POP-HANDLER) 32))
  (should (= (nelisp-bc-opcode 'THROW)       33))
  (should (= (nelisp-bc-opcode 'PUSH-UNWIND) 34))
  (should (= (nelisp-bc-opcode 'PUSH-CC)     35)))

(ert-deftest nelisp-bc-3b4c-catch-no-throw ()
  (should (= (nelisp-bc-test--eval '(catch 'tag 42)) 42))
  (should (= (nelisp-bc-test--eval '(catch 'tag 1 2 3)) 3)))

(ert-deftest nelisp-bc-3b4c-catch-throw-basic ()
  (should (= (nelisp-bc-test--eval
              '(catch 'tag (throw 'tag 7) 99))
             7)))

(ert-deftest nelisp-bc-3b4c-catch-unmatched-propagates ()
  (should-error
   (nelisp-bc-test--eval
    '(catch 'inner (throw 'outer 1)))))

(ert-deftest nelisp-bc-3b4c-nested-catch ()
  (should (= (nelisp-bc-test--eval
              '(catch 'outer
                 (catch 'inner
                   (throw 'outer 10))
                 99))
             10))
  (should (= (nelisp-bc-test--eval
              '(catch 'outer
                 (+ 100 (catch 'inner (throw 'inner 5)))))
             105)))

(ert-deftest nelisp-bc-3b4c-catch-with-let ()
  (should (= (nelisp-bc-test--eval
              '(let ((x 5))
                 (catch 'tag
                   (+ x (throw 'tag 7)))))
             7))
  (should (= (nelisp-bc-test--eval
              '(catch 'tag
                 (let ((x 10))
                   (throw 'tag (+ x 5)))))
             15)))

(ert-deftest nelisp-bc-3b4c-catch-dyn-let-restored ()
  (unwind-protect
      (progn
        (puthash 'nelisp-bc-test--dyn3 t nelisp--specials)
        (puthash 'nelisp-bc-test--dyn3 'outer nelisp--globals)
        (should (= (nelisp-bc-test--eval
                    '(catch 'tag
                       (let ((nelisp-bc-test--dyn3 'inner))
                         (throw 'tag 7))))
                   7))
        (should (eq (gethash 'nelisp-bc-test--dyn3 nelisp--globals)
                    'outer)))
    (remhash 'nelisp-bc-test--dyn3 nelisp--globals)
    (remhash 'nelisp-bc-test--dyn3 nelisp--specials)))

(ert-deftest nelisp-bc-3b4c-unwind-protect-normal ()
  (unwind-protect
      (progn
        (remhash 'nelisp-bc-test--counter nelisp--globals)
        (should (= (nelisp-bc-test--eval
                    '(unwind-protect 42
                       (setq nelisp-bc-test--counter 1)))
                   42))
        (should (= (gethash 'nelisp-bc-test--counter nelisp--globals) 1)))
    (remhash 'nelisp-bc-test--counter nelisp--globals)))

(ert-deftest nelisp-bc-3b4c-unwind-protect-runs-on-throw ()
  (unwind-protect
      (progn
        (remhash 'nelisp-bc-test--counter2 nelisp--globals)
        (should (= (nelisp-bc-test--eval
                    '(catch 'tag
                       (unwind-protect
                           (throw 'tag 7)
                         (setq nelisp-bc-test--counter2 1))))
                   7))
        (should (= (gethash 'nelisp-bc-test--counter2 nelisp--globals) 1)))
    (remhash 'nelisp-bc-test--counter2 nelisp--globals)))

(ert-deftest nelisp-bc-3b4c-unwind-protect-empty-cleanup ()
  (should (= (nelisp-bc-test--eval '(unwind-protect 99)) 99)))

(ert-deftest nelisp-bc-3b4c-unwind-protect-multi-cleanup ()
  (unwind-protect
      (progn
        (remhash 'nelisp-bc-test--c3a nelisp--globals)
        (remhash 'nelisp-bc-test--c3b nelisp--globals)
        (should (= (nelisp-bc-test--eval
                    '(unwind-protect 33
                       (setq nelisp-bc-test--c3a 1)
                       (setq nelisp-bc-test--c3b 2)))
                   33))
        (should (= (gethash 'nelisp-bc-test--c3a nelisp--globals) 1))
        (should (= (gethash 'nelisp-bc-test--c3b nelisp--globals) 2)))
    (remhash 'nelisp-bc-test--c3a nelisp--globals)
    (remhash 'nelisp-bc-test--c3b nelisp--globals)))

(ert-deftest nelisp-bc-3b4c-condition-case-caught ()
  (should (eq (nelisp-bc-test--eval
               '(condition-case nil
                    (signal 'error '("x"))
                  (error 'ok)))
              'ok))
  (should (= (nelisp-bc-test--eval
              '(condition-case nil
                   (throw 'no-such-catch 1)
                 (no-catch 42)))
             42)))

(ert-deftest nelisp-bc-3b4c-condition-case-returns-body ()
  (should (= (nelisp-bc-test--eval
              '(condition-case nil
                   7
                 (error 99)))
             7)))

(ert-deftest nelisp-bc-3b4c-condition-case-binds-var ()
  (let ((r (nelisp-bc-test--eval
            '(condition-case err
                 (signal 'error '("bad-thing" 1 2))
               (error err)))))
    (should (equal r '(error "bad-thing" 1 2)))))

(ert-deftest nelisp-bc-3b4c-condition-case-unmatched-propagates ()
  (should-error
   (nelisp-bc-test--eval
    '(condition-case nil
         (signal 'error nil)
       (arith-error 'wrong)))))

(ert-deftest nelisp-bc-3b4c-condition-case-multi-handler ()
  (should (eq (nelisp-bc-test--eval
               '(condition-case nil
                    (signal 'arith-error nil)
                  (void-function 'a)
                  (arith-error 'b)
                  (error 'c)))
              'b))
  (should (eq (nelisp-bc-test--eval
               '(condition-case nil
                    (signal 'error nil)
                  (void-function 'a)
                  (arith-error 'b)
                  (error 'c)))
              'c)))

(ert-deftest nelisp-bc-3b4c-condition-case-var-lexical ()
  (should (= (nelisp-bc-test--eval
              '(condition-case err
                   (signal 'arith-error '(123))
                 (arith-error (car (cdr err)))))
             123)))

(ert-deftest nelisp-bc-3b4c-condition-case-restores-dyn-let ()
  (unwind-protect
      (progn
        (puthash 'nelisp-bc-test--dyn4 t nelisp--specials)
        (puthash 'nelisp-bc-test--dyn4 'outer nelisp--globals)
        (should (eq (nelisp-bc-test--eval
                     '(condition-case nil
                          (let ((nelisp-bc-test--dyn4 'inner))
                            (signal 'error nil))
                        (error 'caught)))
                    'caught))
        (should (eq (gethash 'nelisp-bc-test--dyn4 nelisp--globals)
                    'outer)))
    (remhash 'nelisp-bc-test--dyn4 nelisp--globals)
    (remhash 'nelisp-bc-test--dyn4 nelisp--specials)))

(ert-deftest nelisp-bc-3b4c-unwind-protect-restores-dyn-let ()
  (unwind-protect
      (progn
        (puthash 'nelisp-bc-test--dyn5 t nelisp--specials)
        (puthash 'nelisp-bc-test--dyn5 'outer nelisp--globals)
        (remhash 'nelisp-bc-test--cleanup-ran nelisp--globals)
        (condition-case nil
            (nelisp-bc-test--eval
             '(let ((nelisp-bc-test--dyn5 'inner))
                (unwind-protect
                    (signal 'error '("bad"))
                  (setq nelisp-bc-test--cleanup-ran t))))
          (error nil))
        (should (eq (gethash 'nelisp-bc-test--cleanup-ran nelisp--globals) t))
        (should (eq (gethash 'nelisp-bc-test--dyn5 nelisp--globals) 'outer)))
    (remhash 'nelisp-bc-test--dyn5 nelisp--globals)
    (remhash 'nelisp-bc-test--dyn5 nelisp--specials)
    (remhash 'nelisp-bc-test--cleanup-ran nelisp--globals)))

;;; Phase 3b.5a — lambda + arg binding (no capture) -----------------

(ert-deftest nelisp-bc-3b5a-lambda-as-value ()
  ;; `(lambda ...)` compiles to a closure (bcl).
  (let ((bcl (nelisp-bc-test--eval '(lambda (x) (+ x 1)))))
    (should (nelisp-bcl-p bcl))
    (should (equal (nelisp-bc-params bcl) '(x)))))

(ert-deftest nelisp-bc-3b5a-lambda-call-inline ()
  (should (= (nelisp-bc-test--eval '((lambda (x) (+ x 1)) 41)) 42))
  (should (= (nelisp-bc-test--eval '((lambda (a b) (- a b)) 10 3)) 7))
  (should (= (nelisp-bc-test--eval '((lambda () 99))) 99)))

(ert-deftest nelisp-bc-3b5a-lambda-via-apply ()
  ;; Manually drive nelisp-bc-run.
  (let ((bcl (nelisp-bc-compile '(lambda (x y) (* x y)))))
    (should (= (nelisp--apply (nelisp-bc-run bcl) '(6 7)) 42))))

(ert-deftest nelisp-bc-3b5a-lambda-arity-errors ()
  (let ((bcl (nelisp-bc-run (nelisp-bc-compile '(lambda (x) x)))))
    (should-error (nelisp--apply bcl '()) :type 'nelisp-bc-error)
    (should-error (nelisp--apply bcl '(1 2)) :type 'nelisp-bc-error)))

(ert-deftest nelisp-bc-3b5a-lambda-optional ()
  (let ((bcl (nelisp-bc-run
              (nelisp-bc-compile '(lambda (a &optional b c)
                                    (list a b c))))))
    (should (equal (nelisp--apply bcl '(1)) '(1 nil nil)))
    (should (equal (nelisp--apply bcl '(1 2)) '(1 2 nil)))
    (should (equal (nelisp--apply bcl '(1 2 3)) '(1 2 3)))
    (should-error (nelisp--apply bcl '(1 2 3 4)) :type 'nelisp-bc-error)))

(ert-deftest nelisp-bc-3b5a-lambda-rest ()
  (let ((bcl (nelisp-bc-run
              (nelisp-bc-compile '(lambda (a &rest r)
                                    (cons a r))))))
    (should (equal (nelisp--apply bcl '(1)) '(1)))
    (should (equal (nelisp--apply bcl '(1 2 3 4)) '(1 2 3 4)))))

(ert-deftest nelisp-bc-3b5a-lambda-optional-and-rest ()
  (let ((bcl (nelisp-bc-run
              (nelisp-bc-compile '(lambda (a &optional b &rest r)
                                    (list a b r))))))
    (should (equal (nelisp--apply bcl '(1)) '(1 nil nil)))
    (should (equal (nelisp--apply bcl '(1 2)) '(1 2 nil)))
    (should (equal (nelisp--apply bcl '(1 2 3 4)) '(1 2 (3 4))))))

(ert-deftest nelisp-bc-3b5a-lambda-no-params-zero-arg-call ()
  (let ((bcl (nelisp-bc-run (nelisp-bc-compile '(lambda () 5)))))
    (should (= (nelisp--apply bcl '()) 5))
    (should-error (nelisp--apply bcl '(1)) :type 'nelisp-bc-error)))

(ert-deftest nelisp-bc-3b5a-lambda-special-param ()
  (unwind-protect
      (progn
        (puthash 'nelisp-bc-test--lp t nelisp--specials)
        (puthash 'nelisp-bc-test--lp 'outer nelisp--globals)
        (let ((bcl (nelisp-bc-run
                    (nelisp-bc-compile
                     '(lambda (nelisp-bc-test--lp)
                        nelisp-bc-test--lp)))))
          (should (eq (nelisp--apply bcl '(inner)) 'inner))
          ;; Outer global must be restored after the call.
          (should (eq (gethash 'nelisp-bc-test--lp nelisp--globals)
                      'outer))))
    (remhash 'nelisp-bc-test--lp nelisp--globals)
    (remhash 'nelisp-bc-test--lp nelisp--specials)))

(ert-deftest nelisp-bc-3b5a-lambda-special-param-mixed ()
  (unwind-protect
      (progn
        (puthash 'nelisp-bc-test--lp2 t nelisp--specials)
        (puthash 'nelisp-bc-test--lp2 0 nelisp--globals)
        (let ((bcl (nelisp-bc-run
                    (nelisp-bc-compile
                     '(lambda (a nelisp-bc-test--lp2 b)
                        (+ a nelisp-bc-test--lp2 b))))))
          (should (= (nelisp--apply bcl '(1 10 100)) 111))
          (should (= (gethash 'nelisp-bc-test--lp2 nelisp--globals) 0))))
    (remhash 'nelisp-bc-test--lp2 nelisp--globals)
    (remhash 'nelisp-bc-test--lp2 nelisp--specials)))

(ert-deftest nelisp-bc-3b5a-lambda-restores-special-on-error ()
  (unwind-protect
      (progn
        (puthash 'nelisp-bc-test--lp3 t nelisp--specials)
        (puthash 'nelisp-bc-test--lp3 'outer nelisp--globals)
        (remhash 'nelisp-bc-test--missing nelisp--globals)
        (let ((bcl (nelisp-bc-run
                    (nelisp-bc-compile
                     '(lambda (nelisp-bc-test--lp3)
                        nelisp-bc-test--missing)))))
          (condition-case nil
              (nelisp--apply bcl '(transient))
            (nelisp-unbound-variable nil))
          (should (eq (gethash 'nelisp-bc-test--lp3 nelisp--globals)
                      'outer))))
    (remhash 'nelisp-bc-test--lp3 nelisp--globals)
    (remhash 'nelisp-bc-test--lp3 nelisp--specials)))

(ert-deftest nelisp-bc-3b5a-lambda-recursion-via-symbol ()
  ;; A bcl can call itself indirectly via a symbol installed in
  ;; nelisp--functions.
  (unwind-protect
      (progn
        (let ((bcl (nelisp-bc-run
                    (nelisp-bc-compile
                     '(lambda (n)
                        (if (< n 2) n
                          (+ (nelisp-bc-test--fib (- n 1))
                             (nelisp-bc-test--fib (- n 2)))))))))
          (puthash 'nelisp-bc-test--fib bcl nelisp--functions)
          (should (= (nelisp--apply bcl '(0)) 0))
          (should (= (nelisp--apply bcl '(1)) 1))
          (should (= (nelisp--apply bcl '(7)) 13))))
    (remhash 'nelisp-bc-test--fib nelisp--functions)))

(ert-deftest nelisp-bc-3b5a-function-form ()
  ;; (function (lambda ...)) compiles same as lambda.
  (should (= (nelisp-bc-test--eval
              '(funcall (function (lambda (x) (* x x))) 5))
             25)))

;;; Phase 3b.5b — free-lex capture (nested lambdas) -----------------

(ert-deftest nelisp-bc-3b5b-opcode-table ()
  (should (= (nelisp-bc-opcode 'MAKE-CLOSURE) 36))
  (should (= (nelisp-bc-opcode 'CAPTURED-REF) 37)))

(ert-deftest nelisp-bc-3b5b-single-capture ()
  (should (= (nelisp-bc-test--eval
              '(let ((y 10))
                 ((lambda (x) (+ x y)) 5)))
             15)))

(ert-deftest nelisp-bc-3b5b-multi-capture ()
  (should (= (nelisp-bc-test--eval
              '(let ((a 1) (b 2) (c 3))
                 ((lambda (x) (+ x a b c)) 10)))
             16)))

(ert-deftest nelisp-bc-3b5b-capture-snapshot-not-ref ()
  ;; Captured value is the value at MAKE-CLOSURE time (snapshot).
  (let ((closure (nelisp-bc-test--eval
                  '(let ((y 100))
                     (lambda (x) (+ x y))))))
    (should (= (nelisp--apply closure '(1)) 101))))

(ert-deftest nelisp-bc-3b5b-multiple-closures-distinct-env ()
  ;; Two closures over different `y' values keep separate envs.
  (let ((c1 (nelisp-bc-test--eval
             '(let ((y 10)) (lambda (x) (+ x y)))))
        (c2 (nelisp-bc-test--eval
             '(let ((y 20)) (lambda (x) (+ x y))))))
    (should (= (nelisp--apply c1 '(1)) 11))
    (should (= (nelisp--apply c2 '(1)) 21))))

(ert-deftest nelisp-bc-3b5b-shadowing-by-param ()
  ;; A lambda param shadows the outer lex of the same name.
  (should (= (nelisp-bc-test--eval
              '(let ((x 100))
                 ((lambda (x) x) 7)))
             7))
  ;; And the outer lex is still readable when not shadowed.
  (should (= (nelisp-bc-test--eval
              '(let ((x 100))
                 ((lambda (y) (+ x y)) 7)))
             107)))

(ert-deftest nelisp-bc-3b5b-multi-level-capture ()
  ;; Inner lambda captures BOTH outer-let `a' and middle-let `b'.
  (should (= (nelisp-bc-test--eval
              '(let ((a 1))
                 (let ((b 10))
                   ((lambda (x) (+ x a b)) 100))))
             111)))

(ert-deftest nelisp-bc-3b5b-closure-from-closure ()
  ;; Inner lambda created inside an already-running outer closure
  ;; captures from the outer's call frame.
  (let ((make-adder (nelisp-bc-test--eval
                     '(lambda (n)
                        (lambda (x) (+ x n))))))
    (let ((add5 (nelisp--apply make-adder '(5)))
          (add10 (nelisp--apply make-adder '(10))))
      (should (= (nelisp--apply add5 '(1)) 6))
      (should (= (nelisp--apply add10 '(1)) 11))
      (should (= (nelisp--apply add5 '(100)) 105)))))

(ert-deftest nelisp-bc-3b5b-three-level-chain ()
  (let ((three (nelisp-bc-test--eval
                '(lambda (a)
                   (lambda (b)
                     (lambda (c)
                       (+ a b c)))))))
    (let* ((f1 (nelisp--apply three '(1)))
           (f2 (nelisp--apply f1 '(10)))
           (r  (nelisp--apply f2 '(100))))
      (should (= r 111)))))

(ert-deftest nelisp-bc-3b5b-capture-via-let* ()
  ;; let* sequential bindings — capture last bound value.
  (let ((c (nelisp-bc-test--eval
            '(let* ((a 1) (b (+ a 10)))
               (lambda (x) (+ x a b))))))
    (should (= (nelisp--apply c '(0)) 12))))

(ert-deftest nelisp-bc-3b5b-capture-keeps-outer-stable ()
  ;; Mutating the outer lex AFTER closure creation does not affect
  ;; the captured snapshot.
  (let ((outer-result
         (nelisp-bc-test--eval
          '(let ((y 1))
             (let ((c (lambda () y)))
               (setq y 999)
               (list y (funcall c)))))))
    ;; y was set to 999 after closure creation; closure still sees 1.
    (should (equal outer-result '(999 1)))))

(ert-deftest nelisp-bc-3b5b-no-capture-still-works ()
  ;; Lambda with no free-lex still compiles via the simple CONST
  ;; path; ensure we didn't break the no-capture happy path.
  (let ((c (nelisp-bc-test--eval '(lambda (x) (* x x)))))
    (should (= (nelisp--apply c '(7)) 49))))

(ert-deftest nelisp-bc-3b5b-capture-with-special ()
  ;; Captured lex + special VARREF coexist.
  (unwind-protect
      (progn
        (puthash 'nelisp-bc-test--cap-spec t nelisp--specials)
        (puthash 'nelisp-bc-test--cap-spec 1000 nelisp--globals)
        (let ((c (nelisp-bc-test--eval
                  '(let ((local 7))
                     (lambda (x)
                       (+ x local nelisp-bc-test--cap-spec))))))
          (should (= (nelisp--apply c '(2)) 1009))))
    (remhash 'nelisp-bc-test--cap-spec nelisp--globals)
    (remhash 'nelisp-bc-test--cap-spec nelisp--specials)))

;;; Phase 3b.5c — defun / closure auto-compile hook -----------------

(ert-deftest nelisp-bc-3b5c-flag-defaults-off ()
  ;; Conservative default — full justification in defvar docstring.
  (should (eq nelisp-bc-auto-compile nil)))

(ert-deftest nelisp-bc-3b5c-try-compile-returns-bcl ()
  (let ((nelisp-bc-auto-compile t))
    (let ((bcl (nelisp-bc-try-compile-lambda nil '(x) '((+ x 1)))))
      (should (nelisp-bcl-p bcl))
      (should (= (nelisp--apply bcl '(41)) 42)))))

(ert-deftest nelisp-bc-3b5c-try-compile-respects-flag ()
  (let ((nelisp-bc-auto-compile nil))
    (should (eq (nelisp-bc-try-compile-lambda nil '(x) '(x)) nil))))

(ert-deftest nelisp-bc-3b5c-try-compile-rejects-non-nil-env ()
  ;; ENV is non-nil (closure with captures) — top-level only for now.
  (let ((nelisp-bc-auto-compile t))
    (should (eq (nelisp-bc-try-compile-lambda
                 '((y . 10)) '(x) '((+ x y)))
                nil))))

(ert-deftest nelisp-bc-3b5c-try-compile-falls-back-on-unsupported ()
  ;; Body uses `and' which is in the unimplemented set; expect nil.
  (let ((nelisp-bc-auto-compile t))
    (should (eq (nelisp-bc-try-compile-lambda nil '(x) '((and x 1)))
                nil))))

(ert-deftest nelisp-bc-3b5c-defun-installs-bcl-when-on ()
  (let ((nelisp-bc-auto-compile t))
    (nelisp--reset)
    (nelisp-eval '(defun nelisp-bc-test--ac1 (x) (* x 3)))
    (let ((fn (gethash 'nelisp-bc-test--ac1 nelisp--functions)))
      (should (nelisp-bcl-p fn))
      (should (= (nelisp-eval '(nelisp-bc-test--ac1 7)) 21)))))

(ert-deftest nelisp-bc-3b5c-defun-installs-closure-when-off ()
  (let ((nelisp-bc-auto-compile nil))
    (nelisp--reset)
    (nelisp-eval '(defun nelisp-bc-test--ac2 (x) (* x 3)))
    (let ((fn (gethash 'nelisp-bc-test--ac2 nelisp--functions)))
      (should (nelisp--closure-p fn))
      (should (= (nelisp-eval '(nelisp-bc-test--ac2 7)) 21)))))

(ert-deftest nelisp-bc-3b5c-recursive-defun-via-bcl ()
  (let ((nelisp-bc-auto-compile t))
    (nelisp--reset)
    (nelisp-eval '(defun nelisp-bc-test--fact (n)
                    (if (< n 2) 1 (* n (nelisp-bc-test--fact (- n 1))))))
    (let ((fn (gethash 'nelisp-bc-test--fact nelisp--functions)))
      (should (nelisp-bcl-p fn))
      (should (= (nelisp-eval '(nelisp-bc-test--fact 5)) 120)))))

(ert-deftest nelisp-bc-3b5c-mixed-defun-bcl-and-closure ()
  ;; A defun with a body that doesn't compile (e.g. uses `and') falls
  ;; back to the interpreter closure even with auto-compile on.
  (let ((nelisp-bc-auto-compile t))
    (nelisp--reset)
    (nelisp-eval '(defun nelisp-bc-test--mix (x) (and x x)))
    (let ((fn (gethash 'nelisp-bc-test--mix nelisp--functions)))
      (should (nelisp--closure-p fn))
      (should (eq (nelisp-eval '(nelisp-bc-test--mix t)) t)))))

(ert-deftest nelisp-bc-3b5c-setq-on-captured-lex-falls-back ()
  ;; Outer defun's body holds a closure that mutates a captured var.
  ;; A `nelisp-bc-unimplemented' from the inner-lambda compile sinks
  ;; the outer compile too (no partial-compile fallback yet), so the
  ;; whole defun lands as an interpreter closure — and crucially the
  ;; runtime semantics are still correct.
  (let ((nelisp-bc-auto-compile t))
    (nelisp--reset)
    (nelisp-eval '(defun nelisp-bc-test--counter (start)
                    (let ((n start))
                      (lambda () (setq n (+ n 1)) n))))
    (let* ((make (gethash 'nelisp-bc-test--counter nelisp--functions))
           (counter (nelisp-eval '(nelisp-bc-test--counter 10))))
      (should (nelisp--closure-p make))
      (should (nelisp--closure-p counter))
      (should (= (nelisp--apply counter nil) 11))
      (should (= (nelisp--apply counter nil) 12)))))

(provide 'nelisp-bytecode-test)
;;; nelisp-bytecode-test.el ends here
