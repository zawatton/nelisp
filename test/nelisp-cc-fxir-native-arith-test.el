;;; nelisp-cc-fxir-native-arith-test.el --- guarded logand and n-ary +/- lowering -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; `logand' has no GNU opcode, so its inline arm is guarded by the live
;; function cell of the call-site symbol (Subr header id byte 44).  `logior'
;; has no identity token and stays a generic call.  N-ary + / - fold into
;; checked fixnum chains (GNU Bplus / Bdiff are not redefinable).  The lowered
;; dialect is evaluated against a fake heap below.

;;; Code:

(require 'ert)
(require 'cl-lib)
(defvar nelisp-cc-fxir-native-windows)
(let ((nelisp-cc-fxir-native-windows t))
  (require 'nelisp-cc-fxir-native))
(require 'nelisp-cc-fxir-effects)
(defvar nelisp-cc-fxir-native--temps)
(defvar nelisp-cc-fxir-native--callees)
(defvar nelisp-cc-fxir-native--maxargc)
(defvar nelisp-cc-fxir-native--nbd)
(defvar nelisp-cc-fxir-native--ncatch)
(defvar nelisp-cc-fxir-native--uses-window)
(defvar nelisp-cc-fxir-native--inline-sites)
(defvar nelisp-cc-fxir-native--inline-budget)
(defvar nelisp-cc-fxir-native--inline-count)
(defvar nelisp-cc-fxir-native--direct-ids)

(defconst nfna-cases
  '((logand (:call 1 2 (:slot 0) (:slot 1)) (logand))
    (logior (:call 1 2 (:slot 0) (:slot 1)) (logior))
    (plus3 (:call 1 3 (:slot 0) (:slot 1) (:slot 2)) (+))
    (plus3-call (:call 1 3 (:slot 0) (:call 2 1 (:slot 1)) (:slot 2)) (+ f))
    (plus2 (:call 1 2 (:slot 0) (:slot 1)) (+))
    (minus3 (:call 1 3 (:slot 0) (:slot 1) (:slot 2)) (-))))

(defun nfna--run (c)
  (let ((nelisp-cc-fxir-native--temps nil)
        (nelisp-cc-fxir-native--callees (copy-sequence (nth 2 c)))
        (nelisp-cc-fxir-native--maxargc 0) (nelisp-cc-fxir-native--nbd 0)
        (nelisp-cc-fxir-native--ncatch 0) (nelisp-cc-fxir-native--uses-window nil)
        (nelisp-cc-fxir-native--inline-sites nil)
        (nelisp-cc-fxir-native--inline-budget 0)
        (nelisp-cc-fxir-native--inline-count 0)
        (nelisp-cc-fxir-native--direct-ids nil))
    (nelisp-cc-fxir-native--lower (nth 1 c))))

(defconst nfna-golden
  '((logand (((nil setq t0 (if (if (= (logand l0 3) 1) (= (logand l1 3) 1) 0) (if (let* ((lgs (heap-read-u64 (+ base (* 1 8)) 0)) (lgsym (if (= (logand lgs 7) 6) (if (< lgs 4096) 0 (let* ((lgh (ptr-read-u64 (- lgs 6) 0))) (if (= (logand lgh 63) 8) (ptr-read-u64 (- lgs 6) 8) (if (= (logand lgh 63) 4) lgs 0)))) 0)) (lgsy (if (= (logand lgsym 7) 6) (if (< lgsym 4096) 0 (if (= (logand (ptr-read-u64 (- lgsym 6) 0) 63) 4) lgsym 0)) 0)) (lgfn (if (= lgsy 0) 0 (ptr-read-u64 (+ lgsy 18) 0)))) (if (= (logand lgfn 7) 6) (if (< lgfn 4096) 0 (if (= (logand (ptr-read-u64 (- lgfn 6) 0) 63) 20) (= (logand (shr (ptr-read-u64 (- lgfn 6) 0) 8) 255) 44) 0)) 0)) (logand l0 l1) 0) 0)) (t if (= t0 0) (seq (setq st (nc_call2 env (heap-read-u64 (+ base (* 1 8)) 0) mem oslot l0 l1)) (if (= st 0) (seq (setq t1 (ptr-read-u64 mem 0)) (setq t0 t1)) 0)) 0)) . t0))
  (logior (((t setq st (nc_call2 env (heap-read-u64 (+ base (* 1 8)) 0) mem oslot l0 l1)) (nil setq t0 (ptr-read-u64 mem 0))) . t0))
  (plus3 (((nil if (if (= (logand l0 3) 1) (if (= (logand l1 3) 1) (= (logand l2 3) 1) 0) 0) (seq (setq t0 (+ (sar l0 2) (sar l1 2))) (if (= (sar (shl t0 2) 2) t0) (seq (setq t0 (+ t0 (sar l2 2))) (if (= (sar (shl t0 2) 2) t0) (setq t0 (+ (shl t0 2) 1)) (setq t0 -1))) (setq t0 -1))) (setq t0 -1)) (t if (= t0 -1) (seq (setq st (nc_call3 env (heap-read-u64 (+ base (* 1 8)) 0) mem oslot l0 l1 l2)) (if (= st 0) (seq (setq t1 (ptr-read-u64 mem 0)) (setq t0 t1)) 0)) 0)) . t0))
  (plus3-call (((nil setq t1 l0) (t setq st (nc_call1 env (heap-read-u64 (+ base (* 2 8)) 0) mem oslot l1)) (nil setq t0 (ptr-read-u64 mem 0)) (nil if (if (= (logand t1 3) 1) (if (= (logand t0 3) 1) (= (logand l2 3) 1) 0) 0) (seq (setq t2 (+ (sar t1 2) (sar t0 2))) (if (= (sar (shl t2 2) 2) t2) (seq (setq t2 (+ t2 (sar l2 2))) (if (= (sar (shl t2 2) 2) t2) (setq t2 (+ (shl t2 2) 1)) (setq t2 -1))) (setq t2 -1))) (setq t2 -1)) (t if (= t2 -1) (seq (setq st (nc_call3 env (heap-read-u64 (+ base (* 1 8)) 0) mem oslot t1 t0 l2)) (if (= st 0) (seq (setq t3 (ptr-read-u64 mem 0)) (setq t2 t3)) 0)) 0)) . t2))
  (plus2 (((t setq st (nc_call2 env (heap-read-u64 (+ base (* 1 8)) 0) mem oslot l0 l1)) (nil setq t0 (ptr-read-u64 mem 0))) . t0))))

(ert-deftest nfna-golden-lowering ()
  (dolist (g nfna-golden)
    (when (assq (car g) nfna-cases)
      (should (equal (nfna--run (assq (car g) nfna-cases)) (cadr g))))))

(ert-deftest nfna-two-arg-plus-and-logior-unchanged ()
  ;; Byte-identical to the pre-change lowering: both stay one generic call.
  (dolist (n '(plus2 logior))
    (let ((low (nfna--run (assq n nfna-cases))))
      (should (= 2 (length (car low))))
      (should (eq (car (nth 3 (car (car low)))) 'nc_call2)))))

(ert-deftest nfna-effects-and-census ()
  (dolist (n '(logand plus3 plus3-call minus3))
    (let* ((c (assq n nfna-cases)) (low (nfna--run c)))
      (should (nelisp-cc-fxir-native--effects-consistent-p (nth 1 c) low))
      (should (plist-get (nelisp-cc-fxir-native-call-census low) :guards))
      (should (> (plist-get (nelisp-cc-fxir-native-call-census low) :call-outs) 0))))
  ;; The generic call sits only on the fallback edge: not a top-level statement.
  (dolist (n '(logand plus3))
    (let ((low (nfna--run (assq n nfna-cases))))
      (should-not (cl-some (lambda (s) (and (eq (cadr s) 'setq) (eq (nth 2 s) 'st))) (car low)))))
  (should (assq 'logand nelisp-cc-fxir-effects-callee-table))
  (should (plist-get (cdr (assq 'logand nelisp-cc-fxir-effects-callee-table)) :fast-path-pure)))

;; --- Evaluator for the lowered dialect over a fake heap -------------------

(defvar nfna--mem nil)
(defvar nfna--calls nil)
(defvar nfna--call-result 777)

(defun nfna--wrap (x)
  (let ((y (logand x #xFFFFFFFFFFFFFFFF)))
    (if (>= y (ash 1 63)) (- y (ash 1 64)) y)))

(defun nfna--true (v) (and v (not (eql v 0))))

(defun nfna--xform (f)
  "Rewrite F so that every `if' test treats 0 as false, as the dialect does."
  (cond ((not (consp f)) f)
        ((eq (car f) 'if)
         `(if (nfna--true ,(nfna--xform (nth 1 f))) ,(nfna--xform (nth 2 f))
            ,@(mapcar #'nfna--xform (nthcdr 3 f))))
        (t (cons (nfna--xform (car f)) (nfna--xform (cdr f))))))

(defun nfna--fix (n) (+ (* n 4) 1))

(defun nfna--eval (case slots fnword)
  "Evaluate CASE's lowering with SLOTS (l0..); FNWORD is the logand cell word.
Answers (VALUE . CALLS), CALLS the argument lists the generic call got."
  (let* ((low (nfna--run (assq case nfna-cases)))
         (nfna--mem (make-hash-table))
         (nfna--calls nil)
         (binds (append (cl-loop for i from 0 for s in slots collect (list (intern (format "l%d" i)) s))
                        (cl-loop for i below 6 collect (list (intern (format "t%d" i)) 0))
                        '((st 0) (base 1000) (mem 5000) (env 0) (oslot 0) (nb 0)))))
    ;; site: slot 1 of BASE holds the call-cache vector; sym; cell; Subr.
    (puthash 1008 12006 nfna--mem) (puthash 12000 8 nfna--mem) (puthash 12008 13006 nfna--mem)
    (puthash 13000 4 nfna--mem) (puthash 13024 fnword nfna--mem)
    (puthash 14000 (logior 20 (ash 44 8)) nfna--mem)
    (puthash 14100 (logior 20 (ash 99 8)) nfna--mem)
    (cl-letf (((symbol-function 'ptr-read-u64) (lambda (a o) (gethash (+ a o) nfna--mem 0)))
              ((symbol-function 'heap-read-u64) (lambda (a o) (gethash (+ a o) nfna--mem 0)))
              ((symbol-function 'sar) (lambda (v n) (ash v (- n))))
              ((symbol-function 'shr) (lambda (v n) (ash (logand v #xFFFFFFFFFFFFFFFF) (- n))))
              ((symbol-function 'shl) (lambda (v n) (nfna--wrap (ash v n))))
              ((symbol-function 'seq) (cons 'macro (lambda (&rest b) (cons 'progn b))))
              ((symbol-function 'nc_call1) (lambda (&rest a) (push (nthcdr 4 a) nfna--calls)
                                              (puthash 5000 (nfna--fix 5) nfna--mem) 0))
              ((symbol-function 'nc_call2) (lambda (&rest a) (push (nthcdr 4 a) nfna--calls)
                                              (puthash 5000 nfna--call-result nfna--mem) 0))
              ((symbol-function 'nc_call3) (lambda (&rest a) (push (nthcdr 4 a) nfna--calls)
                                              (puthash 5000 nfna--call-result nfna--mem) 0)))
      (let ((v (eval (nfna--xform `(let ,binds ,@(mapcar #'cdr (car low))
                           ,(cdr low)))
                     nil)))
        (cons v (reverse nfna--calls))))))

(defconst nfna-mpf (1- (ash 1 61)))

(ert-deftest nfna-logand-fast-path ()
  (let ((r (nfna--eval 'logand (list (nfna--fix 12) (nfna--fix 10)) 14006)))
    (should (equal (car r) (nfna--fix 8)))
    (should-not (cdr r)))
  ;; Negative operands: tagged and-ing keeps the 01 tag.
  (let ((r (nfna--eval 'logand (list (nfna--fix -4) (nfna--fix 7)) 14006)))
    (should (equal (car r) (nfna--fix 4)))
    (should-not (cdr r))))

(ert-deftest nfna-logand-non-fixnum-generic ()
  (let ((r (nfna--eval 'logand (list 2 (nfna--fix 3)) 14006)))
    (should (equal (car r) nfna--call-result))
    (should (equal (cdr r) (list (list 2 (nfna--fix 3)))))))

(ert-deftest nfna-logand-guard-invalidated ()
  ;; fset/advice: the cell holds another subr, a closure word or nothing.
  (dolist (fn '(14106 5006 0 3))
    (let ((r (nfna--eval 'logand (list (nfna--fix 12) (nfna--fix 10)) fn)))
      (should (equal (car r) nfna--call-result))
      (should (equal (cdr r) (list (list (nfna--fix 12) (nfna--fix 10))))))))

(ert-deftest nfna-plus3-fast-and-minus3 ()
  (let ((r (nfna--eval 'plus3 (list (nfna--fix 1) (nfna--fix 2) (nfna--fix 3)) 14006)))
    (should (equal r (cons (nfna--fix 6) nil))))
  (let ((r (nfna--eval 'plus3 (list (nfna--fix -7) (nfna--fix 2) (nfna--fix 3)) 14006)))
    (should (equal r (cons (nfna--fix -2) nil))))
  (let ((r (nfna--eval 'minus3 (list (nfna--fix 10) (nfna--fix 3) (nfna--fix 2)) 14006)))
    (should (equal r (cons (nfna--fix 5) nil)))))

(ert-deftest nfna-plus3-non-fixnum-and-overflow-generic ()
  (dolist (ws (list (list (nfna--fix 1) 2 (nfna--fix 3))
                    (list (nfna--fix nfna-mpf) (nfna--fix 1) (nfna--fix -1))
                    (list (nfna--fix (1- nfna-mpf)) (nfna--fix 1) (nfna--fix 1))
                    (list (nfna--fix 1) (nfna--fix 1) (nfna--fix nfna-mpf))))
    (let ((r (nfna--eval 'plus3 ws 14006)))
      (should (equal (car r) nfna--call-result))
      (should (equal (cdr r) (list ws))))))

(ert-deftest nfna-plus3-call-arg-once-in-order ()
  ;; The call argument runs once, before the sum; its result is the middle term.
  (let ((r (nfna--eval 'plus3-call (list (nfna--fix 1) (nfna--fix 9) (nfna--fix 3)) 14006)))
    (should (equal (car r) (nfna--fix 9)))
    (should (equal (cdr r) (list (list (nfna--fix 9)))))))
