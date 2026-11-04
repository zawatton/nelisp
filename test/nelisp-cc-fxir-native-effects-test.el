;;; nelisp-cc-fxir-native-effects-test.el --- Native lowering reads the FXIR effect table -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; S1.2: the native lowering reads `nelisp-cc-fxir-effects-table' for the
;; per-op FALLIBLE flag.  The golden outputs below were captured from
;; `nelisp-cc-fxir-native--lower' BEFORE the change, so the refactor is
;; output-preserving.

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

(defconst nfne-cases
  '((const (:const 3))
    (slot (:slot 1))
    (varref (:varref 4))
    (setslot (:setslot 0 (:const 2)))
    (seq (:seq (:const 1) (:setslot 1 (:const 2)) (:slot 1)))
    (if (:if (:slot 0) (:const 1) (:const 2)))
    (if-fallible (:if (:slot 0) (:varref 1) (:const 2)))
    (and (:and (:slot 0) (:slot 1) (:const 2)))
    (or (:or (:slot 0) (:slot 1) (:const 2)))
    (let-store (:let ((:store 0 (:const 1))) (:slot 0) 0))
    (let-varbind (:let ((:varbind 5 (:const 1))) (:slot 0) 1))
    (while (:while (:slot 0) (:setslot 1 (:const 2))))
    (call (:call 1 1 (:slot 0)) (foo))
    (call-eq (:call 1 2 (:slot 0) (:slot 1)) (eq))
    (call-car (:call 1 1 (:slot 0)) (car))
    (inline (:inline 4 2 (:slot 0) (:slot 1)))
    (catch (:catch (:const 1) (:slot 0)))
    (throw (:throw (:const 1) (:const 2)))
    (condition-case (:condition-case nil (:slot 0) nil (7 (:const 1))))
    (unwind-protect (:unwind-protect (:slot 0) (:const 1))))
  "Entries are (NAME NODE [CALLEES]).")

(defconst nfne-golden
  '(
(const
 (nil heap-read-u64 (+ base (* 3 8)) 0))
(slot
 (nil . l1))
(varref
 (((t setq st (wf_bytecode_w_varref env (heap-read-u64 (+ base (* 4 8)) 0) mem 0)) (nil setq t0 (ptr-read-u64 mem 0))) . t0))
(setslot
 (((nil setq l0 (heap-read-u64 (+ base (* 2 8)) 0))) . l0))
(seq
 (((nil setq l1 (heap-read-u64 (+ base (* 2 8)) 0))) . l1))
(if
 (((nil if (if (= l0 3) 0 (if (/= (logand l0 7) 0) 1 (nc_truthy_slow l0))) (setq t0 (heap-read-u64 (+ base (* 1 8)) 0)) (setq t0 (heap-read-u64 (+ base (* 2 8)) 0)))) . t0))
(if-fallible
 (((t if (if (= l0 3) 0 (if (/= (logand l0 7) 0) 1 (nc_truthy_slow l0))) (seq (setq st (wf_bytecode_w_varref env (heap-read-u64 (+ base (* 1 8)) 0) mem 0)) (if (= st 0) (seq (setq t0 (ptr-read-u64 mem 0)) (setq t1 t0)) 0)) (setq t1 (heap-read-u64 (+ base (* 2 8)) 0)))) . t1))
(and
 (((nil if (if (= l0 3) 0 (if (/= (logand l0 7) 0) 1 (nc_truthy_slow l0))) (seq (if (if (= l1 3) 0 (if (/= (logand l1 7) 0) 1 (nc_truthy_slow l1))) (setq t0 (heap-read-u64 (+ base (* 2 8)) 0)) (setq t0 3)) (setq t1 t0)) (setq t1 3))) . t1))
(or
 (((nil setq t0 l0) (nil if (if (= t0 3) 0 (if (/= (logand t0 7) 0) 1 (nc_truthy_slow t0))) (setq t3 t0) (seq (setq t1 l1) (if (if (= t1 3) 0 (if (/= (logand t1 7) 0) 1 (nc_truthy_slow t1))) (setq t2 t1) (setq t2 (heap-read-u64 (+ base (* 2 8)) 0))) (setq t3 t2)))) . t3))
(let-store
 (((nil setq l0 (heap-read-u64 (+ base (* 1 8)) 0))) . l0))
(let-varbind
 (((t setq st (wf_bytecode_varbind env (heap-read-u64 (+ base (* 5 8)) 0) (heap-read-u64 (+ base (* 1 8)) 0))) (nil setq nb (+ nb 1)) (nil setq t0 l0) (nil seq (wf_bytecode_unbind (+ env 32) 1) (setq nb (- nb 1)))) . t0))
(while
 (((nil while (if (= st 0) (if (= st 0) (if (= l0 3) 0 (if (/= (logand l0 7) 0) 1 (nc_truthy_slow l0))) 0) 0) (seq (setq l1 (heap-read-u64 (+ base (* 2 8)) 0)) (if (< (heap-read-u64 (data-addr nl_gc_until) 0) 0) (wf_bytecode_poll) 0)))) . 3))
(call
 (((t setq st (nc_call1 env (heap-read-u64 (+ base (* 1 8)) 0) mem oslot l0)) (nil setq t0 (ptr-read-u64 mem 0))) . t0))
(call-eq
 (((nil setq t0 (if (= l0 l1) 7 (if (= (logand (logand l0 l1) 1) 1) 3 (if (if (= (logand l0 7) 2) (= (logand l1 7) 2) 0) 3 (if (= (wf_bytecode_w_eqw l0 l1) 1) 7 3)))))) . t0))
(call-car
 (((nil setq t0 (if (= (logand l0 7) 2) (ptr-read-u64 (- l0 2) 0) (if (= l0 3) 3 0))) (t if (= t0 0) (seq (setq st (nc_call1 env (heap-read-u64 (+ base (* 1 8)) 0) mem oslot l0)) (if (= st 0) (seq (setq t1 (ptr-read-u64 mem 0)) (setq t0 t1)) 0)) 0)) . t0))
(inline
 (((nil if (if (= (logand l0 3) 1) (= (logand l1 3) 1) 0) (seq (setq t0 (+ (sar l0 2) (sar l1 2))) (if (= (sar (shl t0 2) 2) t0) (setq t0 (+ (shl t0 2) 1)) (setq t0 -1))) (setq t0 -1)) (t if (= t0 -1) (seq (setq st (nc_arith env (heap-read-u64 (+ base (* 2 8)) 0) mem oslot 516 l0 l1)) (if (= st 0) (setq t0 (ptr-read-u64 mem 0)) 0)) 0)) . t0))
(catch
 (((t seq (nc_catch_push env (+ mem (* (+ cbase 0) 8)) (heap-read-u64 (+ base (* 1 8)) 0)) (setq t0 l0) (nc_catch_pop env (+ mem (* (+ cbase 0) 8))) (if (> st 0) (if (= (nc_catch_match (+ mem (* (+ cbase 0) 8))) 1) (seq (setq nb (nc_unbind_to env nb 0)) (setq t0 (nl_ct_caught_word (nl_exit_val_w) 0)) (setq st 0)) 0) 0))) . t0))
(throw
 (((t setq st (nl_ct_throw_words env (heap-read-u64 (+ base (* 1 8)) 0) (heap-read-u64 (+ base (* 2 8)) 0)))) . 3))
(condition-case
 (((t seq (setq t0 l0) (if (= st 0) 0 (if (> st 0) (if (= (nc_cc_match env (heap-read-u64 (+ base (* 7 8)) 0)) 1) (seq (setq nb (nc_unbind_to env nb 0)) (setq st 0) (m5_fx_exit_condition 0 0) (setq t0 (heap-read-u64 (+ base (* 1 8)) 0))) 0) 0)))) . t0))
(unwind-protect
 (((t seq (setq t0 l0) (if (> st 0) (seq (setq t1 (+ (shl st 2) 1)) (setq t2 (+ (shl (heap-read-u64 268435472 0) 2) 1)) (setq t3 (nl_exit_tag_w)) (setq t4 (nl_exit_val_w)) (setq nb (nc_unbind_to env nb 0)) (setq st 0)) (setq t1 1)) 0 (if (= st 0) (if (= t1 1) 0 (seq (nc_exit_put t3 t4 (sar t2 2)) (setq st (sar t1 2)))) 0))) . t0))
    )
  "Entries are (NAME LOWERED), captured before the effect-table change.")

(defun nfne--run (c)
  "Lower case C with fresh lowering state; return the lowered pair."
  (let ((nelisp-cc-fxir-native--temps nil)
        (nelisp-cc-fxir-native--callees (copy-sequence (nth 2 c)))
        (nelisp-cc-fxir-native--maxargc 0) (nelisp-cc-fxir-native--nbd 0)
        (nelisp-cc-fxir-native--ncatch 0) (nelisp-cc-fxir-native--uses-window nil)
        (nelisp-cc-fxir-native--inline-sites nil)
        (nelisp-cc-fxir-native--inline-budget 0)
        (nelisp-cc-fxir-native--inline-count 0)
        (nelisp-cc-fxir-native--direct-ids nil))
    (nelisp-cc-fxir-native--lower (nth 1 c))))

(ert-deftest nfne-golden-lowering ()
  (dolist (c nfne-cases)
    (should (equal (nfne--run c) (cadr (assq (car c) nfne-golden))))))

(ert-deftest nfne-effects-consistent ()
  (dolist (c nfne-cases)
    (should (nelisp-cc-fxir-native--effects-consistent-p (nth 1 c) (nfne--run c)))))

(ert-deftest nfne-op-fallible-p ()
  (should-not (nelisp-cc-fxir-native--op-fallible-p :const))
  (should-not (nelisp-cc-fxir-native--op-fallible-p :store))
  (should (nelisp-cc-fxir-native--op-fallible-p :call))
  (should (nelisp-cc-fxir-native--op-fallible-p :varbind))
  (should (nelisp-cc-fxir-native--op-fallible-p :throw))
  (should (nelisp-cc-fxir-native--op-fallible-p :no-such-op)))

(ert-deftest nfne-consistency-detects-violations ()
  ;; A non-fallible op whose lowering carries a fallible statement.
  (should-not (nelisp-cc-fxir-native--effects-consistent-p
               '(:const 1) '(((t setq st 0)) . 3)))
  ;; An unknown op is never consistent.
  (should-not (nelisp-cc-fxir-native--effects-consistent-p '(:bogus 1) '(nil . 3))))

;; Doc 231 stage 1/2: callee-refined effects and the static call census.
;; Census goldens were captured from the current lowering.

(defconst nfne-census-cases
  '((call-generic (:call 1 1 (:slot 0)) (foo)
     (:stmts 2 :fallible 1 :call-outs 1 :guards 0 :temps 1 :forms-size 24))
    (call-eq (:call 1 2 (:slot 0) (:slot 1)) (eq)
     (:stmts 1 :fallible 0 :call-outs 0 :guards 5 :temps 1 :forms-size 53))
    (call-car (:call 1 1 (:slot 0)) (car)
     (:stmts 2 :fallible 1 :call-outs 1 :guards 3 :temps 2 :forms-size 73))
    (call-nested (:call 1 1 (:call 2 1 (:slot 0))) (g f)
     (:stmts 4 :fallible 2 :call-outs 2 :guards 0 :temps 2 :forms-size 48))
    (call-two-args (:call 1 2 (:call 2 0) (:call 3 0)) (h a b)
     (:stmts 7 :fallible 3 :call-outs 3 :guards 0 :temps 4 :forms-size 74))
    (call-eq-nested (:call 1 2 (:call 2 1 (:slot 0)) (:slot 1)) (eq f)
     (:stmts 3 :fallible 1 :call-outs 1 :guards 5 :temps 2 :forms-size 77)))
  "Entries are (NAME NODE CALLEES CENSUS).")

(ert-deftest nfne-callee-table-has-inline-arms ()
  (dolist (e nelisp-cc-fxir-effects-callee-table)
    (let* ((arity (plist-get (cdr e) :arity))
           (vals (cl-loop for i below arity collect (intern (format "v%d" i))))
           (fast (nelisp-cc-fxir-native--inline-fast (car e) vals)))
      (should fast)
      (should-not (nelisp-cc-fxir-native--inline-fast (car e) (cons 'x vals)))
      ;; :fast-path-pure without an out-of-line helper, except `eq'
      ;; whose total fast path is the recorded non-fallible arm.
      (unless (eq (car e) 'eq)
        (should-not (cl-some (lambda (s) (and (symbolp s) (string-match-p "\\`\\(nc\\|wf\\)_" (symbol-name s))))
                             (flatten-tree fast)))))))

(ert-deftest nfne-callee-refined-class ()
  (should-not (nelisp-cc-fxir-native--call-fallible-p 'eq))
  (should (nelisp-cc-fxir-native--call-fallible-p 'car))
  (should (nelisp-cc-fxir-native--call-fallible-p 'unknown-fn))
  (should (equal (nelisp-cc-fxir-effects-call-of 'unknown-fn)
                 (nelisp-cc-fxir-effects-of :call))))

(ert-deftest nfne-census-golden ()
  (dolist (c nfne-census-cases)
    (should (equal (nelisp-cc-fxir-native-call-census (nfne--run c)) (nth 3 c)))))

(ert-deftest nfne-census-consistency ()
  (dolist (c nfne-census-cases)
    (should (nelisp-cc-fxir-native--effects-consistent-p
             (nth 1 c) (nfne--run c) (car (nth 2 c))))))

(ert-deftest nfne-call-argument-order ()
  ;; Doc 231: each argument form is evaluated once, left to right.  The
  ;; generic call (site 1) must come after the two argument calls (sites 2
  ;; then 3) and consume their results in that order.
  (let* ((stmts (car (nfne--run (assq 'call-two-args nfne-census-cases))))
         (outs (cl-remove-if-not (lambda (s) (eq (cadr s) 'setq)) stmts))
         (sites nil) (final nil))
    (dolist (s stmts)
      (let ((f (cdr s)))
        (when (and (eq (car f) 'setq) (eq (cadr f) 'st))
          (push (nth 1 (nth 2 (nth 1 (nth 2 (nth 2 f))))) sites)
          (setq final (nth 2 f)))))
    (should outs)
    (should (equal (nreverse sites) '(2 3 1)))
    ;; nc_call2 takes (t2 t1): t2 holds the FIRST argument call's result.
    (should (equal (last final 2) '(t2 t1)))
    (should (equal (cdr (nth 2 stmts)) '(setq t2 t0)))))

(ert-deftest nfne-census-miscount-detected ()
  ;; Guard against a census that ignores the guard tests.
  (should (> (plist-get (nelisp-cc-fxir-native-call-census
                         (nfne--run (assq 'call-eq nfne-cases)))
                        :guards)
             0)))

(provide 'nelisp-cc-fxir-native-effects-test)
;;; nelisp-cc-fxir-native-effects-test.el ends here
