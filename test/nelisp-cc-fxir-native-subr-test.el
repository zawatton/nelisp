;;; nelisp-cc-fxir-native-subr-test.el --- guarded direct subr lowering -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; A :call to a known subr with a directcall id (gethash, puthash, make-vector,
;; make-string, vector, list, concat, substring) stages the evaluated argument
;; words in the window, tests the live function cell (Subr header id byte =
;; directcall id + 17), and on success calls `nc_dsubr_call' (the runtime's
;; `nl_directcall_invoke' entry); otherwise the generic `nc_call' runs on the
;; SAME staged words.  The lowered dialect is evaluated against a fake heap.

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

(defconst nfns-cases
  ;; (NAME NODE CALLEES HEADER-ID)
  '((gethash (:call 1 2 (:slot 0) (:slot 1)) (gethash) 118)
    (gethash3 (:call 1 3 (:slot 0) (:slot 1) (:slot 2)) (gethash) 118)
    (puthash (:call 1 3 (:slot 0) (:slot 1) (:slot 2)) (puthash) 119)
    (make-vector (:call 1 2 (:slot 0) (:slot 1)) (make-vector) 130)
    (make-string (:call 1 2 (:slot 0) (:slot 1)) (make-string) 134)
    (vector (:call 1 2 (:slot 0) (:slot 1)) (vector) 129)
    (list (:call 1 2 (:slot 0) (:slot 1)) (list) 131)
    (concat (:call 1 2 (:slot 0) (:slot 1)) (concat) 132)
    (substring (:call 1 3 (:slot 0) (:slot 1) (:slot 2)) (substring) 133)
    (concat-nested (:call 1 2 (:call 2 1 (:slot 0)) (:slot 1)) (concat f) 132)))

(defun nfns--run (c)
  (let ((nelisp-cc-fxir-native--temps nil)
        (nelisp-cc-fxir-native--callees (copy-sequence (nth 2 c)))
        (nelisp-cc-fxir-native--maxargc 0) (nelisp-cc-fxir-native--nbd 0)
        (nelisp-cc-fxir-native--ncatch 0) (nelisp-cc-fxir-native--uses-window nil)
        (nelisp-cc-fxir-native--inline-sites nil)
        (nelisp-cc-fxir-native--inline-budget 0)
        (nelisp-cc-fxir-native--inline-count 0)
        (nelisp-cc-fxir-native--direct-ids nil))
    (nelisp-cc-fxir-native--lower (nth 1 c))))

(defun nfns--find (form head)
  "All sub-forms of FORM whose car is HEAD."
  (let (acc)
    (cl-labels ((walk (x) (when (consp x)
                            (when (eq (car x) head) (push x acc))
                            (walk (car x)) (walk (cdr x)))))
      (walk form))
    (nreverse acc)))

(ert-deftest nfns-lowering-shape ()
  (dolist (c nfns-cases)
    (let* ((low (nfns--run c)) (stmts (car low)) (all (list stmts (cdr low))))
      ;; Exactly one guarded direct call and one generic call, on the fallback edge.
      (should (= 1 (length (nfns--find all 'nc_dsubr_call))))
      (should (= 1 (length (nfns--find all 'nc_call))))
      (unless (eq (car c) 'concat-nested)
        (should-not (nfns--find all 'nc_call1)))
      (should-not (nfns--find all 'nc_call2))
      (should-not (nfns--find all 'nc_call3))
      ;; The guard compares the header id byte with directcall id + 17.
      (should (member `(= (logand (shr (ptr-read-u64 (- lgfn 6) 0) 8) 255) ,(nth 3 c))
                      (nfns--find all '=)))
      ;; Both calls sit in the arms of the same `if' whose test is the guard.
      (let ((ifs (cl-remove-if-not
                  (lambda (f) (and (nfns--find (nth 2 f) 'nc_dsubr_call)
                                   (nfns--find (nth 3 f) 'nc_call)))
                  (nfns--find all 'if))))
        (should (= 1 (length ifs))))
      (should (nelisp-cc-fxir-native--effects-consistent-p
               (nth 1 c) low (car (nth 2 c)))))))

(ert-deftest nfns-census ()
  (dolist (c nfns-cases)
    (let ((cen (nelisp-cc-fxir-native-call-census (nfns--run c))))
      ;; direct primitive entry + generic fallback; nested case adds its own nc_call1.
      (should (= (plist-get cen :call-outs) (if (eq (car c) 'concat-nested) 3 2)))
      (should (plist-get cen :guards)))))

(ert-deftest nfns-unsupported-argc-stays-generic ()
  ;; gethash with 1 argument is outside the lowered range: plain nc_call1.
  (let* ((c '(g (:call 1 1 (:slot 0)) (gethash) 118))
         (low (nfns--run c)))
    (should-not (nfns--find (list (car low)) 'nc_dsubr_call))
    (should (nfns--find (list (car low)) 'nc_call1))))

(ert-deftest nfns-effects-table ()
  (dolist (e nelisp-cc-fxir-effects-direct-subr-table)
    (should (integerp (plist-get (cdr e) :id)))
    (should (plist-get (cdr e) :may-signal)))
  (should (plist-get (cdr (assq 'gethash nelisp-cc-fxir-effects-direct-subr-table)) :may-call-lisp))
  ;; call-of stays the plain conservative :call class.
  (dolist (e nelisp-cc-fxir-effects-direct-subr-table)
    (should (equal (nelisp-cc-fxir-effects-call-of (car e)) (nelisp-cc-fxir-effects-of :call))))
  (should-not (nelisp-cc-fxir-effects-direct-subr 'gethash 1))
  (should (nelisp-cc-fxir-effects-direct-subr 'gethash 3)))

;; --- Evaluator -------------------------------------------------------------

(defvar nfns--mem nil)
(defvar nfns--evals nil)
(defvar nfns--direct nil)
(defvar nfns--generic nil)

(defun nfns--true (v) (and v (not (eql v 0))))
(defun nfns--xform (f)
  (cond ((not (consp f)) f)
        ((eq (car f) 'if)
         `(if (nfns--true ,(nfns--xform (nth 1 f))) ,(nfns--xform (nth 2 f))
            ,@(mapcar #'nfns--xform (nthcdr 3 f))))
        (t (cons (nfns--xform (car f)) (nfns--xform (cdr f))))))

(defun nfns--eval (case slots hdr)
  "Evaluate CASE with SLOTS; HDR is the Subr header word (nil: cell is 0).
Answers (VALUE DIRECT-ARGS GENERIC-ARGS)."
  (let* ((c (assq case nfns-cases))
         (low (nfns--run c))
         (nfns--mem (make-hash-table))
         (nfns--direct nil) (nfns--generic nil)
         (binds (append (cl-loop for i from 0 for s in slots collect (list (intern (format "l%d" i)) s))
                        (cl-loop for i below 6 collect (list (intern (format "t%d" i)) 0))
                        '((st 0) (base 1000) (mem 5000) (env 0) (oslot 0) (nb 0)))))
    (puthash 1008 12006 nfns--mem) (puthash 12000 8 nfns--mem) (puthash 12008 13006 nfns--mem)
    (puthash 13000 4 nfns--mem) (puthash 13024 (if hdr 14006 0) nfns--mem)
    (when hdr (puthash 14000 hdr nfns--mem))
    (cl-flet ((args (n) (cl-loop for i below n collect (gethash (+ 5000 24 (* 8 i)) nfns--mem))))
      (cl-letf (((symbol-function 'ptr-read-u64) (lambda (a o) (gethash (+ a o) nfns--mem 0)))
                ((symbol-function 'heap-read-u64) (lambda (a o) (gethash (+ a o) nfns--mem 0)))
                ((symbol-function 'ptr-write-u64) (lambda (a o v) (puthash (+ a o) v nfns--mem)))
                ((symbol-function 'shr) (lambda (v n) (ash (logand v #xFFFFFFFFFFFFFFFF) (- n))))
                ((symbol-function 'seq) (cons 'macro (lambda (&rest b) (cons 'progn b))))
                ((symbol-function 'nc_call1) (lambda (&rest a) (puthash 5000 (+ 4 1) nfns--mem) 0))
                ((symbol-function 'nc_dsubr_call)
                 (lambda (_e _s _stk argc &rest _) (push (args argc) nfns--direct)
                   (puthash 5000 111 nfns--mem) 0))
                ((symbol-function 'nc_call)
                 (lambda (_e _s _stk argc &rest _) (push (args argc) nfns--generic)
                   (puthash 5000 222 nfns--mem) 0)))
        (let ((v (eval (nfns--xform `(let ,binds ,@(mapcar #'cdr (car low)) ,(cdr low))) nil)))
          (list v (reverse nfns--direct) (reverse nfns--generic)))))))

(ert-deftest nfns-guard-ok-direct ()
  (dolist (c nfns-cases)
    (unless (eq (car c) 'concat-nested)
      (let* ((n (nth 2 (nth 1 c)))
             (slots (cl-loop for i below n collect (+ 100 (* 4 i))))
             (r (nfns--eval (car c) slots (logior 20 (ash (nth 3 c) 8)))))
        (should (equal (car r) 111))
        (should (equal (nth 1 r) (list slots)))
        (should-not (nth 2 r))))))

(ert-deftest nfns-guard-invalidated-generic-same-args ()
  (dolist (hdr (list nil (logior 20 (ash 99 8)) (logior 4 (ash 118 8))))
    (dolist (c nfns-cases)
      (unless (eq (car c) 'concat-nested)
        (let* ((n (nth 2 (nth 1 c)))
               (slots (cl-loop for i below n collect (+ 100 (* 4 i))))
               (r (nfns--eval (car c) slots hdr)))
          (should (equal (car r) 222))
          (should-not (nth 1 r))
          (should (equal (nth 2 r) (list slots))))))))

(ert-deftest nfns-args-evaluated-once-in-order ()
  ;; The nested call (nc_call1) is a top-level statement before the staging, once.
  (let* ((low (nfns--run (assq 'concat-nested nfns-cases)))
         (calls (nfns--find (car low) 'nc_call1)))
    (should (= 1 (length calls)))
    (should (= 1 (length (nfns--find (car low) 'nc_dsubr_call))))
    ;; The nested call is the first statement; the window then receives its
    ;; held result (t0) before slot 1, each written exactly once.
    (should (eq (car (nth 3 (car (car low)))) 'nc_call1))
    (let ((writes (nfns--find (car low) 'ptr-write-u64)))
      (should (equal writes '((ptr-write-u64 (+ mem (* 3 8)) 0 t0)
                              (ptr-write-u64 (+ mem (* 4 8)) 0 l1)))))))

(provide 'nelisp-cc-fxir-native-subr-test)
;;; nelisp-cc-fxir-native-subr-test.el ends here
