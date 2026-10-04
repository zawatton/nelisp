;;; nelisp-bytecode-native-call1-layout.el --- pure fixed CALL1 layout check -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Describe one narrowly verified GNU CALL1 frame shape. This grants no
;; lowering, import, artifact, or execution capability.

;;; Code:

(require 'cl-lib)

(defun nelisp-bytecode-native-call1-layout--bounded-plist-p (value)
  "Return non-nil for a finite, duplicate-free plist VALUE of at most 128 pairs."
  (let ((tail value) (cells nil) (keys nil) (count 0) (ok t))
    (while (and ok (consp tail))
      (if (or (memq tail cells) (>= count 128) (not (keywordp (car tail)))
              (memq (car tail) keys) (not (consp (cdr tail))))
          (setq ok nil)
        (push tail cells) (push (car tail) keys)
        (setq tail (cddr tail) count (1+ count))))
    (and ok (null tail))))

(defun nelisp-bytecode-native-call1-layout--token-p (token tag)
  "Check a bounded three-field TOKEN tagged with TAG."
  (and (consp token) (eq (car token) tag)
       (consp (cdr token)) (integerp (cadr token))
       (consp (cddr token)) (integerp (caddr token))
       (null (cdddr token))))

(defun nelisp-bytecode-native-call1-layout (input)
  "Describe the single verified fixed-arity-two CALL1 layout in INPUT."
  (let* ((frame (and (nelisp-bytecode-native-call1-layout--bounded-plist-p input)
                     (plist-get input :frame-result)))
         (blocks (and (nelisp-bytecode-native-call1-layout--bounded-plist-p frame)
                      (eq (plist-get input :status) 'complete)
                      (eq (plist-get frame :status) 'complete)
                      (integerp (plist-get input :argument-count))
                      (= (plist-get input :argument-count) 2)
                      (integerp (plist-get input :argument-min))
                      (= (plist-get input :argument-min) 2)
                      (integerp (plist-get input :argument-max))
                      (= (plist-get input :argument-max) 2)
                      (plist-get frame :blocks)))
         (block (and (vectorp blocks) (= (length blocks) 1) (aref blocks 0)))
         (start (and (nelisp-bytecode-native-call1-layout--bounded-plist-p block)
                     (plist-get block :start)))
         (rows (and (integerp start) (plist-get block :instructions))))
    (if (not (and (vectorp rows) (= (length rows) 4)
                  (cl-every #'nelisp-bytecode-native-call1-layout--bounded-plist-p
                            (append rows nil))))
        (list :status 'unsupported :reason "expected four bounded instruction records")
      (let* ((a (aref rows 0)) (b (aref rows 1))
             (call (aref rows 2)) (ret (aref rows 3))
             (ai (plist-get a :inputs)) (bi (plist-get b :inputs))
             (ao (plist-get a :outputs)) (bo (plist-get b :outputs))
             (ci (plist-get call :inputs)) (co (plist-get call :outputs))
             (ri (plist-get ret :inputs)))
        (if (and (eq (plist-get a :kind) 'stack-ref)
                 (eq (plist-get b :kind) 'stack-ref)
                 (integerp (plist-get a :operand)) (= (plist-get a :operand) 1)
                 (integerp (plist-get b :operand)) (= (plist-get b :operand) 1)
                 (and (consp ai) (null (cdr ai))
                      (nelisp-bytecode-native-call1-layout--token-p (car ai) :entry)
                      (= (cadr (car ai)) start) (= (caddr (car ai)) 0))
                 (and (consp bi) (null (cdr bi))
                      (nelisp-bytecode-native-call1-layout--token-p (car bi) :entry)
                      (= (cadr (car bi)) start) (= (caddr (car bi)) 1))
                 (consp ao) (null (cdr ao)) (consp bo) (null (cdr bo))
                 (nelisp-bytecode-native-call1-layout--token-p
                  (car ao) :value)
                 (nelisp-bytecode-native-call1-layout--token-p
                  (car bo) :value)
                 (eq (plist-get call :kind) 'call)
                 (integerp (plist-get call :opcode))
                 (<= 32 (plist-get call :opcode) 39)
                 (integerp (plist-get call :operand)) (= (plist-get call :operand) 1)
                 (consp ci) (consp (cdr ci)) (null (cddr ci))
                 (nelisp-bytecode-native-call1-layout--token-p (car ci) :value)
                 (nelisp-bytecode-native-call1-layout--token-p (cadr ci) :value)
                 (equal ci (list (car ao) (car bo)))
                 (consp co) (null (cdr co))
                 (nelisp-bytecode-native-call1-layout--token-p
                  (car co) :value)
                 (eq (plist-get ret :kind) 'return)
                 (consp ri) (null (cdr ri))
                 (nelisp-bytecode-native-call1-layout--token-p (car ri) :value)
                 (equal ri co)
                 (null (plist-get ret :outputs)))
            (list :status 'complete :function-root 1 :argument-root 2
                  :result-root 3 :exit-roots '(4 5 6) :exit-root-base 4
                  :next-root 7 :required-root-count 7
                  :provider 'nl_native_call_v2)
          (list :status 'unsupported
                :reason "instruction sequence is not stack-ref1, stack-ref1, CALL1, return"))))))

(provide 'nelisp-bytecode-native-call1-layout)
;;; nelisp-bytecode-native-call1-layout.el ends here
