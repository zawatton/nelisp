;;; nelisp-bytecode-frame-ir.el --- Stack-slot CFG for GNU byte-code -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Build a non-executing, stack-slot control-flow representation from the
;; structural byte-code decoder. This module does not lower Lisp expressions.

;;; Code:

(require 'cl-lib)
(require 'nelisp-bytecode-ir)

(defun nelisp-bytecode-frame-ir--result (status reason &optional blocks max-depth)
  (list :status status :reason reason :blocks blocks :max-stack-depth max-depth))

(defun nelisp-bytecode-frame-ir--unsupported (reason)
  (nelisp-bytecode-frame-ir--result 'unsupported reason))

(defun nelisp-bytecode-frame-ir--min-inputs (row)
  "Return explicit input count for ROW, or nil for unsupported semantics."
  (let ((op (aref row 1)) (metadata (aref row 4)))
    (cond
     ((or (= op 129) (<= 192 op 255) (<= 1 op 5) (= op 130)) 0)
     ((memq op '(131 132 133 134 135 136 83 84 91 57 58 59 60 63 64 65)) 1)
     ((or (memq op '(61 85 86 87 88 89 90 92 95))) 2)
     ((and (<= 8 op 15) (eq (plist-get metadata :kind) 'variable-ref)) 0)
     ((and (<= 16 op 23) (eq (plist-get metadata :kind) 'variable-set)) 1)
     ((and (<= 32 op 39) (eq (plist-get metadata :kind) 'call))
      (1+ (or (aref row 3) (logand op 7))))
     ((= op 137) 1)
     (t nil))))

(defun nelisp-bytecode-frame-ir--simple-kind (row)
  (let ((op (aref row 1)) (kind (plist-get (aref row 4) :kind)))
    (cond
     ((eq kind 'constant) 'constant)
     ((eq kind 'stack-ref) 'stack-ref)
     ((eq kind 'variable-ref) 'variable-ref)
     ((eq kind 'variable-set) 'variable-set)
     ((eq kind 'call) 'call)
     ((memq op '(130 131 132 133 134)) 'branch)
     ((= op 135) 'return)
     ((= op 136) 'discard)
     ((= op 137) 'dup)
     ((memq op '(57 58 59 60)) 'predicate)
     ((memq op '(61 63 64 65 83 84 85 86 87 88 89 90 91 92 95)) 'primitive)
     (t nil))))

(defun nelisp-bytecode-frame-ir--block-successors (rows)
  (let* ((last-row (car (last rows)))
         (op (aref last-row 1)) (next (aref last-row 2))
         (target (aref last-row 3)))
    (cond
     ((= op 135) nil)
     ((= op 130) (list (cons target 'goto)))
     ((memq op '(131 132 133 134))
      (list (cons next 'fallthrough) (cons target 'taken)))
     (t (list (cons next 'fallthrough))))))

(defun nelisp-bytecode-frame-ir--emit-block (start rows entry-depth block-depths)
  (let ((stack (cl-loop for i below entry-depth collect (list :entry start i)))
        (instructions nil) (failure nil) special-taken-stack)
    (dolist (row rows)
      (unless failure
        (let* ((pc (aref row 0)) (op (aref row 1))
               (kind (nelisp-bytecode-frame-ir--simple-kind row))
               (need (nelisp-bytecode-frame-ir--min-inputs row))
               (depth (length stack)) (inputs nil) (outputs nil)
               (out (list :value pc 0)))
          (cond
           ((null kind) (setq failure (format "unsupported opcode %d at %d" op pc)))
           ((null need) (setq failure (format "no stack transfer for opcode %d at %d" op pc)))
           ((< depth need) (setq failure (format "operand underflow at %d" pc)))
           (t
            (setq inputs (if (= need 0) nil (last stack need)))
            (pcase kind
              ((or 'constant 'variable-ref 'stack-ref)
               (when (eq kind 'stack-ref)
                 (let ((offset (or (plist-get (aref row 4) :stack-offset) op)))
                   (if (>= offset depth)
                       (setq failure (format "stack reference outside depth at %d" pc))
                     (setq inputs (list (nth (- depth 1 offset) stack))))))
               (unless failure
                 (setq outputs (list out) stack (append stack (list out)))))
              ('dup
               (setq inputs (list (car (last stack))) outputs (list out)
                     stack (append stack (list out))))
              ('discard (setq stack (butlast stack)))
              ('variable-set (setq stack (butlast stack)))
              ('call
               (setq outputs (list out)
                     stack (append (butlast stack need) (list out))))
              ('primitive
               (setq outputs (list out)
                     stack (append (butlast stack need) (list out))))
              ('predicate (setq outputs (list out)
                                stack (append (butlast stack) (list out))))
              ('return (setq stack (butlast stack)))
              ('branch
               (when (memq op '(133 134)) (setq special-taken-stack stack))
               (when (memq op '(131 132 133 134)) (setq stack (butlast stack))))
              (_ (setq failure (format "unsupported transfer for opcode %d" op))))
            (unless failure
              (push (list :pc pc :opcode op :kind kind :operand (aref row 3)
                          :constant-index
                          (or (plist-get (aref row 4) :constant-index)
                              (and (memq kind '(variable-ref variable-set))
                                   (aref row 3)))
                          :inputs inputs :outputs outputs)
                    instructions)))))))
    (if failure
        (cons nil failure)
      (let* ((last-row (car (last rows)))
             (op (aref last-row 1))
             (successors
              (mapcar
               (lambda (edge)
                 (let* ((target (car edge))
                        (edge-stack
                        (if (and (memq op '(133 134)) (eq (cdr edge) 'taken))
                             special-taken-stack stack)))
                   (list :target target :kind (cdr edge)
                         :slots (vconcat edge-stack)
                         :target-slots
                         (vconcat (cl-loop for i below (or (cdr (assq target block-depths)) 0)
                                           collect (list :entry target i))))))
               (nelisp-bytecode-frame-ir--block-successors rows))))
        (cons (list :start start :entry-stack-depth entry-depth
                    :instructions (vconcat (nreverse instructions))
                    :successors (vconcat successors)) nil)))))

(defun nelisp-bytecode-frame-ir-build (code constants &optional initial-depth)
  "Build stack-slot CFG for CODE and CONSTANTS without executing byte-code.

Return a plist with :status `complete', `malformed', or `unsupported'. Calls
remain explicit effectful instructions. Constant instructions retain indexes
into CONSTANTS, preserving object identity for the eventual gateway."
  (let* ((initial-depth (or initial-depth 0))
         (decoded (nelisp-bytecode-ir-decode-result code constants))
         (validation (nelisp-bytecode-ir-validate code constants initial-depth))
         (rows (plist-get validation :instructions))
         (analysis (plist-get validation :stack-analysis))
         (depths (plist-get analysis :depths))
         (table (nelisp-bytecode-ir--instruction-table rows))
         (leaders (list 0)) (failure nil) (unsupported nil) blocks)
    (cond
     ((eq (plist-get validation :status) 'malformed)
      (if (and (memq (plist-get decoded :status) '(valid unsupported))
               (cl-some (lambda (row) (memq (aref row 1) '(48 49 50 183))) rows)
               (not (nelisp-bytecode-ir-validate-targets rows (length code))))
          (nelisp-bytecode-frame-ir--unsupported "handler or table-driven transfer")
        (nelisp-bytecode-frame-ir--result 'malformed (plist-get validation :reason))))
     ((not (eq (plist-get analysis :status) 'complete))
      (nelisp-bytecode-frame-ir--unsupported (or (plist-get analysis :reason)
                                                  "incomplete stack analysis")))
     (t
      ;; Only semantics with an explicit frame transfer are admitted. Handler
      ;; control and table switches stay unsupported even if structurally valid.
      (dotimes (i (length rows))
        (unless (nelisp-bytecode-frame-ir--simple-kind (aref rows i))
          (setq unsupported (format "unsupported opcode %d at %d"
                                    (aref (aref rows i) 1)
                                    (aref (aref rows i) 0)))))
      (if unsupported
          (nelisp-bytecode-frame-ir--unsupported unsupported)
        (dotimes (i (length rows))
          (let* ((row (aref rows i)) (op (aref row 1))
                 (next (aref row 2)) (target (aref row 3)))
            (when (memq op '(130 131 132 133 134))
              (push target leaders)
              (when (memq op '(131 132 133 134)) (push next leaders)))))
        (setq leaders (sort (delete-dups leaders) #'<))
        (let ((ranges nil))
          (dolist (start leaders)
            (when (assq start table)
              (let ((pc start) (items nil) (done nil))
                (while (and (not done) (assq pc table))
                  (let ((row (cdr (assq pc table))))
                    (push row items)
                    (setq pc (aref row 2))
                    (when (or (memq (aref row 1) '(130 131 132 133 134 135))
                              (memq pc leaders))
                      (setq done t))))
                (push (cons start (nreverse items)) ranges))))
          (dolist (range (nreverse ranges))
            (when (assq (car range) depths)
              (let ((emitted
                     (nelisp-bytecode-frame-ir--emit-block
                      (car range) (cdr range) (cdr (assq (car range) depths)) depths)))
                (if (cdr emitted)
                    (setq failure (cdr emitted))
                  (push (car emitted) blocks)))))
          (if failure
              (nelisp-bytecode-frame-ir--result 'malformed failure)
            (nelisp-bytecode-frame-ir--result
             'complete nil (vconcat (nreverse blocks))
             (plist-get analysis :max-depth)))))))))

(provide 'nelisp-bytecode-frame-ir)
;;; nelisp-bytecode-frame-ir.el ends here
