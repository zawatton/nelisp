;;; nelisp-bytecode-handlers-u8.el --- Handler frame analysis -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; U8a only: a non-executing, bounded handler CFG and a mutable operand-bank
;; reference.  This deliberately does not change native admission or coverage.
;; Root names below are logical roots, to be allocated by the U8b planner.

;;; Code:
(require 'cl-lib)
(require 'nelisp-bytecode-frame-ir)

(defconst nelisp-bytecode-handlers-u8-state-limit 4096
  "Admission limit on distinct path states, never an execution loop limit.")

(defun nelisp-bytecode-handlers-u8--kind (row)
  "Return the handler or existing frame transfer kind for ROW."
  (pcase (aref row 1)
    (48 'handler-pop) (49 'handler-condition) (50 'handler-catch)
    (_ (nelisp-bytecode-frame-ir--simple-kind row))))

(defun nelisp-bytecode-handlers-u8--may-exit-p (kind)
  "Whether KIND requires an exceptional edge before its normal writes."
  (memq kind '(call primitive variable-ref variable-set dynamic-bind
              dynamic-unbind frame-save frame-cleanup handler-condition
              handler-catch)))

(defun nelisp-bytecode-handlers-u8--slots (pc depth)
  "Return DEPTH canonical entry slots for PC."
  (cl-loop for i below depth collect (list :entry pc i)))

(defun nelisp-bytecode-handlers-u8-build (code constants &optional initial-depth)
  "Build U8a handler frames for CODE, CONSTANTS and INITIAL-DEPTH.
Analyze normal and exceptional predecessors together.  A state contains the
operand depth, typed handler chain, and ordered unwind entries.  Distinct
finite handler/unwind states at a join are retained, not collapsed to one
linear snapshot.  A saved depth denotes mutable bank cells, never saved values.
Complete analysis does NOT establish native dispatch, hook ordering or parity."
  (let* ((initial-depth (or initial-depth 0))
         (decoded (nelisp-bytecode-ir-decode-result code constants))
         (rows (plist-get decoded :instructions))
         (table (make-hash-table :test 'eql))
         (states (make-hash-table :test 'eql))
         (depths (make-hash-table :test 'eql))
         (edge-index (make-hash-table :test 'equal))
         (outgoing-index (make-hash-table :test 'eql))
         (incoming-index (make-hash-table :test 'eql))
         (pending nil) (edges nil) (pushes nil) (count 0) (steps 0)
         (maximum initial-depth) (max-handlers 0) (max-unwind 0)
         (failure nil) (unsupported nil))
    (dolist (row (append rows nil)) (puthash (aref row 0) row table))
    (cl-labels
        ((enqueue (pc state)
           (cond
            ((not (gethash pc table))
             (setq failure (format "flow reaches missing instruction %S" pc)))
            ((let ((known (gethash pc depths :absent)))
               (and (not (eq known :absent)) (/= known (car state))))
             (setq failure (format "operand depth mismatch at %d" pc)))
            ((not (member state (gethash pc states)))
             (if (>= count nelisp-bytecode-handlers-u8-state-limit)
                 (setq unsupported "handler state admission limit exceeded")
               (setq count (1+ count))
               (puthash pc (car state) depths)
               (puthash pc (cons state (gethash pc states)) states)
               (push (cons pc state) pending)))))
         (edge (pc target kind state slots &optional handler)
           (let ((item (list :from-pc pc :target target :kind kind
                             :slots (vconcat slots)
                             :target-slots
                             (vconcat (nelisp-bytecode-handlers-u8--slots target (car state)))
                             :handler handler :handler-state (cadr state)
                             :unwind-state (caddr state))))
             (unless (gethash item edge-index)
               (puthash item t edge-index)
               (push item edges)
               (puthash pc (cons item (gethash pc outgoing-index)) outgoing-index)
               (puthash target (cons item (gethash target incoming-index)) incoming-index)))
           (enqueue target state)))
      (cond
       ((eq (plist-get decoded :status) 'malformed)
        (setq failure (plist-get decoded :reason)))
       ((not (and (integerp initial-depth) (>= initial-depth 0)))
        (setq failure "initial stack depth must be a nonnegative integer"))
       ((nelisp-bytecode-ir-validate-targets rows (length code))
        (setq failure (nelisp-bytecode-ir-validate-targets rows (length code))))
       (t (enqueue 0 (list initial-depth nil nil))))
      (while (and pending (not failure) (not unsupported))
        (setq steps (1+ steps))
        (let* ((item (pop pending)) (pc (car item)) (state (cdr item))
               (depth (car state)) (handlers (cadr state)) (unwind (caddr state))
               (row (gethash pc table)) (op (aref row 1))
               (kind (nelisp-bytecode-handlers-u8--kind row))
               (need (if (= op 48) 0 (if (memq op '(49 50)) 1
                                     (nelisp-bytecode-frame-ir--min-inputs row))))
               (delta (plist-get (aref row 4) :stack-delta))
               (after nil) (normal nil) (emitted nil))
          (cond
           ((or (null kind) (null need) (not (integerp delta)) (= op 183))
            (setq unsupported (format "unsupported handler-region transfer at %d" pc)))
           ((< depth need) (setq failure (format "operand underflow at %d" pc)))
           (t
            (setq after (+ depth delta))
            (when (and (eq kind 'stack-ref)
                       (>= (or (plist-get (aref row 4) :stack-offset) op) depth))
              (setq failure (format "stack reference outside depth at %d" pc)))
            ;; Every possible exit sees the PRE-operation bank and handler
            ;; state. Failed calls never publish normal-result bank writes.
            (when (or (nelisp-bytecode-handlers-u8--may-exit-p kind)
                      ;; U1b selects a DFS feedback-edge poll set, whose
                      ;; edges need not point backward in byte offsets.
                      ;; Conservatively retain all branch poll candidates.
                      (memq op '(130 131 132 133 134)))
              (let ((remaining handlers))
                (dolist (handler handlers)
                  (setq remaining (cdr remaining))
                  (let* ((saved (plist-get handler :saved-depth))
                         (target (plist-get handler :target))
                         (slots (append (cl-loop for i below saved collect (list :bank i))
                                        (list (list :caught pc (plist-get handler :pc))))))
                    (edge pc target 'exceptional
                          (list (1+ saved) remaining (plist-get handler :unwind-state))
                          slots handler)))))
            (pcase op
              ((or 49 50)
               (let ((handler (list :pc pc :kind (if (= op 50) 'catch 'condition)
                                    :target (aref row 3) :saved-depth after
                                    :unwind-watermark (length unwind) :unwind-state unwind
                                    :parent (and handlers (plist-get (car handlers) :pc)))))
                 (push handler handlers)
                 (unless (member handler pushes) (push handler pushes))))
              (48 (if handlers (setq handlers (cdr handlers))
                    (setq failure (format "POP-HANDLER underflow at %d" pc)))))
            (pcase kind
              ((or 'dynamic-bind 'frame-save 'frame-cleanup)
               (push (list kind pc) unwind))
              ('dynamic-unbind
               (let ((n (aref row 3)))
                 (if (> n (length unwind))
                     (setq failure (format "dynamic unbind underflow at %d" pc))
                   (setq unwind (nthcdr n unwind))))))
            ;; Discarding a registration's unwind watermark would require
            ;; checked runtime state. Do not manufacture a valid abstract one.
            (when (cl-some (lambda (h) (> (plist-get h :unwind-watermark) (length unwind)))
                           handlers)
              (setq unsupported "unbind crosses active handler watermark"))
            (setq maximum (max maximum depth after)
                  max-handlers (max max-handlers (length handlers))
                  max-unwind (max max-unwind (length unwind)))
            (unless (or failure unsupported)
              (setq normal (list after handlers unwind))
              (if (memq op '(48 49 50))
                  (edge pc (aref row 2) 'fallthrough normal
                        (nelisp-bytecode-handlers-u8--slots pc after))
                (setq emitted (nelisp-bytecode-frame-ir--emit-block pc (list row) depth nil))
                (if (cdr emitted) (setq failure (cdr emitted))
                  (dolist (e (append (plist-get (car emitted) :successors) nil))
                    (edge pc (plist-get e :target) (plist-get e :kind)
                          (list (length (plist-get e :slots)) handlers unwind)
                          (append (plist-get e :slots) nil)))))))))))
      (cond
       (failure (list :status 'malformed :reason failure))
       (unsupported (list :status 'unsupported :reason unsupported))
       (t
        (let ((blocks nil) (phis nil))
          (dolist (row (append rows nil))
            (let* ((pc (aref row 0)) (depth (gethash pc depths)) (op (aref row 1)))
              (when depth
                (let* ((kind (nelisp-bytecode-handlers-u8--kind row))
                       (entry (nelisp-bytecode-handlers-u8--slots pc depth))
                       (instruction
                        (if (memq op '(48 49 50))
                            (list :pc pc :opcode op :kind kind :operand (aref row 3)
                                  :inputs (and (/= op 48) (last entry)) :outputs nil)
                          (aref (plist-get (car (nelisp-bytecode-frame-ir--emit-block
                                                pc (list row) depth nil))
                                            :instructions) 0)))
                       (outgoing (gethash pc outgoing-index))
                       (normal (cl-find-if (lambda (e) (not (eq (plist-get e :kind) 'exceptional))) outgoing))
                       (writes nil))
                  (when normal
                    (cl-loop for slot across (plist-get normal :slots) for i from 0 do
                             (when (member slot (plist-get instruction :outputs))
                               (push (list :slot i :source slot) writes))))
                  (setq instruction (plist-put instruction :bank-writes (nreverse writes)))
                  (push (list :start pc :entry-stack-depth depth
                              :handler-entry-states (copy-tree (gethash pc states))
                              :instructions (vector instruction)
                              :successors (vconcat (cl-remove-if
                                                    (lambda (e) (eq (plist-get e :kind) 'exceptional)) outgoing))
                              :exceptional-successors
                              (vconcat (cl-remove-if-not
                                        (lambda (e) (eq (plist-get e :kind) 'exceptional)) outgoing))) blocks)
                  (dotimes (i depth)
                    (let ((incoming nil) (exceptional nil))
                      (dolist (e (gethash pc incoming-index))
                        (push (list :from-pc (plist-get e :from-pc) :kind (plist-get e :kind)
                                      :handler (plist-get e :handler)
                                      :source (aref (plist-get e :slots) i)) incoming)
                        (when (eq (plist-get e :kind) 'exceptional) (setq exceptional t)))
                      (when exceptional
                        (push (list :block pc :slot i :destination (nth i entry)
                                    :incoming (vconcat (nreverse incoming))) phis))))))))
          (list :status 'complete :version 'handler-frame-u8a-1
                :native-status 'runtime-op-pending :blocks (vconcat (nreverse blocks))
                :edges (vconcat (nreverse edges)) :pushes (vconcat (nreverse pushes))
                :exceptional-phis (vconcat (nreverse phis)) :max-stack-depth maximum
                :max-handler-depth max-handlers :max-binding-depth max-unwind
                :operand-bank (list :count maximum :roots (vconcat (cl-loop for i below maximum
                                                                            collect (list :bank i)))
                                    :restore 'current-cells :publication 'success-only)
                :exit-transport '(:signal 1 :throw 2 :type-error-base 256
                                  :return-base 512 :exit-base 1024 :root-count 3)
                :analysis-states count :analysis-steps steps))))))

(defun nelisp-bytecode-handlers-u8-bank (values capacity)
  "Create reference bank with live VALUES and physical CAPACITY cells."
  (unless (and (integerp capacity) (>= capacity (length values)))
    (error "Invalid operand bank capacity"))
  (let ((cells (make-vector capacity nil)))
    (cl-loop for value in values for i from 0 do (aset cells i value))
    (vector cells (length values) nil)))

(defun nelisp-bytecode-handlers-u8-bank-push (bank value)
  "Publish VALUE into BANK, preserving cells above its current depth."
  (let ((depth (aref bank 1)))
    (when (>= depth (length (aref bank 0))) (error "Operand bank overflow"))
    (aset (aref bank 0) depth value)
    (aset bank 1 (1+ depth)))
  value)

(defun nelisp-bytecode-handlers-u8-bank-pop (bank)
  "Pop BANK without clearing a physical cell needed by a handler."
  (let ((depth (aref bank 1)))
    (when (= depth 0) (error "Operand bank underflow"))
    (aset bank 1 (1- depth))
    (aref (aref bank 0) (1- depth))))

(defun nelisp-bytecode-handlers-u8-bank-stack-set (bank offset)
  "Apply GNU stack-set OFFSET to current cells in BANK."
  (let ((depth (aref bank 1)))
    (unless (and (integerp offset) (>= offset 0) (< offset depth))
      (error "Invalid stack-set offset"))
    (let ((value (aref (aref bank 0) (1- depth))))
      (aset (aref bank 0) (- depth 1 offset) value)
      (aset bank 1 (1- depth)))))

(defun nelisp-bytecode-handlers-u8-bank-register (bank kind selector target watermark)
  "Consume SELECTOR and register a typed reference handler in BANK.
Only structural state is simulated; matching and evaluator visibility are U8b."
  (unless (and (memq kind '(catch condition)) (integerp target) (>= target 0)
               (integerp watermark) (>= watermark 0) (> (aref bank 1) 0))
    (error "Invalid handler registration"))
  (nelisp-bytecode-handlers-u8-bank-pop bank)
  (let ((handler (list :kind kind :selector selector :target target
                       :saved-depth (aref bank 1) :unwind-watermark watermark)))
    (aset bank 2 (cons handler (aref bank 2)))
    handler))

(defun nelisp-bytecode-handlers-u8-bank-land (bank handler value landing-set)
  "Land a preselected HANDLER from BANK using VALUE and canonical LANDING-SET.
Detach crossed handlers and the selected handler before publishing caught VALUE.
Validate first, so refusal cannot partially mutate the reference bank."
  (let ((tail (memq handler (aref bank 2)))
        (saved (plist-get handler :saved-depth))
        (target (plist-get handler :target)))
    (unless (and tail (memq target landing-set) (integerp saved) (>= saved 0)
                 (< saved (length (aref bank 0))))
      (error "Invalid handler landing"))
    (aset bank 2 (cdr tail))
    (aset bank 1 saved)
    (nelisp-bytecode-handlers-u8-bank-push bank value)
    (list :target target :stack (cl-loop for i below (aref bank 1)
                                       collect (aref (aref bank 0) i)))))

(provide 'nelisp-bytecode-handlers-u8)
;;; nelisp-bytecode-handlers-u8.el ends here
