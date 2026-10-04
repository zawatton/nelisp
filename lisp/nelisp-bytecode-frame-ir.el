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

(defconst nelisp-bytecode-frame-ir--operation-effect-schema
  '((variable-bind :operation dynamic-bind :stack-inputs 1 :stack-delta -1
                   :binding-kind bind :may-nonlocal-exit t)
    (unbind :operation dynamic-unbind :stack-inputs 0 :stack-delta 0
            :binding-kind unbind :may-nonlocal-exit t))
  "Frame effects for byte-code operations that alter dynamic binding depth.")

(defun nelisp-bytecode-frame-ir--operation-effect (kind row)
  "Return the explicit frame effect for KIND represented by ROW."
  (let ((schema (cdr (assq kind nelisp-bytecode-frame-ir--operation-effect-schema)))
        (metadata (aref row 4)) (operand (aref row 3))
        (pc (aref row 0)) (opcode (aref row 1)))
    (when schema
      (let ((effect (copy-sequence schema)))
        (setq effect (plist-put effect :pc pc)
              effect (plist-put effect :opcode opcode)
              effect (plist-put effect :exceptional-edge
                                (list :kind 'possible-nonlocal-exit
                                      :target 'unresolved :pc pc)))
        (if (eq kind 'variable-bind)
            (setq effect (plist-put effect :constant-index
                                    (plist-get metadata :constant-index))
                  effect (plist-put effect :binding-delta 1))
          (setq effect (plist-put effect :binding-count operand)
                effect (plist-put effect :binding-delta (- operand))))
        effect))))

(defun nelisp-bytecode-frame-ir--min-inputs (row)
  "Return explicit input count for ROW, or nil for unsupported semantics."
  (let ((op (aref row 1)) (metadata (aref row 4)))
    (cond
     ((or (= op 129) (<= 192 op 255) (<= 1 op 5) (= op 130)) 0)
     ((memq op '(131 132 133 134 135 136 83 84 91 57 58 59 60 63 64 65
                 162 163)) 1)
     ((or (memq op '(61 66 85 86 87 88 89 90 92 95))) 2)
     ((and (<= 8 op 15) (eq (plist-get metadata :kind) 'variable-ref)) 0)
     ((and (<= 16 op 23) (eq (plist-get metadata :kind) 'variable-set)) 1)
     ((memq (plist-get metadata :kind) '(variable-bind unbind))
      (plist-get (cdr (assq (plist-get metadata :kind)
                            nelisp-bytecode-frame-ir--operation-effect-schema))
                 :stack-inputs))
     ((and (<= 32 op 39) (eq (plist-get metadata :kind) 'call))
      (1+ (or (aref row 3) (logand op 7))))
     ((= op 137) 1)
     ((= op 183) 2)
     (t nil))))

(defun nelisp-bytecode-frame-ir--simple-kind (row)
  (let ((op (aref row 1)) (kind (plist-get (aref row 4) :kind)))
    (cond
     ((eq kind 'constant) 'constant)
     ((eq kind 'stack-ref) 'stack-ref)
     ((eq kind 'variable-ref) 'variable-ref)
     ((eq kind 'variable-set) 'variable-set)
     ((eq kind 'variable-bind) 'dynamic-bind)
     ((eq kind 'unbind) 'dynamic-unbind)
     ((eq kind 'call) 'call)
     ((= op 183) 'switch)
     ((memq op '(130 131 132 133 134)) 'branch)
     ((= op 135) 'return)
     ((= op 136) 'discard)
     ((= op 137) 'dup)
     ((memq op '(57 58 59 60)) 'predicate)
     ((memq op '(61 63 64 65 66 83 84 85 86 87 88 89 90 91 92 95
                 162 163)) 'primitive)
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
     ((= op 183)
      (append (list (cons next 'fallthrough))
              (mapcar (lambda (case) (cons (car case) 'switch))
                      (plist-get (aref last-row 4) :switch-cases))))
     (t (list (cons next 'fallthrough))))))

(defun nelisp-bytecode-frame-ir--analyze-switch-stack
    (instructions constants initial-depth)
  "Analyze stack states and constant Bswitch tables to a fixed point.

Return stack depths plus validated table cases. Unknown table provenance is
reported as unsupported; impossible stack states or table targets are invalid."
  (setq instructions (vconcat instructions))
  (let* ((table (nelisp-bytecode-ir--instruction-table instructions))
         (initial-state (cl-loop repeat initial-depth collect :unknown))
         (states (list (cons 0 initial-state))) (depths nil) (switches nil)
         (pending (list (cons 0 initial-state)))
         (maximum initial-depth) (failure nil) (unsupported nil) (steps 0)
         (limit (* 2 (max 1 (length instructions))
                   (max 1 (+ initial-depth (length instructions))))))
    (cl-labels
        ((enqueue (pc stack)
           (let ((insn (cdr (assq pc table))) (old (assq pc states)))
             (cond
              ((not insn)
               (setq failure (format "control flow reaches non-instruction offset %d" pc)))
              ((and old (/= (length (cdr old)) (length stack)))
               (setq failure (format "inconsistent stack depth at %d: %d vs %d"
                                     pc (length (cdr old)) (length stack))))
              ((null old)
               (push (cons pc stack) states)
               (push (cons pc stack) pending))
              (t
               (let ((merged
                      (cl-mapcar (lambda (left right)
                                   (if (equal left right) left :unknown))
                                 (cdr old) stack)))
                 (unless (equal merged (cdr old))
                   (setcdr old merged)
                   (push (cons pc merged) pending)))))))
         (switch-cases (pc table-index)
           (let ((object (and (integerp table-index)
                              (<= 0 table-index)
                              (< table-index (length constants))
                              (aref constants table-index)))
                 (cases nil) (bad nil))
             (if (not (hash-table-p object))
                 (setq unsupported
                       (format "Bswitch table provenance at %d is not a constant hash table" pc))
               (maphash
                (lambda (key target)
                  (if (not (and (integerp target) (assq target table)))
                      (setq bad (format "invalid switch target %S at %d" target pc))
                    (let ((entry (assq target cases)))
                      (if entry
                          (setcdr entry (append (cdr entry) (list key)))
                        (push (cons target (list key)) cases)))))
                object)
               (when bad (setq failure bad)))
             (sort cases (lambda (left right) (< (car left) (car right)))))))
      (when (not (and (integerp initial-depth) (>= initial-depth 0)))
        (setq failure "initial stack depth must be a nonnegative integer"))
      (while (and pending (not failure) (not unsupported) (< steps limit))
        (setq steps (1+ steps))
        (let* ((item (pop pending)) (pc (car item))
               (row (cdr (assq pc table))) (op (aref row 1))
               (next (aref row 2)) (target (aref row 3))
               (state (cdr (assq pc states)))
               (kind (nelisp-bytecode-frame-ir--simple-kind row))
               (need (nelisp-bytecode-frame-ir--min-inputs row))
               (depth (length state)) (after nil) (outputs nil)
               (switch-info nil) (cases nil))
          (setq maximum (max maximum depth))
          (cond
           ((or (null kind) (null need))
            (setq unsupported (format "unsupported stack semantics at %d" pc)))
           ((< depth need)
            (setq failure (format "operand underflow at %d" pc)))
           (t
            (setq after state)
            (pcase kind
              ('constant
               (setq outputs
                     (list (list :constant
                                 (or (plist-get (aref row 4) :constant-index)
                                     (and (= op 129) (aref row 3)))))
                     after (append state outputs)))
              ('stack-ref
               (let ((offset (or (plist-get (aref row 4) :stack-offset)
                                 (and (<= 1 op 5) op))))
                 (if (or (null offset) (>= offset depth))
                     (setq failure (format "stack reference outside depth at %d" pc))
                   (setq after (append state
                                       (list (nth (- depth 1 offset) state)))))))
              ('dup (setq after (append state (list (car (last state))))))
              ((or 'discard 'variable-set 'return)
               (setq after (butlast state need)))
              ((or 'call 'primitive)
               (setq after (append (butlast state need) (list :unknown))))
              ('predicate (setq after (append (butlast state) (list :unknown))))
              ('switch
               (let* ((inputs (last state 2))
                      (table-source (cadr inputs)))
                 (if (not (and (consp table-source)
                               (eq (car table-source) :constant)))
                     (setq unsupported
                           (format "Bswitch table provenance is unknown at %d" pc))
                   (let ((table-index (cadr table-source)))
                     (setq cases (switch-cases pc table-index))
                     (unless failure
                       (if unsupported nil
                         (setq after (butlast state 2)
                               switch-info (list pc table-index cases))))))))
              ('branch
               (when (memq op '(133 134))
                 (setq outputs (list (cons 'taken state))))
               (when (memq op '(131 132 133 134))
                 (setq after (butlast state))))
              (_ (setq unsupported (format "unsupported stack semantics at %d" pc))))
            (when (and (not failure) (not unsupported))
              (setq maximum (max maximum (length after)))
              (when switch-info (push switch-info switches))
              (pcase op
                (135 nil)
                (130 (enqueue target after))
                ((or 131 132)
                 (enqueue target after)
                 (enqueue next after))
                ((or 133 134)
                 (enqueue target state)
                 (enqueue next after))
                (183
                 (unless (assq next table)
                   (setq failure (format "execution falls off bytecode at %d" next)))
                 (unless failure
                   (enqueue next after)
                   (dolist (case cases) (enqueue (car case) after))))
                (_ (if (assq next table)
                       (enqueue next after)
                     (setq failure (format "execution falls off bytecode at %d" next)))))))))))
      (cond
       (failure (list :status 'invalid :reason failure))
       (unsupported (list :status 'unsupported :reason unsupported))
       ((>= steps limit) (list :status 'invalid :reason "switch dataflow step limit exceeded"))
       (t
        (setq depths (mapcar (lambda (entry) (cons (car entry) (length (cdr entry))))
                             states))
        (list :status 'complete :depths depths :switches (nreverse switches)
              :max-depth maximum)))))

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
              ('dynamic-bind (setq stack (butlast stack need)))
              ('dynamic-unbind nil)
              ('call
               (setq outputs (list out)
                     stack (append (butlast stack need) (list out))))
              ('primitive
               (setq outputs (list out)
                     stack (append (butlast stack need) (list out))))
              ('switch
               (setq stack (butlast stack need)))
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
                          :operation-effect
                          (and (memq kind '(dynamic-bind dynamic-unbind))
                               (nelisp-bytecode-frame-ir--operation-effect
                                (plist-get (aref row 4) :kind) row))
                          :inputs inputs :outputs outputs
                          :table-constant-index
                          (plist-get (aref row 4) :switch-table-constant-index))
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
                         :keys (and (eq (cdr edge) 'switch)
                                    (cdr (assq target
                                               (plist-get (aref (car (last rows)) 4)
                                                          :switch-cases))))
                         :slots (vconcat edge-stack)
                         :target-slots
                         (vconcat (cl-loop for i below (or (cdr (assq target block-depths)) 0)
                                           collect (list :entry target i))))))
               (nelisp-bytecode-frame-ir--block-successors rows))))
        (cons (list :start start :entry-stack-depth entry-depth
                    :instructions (vconcat (nreverse instructions))
                    :successors (vconcat successors)) nil)))))

(defun nelisp-bytecode-frame-ir--analyze-binding-depths (blocks)
  "Validate exact dynamic binding depths across CFG BLOCKS."
  (let ((depths (make-hash-table :test 'eql))
        (block-index (make-hash-table :test 'eql)) (pending (list 0))
        (failure nil) (maximum 0) (possible-exit nil))
    (dotimes (i (length blocks))
      (puthash (plist-get (aref blocks i) :start) i block-index))
    (puthash 0 0 depths)
    (while (and pending (not failure))
      (let* ((start (pop pending)) (block (cl-find start blocks
                                                  :key (lambda (item)
                                                         (plist-get item :start))))
             (depth (gethash start depths)) (exceptional nil))
        (unless block (setq failure (format "binding flow reaches missing block %d" start)))
        (when block
          (setq block (copy-sequence block))
          (setq block (plist-put block :entry-binding-depth depth))
          (dolist (insn (append (plist-get block :instructions) nil))
            (let ((effect (plist-get insn :operation-effect)))
              (when effect
                (let ((delta (plist-get effect :binding-delta)))
                  (setq depth (+ depth delta)
                        possible-exit t
                        exceptional (cons (plist-get effect :exceptional-edge)
                                          exceptional))
                  (when (< depth 0)
                    (setq failure (format "dynamic unbind underflow at %d"
                                          (plist-get insn :pc))))
                  (setq maximum (max maximum depth))))))
          (setq block (plist-put block :exit-binding-depth depth)
                block (plist-put block :exceptional-exits (vconcat (nreverse exceptional))))
          (let ((index (gethash start block-index)))
            (when index (aset blocks index block)))
          (unless failure
            (dolist (edge (append (plist-get block :successors) nil))
              (let* ((target (plist-get edge :target)) (known (gethash target depths :unknown)))
                (cond
                 ((eq known :unknown)
                  (puthash target depth depths)
                  (push target pending))
                 ((/= known depth)
                  (setq failure
                        (format "binding depth mismatch at block %d: %d vs %d"
                                target known depth))))))))))
    (if failure
        (list :status 'malformed :reason failure)
      (list :status 'complete :blocks blocks :max-binding-depth maximum
            :possible-nonlocal-exit possible-exit))))

(defun nelisp-bytecode-frame-ir--handler-diagnostic (rows initial-depth)
  "Describe handler structure in ROWS without claiming verified CFG edges.

INITIAL-DEPTH is the incoming operand-stack depth.  Handler targets and
linear binding depths are structural facts; call transfer stack effects remain
unresolved because calls can be redefined at runtime."
  (when (and (cl-some (lambda (row) (memq (aref row 1) '(48 50)))
                      (append rows nil))
             (not (cl-some (lambda (row) (= (aref row 1) 49))
                           (append rows nil))))
    (let ((handlers nil) (binding-depth 0) (max-binding-depth 0)
          (max-handler-depth 0) (stack-depth initial-depth)
          (pushes nil) (possible-transfers nil) (failure nil))
      (dolist (row (append rows nil))
        (let* ((pc (aref row 0)) (op (aref row 1))
               (metadata (aref row 4)) (kind (plist-get metadata :kind))
               (delta (plist-get metadata :stack-delta)))
          (when (numberp delta) (setq stack-depth (+ stack-depth delta)))
          (cond
           ((= op 50)
            (let ((entry (list :pc pc :target (aref row 3)
                               :handler-depth (1+ (length handlers))
                               :binding-depth binding-depth
                               :stack-depth stack-depth)))
              (push entry handlers)
              (push entry pushes)
              (setq max-handler-depth
                    (max max-handler-depth (length handlers)))))
           ((= op 48)
            (if handlers
                (pop handlers)
              (setq failure (format "POP-HANDLER underflow at %d" pc))))
           ((eq kind 'variable-bind)
            (setq binding-depth (1+ binding-depth)
                  max-binding-depth (max max-binding-depth binding-depth)))
           ((eq kind 'unbind)
            (let ((count (aref row 3)))
              (if (> count binding-depth)
                  (setq failure (format "dynamic unbind underflow at %d" pc))
                (setq binding-depth (- binding-depth count)))))
           ((and (eq kind 'call) handlers)
            (push (list :from-pc pc :target (plist-get (car handlers) :target)
                        :handler-depth (length handlers)
                        :binding-depth binding-depth
                        :saved-binding-depth
                        (plist-get (car handlers) :binding-depth)
                        :stack-transfer 'unresolved
                        :binding-transfer 'unresolved)
                  possible-transfers)))))
      (cond
       (failure (list :status 'malformed :reason failure))
       (handlers
        (list :status 'malformed
              :reason (format "PUSH-CATCH at %d has no POP-HANDLER"
                              (plist-get (car handlers) :pc))))
       ((/= binding-depth 0)
        (list :status 'malformed
              :reason (format "dynamic binding depth ends at %d" binding-depth)))
       (t
        (list :status 'unsupported
              :reason "handler snapshot and mutable call-cell transfer contract unresolved"
              :pushes (vconcat (nreverse pushes))
              :possible-nonlocal-transfers (vconcat (nreverse possible-transfers))
              :max-handler-depth max-handler-depth
              :max-binding-depth max-binding-depth
              :stack-transfer 'unresolved))))))

(defun nelisp-bytecode-frame-ir-build (code constants &optional initial-depth)
  "Build stack-slot CFG for CODE and CONSTANTS without executing byte-code.

Return a plist with :status `complete', `malformed', or `unsupported'. Calls
remain explicit effectful instructions. Constant instructions retain indexes
into CONSTANTS, preserving object identity for the eventual gateway. Switch CFG
edges snapshot the current hash-table contents; runtime lowering is explicitly
unsupported until it guards or otherwise accounts for subsequent mutations."
  (let* ((initial-depth (or initial-depth 0))
         (decoded (nelisp-bytecode-ir-decode-result code constants))
         (validation (nelisp-bytecode-ir-validate code constants initial-depth))
         (rows (plist-get validation :instructions))
         (switch-row (cl-find 183 rows :key (lambda (row) (aref row 1))))
         (switch-analysis
          (and switch-row
               (nelisp-bytecode-frame-ir--analyze-switch-stack
                rows constants initial-depth)))
         (analysis (or switch-analysis (plist-get validation :stack-analysis)))
         (depths (plist-get analysis :depths))
         (table (nelisp-bytecode-ir--instruction-table rows))
         (leaders (list 0)) (failure nil) (unsupported nil) blocks)
    (when (and switch-analysis
               (not (eq (plist-get validation :status) 'malformed))
               (eq (plist-get switch-analysis :status) 'invalid))
      (setq validation (plist-put validation :status 'malformed)
            validation (plist-put validation :reason
                                  (plist-get switch-analysis :reason))))
    (when (eq (plist-get analysis :status) 'complete)
      (dolist (switch (plist-get analysis :switches))
        (let* ((row (cdr (assq (car switch) table)))
               (metadata (aref row 4)))
          (setq metadata (plist-put metadata :switch-table-constant-index (cadr switch))
                metadata (plist-put metadata :switch-cases (nth 2 switch)))
          (aset row 4 metadata))))
    (cond
     ((eq (plist-get validation :status) 'malformed)
      (let* ((reason (plist-get validation :reason))
             (stack-reason (plist-get analysis :reason))
             (failure-pc (and (stringp stack-reason)
                              (string-match "\\([0-9]+\\)$" stack-reason)
                              (string-to-number (match-string 1 stack-reason))))
             (failure-row (and failure-pc (cdr (assq failure-pc table))))
             (failure-op (and failure-row (aref failure-row 1))))
        (if (and (not switch-analysis)
                 (memq (plist-get decoded :status) '(valid unsupported))
                 (memq failure-op '(48 49 50 183))
                 (not (nelisp-bytecode-ir-validate-targets rows (length code))))
            (nelisp-bytecode-frame-ir--unsupported
             "handler or table-driven transfer has unknown stack effect")
          (nelisp-bytecode-frame-ir--result 'malformed reason))))
     ((not (eq (plist-get analysis :status) 'complete))
      (let ((handler (nelisp-bytecode-frame-ir--handler-diagnostic rows initial-depth)))
        (cond
         ((and handler (eq (plist-get handler :status) 'malformed))
          (nelisp-bytecode-frame-ir--result 'malformed
                                            (plist-get handler :reason)))
         (handler
          (let ((result
                 (nelisp-bytecode-frame-ir--unsupported
                  (plist-get handler :reason))))
            (plist-put result :handler-control-flow handler)))
         (t
          (nelisp-bytecode-frame-ir--unsupported
           (or (plist-get analysis :reason) "incomplete stack analysis"))))))
     (t
      ;; Only semantics with an explicit frame transfer are admitted. Handler
      ;; control remains unsupported even when its byte shape is valid.
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
              (when (memq op '(131 132 133 134)) (push next leaders)))
            (when (= op 183)
              (push next leaders)
              (dolist (case (plist-get (aref row 4) :switch-cases))
                (push (car case) leaders)))))
        (setq leaders (sort (delete-dups leaders) #'<))
        (let ((ranges nil))
          (dolist (start leaders)
            (when (assq start table)
              (let ((pc start) (items nil) (done nil))
                (while (and (not done) (assq pc table))
                  (let ((row (cdr (assq pc table))))
                    (push row items)
                    (setq pc (aref row 2))
                    (when (or (memq (aref row 1) '(130 131 132 133 134 135 183))
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
            (let* ((binding-analysis
                    (nelisp-bytecode-frame-ir--analyze-binding-depths
                     (vconcat (nreverse blocks))))
                   (result
                    (if (eq (plist-get binding-analysis :status) 'malformed)
                        (nelisp-bytecode-frame-ir--result
                         'malformed (plist-get binding-analysis :reason))
                      (nelisp-bytecode-frame-ir--result
                       'complete nil (plist-get binding-analysis :blocks)
                       (plist-get analysis :max-depth)))))
              (when (eq (plist-get result :status) 'complete)
                (setq result (plist-put result :max-binding-depth
                                        (plist-get binding-analysis :max-binding-depth)))
                (when (plist-get binding-analysis :possible-nonlocal-exit)
                  (setq result
                        (plist-put result :nonlocal-exit-control-flow 'unresolved)))
                (when switch-row
                  (setq result (plist-put result :switch-edges 'constant-table-snapshot)
                        result (plist-put
                                result :runtime-switch-lowering
                                'unsupported-until-mutation-guard))))
              result))))))))

(defun nelisp-bytecode-frame-ir-native-package-dependencies ()
  "Return frame builder identities guarded by native packages."
  '(nelisp-bytecode-frame-ir--result
    nelisp-bytecode-frame-ir--unsupported
    nelisp-bytecode-frame-ir--operation-effect
    nelisp-bytecode-frame-ir--min-inputs
    nelisp-bytecode-frame-ir--simple-kind
    nelisp-bytecode-frame-ir--block-successors
    nelisp-bytecode-frame-ir--analyze-switch-stack
    nelisp-bytecode-frame-ir--emit-block
    nelisp-bytecode-frame-ir--analyze-binding-depths
    nelisp-bytecode-frame-ir--handler-diagnostic
    nelisp-bytecode-frame-ir-build))

(provide 'nelisp-bytecode-frame-ir)
;;; nelisp-bytecode-frame-ir.el ends here
