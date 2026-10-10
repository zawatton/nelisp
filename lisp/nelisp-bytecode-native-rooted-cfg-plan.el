;;; nelisp-bytecode-native-rooted-cfg-plan.el --- verified rooted CFG plans -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Plan a deliberately bounded subset of verified GNU byte-code frame IR for
;; a future protected-root native emitter.  This module emits a structural
;; entry AST; it does not compile, publish, map, or execute native code.

;;; Code:

(require 'cl-lib)
(require 'nelisp-bytecode-compiler-input)
(require 'nelisp-bytecode-native-rooted-cfg)
(require 'nelisp-bytecode-native-arithmetic-lowering)
(require 'nelisp-bytecode-native-guarded-lowering)
(require 'nelisp-bytecode-native-call1-layout)
(require 'nelisp-native-funcall-v2)
(require 'nelisp-native-frame-v2)

(require 'nelisp-native-poll)

;; Resolve cl-every's autoload before the planner seals its function cell.
(cl-every #'identity nil)

(let ((guard-owners nil) (guard-context nil) (arithmetic-context nil)
      (source-shapes (make-hash-table :test #'eql))
      (guard-owner-checker (symbol-function 'nelisp-bytecode-native-guarded-lowering-owner-valid-p))
      (lookup (symbol-function 'symbol-function))
      (same (symbol-function 'eq))
      (head (symbol-function 'car)) (tail (symbol-function 'cdr)))
  (cl-labels
      ((pre-copy-valid-p ()
         (condition-case nil (progn (funcall guard-owner-checker) t) (error nil)))
       (provider-source-context-p (value)
         ;; Only the authenticated provider's source slot is syntax data.
         ;; A native FUNCTIONP may classify a (builtin ...) binding as callable.
         (and (vectorp value) (= (length value) 16)
              (funcall same (aref value 0)
                       (funcall lookup 'nelisp-native-arithmetic-v2-source))
              (funcall same (aref value 3)
                       (funcall lookup 'nelisp-native-arithmetic-v2-dependency-context))
              (funcall same (aref value 10)
                       (funcall lookup 'nelisp-native-arithmetic-v2-plus-body))
              (funcall same (aref value 12)
                       (funcall lookup 'nelisp-native-arithmetic-v2-direct-source))
              (funcall same (aref value 13)
                       (funcall lookup 'nelisp-native-arithmetic-v2-direct-runtime-imports))))
       (source-slot-data-p (value index)
         (or (and (memq index '(7 11 14)) (provider-source-context-p value))
             (and (= index 2) (vectorp value) (= (length value) 4)
                  (provider-source-context-p (aref value 1))
                  (let ((owners (aref value 0)))
                    (and (consp owners) (consp (funcall tail owners))
                         (funcall same (funcall head (funcall tail owners))
                                  (funcall lookup 'nelisp-native-optimization-guard-v1-source))
                         (progn
                           (dotimes (_ 4) (setq owners (and (consp owners) (funcall tail owners))))
                           (and (consp owners)
                                (funcall same (funcall head owners)
                                         (funcall lookup 'nelisp-native-optimization-guard-v1-dependency-context)))))))))
       (source-shape (value)
         ;; Index the private finite syntax once. Equality against this shape
         ;; can only follow that finite tree before a mismatch; a cyclic or
         ;; larger supplied tree cannot extend the comparison beyond it.
         (let* ((key (sxhash-eq value))
                (found (assq value (gethash key source-shapes))))
           (or (cdr found)
               (let ((nodes 0) (maximum 0))
                 (cl-labels ((scan (item depth)
                               (setq nodes (1+ nodes) maximum (max maximum depth))
                               (when (or (> nodes 8192) (> depth 64))
                                 (error "rooted-cfg: source context bound exceeded"))
                               (cond ((consp item)
                                      (scan (car item) (1+ depth))
                                      (scan (cdr item) depth))
                                     ((vectorp item)
                                      (when (> (length item) 256)
                                        (error "rooted-cfg: source vector bound exceeded"))
                                      (dotimes (i (length item))
                                        (scan (aref item i) (1+ depth)))))))
                   (scan value 0))
                 (let ((shape (cons nodes maximum)))
                   (puthash key (cons (cons value shape) (gethash key source-shapes)) source-shapes)
                   shape)))))
       (guard-valid-p (&optional supplied-context supplied-arithmetic)
         (let ((owners guard-owners) (valid t))
           (while owners
             (let ((entry (funcall head owners)))
               (if (funcall same (funcall tail entry) (funcall lookup (funcall head entry)))
                   nil (setq valid nil owners nil)))
             (if owners (setq owners (funcall tail owners))))
           (if (if valid (pre-copy-valid-p) nil)
                ;; Bound the source-owned context independently of bytecode
                ;; admission; opaque functions are compared without traversal.
                (let ((budget 8192))
                  (cl-labels ((walk (left right depth &optional source-data-p)
                                (setq budget (1- budget))
                                (and (>= budget 0) (<= depth 64)
                                     (cond
                                      (source-data-p
                                       (let ((shape (source-shape left)))
                                         ;; WALK has already charged the root.
                                         (setq budget (- budget (1- (car shape))))
                                         (and (>= budget 0) (<= (+ depth (cdr shape)) 64)
                                              (equal left right))))
                                      ((and (not source-data-p)
                                            (or (functionp left) (functionp right)))
                                       (funcall same left right))
                                      ((and (consp left) (consp right))
                                       (and (walk (car left) (car right) (1+ depth) source-data-p)
                                            (walk (cdr left) (cdr right) depth source-data-p)))
                                      ((or (consp left) (consp right)) nil)
                                      ((and (vectorp left) (vectorp right))
                                       (and (= (length left) (length right))
                                            (<= (length left) 256)
                                            (let ((i 0) (ok t))
                                              (while (and ok (< i (length left)))
                                                (setq ok (walk (aref left i) (aref right i) (1+ depth)
                                                               (or source-data-p
                                                                   (and (source-slot-data-p left i)
                                                                        (source-slot-data-p right i))))
                                                      i (1+ i))) ok)))
                                      (t (equal left right))))))
                    (and (walk guard-context
                               (nelisp-bytecode-native-guarded-lowering-dependency-context) 0)
                         (if supplied-context
                             (progn (setq budget 8192) (walk guard-context supplied-context 0)) t)
                         (if supplied-arithmetic
                             (progn (setq budget 8192)
                                    (walk arithmetic-context supplied-arithmetic 0)) t)))) nil)))
       (guard-context-copy (value &optional source-data-p)
         ;; Eligibility already established the source-owned bounded shape.
         ;; Never expose the private original snapshot through a returned plan.
         (cond ((and (not source-data-p) (functionp value)) value)
               ((consp value) (cons (guard-context-copy (car value) source-data-p)
                                    (guard-context-copy (cdr value) source-data-p)))
               ((vectorp value)
                (let ((index 0) (items nil))
                  (while (< index (length value))
                    (push (guard-context-copy
                           (aref value index)
                           (or source-data-p
                               (source-slot-data-p value index))) items)
                    (setq index (1+ index)))
                  (vconcat (nreverse items))))
               ((stringp value) (copy-sequence value))
               (t value))))

(defconst nelisp-bytecode-native-rooted-cfg--gateway-opcodes
  '((64 . car) (65 . cdr) (66 . cons) (92 . add)
    (162 . car-safe) (163 . cdr-safe))
  "Pinned GNU 31.1 primitive opcodes admitted by the first CFG planner.")

(defun nelisp-bytecode-native-rooted-cfg--gateway-import (operation)
  "Return the authenticated gateway import name for OPERATION."
  (pcase operation
    ((or 'car 'car-safe) "nl_native_car_v2")
    ((or 'cdr 'cdr-safe) "nl_native_cdr_v2")
    ('cons "nl_native_cons_v2")
    ('add "nl_native_add_v2")
    ((or 'primitive-call 'list-build 'funcall 'switch) "nl_native_funcall_v2")))

(defun nelisp-bytecode-native-rooted-cfg--unsupported (reason)
  (list :status 'unsupported :reason reason))

(defun nelisp-bytecode-native-rooted-cfg--block-map (blocks)
  (let ((map (make-hash-table :test #'eql)) (duplicate nil))
    (dolist (block blocks)
      (let ((start (plist-get block :start)))
        (if (or (not (integerp start)) (gethash start map))
            (setq duplicate t)
          (puthash start block map))))
    (and (not duplicate) map)))

(defun nelisp-bytecode-native-rooted-cfg--incoming-edges (blocks target)
  (let (edges)
    (dolist (block blocks)
      (dolist (edge (append (plist-get block :successors) nil))
        (when (= (plist-get edge :target) target)
          (push (cons block edge) edges))))
    (nreverse edges)))

(defun nelisp-bytecode-native-rooted-cfg--input-root (token state)
  (cdr (assoc token state)))

(defun nelisp-bytecode-native-rooted-cfg--canonical-input-p (input)
  "Return non-nil when INPUT exactly matches the pinned public builder output."
  (condition-case nil
      (let* ((function (plist-get input :function))
             (dialect (plist-get input :dialect-evidence))
             (canonical (and (byte-code-function-p function)
                             (nelisp-bytecode-compiler-input-build function))))
        (and canonical
             (eq (plist-get dialect :status) 'pinned)
             (equal dialect (nelisp-bytecode-compiler-input-dialect))
             (equal input canonical)))
    (error nil)))

(defun nelisp-bytecode-native-rooted-cfg--safe-input-p (input)
  "Return non-nil when INPUT is incomplete only at verified safe primitives."
  (let* ((ir (plist-get input :ir-result))
         (frame (plist-get input :frame-result))
         (unsupported (plist-get ir :unsupported))
         (rows (plist-get ir :instructions))
         (blocks (plist-get frame :blocks))
         safe-pcs admitted-pcs unsupported-pcs frame-instructions)
    (when (and (nelisp-bytecode-native-rooted-cfg--canonical-input-p input)
               (eq (plist-get input :status) 'unsupported)
               (eq (plist-get ir :status) 'unsupported)
               (eq (plist-get frame :status) 'complete)
               (consp unsupported)
               (vectorp rows) (vectorp blocks)
               (cl-every (lambda (block) (vectorp (plist-get block :instructions)))
                         (append blocks nil)))
      (dolist (block (append blocks nil))
        (dolist (instruction (append (plist-get block :instructions) nil))
          (push instruction frame-instructions)
          (when (memq (plist-get instruction :opcode) '(162 163))
            (push (plist-get instruction :pc) safe-pcs))))
      (setq frame-instructions (nreverse frame-instructions))
      (dolist (row (append rows nil))
        (when (and (vectorp row) (>= (length row) 5))
          (let* ((opcode (aref row 1))
                 (meta (aref row 4))
                 (pc (aref row 0))
                 (reason (cdr (assq pc unsupported))))
            (when (and (not (plist-get meta :lowerable))
                       (or (and (eq reason 'unsupported-semantics)
                                (memq opcode '(66 162 163)))
                           (and (eq reason 'non-fixnum-constant)
                                (eq (plist-get meta :kind) 'constant)
                                (integerp (plist-get meta :constant-index))
                                (< -1 (plist-get meta :constant-index)
                                   (length (plist-get input :constants))))))
              (push pc admitted-pcs)))))
      (dolist (item unsupported)
        (when (and (consp item) (integerp (car item))
                   (memq (cdr item) '(unsupported-semantics non-fixnum-constant)))
          (push (car item) unsupported-pcs)))
      (and safe-pcs
           (= (length admitted-pcs) (length unsupported))
           (= (length rows) (length frame-instructions))
           (cl-every
            (lambda (row)
              (let ((frame-instruction
                     (cl-find (and (vectorp row) (> (length row) 0)
                                   (aref row 0)) frame-instructions
                              :key (lambda (instruction)
                                     (plist-get instruction :pc)))))
              (and (vectorp row) (>= (length row) 5)
                   frame-instruction
                   (= (aref row 1) (plist-get frame-instruction :opcode))
                   (or (plist-get (aref row 4) :lowerable)
                       (memq (aref row 0) admitted-pcs)))))
            (append rows nil))
           (equal (sort admitted-pcs #'<) (sort unsupported-pcs #'<))))))

(cl-defun nelisp-bytecode-native-rooted-cfg-plan (input &optional lowering-mode arithmetic-guard-mode)
  "Plan a supported bounded subset of verified compiler INPUT.

The result maps values to protected root indexes and records scalar root-index
phis at joins.  It refuses before any backend or artifact side effect."
  (cl-block nelisp-bytecode-native-rooted-cfg-plan
  (let* ((frame (plist-get input :frame-result))
         (handler-p (eq (plist-get frame :version) 'handler-frame-u8a-1))
         (handler-bank nil) (handler-pairs nil)
         (blocks (and (vectorp (plist-get frame :blocks))
                      (append (plist-get frame :blocks) nil)))
         (arity (if (and (integerp (plist-get input :argument-descriptor))
                         (null (plist-get input :argument-count)))
                    (plist-get input :initial-stack-depth)
                  (plist-get input :argument-count)))
         (initial-depth (plist-get input :initial-stack-depth))
         (descriptor (plist-get input :argument-descriptor))
         (constants (plist-get input :constants))
         (block-map (and blocks (nelisp-bytecode-native-rooted-cfg--block-map blocks)))
         (incoming-by-target
          (let ((table (make-hash-table :test 'eql)))
            (dolist (block blocks)
              (dolist (edge (append (plist-get block :successors) nil))
                (let ((target (plist-get edge :target)))
                  (puthash target (cons (cons block edge) (gethash target table)) table))))
            (maphash (lambda (target edges) (puthash target (nreverse edges) table)) table)
            table))
         (topology (and frame (nelisp-bytecode-native-rooted-cfg-topology-check frame)))
         (order (and block-map (eq (plist-get topology :status) 'complete)
                     (mapcar (lambda (start) (gethash start block-map))
                             (plist-get topology :block-order))))
         (cyclic (plist-get topology :cyclic))
         (switch-p (cl-some (lambda (b)
                              (cl-some (lambda (i) (eq (plist-get i :kind) 'switch))
                                       (append (plist-get b :instructions) nil))) blocks))
         (frame-p (or handler-p (cl-some (lambda (b)
                             (cl-some (lambda (i) (or (memq (plist-get i :kind)
                                                       '(dynamic-bind dynamic-unbind variable-ref variable-set frame-save frame-cleanup))
                                                   (memq (plist-get i :opcode) '(144 145))))
                                      (append (plist-get b :instructions) nil))) blocks)))
         (banked (or cyclic switch-p frame-p))
         (frame-state-root nil) (frame-staging-roots nil) (frame-result-root nil)
         (frame-enter-roots nil)
         (switch-root nil)
         (stack-edit-p
          (cl-some (lambda (block)
                     (cl-some (lambda (instruction)
                                (memq (plist-get instruction :kind) '(stack-set discard-n)))
                              (append (plist-get block :instructions) nil))) blocks))
         (list-p
          (cl-some (lambda (block)
                     (cl-some (lambda (instruction)
                                (memq (plist-get instruction :opcode) '(175 176 177)))
                              (append (plist-get block :instructions) nil))) blocks))
         (cycle-entries nil) (cycle-phis nil) (copy-roots nil) (poll-root nil) (poll-exit-roots nil)
         (entry-bank nil) (staging-size 0)
         (constant-roots nil) (immediate-roots nil)
         (root-next 0) (phi-next 0) (states nil)
         (phis nil) (planned-blocks nil) (returns nil) (failure nil)
         (arithmetic-p nil) (exit-root-base nil) (call1-layout nil)
         (f1-p (or banked
                   (cl-some (lambda (block)
                              (cl-some (lambda (instruction)
                                         (memq (plist-get instruction :opcode)
                                               '(56 57 58 59 60 61 62 63 67 68 69 70 175 71 72 73 74 75 76 77 78 79 80 81 82 176 177
                                                 83 84 85 86 87 88 89 90 91 93 94 95
                                                 164 165 166 167 168
                                                 96 98 99 100 101 102 103 104 105 106
                                                 108 109 110 111 112 113 116 117 118
                                                 119 120 121 122 123 124 125 126 127 139 141 143 144 145
                                                 147 148 149 150 151 152 153 154 155
                                                 156 157 158 159 160 161)))
                                       (append (plist-get block :instructions) nil))) blocks)
                   (and (not (eq (plist-get (nelisp-bytecode-native-call1-layout input) :status) 'complete))
                    (cl-some (lambda (block)
                               (cl-some (lambda (instruction) (eq (plist-get instruction :kind) 'call))
                                        (append (plist-get block :instructions) nil))) blocks))))
         ;; Index the same IR rows once. Retain a list per PC so even duplicate
         ;; rows have exactly the old `cl-some' admission semantics.
         (ir-rows-by-pc
          (let ((table (make-hash-table :test 'eql)))
            (dolist (row (append (plist-get (plist-get input :ir-result) :instructions) nil))
              (puthash (aref row 0) (cons row (gethash (aref row 0) table)) table))
            table))
         (primitive-roots nil) (scratch-root nil)
         (guard-mode (or arithmetic-guard-mode 'off)))
    (unless (and (guard-valid-p)
                 (memq arithmetic-guard-mode '(nil off on))
                 (memq lowering-mode '(nil safe-primitives-v3))
                 (if (eq lowering-mode 'safe-primitives-v3)
                     (or (and (eq (plist-get input :status) 'complete)
                              (nelisp-bytecode-native-rooted-cfg--canonical-input-p input))
                         (nelisp-bytecode-native-rooted-cfg--safe-input-p input))
                   (or (eq (plist-get input :status) 'complete)
                       (and (or f1-p stack-edit-p)
                            (nelisp-bytecode-native-rooted-cfg--canonical-input-p input)
                            (cl-every (lambda (item)
                                        (or (and handler-p (eq (cdr item) 'handler-semantics)
                                                 (cl-some (lambda (row) (and (= (aref row 0) (car item)) (memq (aref row 1) '(48 49 50))))
                                                          (gethash (car item) ir-rows-by-pc)))
                                            (and (eq (cdr item) 'non-fixnum-constant))
                                            (and (eq (cdr item) 'unsupported-semantics)
                                                 (cl-some (lambda (row)
                                                            (and (= (aref row 0) (car item))
                                                                 (or (memq (aref row 1) '(64 65 66 162 163 136 178 179 182 183 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 40 41 42 43 44 45 46 47))
                                                                     (<= 32 (aref row 1) 39))))
                                                          (gethash (car item) ir-rows-by-pc)))))
                                      (plist-get (plist-get input :ir-result) :unsupported)))))
                 (eq (plist-get frame :status) 'complete)
                 ;; Unsupported decoder markers precede this check in the
                 ;; public input builder. Do not admit a canonical U5 input
                 ;; whose declared stack storage is smaller than its frame.
                 (integerp (plist-get input :declared-stack-depth))
                 (>= (plist-get input :declared-stack-depth)
                     (plist-get frame :max-stack-depth))
                 (integerp arity) (>= arity 0)
                 (integerp initial-depth) (= initial-depth arity)
                 (integerp (plist-get input :argument-min))
                 (= (plist-get input :argument-min)
                    (if (integerp descriptor) (logand descriptor 127) 0))
                 (or (and (= arity 0) (null descriptor))
                     (and (integerp descriptor) (<= 0 descriptor 65535)
                          (<= (logand descriptor 127) (ash descriptor -8))
                          (= arity (+ (ash descriptor -8)
                                      (if (= (logand descriptor 128) 0) 0 1)))))
                 (vectorp constants)
                 (not (plist-get input :potential-capture-placeholder-p))
                 (not (plist-get input :capture-values-available))
                 (or (null (plist-get input :closure-template-descriptor))
                     (and (eq (plist-get input :metadata-role) 'lazy-documentation-reference)
                          (eq (plist-get input :closure-template-descriptor)
                              (plist-get input :documentation-reference))))
                 order
                 (<= (length blocks) nelisp-bytecode-native-rooted-cfg-max-blocks))
      (cl-return-from nelisp-bytecode-native-rooted-cfg-plan
        (nelisp-bytecode-native-rooted-cfg--unsupported
         "input must be complete lexical code with a bounded argument layout and reachable verified frame")))
    ;; The helper is consulted only after the existing owner seal and input
    ;; admission pass.  Its result is layout evidence, not runtime permission.
    (setq call1-layout (nelisp-bytecode-native-call1-layout input))
    ;; Root 0 belongs to the checked caller's frame marker.  Argument roots
    ;; precede hidden constant roots; operation results follow both groups.
    (setq root-next (1+ arity))
    (dotimes (index (length constants))
      (push (cons index root-next) constant-roots)
      (setq root-next (1+ root-next)))
    (setq constant-roots (nreverse constant-roots))
    (when f1-p
      ;; Preserve F1's established root layout; add only used family values.
      ;; This avoids reserving eleven hidden roots in every existing caller.
      (let ((names (if frame-p '(car cdr cons symbol-value set) '(car cdr cons))))
        (dolist (block blocks)
          (dolist (instruction (append (plist-get block :instructions) nil))
            (let ((primitive (nelisp-native-funcall-v2-primitive
                              (plist-get instruction :opcode))))
              (when (and primitive (not (memq (nth 1 primitive) names)))
                (setq names (append names (list (nth 1 primitive))))))))
        ;; Long sequence calls pack operands exactly like U3a LISTN, then
        ;; apply the frozen target once. No partial concat or insert chunks.
        (when (cl-some (lambda (block)
                        (cl-some (lambda (instruction)
                                   (and (memq (plist-get instruction :opcode) '(176 177))
                                        (> (plist-get instruction :operand) 32)))
                                 (append (plist-get block :instructions) nil))) blocks)
          (setq names (append names '(apply))))
        (dolist (name names)
          (if (eq name 'interactive-p)
              ;; GNU Binteractive_p deliberately observes the current cell.
              (push (cons name root-next) immediate-roots)
            (push (cons name root-next) primitive-roots))
          (setq root-next (1+ root-next)))))
    (when frame-p
      (setq frame-state-root root-next root-next (1+ root-next)
            frame-staging-roots (list root-next (1+ root-next)) root-next (+ root-next 2)
            frame-result-root root-next root-next (+ root-next (if handler-p 6 4)))
      (dolist (value (list 1 arity))
        (push root-next frame-enter-roots)
        (push (cons value root-next) immediate-roots)
        (setq root-next (1+ root-next)))
      (setq frame-enter-roots (nreverse frame-enter-roots)))
    ;; Catch compares object identity. Handler constant initializers therefore
    ;; read the live function vector; serialized recipe values only prove shape.
    (when handler-p
      (setq handler-bank (number-sequence root-next (+ root-next (plist-get frame :max-stack-depth) -1)))
      (setq root-next (+ root-next (plist-get frame :max-stack-depth)))
      (setq handler-pairs root-next root-next (+ root-next (* 2 (plist-get frame :max-handler-depth)))))
    (when switch-p
      (setq switch-root root-next root-next (1+ root-next)))
    (when banked
      ;; Only one block executes at a time. Edges already snapshot all inputs
      ;; through COPY-ROOTS before publishing the destination bank, so entry
      ;; stack positions can share a bank across blocks, including backedges.
      (unless handler-p
        (let ((depth (apply #'max (mapcar (lambda (b) (plist-get b :entry-stack-depth)) order))))
          (setq entry-bank (number-sequence root-next (+ root-next depth -1))
                root-next (+ root-next depth))))
      (dolist (block order)
        (let ((start (plist-get block :start)) (entry nil) (block-phis nil))
          (dotimes (slot (plist-get block :entry-stack-depth))
            (push (cons (list :entry start slot) (nth slot (if handler-p handler-bank entry-bank))) entry)
            (unless handler-p (push (list :id phi-next :block start :slot slot :root (nth slot entry-bank) :incoming nil) block-phis))
            (setq phi-next (1+ phi-next)))
          (push (cons start (nreverse entry)) cycle-entries)
          (push (cons start (nreverse block-phis)) cycle-phis)))
      (dotimes (_ (apply #'max (mapcar (lambda (b) (plist-get b :entry-stack-depth)) order)))
        (push root-next copy-roots) (setq root-next (1+ root-next)))
      (setq copy-roots (nreverse copy-roots) poll-root root-next root-next (1+ root-next))
      (dolist (value '(1 quit nil))
        (push root-next poll-exit-roots)
        (push (cons value root-next) immediate-roots)
        (setq root-next (1+ root-next)))
      (setq poll-exit-roots (nreverse poll-exit-roots)))
    (when (>= root-next 256)
        (cl-return-from nelisp-bytecode-native-rooted-cfg-plan
          (nelisp-bytecode-native-rooted-cfg--unsupported "root-slot limit exceeded")))
    (dolist (block order)
      (let* ((start (plist-get block :start))
             (entry-depth (plist-get block :entry-stack-depth))
             (incoming (gethash start incoming-by-target))
             (entry-state nil) (phis-here nil) (ops nil))
        (if banked
            (setq entry-state (reverse (copy-tree (cdr (assq start cycle-entries))))
                  phis-here (reverse (cdr (assq start cycle-phis))))
          (if (= start (plist-get (car blocks) :start))
          (progn
            (unless (= entry-depth arity)
              (setq failure "entry stack depth does not equal lexical arity"))
            (dotimes (slot arity)
              (push (cons (list :entry start slot) (1+ slot)) entry-state)))
          (progn
            (unless incoming (setq failure "non-entry block has no predecessor"))
            (let ((expected-slots
                   (cl-loop for slot below entry-depth
                            collect (list :entry start slot))))
              (dolist (source incoming)
                (unless (equal (append (plist-get (cdr source) :target-slots) nil)
                               expected-slots)
                  (setq failure (or failure
                                    "edge target slots are not the target block's entry slots")))))
            (dotimes (slot entry-depth)
              (let (values)
                (dolist (source incoming)
                  (let* ((edge (cdr source))
                         (edge-slots (append (plist-get edge :slots) nil))
                         (target-slots (append (plist-get edge :target-slots) nil))
                         (state (cdr (assq (plist-get (car source) :start) states))))
                    (unless (and (= (length edge-slots) entry-depth)
                                 (= (length target-slots) entry-depth))
                      (setq failure "edge stack shape disagrees with target depth"))
                    (let* ((token (nth slot edge-slots))
                           (root (nelisp-bytecode-native-rooted-cfg--input-root
                                  token state)))
                      (unless (or root failure)
                        (setq failure
                              (format "edge token %S missing in predecessor %d state %S"
                                      token (plist-get (car source) :start) state)))
                      (push root values))))
                (setq values (nreverse values))
                (when (or failure (memq nil values))
                  (setq failure
                        (or failure
                            (format "undefined edge SSA value at block %d slot %d"
                                    start slot))))
                (unless failure
                  (let* ((unique (delete-dups (copy-sequence values)))
                         (root (if (= (length unique) 1)
                                   (car unique)
                                 (let ((id phi-next))
                                   (setq phi-next (1+ phi-next))
                                   (push (list :id id :block start :slot slot
                                               :incoming (cl-mapcar
                                                          (lambda (source value)
                                                            (cons (plist-get (car source) :start)
                                                                  value))
                                                          incoming values))
                                         phis-here)
                                   (list :phi id)))))
                    (push (cons (list :entry start slot) root) entry-state))))))))
        (setq entry-state (nreverse entry-state))
        (dolist (instruction (append (plist-get block :instructions) nil))
          (unless failure
            (let* ((kind (plist-get instruction :kind))
                   (opcode (plist-get instruction :opcode))
                   (inputs (mapcar (lambda (token)
                                     (nelisp-bytecode-native-rooted-cfg--input-root
                                      token entry-state))
                                   (plist-get instruction :inputs)))
                   (outputs (append (plist-get instruction :outputs) nil))
                   (op nil) (output-root nil) (constant-index
                                               (plist-get instruction :constant-index)))
              (cond
               ((eq kind 'stack-ref) (setq op 'stack-ref output-root (car inputs)))
               ((eq kind 'dup) (setq op 'dup output-root (car inputs)))
               ((eq kind 'discard) (setq op 'discard))
               ((memq kind '(stack-set discard-n))
                ;; Frame transfer already replaced/dropped the destination
                ;; token. Normalize the surviving TOS alias to the existing
                ;; inline copy form; never mutate its protected source root.
                ;; The cyclic emitter stages these aliases in parallel at edges.
                (if outputs
                    (setq op 'stack-ref output-root (car inputs))
                  (setq op 'discard)))
               ((eq kind 'constant)
                (setq op 'const
                      output-root (if (integerp constant-index)
                                      (cdr (assq constant-index constant-roots))
                                    (let* ((key (plist-get instruction :operand))
                                           (entry (assoc key immediate-roots)))
                                      (or (cdr entry)
                                          (let ((root root-next))
                                            (setq root-next (1+ root-next))
                                            (push (cons key root) immediate-roots)
                                            root)))))
                (unless output-root (setq failure "constant has no protected root")))
               ((memq kind '(handler-catch handler-condition handler-pop))
                (setq op kind))
               ((and frame-p (memq kind '(dynamic-bind dynamic-unbind variable-ref variable-set frame-save frame-cleanup)))
                (setq op (pcase kind ('dynamic-bind 'frame-specbind) ('dynamic-unbind 'frame-unbind)
                                ('variable-ref 'frame-varref) ('variable-set 'frame-varset)
                                ('frame-save 'frame-save) ('frame-cleanup 'frame-cleanup)))
                (when (memq kind '(variable-ref variable-set))
                  (setq output-root root-next root-next (1+ root-next))))
               ((eq kind 'branch)
                (cond ((and (= opcode 130) (null inputs)) (setq op 'goto))
                      ((and (memq opcode '(131 132 133 134)) (= (length inputs) 1))
                       (setq op 'conditional-branch))
                      (t (setq failure
                               (format "unsupported branch opcode or condition shape %S/%S"
                                       opcode inputs)))))
               ((eq kind 'switch)
                (setq op 'switch output-root root-next root-next (1+ root-next)))
               ((eq kind 'goto) (setq op 'goto))
               ((eq kind 'return)
                (if (= (length inputs) 1)
                    (progn (setq op 'return)
                           (push (cons start (car inputs)) returns))
                  (setq failure "return must select one stack value")))
               ((and f1-p (eq kind 'call))
                (if (and (<= 32 opcode 39) (= (length outputs) 1)
                         (> (length inputs) 0))
                    (setq op 'funcall output-root root-next root-next (1+ root-next))
                  (setq failure "Malformed generic byte-call")))
               ((eq kind 'call)
                (if (and (eq (plist-get call1-layout :status) 'complete)
                         (integerp (plist-get call1-layout :function-root))
                         (= (plist-get call1-layout :function-root) 1)
                         (integerp (plist-get call1-layout :argument-root))
                         (= (plist-get call1-layout :argument-root) 2)
                         (integerp (plist-get call1-layout :result-root))
                         (= (plist-get call1-layout :result-root) 3)
                         (equal (plist-get call1-layout :exit-roots) '(4 5 6))
                         (integerp (plist-get call1-layout :exit-root-base))
                         (= (plist-get call1-layout :exit-root-base) 4)
                         (integerp (plist-get call1-layout :next-root))
                         (= (plist-get call1-layout :next-root) 7)
                         (integerp (plist-get call1-layout :required-root-count))
                         (= (plist-get call1-layout :required-root-count) 7)
                         (null lowering-mode) (eq guard-mode 'off)
                         (null constant-roots) (null phis-here)
                         (= (length ops) 2)
                         (cl-every (lambda (operation)
                                     (eq (plist-get operation :opcode) 'stack-ref)) ops)
                         (equal inputs '(1 2)) (= (length outputs) 1)
                         (= root-next 3))
                    (setq op 'call1 output-root 3 root-next 7 exit-root-base 4)
                  (setq failure "CALL1 is outside the qualified fixed-layout subset")))
               ((and f1-p (eq kind 'primitive)
                     (not (and (eq lowering-mode 'safe-primitives-v3)
                               (memq opcode '(162 163))))
                     (nelisp-native-funcall-v2-primitive opcode))
                (let ((primitive (nelisp-native-funcall-v2-primitive opcode)))
                  (if (and (= (length inputs) (if (eq (nth 2 primitive) 'operand)
                                                 (plist-get instruction :operand)
                                               (nth 2 primitive)))
                           (= (length outputs) 1))
                      (setq op (if (and (memq opcode '(175 176 177)) (> (length inputs) 32))
                                   'list-build (if (= opcode 116) 'funcall 'primitive-call))
                            output-root root-next root-next (1+ root-next))
                    (setq failure "Malformed generic primitive call"))))
               ((eq kind 'primitive)
                (setq op (cdr (assq opcode
                                    nelisp-bytecode-native-rooted-cfg--gateway-opcodes)))
                (if (and op
                         (or (not (memq opcode '(162 163)))
                             (eq lowering-mode 'safe-primitives-v3))
                         (= (length inputs) (if (memq op '(cons add)) 2 1))
                         (= (length outputs) 1))
                    (progn
                           (when (eq op 'add) (setq arithmetic-p t))
                           (setq output-root root-next)
                           (setq root-next (1+ root-next)))
                  (setq failure (format "unsupported primitive opcode %s" opcode))))
               (t (setq failure (format "unsupported instruction kind %s" kind))))
              (when (and inputs (memq nil inputs))
                (setq failure (or failure "instruction reads an undefined value root")))
              (unless failure
                (when (and outputs output-root)
                  (push (cons (car outputs) output-root) entry-state))
                (let ((operation
                       (list :opcode op :bytecode-opcode opcode
                             :input-roots inputs :output-root output-root
                             :condition-test (and (eq op 'conditional-branch)
                                                  '(nil-tag-p condition-root))
                             :taken-if (and (eq op 'conditional-branch)
                                            (if (memq opcode '(131 133)) 'nil 'not-nil))
                             :type-error-input (and (memq op '(car cdr)) (car inputs))
                             :pc (plist-get instruction :pc))))
                  (when (memq op '(handler-catch handler-condition handler-pop))
                    (let ((arguments nil))
                      (unless (eq op 'handler-pop)
                        (push (cons (vector (if (eq op 'handler-catch) 2 1)
                                            (plist-get instruction :operand)
                                            (1- (plist-get block :entry-stack-depth))
                                            handler-pairs (plist-get frame :max-handler-depth)
                                            (car handler-bank)) root-next) immediate-roots)
                        (setq arguments (list (car inputs) root-next) root-next (1+ root-next)))
                      (setq operation (append operation (list :argument-roots arguments
                                                               :frame-action (if (eq op 'handler-pop) 9 8))))))
                  (when (memq op '(frame-specbind frame-unbind frame-varref frame-varset frame-save frame-cleanup))
                    (let* ((symbol-root (cdr (assq (plist-get instruction :operand) constant-roots)))
                           (arguments (cond ((eq op 'frame-save) nil)
                                            ((eq op 'frame-cleanup) inputs)
                                            ((eq op 'frame-unbind)
                                          (let ((root root-next))
                                            (push (cons (plist-get instruction :operand) root) immediate-roots)
                                            (setq root-next (1+ root-next)) (list root)))
                                            (t (cons symbol-root inputs)))))
                      (setq operation (append operation
                                              (list :argument-roots arguments
                                                    :frame-action (pcase op
                                                                    ('frame-specbind 1) ('frame-unbind 2)
                                                                    ('frame-cleanup 4)
                                                                    ('frame-save (pcase opcode ((or 97 114) 5) (138 6) (140 7))))
                                                    :argument-count (length arguments))))
                      (when (memq op '(frame-varref frame-varset))
                        (setq operation (append operation
                                                (list :function-root (cdr (assq (if (eq op 'frame-varref)
                                                                                    'symbol-value 'set) primitive-roots))
                                                      :staging-roots (number-sequence root-next
                                                                                     (+ root-next (length arguments) -1)))))
                        (if banked (setq staging-size (max staging-size (length arguments)))
                          (setq root-next (+ root-next (length arguments)))))))
                  (when (eq op 'switch)
                    (let* ((targets (delete-dups (mapcar (lambda (edge) (plist-get edge :target))
                                                       (append (plist-get block :successors) nil))))
                           (target-root root-next))
                      (push (cons targets target-root) immediate-roots)
                      (setq root-next (1+ root-next))
                      (setq operation (append operation
                                              (list :function-root switch-root
                                                    :argument-roots (append inputs (list target-root))
                                                    :argument-count 3
                                                    :staging-roots (number-sequence root-next (+ root-next 2)))))
                      (if banked (setq staging-size (max staging-size 3))
                        (setq root-next (+ root-next 3)))))
                  (when (memq op '(primitive-call list-build funcall))
                    (let* ((primitive (and (or (= opcode 116) (memq op '(primitive-call list-build)))
                                           (nelisp-native-funcall-v2-primitive opcode)))
                           ;; Bindent_to consumes one VM operand but calls
                           ;; Findent_to(column, nil). Keep nil authenticated.
                           (arguments (if primitive inputs (cdr inputs)))
                           (_indent (when (= opcode 106)
                                      (push (cons nil root-next) immediate-roots)
                                      (setq arguments (append arguments (list root-next))
                                            root-next (1+ root-next))))
                           (count (length arguments))
                           (staged-count (if (eq op 'list-build) 2 count)))
                      (setq operation
                            (append operation
                                    (list :function-root (if (= opcode 116) (cdr (assq 'interactive-p immediate-roots))
                                                              (if primitive (cdr (assq (if (eq op 'list-build) 'cons (nth 1 primitive)) primitive-roots)) (car inputs)))
                                          :argument-roots arguments :argument-count count
                                          :staging-roots (number-sequence root-next (+ root-next staged-count -1))
                                          :provider 'nl_native_funcall_v2)))
                      (if banked (setq staging-size (max staging-size staged-count))
                        (setq root-next (+ root-next staged-count)))
                      (when (memq opcode '(144 145))
                        (plist-put operation :legacy-binding-root root-next)
                        (push (cons (if (= opcode 144) 'standard-output 1) root-next) immediate-roots)
                        (setq root-next (1+ root-next)))
                      (when (eq op 'list-build)
                        (when (memq opcode '(176 177))
                          (plist-put operation :apply-function-root (cdr (assq 'apply primitive-roots)))
                          (plist-put operation :target-function-root (cdr (assq (nth 1 primitive) primitive-roots))))
                        (plist-put operation :nil-root root-next)
                        (push (cons nil root-next) immediate-roots)
                        (setq root-next (1+ root-next)))))
                  (when (eq op 'call1)
                    (setq operation
                          (append operation
                                  (list :argument-count 1 :call-base 1
                                        :provider 'nl_native_call_v2
                                        :exit-root-base 4))))
                  ;; Deep stack edits and LISTN can have hundreds of DUP aliases. Their
                  ;; SSA mappings above and full frame trace remain canonical;
                  ;; they generate no code. Avoid duplicating their records in
                  ;; the serialized plan and every authenticated reconstruction.
                  (unless (and (or stack-edit-p list-p)
                               (eq op 'dup))
                    (push operation ops)))))))
        (unless failure
          (let ((successors (append (plist-get block :successors) nil))
                (last-op (car ops)))
            (when (and (eq (plist-get last-op :opcode) 'conditional-branch)
                       (not (and (= (length successors) 2)
                                 (memq 'taken (mapcar (lambda (edge) (plist-get edge :kind)) successors))
                                 (memq 'fallthrough
                                       (mapcar (lambda (edge) (plist-get edge :kind)) successors)))))
              (setq failure "conditional branch must have taken and fallthrough edges"))
            (when (and (eq (plist-get last-op :opcode) 'goto)
                       (/= (length successors) 1))
              (setq failure "goto must have one successor"))
            (when (and (eq (plist-get last-op :opcode) 'return) successors)
              (setq failure "return block cannot have successors"))
            (when (and (not (memq (plist-get last-op :opcode)
                                  '(conditional-branch goto return switch)))
                       (not (and (= (length successors) 1)
                                 (eq (plist-get (car successors) :kind) 'fallthrough))))
              (setq failure "ordinary block must fall through to one successor"))
            (setq phis-here (nreverse phis-here))
            (setq phis (append phis phis-here))
            (push (cons start entry-state) states)
            (push (list :start start :phis phis-here
                        :operations (nreverse ops) :successors successors
                        :bank-edge-copies (and handler-p
                                              (mapcar (lambda (edge)
                                                        (cons (plist-get edge :target)
                                                              (mapcar (lambda (token) (nelisp-bytecode-native-rooted-cfg--input-root token entry-state))
                                                                      (append (plist-get edge :slots) nil)))) successors)))
                  planned-blocks)))))
    (when banked
      ;; Resolve all incoming banks only after every block has a stable state.
      (dolist (pair cycle-phis)
        (dolist (phi (cdr pair))
          (let ((incoming nil) (start (car pair)) (slot (plist-get phi :slot)))
            (dolist (source (gethash start incoming-by-target))
              (let* ((from (plist-get (car source) :start))
                     (token (aref (plist-get (cdr source) :slots) slot))
                     (root (nelisp-bytecode-native-rooted-cfg--input-root token (cdr (assq from states)))))
                (unless (integerp root) (setq failure "unresolved cyclic slot transfer"))
                (push (cons from root) incoming)))
            (plist-put phi :incoming (nreverse incoming)))))
      ;; All effectful calls and edge polls preserve every bank in protected roots.
      (dolist (block planned-blocks)
        (plist-put block :poll-targets
                   (mapcar #'cdr (cl-remove-if-not
                                  (lambda (pair) (= (car pair) (plist-get block :start)))
                                  (plist-get topology :poll-edges))))))
    (when (and banked (> staging-size 0))
      ;; Call staging has no lifetime beyond its operation. Keep it separate
      ;; from every live SSA value and initializer, and share it across calls.
      (dolist (block planned-blocks)
        (dolist (operation (plist-get block :operations))
          (when (plist-get operation :staging-roots)
            (plist-put operation :staging-roots
                       (number-sequence root-next
                                        (+ root-next (length (plist-get operation :staging-roots)) -1))))))
      (setq root-next (+ root-next staging-size)))
    (when f1-p
      (setq scratch-root root-next exit-root-base (1+ root-next) root-next (+ root-next 4))
      (dolist (block planned-blocks)
        (dolist (operation (plist-get block :operations))
          (when (memq (plist-get operation :opcode) '(primitive-call list-build funcall switch frame-varref frame-varset))
            (plist-put operation :result-root scratch-root)
            (plist-put operation :exit-root-base exit-root-base)))))
    (when (and f1-p arithmetic-p) (setq failure "F1 numeric coexistence is not qualified"))
    (when arithmetic-p
      (unless (nelisp-bytecode-native-rooted-cfg--canonical-input-p input)
        (setq failure "arithmetic input is not canonical authenticated compiler input"))
      (dolist (block planned-blocks)
        (dolist (operation (plist-get block :operations))
          (when (eq (plist-get operation :opcode) 'add)
            (when (cl-some #'consp (plist-get operation :input-roots))
              (plist-put operation :materialized-input-roots
                         (list root-next (1+ root-next)))
              (setq root-next (+ root-next 2))))))
      ;; Reserve the exit triple after all dedicated operand slots.
      (setq exit-root-base root-next root-next (+ root-next 3))
      (dolist (block planned-blocks)
        (dolist (operation (plist-get block :operations))
          (when (eq (plist-get operation :opcode) 'add)
            (plist-put operation :exit-root-base exit-root-base)))))
    (when (or failure (>= root-next 256))
      (cl-return-from nelisp-bytecode-native-rooted-cfg-plan
        (nelisp-bytecode-native-rooted-cfg--unsupported
         (or failure "root-slot limit exceeded"))))
    (let* ((gateway-imports
                            (sort (delete-dups
                   (mapcar (lambda (operation)
                             (nelisp-bytecode-native-rooted-cfg--gateway-import
                              (plist-get operation :opcode)))
                           (cl-loop for block in planned-blocks append
                                    (cl-remove-if-not
                                     (lambda (op) (memq (plist-get op :opcode)
                                                       '(car cdr cons add car-safe cdr-safe primitive-call list-build funcall)))
                                     (plist-get block :operations)))))
                  #'string<))
           (gateway-imports (if banked (sort (delete-dups (append gateway-imports (list "nl_native_funcall_v2" "nl_native_poll_v2" "nl_root_pin_slot_v2"))) #'string<) gateway-imports))
           (gateway-imports (if frame-p
                                (sort (delete-dups (append gateway-imports (list "nl_native_frame_v2"))) #'string<)
                              gateway-imports))
           (ordered-blocks (nreverse planned-blocks))
           (ordered-phis (nreverse (copy-sequence phis)))
           (ordered-returns (nreverse returns))
           (final-root (and (= (length ordered-returns) 1)
                            (cdar ordered-returns)))
           (entry-ast
            (append
             (list :kind (if (eq lowering-mode 'safe-primitives-v3)
                             'rooted-cfg-safe-primitives-v3 'rooted-cfg-v1)
                  :argument-count arity
                  :frame-root-index 0
                  :argument-roots (number-sequence 1 arity)
                  :constant-roots constant-roots
                  :constant-initializers
                  (cl-loop for (index . root) in constant-roots
                           collect (if (or handler-p (hash-table-p (aref constants index)))
                                       (list :root root :constant-index index :value nil)
                                     (list :root root :value (aref constants index))))
                  :immediate-initializers
                  (mapcar (lambda (entry) (list :root (cdr entry) :value (car entry)))
                          (nreverse (copy-sequence immediate-roots)))
                  :required-root-count root-next
                  :blocks ordered-blocks
                  :return-selectors ordered-returns
                  :gateway-imports gateway-imports
                  :success-encoding '(+ 512 selected-root-index)
                   :type-error-encoding '(+ 256 failed-input-root-index)
                   :infrastructure-status 'propagate-unchanged)
             (and banked
            (list :cyclic cyclic :banked t :sccs (plist-get topology :sccs) :copy-roots copy-roots
                  :entry-copies (cl-loop for pair in (cdr (assq (plist-get (car blocks) :start) cycle-entries))
                                         for source from 1 collect (cons source (cdr pair)))
                  :poll-root poll-root :poll-exit-roots poll-exit-roots))
       (and (eq (plist-get call1-layout :status) 'complete)
                  (list :call-exit-root-base exit-root-base :call-exit-root-count 3))
             (list :arithmetic-guard-mode guard-mode)
             (and lowering-mode (list :lowering-mode lowering-mode)))))
      (append
       (list :status 'complete :input input :arity arity
            :initial-roots (cons 'frame-root (number-sequence 1 arity))
            :constant-roots constant-roots :phis ordered-phis
            :constant-initializers
            (cl-loop for (index . root) in constant-roots
                     collect (if (or handler-p (hash-table-p (aref constants index)))
                                       (list :root root :constant-index index :value nil)
                                     (list :root root :value (aref constants index))))
            :immediate-initializers
            (mapcar (lambda (entry) (list :root (cdr entry) :value (car entry)))
                    (nreverse (copy-sequence immediate-roots)))
            :blocks ordered-blocks :return-selectors ordered-returns
            :final-root final-root :required-root-count root-next
            :gateway-imports gateway-imports :entry-ast entry-ast)
       (and (eq (plist-get call1-layout :status) 'complete)
            (list :call-exit-root-base exit-root-base :call-exit-root-count 3))
       (and banked
            (list :cyclic cyclic :banked t :sccs (plist-get topology :sccs) :copy-roots copy-roots
                  :entry-copies (cl-loop for pair in (cdr (assq (plist-get (car blocks) :start) cycle-entries))
                                         for source from 1 collect (cons source (cdr pair)))
                  :poll-root poll-root :poll-exit-roots poll-exit-roots))
       (and handler-p (list :handler-bank handler-bank :handler-pairs handler-pairs
                            :handler-targets
                            (cl-remove-if-not
                             (lambda (pc) (cl-find pc ordered-blocks :key (lambda (b) (plist-get b :start))))
                             (delete-dups (mapcar (lambda (h) (plist-get h :target))
                                                  (append (plist-get frame :pushes) nil))))))
       (and frame-p
            (list :frame-descriptor (nelisp-native-frame-v2-descriptor)
                  :frame-hash (nelisp-native-frame-v2-hash)
                  :frame-state-root frame-state-root :frame-staging-roots frame-staging-roots
                  :frame-result-root frame-result-root :frame-enter-roots frame-enter-roots))
       (and f1-p
            (list :funcall-version nelisp-native-funcall-v2-version
                  :funcall-descriptor (nelisp-native-funcall-v2-descriptor)
                  :funcall-hash (nelisp-native-funcall-v2-hash)
                  :primitive-initializers
                  (append (mapcar (lambda (pair) (list :root (cdr pair) :primitive (car pair)))
                                  (nreverse primitive-roots))
                          (and banked (list (list :root poll-root :poll t)))
                          (and frame-p (list (list :root frame-state-root :frame t)))
                          (and switch-p (list (list :root switch-root :switch t))))
                  :result-root scratch-root :exit-root-base exit-root-base))
       (and arithmetic-p
            (list :exit-root-base exit-root-base
                  :arithmetic-context
                  (guard-context-copy arithmetic-context)
                  :arithmetic-guard-context (guard-context-copy guard-context)))
       (list :arithmetic-guard-mode guard-mode)
       (and lowering-mode (list :lowering-mode lowering-mode)))))))

(defun nelisp-bytecode-native-rooted-cfg-plan-guard-context-p (plan)
  "Check the opaque guarded owner snapshot carried by authenticated PLAN."
  (and (vectorp (plist-get plan :arithmetic-guard-context))
       (vectorp (plist-get plan :arithmetic-context))
       (guard-valid-p (plist-get plan :arithmetic-guard-context)
                      (plist-get plan :arithmetic-context))))

  (setq guard-owners
        (mapcar (lambda (name) (cons name (funcall lookup name)))
                '(nelisp-native-frame-v2-source nelisp-native-frame-v2-descriptor
                  nelisp-native-frame-v2-hash nelisp-native-frame-v2-initializer
                  nelisp-bytecode-native-guarded-lowering-build
                  nelisp-bytecode-native-guarded-lowering-owner-valid-p
                  nelisp-bytecode-native-guarded-lowering-select
                  nelisp-bytecode-native-guarded-lowering-dependency-context
                  nelisp-bytecode-native-arithmetic-lowering-build
                  nelisp-bytecode-native-arithmetic-lowering-owner-valid-p
                  nelisp-bytecode-native-arithmetic-lowering-dependency-context
                  nelisp-native-optimization-guard-v1-source
                  nelisp-native-optimization-guard-v1-owner-valid-p
                  nelisp-native-optimization-guard-v1-descriptor
                  nelisp-native-optimization-guard-v1-call-select
                  nelisp-native-optimization-guard-v1-dependency-context
                  nelisp-native-arithmetic-v2-source
                  nelisp-native-arithmetic-v2-owner-valid-p
                  nelisp-native-arithmetic-v2-descriptor
                  nelisp-native-arithmetic-v2-runtime-imports
                  nelisp-native-arithmetic-v2-dependency-context
                  nelisp-bytecode-native-rooted-cfg-plan
                  nelisp-bytecode-native-call1-layout
                  nelisp-bytecode-native-call1-layout--bounded-plist-p
                  nelisp-bytecode-native-call1-layout--token-p
                  nelisp-bytecode-native-rooted-cfg-plan-guard-context-p
                  symbol-function eq car cdr functionp consp vectorp integerp
                  sxhash-eq gethash puthash make-hash-table assq max -
                  equal length aref >= <= < = 1- 1+ and or cond cl-labels
                  cons stringp copy-sequence vconcat mapcar append list cl-every))
        guard-context (progn (funcall guard-owner-checker)
                             (nelisp-bytecode-native-guarded-lowering-dependency-context))
        arithmetic-context (progn (funcall guard-owner-checker)
                                  (nelisp-bytecode-native-arithmetic-lowering-dependency-context)))
  ;; Build source-shape indexes once at owner initialization, before admitting
  ;; any compiler input. Later plans still compare the complete current data.
  (unless (guard-valid-p)
    (error "rooted-cfg: initial source context is unavailable"))))

(provide 'nelisp-bytecode-native-rooted-cfg-plan)
;;; nelisp-bytecode-native-rooted-cfg-plan.el ends here
