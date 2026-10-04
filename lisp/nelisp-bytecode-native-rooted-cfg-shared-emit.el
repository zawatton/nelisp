;;; nelisp-bytecode-native-rooted-cfg-shared-emit.el --- shared CFG continuations -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Emit verified acyclic rooted CFGs with each nearest-postdominator
;; continuation represented once.  This is a separate source-AST mode; it does
;; not change the v1 emitter or any artifact contract.

;;; Code:

(require 'cl-lib)
(require 'nelisp-bytecode-native-rooted-cfg-plan)
(require 'nelisp-bytecode-native-arithmetic-lowering)
(require 'nelisp-bytecode-native-guarded-lowering)
(require 'nelisp-bytecode-native-rooted-cfg-postdom)

(defun nelisp-bytecode-native-rooted-cfg-shared-emit--fail (context reason)
  (unless (plist-get context :failure)
    (setq context (plist-put context :failure reason)))
  context)

(defun nelisp-bytecode-native-rooted-cfg-shared-emit--resolve (context root)
  (let* ((phi-p (and (consp root) (eq (car root) :phi)
                     (integerp (cadr root)) (null (cddr root))))
         (resolved (if phi-p
                       (cdr (assq (cadr root) (plist-get context :phi-vars)))
                     root)))
    (and (or (and (integerp resolved) (<= 0 resolved)
                  (< resolved (plist-get context :root-count)))
             (and (symbolp resolved)
                  (gethash resolved (plist-get context :phi-var-set))))
         resolved)))

(defun nelisp-bytecode-native-rooted-cfg-shared-emit--materialize-inputs
    (inputs roots id index body)
  "Copy live INPUTS into dedicated protected ROOTS before BODY."
  (let ((result body))
    (dotimes (reverse-index 2)
      (let* ((operand (- 1 reverse-index))
             (source (intern (format "rooted_cfg_add_source_%d_%d_%d" id index operand)))
             (destination (intern (format "rooted_cfg_add_operand_%d_%d_%d" id index operand))))
        (setq result
              `(let ((,source (extern-call nl_root_pin_slot_v2
                                           env ticket ,(nth operand inputs)))
                     (,destination (extern-call nl_root_pin_slot_v2
                                                env ticket ,(nth operand roots))))
                 (if (or (= ,source 0) (= ,destination 0)) 2
                   (progn
                     ,@(mapcar (lambda (offset)
                                 `(ptr-write-u64 ,destination ,offset
                                                 (ptr-read-u64 ,source ,offset)))
                               '(0 8 16 24))
                     ,result))))))
    result))

(defun nelisp-bytecode-native-rooted-cfg-shared-emit--edge-assignments
    (context from to)
  (let ((target (gethash to (plist-get context :block-map))) (forms nil))
    (unless target
      (nelisp-bytecode-native-rooted-cfg-shared-emit--fail context "missing phi target block"))
    (when target
      (dolist (phi (plist-get target :phis))
        (let* ((incoming (assq from (plist-get phi :incoming)))
               (variable (cdr (assq (plist-get phi :id) (plist-get context :phi-vars))))
               (value (and incoming
                           (nelisp-bytecode-native-rooted-cfg-shared-emit--resolve
                            context (cdr incoming)))))
          (unless (and variable value)
            (nelisp-bytecode-native-rooted-cfg-shared-emit--fail
             context "edge lacks a resolvable incoming value for a phi"))
          (when (and variable value) (push `(setq ,variable ,value) forms)))))
    (setq context (plist-put context :selector-edge-count
                             (+ (plist-get context :selector-edge-count) (length forms))))
    (if forms (append (cons 'progn (nreverse forms)) '(0)) 0)))

(defun nelisp-bytecode-native-rooted-cfg-shared-emit--to
    (context from target stop path)
  (if (and stop (= target stop))
      (nelisp-bytecode-native-rooted-cfg-shared-emit--edge-assignments context from target)
    (nelisp-bytecode-native-rooted-cfg-shared-emit--block context target stop path)))

(defun nelisp-bytecode-native-rooted-cfg-shared-emit--block
    (context start stop path)
  (let ((block (gethash start (plist-get context :block-map))))
    (cond
     ((or (null block) (memq start path))
      (nelisp-bytecode-native-rooted-cfg-shared-emit--fail context
                                                           "missing block or cyclic expansion")
      0)
     (t
      (setq context (plist-put context :expansion-count
                               (1+ (plist-get context :expansion-count))))
      (if (> (plist-get context :expansion-count) 3072)
          (progn
            (nelisp-bytecode-native-rooted-cfg-shared-emit--fail context
                                                                 "structured expansion limit exceeded")
            0)
        (nelisp-bytecode-native-rooted-cfg-shared-emit--operations
         context block 0 stop (cons start path)))))))

(defun nelisp-bytecode-native-rooted-cfg-shared-emit--operations
    (context block index stop path)
  (let* ((operations (plist-get block :operations))
         (id (plist-get block :start)))
    (if (>= index (length operations))
        (let ((successors (plist-get block :successors)))
          (cond
           ((= (length successors) 1)
            (nelisp-bytecode-native-rooted-cfg-shared-emit--to
             context id (plist-get (car successors) :target) stop path))
           (t
            (nelisp-bytecode-native-rooted-cfg-shared-emit--fail context
                                                                 "block ends without one successor or return")
            0)))
      (let* ((operation (nth index operations))
             (opcode (plist-get operation :opcode)))
        (cond
         ((memq opcode '(const stack-ref dup discard))
          (nelisp-bytecode-native-rooted-cfg-shared-emit--operations
           context block (1+ index) stop path))
         ((eq opcode 'add)
          (let* ((inputs (mapcar
                          (lambda (root)
                            (nelisp-bytecode-native-rooted-cfg-shared-emit--resolve context root))
                          (plist-get operation :input-roots)))
                 (materialized (plist-get operation :materialized-input-roots))
                 (record (list :opcode 'add :bytecode-opcode 92
                               :input-roots (or materialized inputs)
                               :output-root (plist-get operation :output-root)
                               :exit-root-base (plist-get operation :exit-root-base)))
                 (lowered (nelisp-bytecode-native-guarded-lowering-build
                           record (plist-get context :root-count) 'env 'ticket
                           (plist-get (plist-get context :plan) :arithmetic-guard-mode)))
                 (status (intern (format "rooted_cfg_add_status_%d_%d" id index))))
            (if (not (eq (plist-get lowered :status) 'complete))
                (progn
                  (nelisp-bytecode-native-rooted-cfg-shared-emit--fail
                   context "authenticated arithmetic roots were refused") 2)
              (let ((body
                     `(let ((,status (extern-call ,@(plist-get lowered :call))))
                        (if (= ,status 0)
                            ,(nelisp-bytecode-native-rooted-cfg-shared-emit--operations
                              context block (1+ index) stop path)
                          (if (= ,status 1) ,(+ 1024 (plist-get record :exit-root-base)) 2)))))
                (if materialized
                    (nelisp-bytecode-native-rooted-cfg-shared-emit--materialize-inputs
                     inputs materialized id index body)
                  body)))))
         ((memq opcode '(car cdr cons))
          (let* ((inputs (mapcar (lambda (root)
                                   (nelisp-bytecode-native-rooted-cfg-shared-emit--resolve
                                    context root))
                                 (plist-get operation :input-roots)))
                 (output (plist-get operation :output-root))
                 (status (intern (format "rooted_cfg_%s_status_%d" opcode id)))
                 (type-root (and (memq opcode '(car cdr)) (car inputs)))
                 (name (intern (format "nl_native_%s_v2" opcode)))
                 (call (if (= (length inputs) 2)
                           `(extern-call ,name env ticket ,(car inputs) ,(cadr inputs) ,output 0)
                         `(extern-call ,name env ticket ,(car inputs) ,output 0 0))))
            (unless (and (cl-every #'identity inputs)
                         (integerp output) (<= 0 output)
                         (< output (plist-get context :root-count)))
              (nelisp-bytecode-native-rooted-cfg-shared-emit--fail
               context "gateway has an unresolved or out-of-frame root"))
            (if (plist-get context :failure) 0
              `(let ((,status ,call))
                 (if (= ,status 0)
                     ,(nelisp-bytecode-native-rooted-cfg-shared-emit--operations
                       context block (1+ index) stop path)
                   ,(if type-root
                        `(if (= ,status 1) (+ 256 ,type-root) ,status)
                      status))))))
         ((eq opcode 'return)
          (let ((root (nelisp-bytecode-native-rooted-cfg-shared-emit--resolve
                       context (car (plist-get operation :input-roots)))))
            (if root `(+ 512 ,root)
              (nelisp-bytecode-native-rooted-cfg-shared-emit--fail context
                                                                  "return root cannot be resolved")
              0)))
         ((eq opcode 'goto)
          (let ((edges (cl-remove-if-not
                        (lambda (edge) (eq (plist-get edge :kind) 'goto))
                        (plist-get block :successors))))
            (if (= (length edges) 1)
                (nelisp-bytecode-native-rooted-cfg-shared-emit--to
                 context id (plist-get (car edges) :target) stop path)
              (nelisp-bytecode-native-rooted-cfg-shared-emit--fail context
                                                                  "goto edge missing or ambiguous")
              0)))
         ((eq opcode 'conditional-branch)
          (nelisp-bytecode-native-rooted-cfg-shared-emit--branch
           context block operation stop path))
         (t
          (nelisp-bytecode-native-rooted-cfg-shared-emit--fail
           context (format "unsupported operation %S" opcode))
          0))))))

(defun nelisp-bytecode-native-rooted-cfg-shared-emit--branch
    (context block operation stop path)
  (let* ((id (plist-get block :start))
         (successors (plist-get block :successors))
         (taken-edges (cl-remove-if-not (lambda (edge) (eq (plist-get edge :kind) 'taken)) successors))
         (fall-edges (cl-remove-if-not (lambda (edge) (eq (plist-get edge :kind) 'fallthrough)) successors))
         (join (cdr (assq id (plist-get context :joins))))
         (condition (nelisp-bytecode-native-rooted-cfg-shared-emit--resolve
                     context (car (plist-get operation :input-roots))))
         (slot (intern (format "rooted_cfg_slot_%d" id)))
         (status (intern (format "rooted_cfg_branch_status_%d" id)))
         (nil-branch (eq (plist-get operation :taken-if) 'nil)))
    (unless (and (= (length taken-edges) 1) (= (length fall-edges) 1)
                 condition (integerp join))
      (nelisp-bytecode-native-rooted-cfg-shared-emit--fail
       context "conditional lacks a verified unique nearest join or edge"))
    (if (plist-get context :failure) 0
      (let* ((taken-form
              (nelisp-bytecode-native-rooted-cfg-shared-emit--to
               context id (plist-get (car taken-edges) :target) join path))
             (fall-form
              (nelisp-bytecode-native-rooted-cfg-shared-emit--to
               context id (plist-get (car fall-edges) :target) join path))
             (arm-form `(let ((,status
                               (if (= (ptr-read-u64 ,slot 0) 0)
                                   ,(if nil-branch taken-form fall-form)
                                 ,(if nil-branch fall-form taken-form))))
                         (if (= ,status 0)
                             ,(if (and stop (= join stop))
                                  0
                                (nelisp-bytecode-native-rooted-cfg-shared-emit--block
                                 context join stop path))
                           ,status)))
             (slot-call `(let ((,slot (extern-call nl_root_pin_slot_v2
                                                   env ticket ,condition 0 0 0)))
                           (if (= ,slot 0) 2 ,arm-form))))
        (setq context (plist-put context :join-count (1+ (plist-get context :join-count))))
        slot-call))))

(defun nelisp-bytecode-native-rooted-cfg-shared-emit-build (plan entry-name)
  "Emit a freshly verified rooted CFG with shared postdominator continuations."
  (let* ((input (plist-get plan :input))
         (verified (and input (nelisp-bytecode-native-rooted-cfg-plan
                               input (plist-get plan :lowering-mode)
                               (plist-get plan :arithmetic-guard-mode))))
         (analysis (and input (nelisp-bytecode-native-rooted-cfg-postdom-analyze input))))
    (if (not (and (eq (plist-get plan :status) 'complete)
                  (or (null (plist-get plan :exit-root-base))
                      (nelisp-bytecode-native-rooted-cfg-plan-guard-context-p plan))
                  (equal verified plan)
                  (eq (plist-get analysis :status) 'complete)
                  (stringp entry-name) (> (length entry-name) 0)))
        (list :status 'unsupported :reason "shared emission needs an unchanged canonical rooted plan")
      (let* ((blocks (plist-get plan :blocks))
             (root-count (plist-get plan :required-root-count))
             (arity (plist-get plan :arity))
             (block-map (make-hash-table :test #'eql))
             (phi-vars nil)
             (context (list :plan plan :root-count root-count :block-map block-map
                            :phi-vars nil :joins (plist-get analysis :nearest-joins)
                            :failure nil :expansion-count 0 :join-count 0
                            :selector-edge-count 0)))
        (dolist (block blocks)
          (puthash (plist-get block :start) block block-map)
          (dolist (phi (plist-get block :phis))
            (let ((id (plist-get phi :id)))
              (if (and (integerp id) (not (assq id phi-vars)))
                  (push (cons id (intern (format "rooted_cfg_phi_%d" id))) phi-vars)
                (nelisp-bytecode-native-rooted-cfg-shared-emit--fail
                 context "phi IDs are not unique integers")))))
        (setq phi-vars (nreverse phi-vars))
        (setq context (plist-put context :phi-vars phi-vars))
        ;; The verified plan admits fewer than 13 blocks and fewer than 256 roots.
        (let ((phi-var-set (make-hash-table :test #'eq
                                            :size (max 1 (length phi-vars)))))
          (dolist (pair phi-vars)
            (puthash (cdr pair) t phi-var-set))
          (setq context (plist-put context :phi-var-set phi-var-set)))
        (let* ((entry (car blocks))
               (body (and entry
                          (nelisp-bytecode-native-rooted-cfg-shared-emit--block
                           context (plist-get entry :start) nil nil)))
               (failure (plist-get context :failure)))
          (if failure
              (list :status 'unsupported :reason failure)
            (list :status 'complete :entry-name entry-name
                  :form `(defun ,(intern entry-name) (env ticket argument-count root-count)
                           (if (/= argument-count ,arity) 3
                             (if (/= root-count ,root-count) 3
                               (let ,(mapcar (lambda (pair) (list (cdr pair) 0)) phi-vars)
                                 ,body))))
                  :gateway-imports
                  (sort (delete-dups
                         (append (plist-get plan :gateway-imports)
                                 '("nl_root_pin_slot_v2")))
                        #'string<)
                  :additional-source
                  (and (plist-get plan :exit-root-base)
                       (nelisp-native-optimization-guard-v1-source
                        (plist-get plan :arithmetic-guard-mode)))
                  :arithmetic-context (plist-get plan :arithmetic-context)
                  :arithmetic-guard-context (plist-get plan :arithmetic-guard-context)
                  :arithmetic-guard-mode (plist-get plan :arithmetic-guard-mode)
                  :exit-root-base (plist-get plan :exit-root-base)
                  :initial-roots (plist-get plan :initial-roots)
                  :constant-initializers (plist-get plan :constant-initializers)
                  :immediate-initializers (plist-get plan :immediate-initializers)
                  :argument-count arity :required-root-count root-count
                  :join-count (plist-get context :join-count)
                  :selector-edge-count (plist-get context :selector-edge-count)
                  :expansion-count (plist-get context :expansion-count))))))))

(provide 'nelisp-bytecode-native-rooted-cfg-shared-emit)
;;; nelisp-bytecode-native-rooted-cfg-shared-emit.el ends here
