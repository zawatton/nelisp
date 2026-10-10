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
(require 'nelisp-native-cfg-grammar)

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
  (if (plist-get context :cyclic)
      (list 'cfg-edge (cdr (assoc (cons from target) (plist-get context :edge-labels))))
    (if (and stop (= target stop))
      (nelisp-bytecode-native-rooted-cfg-shared-emit--edge-assignments context from target)
    (nelisp-bytecode-native-rooted-cfg-shared-emit--block context target stop path))))

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
              (let* ((body `(let ((,status (extern-call ,@(plist-get lowered :call))))
                             (if (= ,status 0) 0
                               (if (= ,status 1) ,(+ 1024 (plist-get record :exit-root-base)) 2))))
                     (slow (if materialized
                               (nelisp-bytecode-native-rooted-cfg-shared-emit--materialize-inputs
                                inputs materialized id index body) body)))
                `(let ((,status ,(nelisp-native-funcall-v2-fixnum-form
                                  92 inputs (plist-get operation :output-root) 0 slow)))
                   (if (= ,status 0)
                       ,(nelisp-bytecode-native-rooted-cfg-shared-emit--operations
                         context block (1+ index) stop path) ,status))))))
         ((eq opcode 'switch)
          (let* ((output (plist-get operation :output-root))
                 (edges (plist-get block :successors))
                 (fall (cl-find 'fallthrough edges :key (lambda (e) (plist-get e :kind))))
                 (cases (mapcar (lambda (edge)
                                  (list (plist-get edge :target)
                                        (cadr (nelisp-bytecode-native-rooted-cfg-shared-emit--to
                                               context id (plist-get edge :target) nil nil))))
                                (cl-remove-if-not (lambda (e) (eq (plist-get e :kind) 'switch)) edges)))
                 (default (cadr (nelisp-bytecode-native-rooted-cfg-shared-emit--to
                                context id (plist-get fall :target) nil nil))))
            (nelisp-native-funcall-v2-emit
             operation (plist-get operation :function-root) (plist-get operation :argument-roots)
             `(let ((switch_slot (extern-call nl_root_pin_slot_v2 env ticket ,output 0 0 0)))
                (if (= switch_slot 0) 2
                  (cfg-dispatch (ptr-read-u64 switch_slot 8) ,cases ,default))))))
         ((memq opcode '(frame-specbind frame-unbind frame-save frame-cleanup handler-catch handler-condition handler-pop))
          (nelisp-native-frame-v2-copy-emit
           (plist-get context :plan) (plist-get operation :frame-action)
           (mapcar (lambda (root) (nelisp-bytecode-native-rooted-cfg-shared-emit--resolve context root))
                   (plist-get operation :argument-roots))
           (nelisp-bytecode-native-rooted-cfg-shared-emit--operations context block (1+ index) stop path)))
         ((memq opcode '(primitive-call list-build funcall frame-varref frame-varset))
          (let ((function (nelisp-bytecode-native-rooted-cfg-shared-emit--resolve
                           context (plist-get operation :function-root)))
                (inputs (mapcar (lambda (root)
                                  (nelisp-bytecode-native-rooted-cfg-shared-emit--resolve context root))
                                (plist-get operation :argument-roots))))
            (if (and function (cl-every #'identity inputs))
                (let ((next (nelisp-bytecode-native-rooted-cfg-shared-emit--operations
                             context block (1+ index) stop path)))
                  (when (memq (plist-get operation :bytecode-opcode) '(144 145))
                    (setq next (nelisp-native-frame-v2-copy-emit
                                (plist-get context :plan)
                                (if (= (plist-get operation :bytecode-opcode) 144) 1 2)
                                (if (= (plist-get operation :bytecode-opcode) 144)
                                    (list (plist-get operation :legacy-binding-root) (plist-get operation :output-root))
                                  (list (plist-get operation :legacy-binding-root))) next)))
                  (if (eq opcode 'list-build)
                      (nelisp-native-funcall-v2-emit-list
                       operation function inputs next (plist-get context :cyclic))
                    (nelisp-native-funcall-v2-emit operation function inputs next
                                                     (and (plist-get (plist-get context :plan) :handler-bank)
                                                          (plist-get context :plan)))))
              (nelisp-bytecode-native-rooted-cfg-shared-emit--fail context "Unresolved F1 function/argument") 0)))
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

(defun nelisp-bytecode-native-rooted-cfg-shared-emit--structured-branch
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

(defun nelisp-bytecode-native-rooted-cfg-shared-emit--branch (context block operation stop path)
  (if (not (plist-get context :cyclic))
      (nelisp-bytecode-native-rooted-cfg-shared-emit--structured-branch context block operation stop path)
    (let* ((id (plist-get block :start))
           (edges (plist-get block :successors))
           (taken (cl-find 'taken edges :key (lambda (e) (plist-get e :kind))))
           (fall (cl-find 'fallthrough edges :key (lambda (e) (plist-get e :kind))))
           (root (car (plist-get operation :input-roots)))
           (yes (nelisp-bytecode-native-rooted-cfg-shared-emit--to context id (plist-get taken :target) nil nil))
           (no (nelisp-bytecode-native-rooted-cfg-shared-emit--to context id (plist-get fall :target) nil nil)))
      `(let ((cycle_condition (extern-call nl_root_pin_slot_v2 env ticket ,root 0 0 0)))
         (if (= cycle_condition 0) 2
           (if (= (ptr-read-u64 cycle_condition 0) 0)
               ,(if (eq (plist-get operation :taken-if) 'nil) yes no)
             ,(if (eq (plist-get operation :taken-if) 'nil) no yes)))))))

(defun nelisp-bytecode-native-rooted-cfg-shared-emit--compact (cfg)
  "Fuse straight-line generated blocks without duplicating effects or loop heads."
  ;; Redirecting the sole incoming edge preserves the incoming counts of
  ;; every surviving block.  Index once instead of repeatedly scanning and
  ;; copying the entire graph; frame status/exit paths make that scan costly.
  (let* ((blocks (copy-tree (cdddr cfg)))
         (map (make-hash-table :test 'eq))
         (counts (make-hash-table :test 'eq))
         (tails (make-hash-table :test 'eq)))
    (puthash (nth 2 cfg) 1 counts)
    (dolist (b blocks)
      (puthash (cadr b) b map)
      (puthash (cadr b) (last (nth 2 b)) tails)
      (let* ((term (nth 3 b))
             (targets (pcase (car term) ('jump (cdr term)) ('branch (cddr term))
                        ('dispatch (cons (nth 3 term) (mapcar #'cadr (nth 2 term)))))))
        (dolist (id targets) (puthash id (1+ (gethash id counts 0)) counts))))
    (dolist (b blocks)
      (when (eq (gethash (cadr b) map) b)
        (let ((again t))
          (while again
            (let* ((term (nth 3 b))
                   (next (and (eq (car term) 'jump) (gethash (cadr term) map))))
              (if (and next (= (gethash (cadr next) counts 0) 1) (not (eq b next)))
                  (progn
                    (when (nth 2 next)
                      (if (gethash (cadr b) tails)
                          (setcdr (gethash (cadr b) tails) (nth 2 next))
                        (setf (nth 2 b) (nth 2 next)))
                      (puthash (cadr b) (gethash (cadr next) tails) tails))
                    (setf (nth 3 b) (nth 3 next))
                    ;; MAP is queried only by label, never enumerated. A nil
                    ;; tombstone has exactly the deleted-entry lookup result
                    ;; and avoids rebuilding the standalone table on removal.
                    (puthash (cadr next) nil map))
                (setq again nil)))))))
    (cons 'cfg (cons 1 (cons (nth 2 cfg)
                            (cl-remove-if-not (lambda (b) (eq (gethash (cadr b) map) b)) blocks))))))

(defun nelisp-bytecode-native-rooted-cfg-shared-emit--cycles (plan entry-name)
  "Emit each verified block once, with selectors or parallel banked root copies."
  (let* ((planned (plist-get plan :blocks)) (serial 0) (raw-blocks nil) (locals nil)
         (block-map (make-hash-table :test 'eql)) (edge-labels nil)
         (free-cache (make-hash-table :test 'eq))
         (lower-cache (make-hash-table :test 'eq))
         (context (list :cyclic t :plan plan :root-count (plist-get plan :required-root-count)
                        :block-map block-map :edge-labels nil :phi-vars nil
                        :phi-var-set (make-hash-table :test #'eq)
                        :selector-edge-count 0 :failure nil)))
    (cl-labels
        ((fresh () (setq serial (1+ serial)) (intern (format "cycle_local_%d" serial)))
         (local () (let ((name (fresh))) (push (list name 0) locals) name))
         (block (forms term &optional id)
           (let ((name (or id (fresh)))) (push (list 'block name forms term) raw-blocks) name))
         (rename (node scope)
           (cond ((symbolp node) (or (cdr (assq node scope)) node))
                 ((atom node) node) ((eq (car node) 'quote) node)
                 (t (mapcar (lambda (child) (rename child scope)) node))))
         (sequence (forms scope out next)
           (let ((entry next))
             (dolist (node (reverse forms)) (setq entry (lower node scope out entry))) entry))
         (identity-get (cache node)
           (assq node (gethash (sxhash-eq node) cache)))
         (identity-put (cache node value)
           ;; Integer keys avoid structural hashing and mutable-cons fallback
           ;; scans in the standalone hash table. Collisions retain EQ tests.
           (let* ((key (sxhash-eq node)) (bucket (gethash key cache))
                  (entry (assq node bucket)))
             (if entry (setcdr entry value)
               (puthash key (cons (cons node value) bucket) cache)))
           value)
         (free-symbols (node)
           ;; Cache relative free names by node identity. Binding names and
           ;; quoted data do not depend on the surrounding rename scope.
           (let ((cached (identity-get free-cache node)))
             (if cached (cdr cached)
               (let ((names
                      (cond
                       ((symbolp node) (list node)) ((atom node) nil)
                       ((eq (car node) 'quote) nil)
                       ((memq (car node) '(let let*))
                        (let ((bound nil) (used nil))
                          (dolist (binding (cadr node))
                            (dolist (name (free-symbols (cadr binding)))
                              (unless (and (eq (car node) 'let*) (memq name bound))
                                (push name used)))
                            (push (car binding) bound))
                          (dolist (body (cddr node))
                            (dolist (name (free-symbols body))
                              (unless (memq name bound) (push name used))))
                          used))
                       (t (let ((used nil))
                            (dolist (child node) (setq used (append (free-symbols child) used)))
                            used)))))
                 (setq names (delete-dups names))
                 (identity-put free-cache node names)))))
         (lower (node scope out next)
           ;; Mutually exclusive branches can share a continuation only when
           ;; every free name resolves to the same local, and output/next agree.
           ;; Ignore unused outer bindings; they cannot affect this fragment.
           (let* ((cached (cdr (identity-get lower-cache node)))
                  (remaining cached) (bindings nil) (bindings-known nil) (entry nil))
             ;; First visits cannot hit. Defer the relative free-name walk
             ;; until an existing output/continuation needs a scope comparison.
             ;; Scopes are persistent lists of immutable rename pairs.
             (while (and remaining (not entry))
               (let ((record (car remaining)))
                 (when (and (equal out (aref record 0)) (equal next (aref record 1)))
                   (if (eq scope (aref record 2)) (setq entry record)
                     (unless bindings-known
                       (setq bindings-known t
                             bindings (mapcar (lambda (name) (or (cdr (assq name scope)) name))
                                              (free-symbols node))))
                     (unless (aref record 3)
                       (aset record 4
                             (mapcar (lambda (name) (or (cdr (assq name (aref record 2))) name))
                                     (free-symbols node)))
                       (aset record 3 t))
                     (when (equal bindings (aref record 4)) (setq entry record)))))
               (setq remaining (cdr remaining)))
             (if entry (aref entry 5)
               (let ((label (lower-new node scope out next)))
                 (identity-put lower-cache node
                               (cons (vector out next scope bindings-known bindings label) cached))
                 label))))
         (lower-new (node scope out next)
           (cond
            ((and (consp node) (eq (car node) 'cfg-repeat))
             ;; Internal list-construction repetition; emitted raw CFG uses
             ;; only the already validated shared jump/branch grammar.
             (let* ((counter (local)) (head (fresh))
                    (increment (block (list `(setq ,counter (+ ,counter 1))) `(jump ,head)))
                    (body (lower (nth 3 node) scope out increment)))
               (block nil `(branch (and (= ,(rename (nth 2 node) scope) 0)
                                        (< ,counter ,(nth 1 node)))
                                   ,body ,next) head)
               (block (list `(setq ,counter 0)) `(jump ,head))))
            ((and (consp node) (eq (car node) 'cfg-dispatch))
             (block nil (list 'dispatch (rename (cadr node) scope) (nth 2 node) (nth 3 node))))
            ((and (consp node) (eq (car node) 'cfg-edge))
             (block nil (list 'jump (cadr node))))
            ((and (consp node) (eq (car node) 'if))
             (let ((yes (lower (nth 2 node) scope out next))
                   (no (lower (nth 3 node) scope out next)))
               (block nil `(branch ,(rename (cadr node) scope) ,yes ,no))))
            ((and (consp node) (memq (car node) '(progn seq)))
             (sequence (cdr node) scope out next))
            ((and (consp node) (memq (car node) '(let let*)))
             (let ((inner scope) (initializers nil))
               (dolist (binding (cadr node))
                 (let ((name (local)))
                   (push (list name (rename (cadr binding) (if (eq (car node) 'let*) inner scope))) initializers)
                   (push (cons (car binding) name) inner)))
               (let ((entry (sequence (cddr node) inner out next)))
                 (dolist (binding initializers)
                   (setq entry (block (list `(setq ,(car binding) ,(cadr binding))) `(jump ,entry)))) entry)))
            (t (block (list `(setq ,out ,(rename node scope))) `(jump ,next))))))
      ;; Acyclic plans keep phi values as root selectors, rather than the
      ;; physical root banks used by cycles.  Give those selectors stable CFG
      ;; locals so compact allocation loops can also follow a DAG join.
      (unless (plist-get plan :banked)
        (dolist (phi (plist-get plan :phis))
          (let ((variable (local)))
            (push (cons (plist-get phi :id) variable) (plist-get context :phi-vars))
            (puthash variable t (plist-get context :phi-var-set)))))
      (dolist (b planned)
        (puthash (plist-get b :start) b block-map)
        (dolist (e (plist-get b :successors))
          (let ((key (cons (plist-get b :start) (plist-get e :target))))
            (unless (assoc key edge-labels) (push (cons key (fresh)) edge-labels)))))
      (plist-put context :edge-labels edge-labels)
      (let* ((out (local)) (finish (fresh)) (entered (local)))
        ;; Create the common status exit only if one generated path uses it.
        (nelisp-bytecode-native-rooted-cfg-shared-emit--stage "raw-blocks-start")
        (dolist (b planned)
          (let* ((id (plist-get b :start))
                 (body (nelisp-bytecode-native-rooted-cfg-shared-emit--operations context b 0 nil nil))
                 (entry (lower body nil out finish)))
            (block nil (list 'jump entry) id))
          (dolist (e (cl-delete-duplicates (copy-sequence (plist-get b :successors))
                                           :key (lambda (edge) (plist-get edge :target)) :test #'eql))
            (let* ((from (plist-get b :start)) (to (plist-get e :target))
                   (target (gethash to block-map))
                   (phis (plist-get target :phis))
                   (inputs (mapcar (lambda (phi) (cdr (assq from (plist-get phi :incoming)))) phis))
                   (destinations (mapcar (lambda (phi) (plist-get phi :root)) phis))
                   (scratch (and (plist-get plan :banked)
                                 (cl-subseq (plist-get plan :copy-roots) 0 (length phis))))
                   (body (list 'cfg-edge to)))
              (when (plist-get plan :handler-bank)
                (let* ((sources (cdr (assq to (plist-get b :bank-edge-copies))))
                       (pairs (cl-remove-if (lambda (pair) (= (car pair) (cdr pair)))
                                            (cl-mapcar #'cons sources
                                                       (cl-subseq (plist-get plan :handler-bank) 0 (length sources)))))
                       (scratch (cl-subseq (plist-get plan :copy-roots) 0 (length pairs))))
                  ;; Identity copies publish nothing. One changed cell needs no
                  ;; parallel staging; multiple cells still snapshot all inputs.
                  (setq body (if (= (length pairs) 1)
                                 (nelisp-native-frame-v2-bank-copy-emit
                                  plan                                   (mapcar #'car pairs) (mapcar #'cdr pairs) body)
                               (nelisp-native-frame-v2-bank-copy-emit
                                  plan                                 (mapcar #'car pairs) scratch
                                (nelisp-native-frame-v2-bank-copy-emit plan scratch (mapcar #'cdr pairs) body))))))
              (when (memq to (plist-get b :poll-targets))
                (let* ((result (plist-get plan :result-root))
                       (exit (plist-get plan :exit-root-base))
                       (quit-form (if (plist-get plan :handler-bank)
                                      (nelisp-native-frame-v2-bank-copy-emit
                                       plan (plist-get plan :poll-exit-roots)
                                       (number-sequence exit (+ exit 2)) (+ 1024 exit))
                                    (nelisp-native-funcall-v2-copy-form
                                     (plist-get plan :poll-exit-roots)
                                     (number-sequence exit (+ exit 2)) (+ 1024 exit)))))
                  (setq body
                        (nelisp-native-funcall-v2-emit
                         (list :poll t :pc (+ 100000 from) :staging-roots nil
                               :result-root result :output-root result)
                         (plist-get plan :poll-root) nil
                         `(let ((cycle_poll_slot (extern-call nl_root_pin_slot_v2 env ticket ,result 0 0 0)))
                            (if (= cycle_poll_slot 0) 2
                              (if (= (ptr-read-u64 cycle_poll_slot 0) 0) ,body ,quit-form)))
                         (and (plist-get plan :handler-bank) plan)))))
              (setq body
                    (if (plist-get plan :banked)
                        (nelisp-native-funcall-v2-copy-form
                         inputs scratch (nelisp-native-funcall-v2-copy-form scratch destinations body))
                      `(progn ,(nelisp-bytecode-native-rooted-cfg-shared-emit--edge-assignments
                                context from to) ,body)))
              (block nil (list 'jump (lower body nil out finish))
                     (cdr (assoc (cons from to) edge-labels))))))
        (nelisp-bytecode-native-rooted-cfg-shared-emit--stage "raw-blocks-end")
        (let* ((copies (plist-get plan :entry-copies))
               (body (if (plist-get plan :handler-bank)
                         (nelisp-native-frame-v2-bank-copy-emit
                          plan (mapcar #'car copies) (mapcar #'cdr copies)
                          (list 'cfg-edge (plist-get (car planned) :start)))
                       (nelisp-native-funcall-v2-copy-form
                        (mapcar #'car copies) (mapcar #'cdr copies)
                        (list 'cfg-edge (plist-get (car planned) :start)))))
               (body (if (plist-get plan :frame-state-root)
                         (nelisp-native-frame-v2-copy-emit plan 0 (plist-get plan :frame-enter-roots) `(progn (setq ,entered 1) ,body))
                       body))
               (entry (lower body nil out finish))
               (guard (lower `(if (/= argument-count ,(plist-get plan :arity)) 3
                               (if (/= root-count ,(plist-get plan :required-root-count)) 3
                                 (cfg-edge ,entry))) nil out finish)))
          (when (cl-some (lambda (b) (member finish (cdr (nth 3 b)))) raw-blocks)
            (if (plist-get plan :frame-state-root)
                (let* ((leave-out (local)) (returned (block nil (list 'return leave-out)))
                       (epilogue (lower `(if (= ,entered 1)
                                     (if (= ,out 2)
                                         (progn (extern-call nl_native_frame_v2 env ticket
                                                             ,(plist-get plan :frame-state-root) 3
                                                             ,(car (plist-get plan :frame-staging-roots))
                                                             ,(plist-get plan :frame-result-root)) 2)
                                       ,(nelisp-native-frame-v2-copy-emit plan 3 nil out))
                                   ,out)
                                        nil leave-out returned)))
                  (if (plist-get plan :handler-bank)
                      (let* ((status (local)) (target (local))
                             (result (plist-get plan :frame-result-root))
                             (exit (plist-get plan :exit-root-base))
                             (landing
                              (lower
                               `(if (and (= ,entered 1) (= ,out ,(+ 1024 exit)))
                                    (let ((,status (extern-call nl_native_frame_v2 env ticket
                                                               ,(plist-get plan :frame-state-root) 11 ,exit ,result)))
                                      (if (= ,status 0)
                                          (let ((,target (extern-call nl_root_pin_slot_v2 env ticket ,result 0 0 0)))
                                            (if (= ,target 0) 2
                                              (cfg-dispatch (ptr-read-u64 ,target 8)
                                                            ,(mapcar (lambda (pc) (list pc pc)) (plist-get plan :handler-targets))
                                                            ,epilogue)))
                                        (progn (setq ,out ,status) (cfg-edge ,epilogue))))
                                  (cfg-edge ,epilogue))
                               nil leave-out returned)))
                        (block nil (list 'jump landing) finish))
                    (block nil (list 'jump epilogue) finish)))
              (block nil (list 'return out) finish)))
          (nelisp-bytecode-native-rooted-cfg-shared-emit--stage "raw-compact-start")
          (let ((cfg (nelisp-bytecode-native-rooted-cfg-shared-emit--compact
                      (cons 'cfg (cons 1 (cons guard (nreverse raw-blocks)))))))
            (nelisp-bytecode-native-rooted-cfg-shared-emit--stage "raw-compact-end")
            (if (plist-get context :failure)
                (list :status 'unsupported :reason (plist-get context :failure))
              (nelisp-native-cfg-grammar-validate cfg)
              (append (list :status 'complete :entry-name entry-name
                          :form `(defun ,(intern entry-name) (env ticket argument-count root-count)
                                   (let ,(nreverse locals) ,cfg))
                          :argument-count (plist-get plan :arity)
                          :required-root-count (plist-get plan :required-root-count)
                          :gateway-imports (plist-get plan :gateway-imports)
                          :join-count 0 :selector-edge-count (length edge-labels)
                          :expansion-count (length planned))
                    (cl-loop for key in '(:exit-root-base :primitive-initializers :initial-roots
                                         :constant-initializers :immediate-initializers)
                             append (list key (plist-get plan key)))))))))))

(declare-function nelisp-native-cache--stage "nelisp-native-cache")
(defun nelisp-bytecode-native-rooted-cfg-shared-emit--stage (label)
  "Report opt-in compile phases without changing the emitted function."
  (when (and (getenv "NELISP_ROOTED_CFG_STAGE_LOG")
             (fboundp 'nelisp-native-cache--stage))
    (nelisp-native-cache--stage label)))

(let ((postdom-owner (symbol-function 'nelisp-bytecode-native-rooted-cfg-postdom-analyze))
      (planner-owner (symbol-function 'nelisp-bytecode-native-rooted-cfg-plan))
      (lookup (symbol-function 'symbol-function))
      (same (symbol-function 'eq))
      (emit-verified nil))
  ;; This closure accepts a fresh planner result only from the two entries
  ;; below. It is never published as a Lisp function or callable token.
  (setq emit-verified
        (lambda (plan verified entry-name)
          (let* ((input (plist-get plan :input))
           (raw-cfg (or (plist-get verified :handler-bank)
			(and (plist-get verified :banked)
			     ;; An acyclic frame activation needs one shared epilogue.
			     ;; Single-block entry phis are already materialized in the
			     ;; physical bank by entry copies; they need no CFG labels.
			     (not (and (plist-get verified :frame-state-root)
				       (or (null (plist-get verified :phis))
					   (and (= (length (plist-get verified :blocks)) 1)
						(cl-every
						 (lambda (phi)
						   (and (null (plist-get phi :incoming))
							(integerp (plist-get phi :root))
							(cl-find (plist-get phi :root)
								 (plist-get verified :entry-copies)
								 :key #'cdr :test #'eql)))
						 (plist-get verified :phis))))
				       (not (plist-get verified :cyclic))
				       (not (cl-some
					     (lambda (b) (cl-some (lambda (op) (eq (plist-get op :opcode) 'switch))
								  (plist-get b :operations)))
					     (plist-get verified :blocks))))))
			(cl-some (lambda (b)
				   (cl-some (lambda (op) (eq (plist-get op :opcode) 'list-build))
					    (plist-get b :operations)))
				 (plist-get verified :blocks))
			;; A phi-free DAG can use immutable root selectors directly.
			;; Extended stack references therefore need no evaluator import.
			(and (null (plist-get verified :phis))
			     (cl-some (lambda (b)
					(cl-some (lambda (op) (memq (plist-get op :bytecode-opcode) '(6 7)))
						 (plist-get b :operations)))
				      (plist-get verified :blocks)))))
           ;; Handler plans always use the shared raw CFG. Their freshly
           ;; authenticated topology/bank has no structured postdominator use.
           ;; The public postdominator entry point admits another plan. This
           ;; entry point already has that fresh plan; retain its independent
           ;; input check, then analyze the same verified topology directly.
           (analysis
           (progn
            (nelisp-bytecode-native-rooted-cfg-shared-emit--stage "emit-analysis-start")
            (prog1
            (if (plist-get verified :handler-bank) verified
              (and input
                   (eq postdom-owner
                       (symbol-function 'nelisp-bytecode-native-rooted-cfg-postdom-analyze))
                   (eq (plist-get verified :status) 'complete)
                   (nelisp-bytecode-native-rooted-cfg-postdom--canonical-input-p input)
                   (let ((topology (nelisp-bytecode-native-rooted-cfg-topology-check
                                    (plist-get input :frame-result))))
                     (and (eq (plist-get topology :status) 'complete)
                          ;; Raw CFG emission uses no nearest-join selectors.
                          ;; Retain the fresh canonical input, owner and topology
                          ;; checks; only omit the unused postdominator fixed point.
                          (if raw-cfg verified
                            (nelisp-bytecode-native-rooted-cfg-postdom--compute
                             (append (plist-get (plist-get input :frame-result) :blocks) nil)
                             (plist-get topology :block-order)))))))
              (nelisp-bytecode-native-rooted-cfg-shared-emit--stage "emit-analysis-end")))))
    (if (not (and (eq (plist-get plan :status) 'complete)
                    (or (null (plist-get plan :exit-root-base))
			(plist-get plan :funcall-version)
			(nelisp-bytecode-native-rooted-cfg-plan-guard-context-p plan))
                    (equal verified plan)
                    (eq (plist-get analysis :status) 'complete)
                    (stringp entry-name) (> (length entry-name) 0)))
          (list :status 'unsupported :reason "shared emission needs an unchanged canonical rooted plan")
	(progn
          (nelisp-bytecode-native-rooted-cfg-shared-emit--stage (if raw-cfg "emit-raw-body-start" "emit-structured-body-start"))
          (if raw-cfg
            (nelisp-bytecode-native-rooted-cfg-shared-emit--cycles plan entry-name)
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
        ;; The verified plan bounds both block count and the physical root bank.
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
            (when (plist-get plan :frame-state-root)
              (let* ((copies (plist-get plan :entry-copies))
                     (entry-body (nelisp-native-funcall-v2-copy-form
                                  (mapcar #'car copies) (mapcar #'cdr copies) body)))
                (setq body
                      `(let ((frame_entered 0))
                         (let ((frame_output
                                ,(nelisp-native-frame-v2-copy-emit
                                  plan 0 (plist-get plan :frame-enter-roots)
                                  `(progn (setq frame_entered 1) ,entry-body))))
                           (if (= frame_entered 1)
                               (if (= frame_output 2)
                                   (progn (extern-call nl_native_frame_v2 env ticket
                                                       ,(plist-get plan :frame-state-root) 3
                                                       ,(car (plist-get plan :frame-staging-roots))
                                                       ,(plist-get plan :frame-result-root)) 2)
                                 ,(nelisp-native-frame-v2-copy-emit plan 3 nil 'frame_output))
                             frame_output))))))
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
                  (and (plist-get plan :arithmetic-context)
                       (nelisp-native-optimization-guard-v1-source
                        (plist-get plan :arithmetic-guard-mode)))
                  :arithmetic-context (plist-get plan :arithmetic-context)
                  :arithmetic-guard-context (plist-get plan :arithmetic-guard-context)
                  :arithmetic-guard-mode (plist-get plan :arithmetic-guard-mode)
                  :exit-root-base (plist-get plan :exit-root-base)
                  :primitive-initializers (plist-get plan :primitive-initializers)
                  :initial-roots (plist-get plan :initial-roots)
                  :constant-initializers (plist-get plan :constant-initializers)
                  :immediate-initializers (plist-get plan :immediate-initializers)
                  :argument-count arity :required-root-count root-count
                  :join-count (plist-get context :join-count)
                  :selector-edge-count (plist-get context :selector-edge-count)
                  :expansion-count (plist-get context :expansion-count)))))))))))

  (defun nelisp-bytecode-native-rooted-cfg-shared-emit-build (plan entry-name)
    "Emit PLAN only after independently rebuilding and comparing it."
    (let* ((input (plist-get plan :input))
           (verified
            (progn
              (nelisp-bytecode-native-rooted-cfg-shared-emit--stage "emit-replan-start")
              (prog1 (and input (nelisp-bytecode-native-rooted-cfg-plan
                                input (plist-get plan :lowering-mode)
                                (plist-get plan :arithmetic-guard-mode)))
                (nelisp-bytecode-native-rooted-cfg-shared-emit--stage "emit-replan-end")))))
      (funcall emit-verified plan verified entry-name)))

  (defun nelisp-bytecode-native-rooted-cfg-shared-emit-build-from-input
      (input entry-name &optional lowering-mode arithmetic-guard-mode)
    "Return a fresh :plan and :emitted pair built from INPUT.
No caller-supplied plan, certificate or validation switch is accepted. The
planner result remains private until emission and owner checks complete."
    (if (not (funcall same planner-owner
                      (funcall lookup 'nelisp-bytecode-native-rooted-cfg-plan)))
        (list :plan nil :emitted (list :status 'unsupported :reason "planner owner changed"))
      (let* ((plan (funcall planner-owner input lowering-mode arithmetic-guard-mode))
             (emitted (and (eq (plist-get plan :status) 'complete)
                           (funcall emit-verified plan plan entry-name))))
        (if (funcall same planner-owner
                     (funcall lookup 'nelisp-bytecode-native-rooted-cfg-plan))
            (list :plan plan :emitted emitted)
          (list :plan nil :emitted (list :status 'unsupported :reason "planner owner changed")))))))

(provide 'nelisp-bytecode-native-rooted-cfg-shared-emit)
;;; nelisp-bytecode-native-rooted-cfg-shared-emit.el ends here
