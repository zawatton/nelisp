;;; nelisp-bytecode-native-rooted-cfg-emit.el --- rooted CFG source AST emitter -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Expand a verified bounded acyclic rooted-CFG plan into raw-v2 source AST.
;; This is source generation only; artifact publication and native execution
;; remain with the authenticated raw-v2 loader/caller.

;;; Code:

(require 'cl-lib)
(require 'nelisp-bytecode-native-rooted-cfg-plan)

(defun nelisp-bytecode-native-rooted-cfg-emit--resolve-root (plan root phi-roots)
  (let* ((phi-p (and (consp root) (eq (car root) :phi)))
         (resolved (if phi-p
                       (cdr (assoc (list :id (cadr root)) phi-roots))
                     root)))
    (and (integerp resolved)
         (<= 0 resolved)
         (< resolved (plist-get plan :required-root-count))
         resolved)))

(cl-defun nelisp-bytecode-native-rooted-cfg-emit (plan entry-name)
  "Emit PLAN as a raw-v2 ENTRY-NAME source form, or refuse before effects.

The plan is recomputed from its verified compiler input before emission.  The
result contains a Lisp AST suitable for `nelisp-aot-compile-to-link-unit'; it
does not write an artifact or invoke a backend." 
  (let* ((input (plist-get plan :input))
         (lowering-mode (plist-get plan :lowering-mode))
         (verified (and input
                        (nelisp-bytecode-native-rooted-cfg-plan input lowering-mode)))
         (blocks (plist-get plan :blocks))
         (block-map (make-hash-table :test #'eql))
         (root-count (plist-get plan :required-root-count))
         (arity (plist-get plan :arity))
         (entry-ast nil)
         (active 0)
         (failure nil))
    (unless (and (eq (plist-get plan :status) 'complete)
                 (equal verified plan)
                 (eq (plist-get (plist-get plan :entry-ast) :kind)
                     (if (eq lowering-mode 'safe-primitives-v3)
                         'rooted-cfg-safe-primitives-v3 'rooted-cfg-v1))
                 (stringp entry-name) (> (length entry-name) 0)
                 (integerp arity) (>= arity 0)
                 (integerp root-count) (> root-count 0) (< root-count 256)
                 (vectorp (plist-get input :constants))
                 (vectorp (plist-get (plist-get input :frame-result) :blocks))
                 (<= (length (plist-get (plist-get input :frame-result) :blocks)) 12))
      (cl-return-from nelisp-bytecode-native-rooted-cfg-emit
        (list :status 'unsupported
              :reason "input plan is not an unchanged, bounded verified rooted-CFG plan")))
    (dolist (block blocks)
      (let ((start (plist-get block :start)))
        (if (or (not (integerp start)) (gethash start block-map))
            (setq failure "plan has invalid or duplicate block IDs")
          (puthash start block block-map))))
    (unless failure
      (cl-labels
          ((block-form
            (start predecessor path inherited-phi-roots)
            (let ((block (gethash start block-map)))
              (cond
               ((or (null block) (memq start path))
                (setq failure "emission encountered missing block or cycle")
                0)
               (t
                (let ((phi-roots (copy-tree inherited-phi-roots)))
                  (dolist (phi (plist-get block :phis))
                    (let* ((incoming (cdr (assq predecessor (plist-get phi :incoming))))
                           (root (nelisp-bytecode-native-rooted-cfg-emit--resolve-root
                                  plan incoming inherited-phi-roots)))
                      (if (integerp root)
                          (progn
                            (push (cons (list :id (plist-get phi :id)) root) phi-roots))
                        (setq failure "phi has no resolvable incoming root for this path"))))
                  (setq active (1+ active))
                  (if (or failure (> active 3072))
                      (progn (unless failure
                               (setq failure "structured expansion exceeds path bound"))
                             0)
                    (instruction-forms
                     (plist-get block :operations) 0 start phi-roots
                     (cons start path))))))))
           (edge-target
            (block kind)
            (let ((matches (cl-remove-if-not
                            (lambda (edge) (eq (plist-get edge :kind) kind))
                            (plist-get block :successors))))
              (and (= (length matches) 1)
                   (plist-get (car matches) :target))))
           (successor-form
            (block phi-roots path)
            (let ((successors (plist-get block :successors)))
              (cond
               ((null successors) (setq failure "non-return block has no successor") 0)
               ((= (length successors) 1)
                (block-form (plist-get (car successors) :target)
                            (plist-get block :start) path phi-roots))
               (t (setq failure "only explicit conditional two-edge blocks are supported") 0))))
           (instruction-forms
            (operations index start phi-roots path)
            (if (>= index (length operations))
                (successor-form (gethash start block-map) phi-roots path)
              (let* ((operation (nth index operations))
                     (opcode (plist-get operation :opcode))
                     (next (lambda ()
                             (instruction-forms operations (1+ index) start
                                                phi-roots path))))
                (cond
                 ((memq opcode '(const stack-ref dup discard)) (funcall next))
                 ((memq opcode '(primitive-call funcall))
                  (nelisp-native-funcall-v2-emit
                   operation
                   (nelisp-bytecode-native-rooted-cfg-emit--resolve-root plan (plist-get operation :function-root) phi-roots)
                   (mapcar (lambda (root) (nelisp-bytecode-native-rooted-cfg-emit--resolve-root plan root phi-roots))
                           (plist-get operation :argument-roots)) (funcall next)))
                 ((memq opcode '(car cdr cons car-safe cdr-safe))
                  (let* ((inputs (mapcar
                                  (lambda (root)
                                    (nelisp-bytecode-native-rooted-cfg-emit--resolve-root
                                     plan root phi-roots))
                                  (plist-get operation :input-roots)))
                         (output (plist-get operation :output-root))
                         (name (pcase opcode
                                 ((or 'car 'car-safe) 'nl_native_car_v2)
                                 ((or 'cdr 'cdr-safe) 'nl_native_cdr_v2)
                                 ('cons 'nl_native_cons_v2)))
                         (status (intern (format "rooted_%s_status" opcode)))
                         (safe-op (memq opcode '(car-safe cdr-safe)))
                         (type-root (and (memq opcode '(car cdr)) (car inputs)))
                         (output-slot (intern (format "rooted_%s_output_slot" opcode)))
                         (call (if (= (length inputs) 2)
                                   `(extern-call ,name env ticket ,(nth 0 inputs)
                                                 ,(nth 1 inputs) ,output 0)
                                 `(extern-call ,name env ticket ,(car inputs)
                                               ,output 0 0))))
                    (unless (and (cl-every #'integerp inputs)
                                 (integerp output) (<= 0 output) (< output root-count))
                      (setq failure "gateway root index is missing or outside the reserved frame"))
                    (if failure 0
                      `(let ((,status ,call))
                         (if (= ,status 0) ,(funcall next)
                           ,(cond
                             (safe-op
                              `(if (= ,status 1)
                                   (let ((,output-slot
                                          (extern-call nl_root_pin_slot_v2
                                                       env ticket ,output 0 0 0)))
                                     (if (= ,output-slot 0) 2
                                       (progn
                                         (ptr-write-u64 ,output-slot 0 0)
                                         (ptr-write-u64 ,output-slot 8 0)
                                         (ptr-write-u64 ,output-slot 16 0)
                                         (ptr-write-u64 ,output-slot 24 0)
                                         ,(funcall next))))
                                 ,status))
                             (type-root
                              `(if (= ,status 1) (+ 256 ,type-root) ,status))
                             (t status)))))))
                 ((eq opcode 'return)
                  (let ((root (nelisp-bytecode-native-rooted-cfg-emit--resolve-root
                               plan (car (plist-get operation :input-roots)) phi-roots)))
                    (if (integerp root) (+ 512 root)
                      (setq failure "return selects an unresolved root") 0)))
                 ((eq opcode 'goto)
                  (let ((target (edge-target (gethash start block-map) 'goto)))
                    (if (integerp target) (block-form target start path phi-roots)
                      (setq failure "goto edge is absent or ambiguous") 0)))
                 ((eq opcode 'conditional-branch)
                  (let* ((block (gethash start block-map))
                         (taken (edge-target block 'taken))
                         (fallthrough (edge-target block 'fallthrough))
                         (condition (nelisp-bytecode-native-rooted-cfg-emit--resolve-root
                                     plan (car (plist-get operation :input-roots)) phi-roots))
                         (slot (intern (format "root_slot_%d" start)))
                         (nil-branch (eq (plist-get operation :taken-if) 'nil)))
                    (unless (and (integerp condition) taken fallthrough)
                      (setq failure "branch root or successor edge is invalid"))
                    (if failure 0
                      `(let ((,slot (extern-call nl_root_pin_slot_v2
                                                 env ticket ,condition 0 0 0)))
                         (if (= ,slot 0) 2
                           (if (= (ptr-read-u64 ,slot 0) 0)
                               ,(if nil-branch
                                    (block-form taken start path phi-roots)
                                  (block-form fallthrough start path phi-roots))
                             ,(if nil-branch
                                  (block-form fallthrough start path phi-roots)
                                (block-form taken start path phi-roots))))))))
                 (t (setq failure (format "unsupported planned operation %S" opcode)) 0))))))
        (let ((entry-block (car blocks)))
          (when entry-block
            (setq entry-ast
                  (block-form (plist-get entry-block :start) nil nil nil))))))
    (if failure
        (list :status 'unsupported :reason failure)
      (list :status 'complete :entry-name entry-name :form
            `(defun ,(intern entry-name) (env ticket argument-count root-count)
               (if (/= argument-count ,arity) 3
                 (if (/= root-count ,root-count) 3 ,entry-ast)))
            :gateway-imports
            (sort (delete-dups
                   (append (plist-get plan :gateway-imports)
                           '("nl_root_pin_slot_v2")))
                  #'string<)
            :primitive-initializers (plist-get plan :primitive-initializers)
            :exit-root-base (plist-get plan :exit-root-base)
            :initial-roots (plist-get plan :initial-roots)
            :constant-initializers (plist-get plan :constant-initializers)
            :immediate-initializers (plist-get plan :immediate-initializers)
            :argument-count arity :required-root-count root-count
            :expansion-count active))))

(provide 'nelisp-bytecode-native-rooted-cfg-emit)
;;; nelisp-bytecode-native-rooted-cfg-emit.el ends here
