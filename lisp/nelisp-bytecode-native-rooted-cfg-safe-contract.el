;;; nelisp-bytecode-native-rooted-cfg-safe-contract.el --- safe CFG contract -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Inert, independently reconstructed contract for the opt-in safe-primitive
;; rooted-CFG mode.  This module does not publish artifacts or admit them to a
;; loader.

;;; Code:

(require 'cl-lib)
(require 'nelisp-bytecode-compiler-input)
(require 'nelisp-bytecode-native-rooted-cfg-contract)
(require 'nelisp-bytecode-native-rooted-cfg-plan)
(require 'nelisp-bytecode-native-rooted-cfg-emit)

(defconst nelisp-bytecode-native-rooted-cfg-safe-contract-version
  "nelisp-native-rooted-cfg-safe-v3")
(defconst nelisp-bytecode-native-rooted-cfg-safe-contract-f1-version
  "nelisp-native-rooted-cfg-safe-f1-v1")
(defconst nelisp-bytecode-native-rooted-cfg-safe-contract-entry
  "nl_native_rooted_cfg_safe_probe_v3")

(defun nelisp-bytecode-native-rooted-cfg-safe-contract--data-p
    (value depth budget &optional byte-code-opaque)
  (and (consp budget) (> (car budget) 0)
       (setcar budget (1- (car budget)))
       (<= depth 256)
       (cond ((and byte-code-opaque (byte-code-function-p value)) t)
             ((or (null value) (eq value t) (integerp value) (floatp value)
                  (symbolp value)) t)
             ((stringp value) (<= (length value) 1048576))
             ((consp value)
              (and (nelisp-bytecode-native-rooted-cfg-safe-contract--data-p
                    (car value) (1+ depth) budget byte-code-opaque)
                   (nelisp-bytecode-native-rooted-cfg-safe-contract--data-p
                    (cdr value) (1+ depth) budget byte-code-opaque)))
             ((vectorp value)
              (and (<= (length value) 4096)
                   (cl-loop for item across value
                            always
                            (nelisp-bytecode-native-rooted-cfg-safe-contract--data-p
                             item (1+ depth) budget byte-code-opaque))))
             (t nil))))

(defun nelisp-bytecode-native-rooted-cfg-safe-contract-bounded-data-p (value)
  "Return non-nil when inert VALUE is acyclic and within safe-v3 bounds."
  (nelisp-bytecode-native-rooted-cfg-safe-contract--data-p
   value 0 (list 20000)))

(defun nelisp-bytecode-native-rooted-cfg-safe-contract-bounded-compiler-spec-p
    (value)
  "Return non-nil when compiler spec VALUE and its bytecode are bounded.

Only INPUT's :function slot may contain a byte-code function. Every function
field is recursively checked as bounded inert data, so nested bytecode values
in constants or other metadata are rejected. A bounded opaque preflight runs
before plist access or copying so cyclic and oversized specs fail first."
  (and (nelisp-bytecode-native-rooted-cfg-safe-contract--data-p
        value 0 (list 20000) t)
       (let* ((input (plist-get value :input))
         (function (plist-get input :function))
         (spec-copy (and (consp value) (copy-sequence value)))
         (input-copy (and (consp input) (copy-sequence input)))
         (plan (plist-get value :plan))
         (plan-copy (and (consp plan) (copy-sequence plan)))
         (plan-input (plist-get plan :input))
         (plan-input-copy (and (consp plan-input) (copy-sequence plan-input)))
         (metadata-bounded
          (and (byte-code-function-p function)
               (<= (length function) 4096)
               (let ((index 0) (ok t) (budget (list 20000)))
                 (while (and ok (< index (length function)))
                   (setq ok
                         (nelisp-bytecode-native-rooted-cfg-safe-contract--data-p
                          (aref function index) 0 budget))
                   (setq index (1+ index)))
                 ok)))
         (canonical
          (and metadata-bounded
               (nelisp-bytecode-compiler-input-build function)))
         (canonical-copy (and (consp canonical) (copy-sequence canonical))))
    (when (and spec-copy input-copy canonical-copy plan-copy plan-input-copy)
      (setf (plist-get input-copy :function) nil)
      (setf (plist-get spec-copy :input) input-copy)
      (setf (plist-get plan-input-copy :function) nil)
      (setf (plist-get plan-copy :input) plan-input-copy)
      (setf (plist-get spec-copy :plan) plan-copy)
      (setf (plist-get canonical-copy :function) nil))
    (and spec-copy input-copy canonical-copy plan-copy plan-input-copy
         metadata-bounded
         (nelisp-bytecode-native-rooted-cfg-safe-contract--data-p
          spec-copy 0 (list 20000))
         (nelisp-bytecode-native-rooted-cfg-safe-contract--data-p
          canonical-copy 0 (list 20000))
         (equal input canonical)
         (equal plan-input input)))))

(defun nelisp-bytecode-native-rooted-cfg-safe-contract--canonical-input-p (input)
  (condition-case nil
      (let* ((function (plist-get input :function))
             (canonical (and (byte-code-function-p function)
                             (nelisp-bytecode-compiler-input-build function)))
             (dialect (nelisp-bytecode-compiler-input-dialect)))
        (and canonical
             (eq (plist-get (plist-get input :dialect-evidence) :status) 'pinned)
             (equal (plist-get input :dialect-evidence) dialect)
             (equal input canonical)))
    (error nil)))

(defun nelisp-bytecode-native-rooted-cfg-safe-contract--portable-dialect ()
  "Return the shared GNU 31.1 inventory identity used by host and runtime."
  (let* ((dialect (nelisp-bytecode-compiler-input-dialect))
         (runtime-witness
          (eq (plist-get dialect :runtime-evidence) 'standalone-build-verified))
         (host-witness
          (and (stringp (plist-get dialect :bytecomp-sha256))
               (stringp (plist-get dialect :comp-sha256))
               (null (plist-get dialect :runtime-evidence)))))
    (and (eq (plist-get dialect :status) 'pinned)
         (equal (plist-get dialect :dialect) "GNU Emacs 31.1")
         (equal (plist-get dialect :inventory-sha256)
                (nelisp-bytecode-compiler-input-inventory-sha256))
         (or runtime-witness host-witness)
         (list :status 'pinned :dialect "GNU Emacs 31.1"
               :inventory-sha256
               (nelisp-bytecode-compiler-input-inventory-sha256)))))

(defun nelisp-bytecode-native-rooted-cfg-safe-contract--plan-data (plan)
  (let ((copy (copy-tree plan)))
    (setq copy (plist-put copy :input nil))
    copy))

(defun nelisp-bytecode-native-rooted-cfg-safe-contract--form-imports (form)
  "Return the sorted external imports actually referenced by canonical FORM."
  (let ((pending (list (list 'visit form 0)))
        (active (make-hash-table :test #'eq))
        (completed (make-hash-table :test #'eq))
        (imports nil) (count 0) (valid t))
    (while (and pending valid (< count 20000))
      (let* ((entry (pop pending))
             (kind (car entry))
             (node (cadr entry))
             (depth (nth 2 entry)))
        (if (eq kind 'exit)
            (progn
              (remhash node active)
              (puthash node t completed))
          (setq count (1+ count))
          (when (> depth 256)
            (setq valid nil))
          (when (and valid
                     (or (consp node)
                         (and (vectorp node) (not (stringp node)))))
            (cond
             ((gethash node active) (setq valid nil))
             ((gethash node completed) nil)
             (t
              (puthash node t active)
              (push (list 'exit node depth) pending)
              (if (consp node)
                  (progn
                    (when (and (eq (car node) 'extern-call)
                               (symbolp (cadr node)))
                      (push (symbol-name (cadr node)) imports))
                    (push (list 'visit (cdr node) (1+ depth)) pending)
                    (push (list 'visit (car node) (1+ depth)) pending))
                (if (> (length node) (- 20000 count))
                    (setq valid nil)
                  (cl-loop for index downfrom (1- (length node)) to 0 do
                           (push (list 'visit (aref node index) (1+ depth))
                                 pending))))))))))
    (when (and valid (null pending) (< count 20000))
      (sort (delete-dups imports) #'string<))))

(defun nelisp-bytecode-native-rooted-cfg-safe-contract--digest (contract)
  (let ((rest contract) (canonical nil))
    (while rest
      (let ((key (pop rest)) (value (pop rest)))
        (unless (eq key :digest)
          (setq canonical (append canonical (list key value))))))
    (secure-hash 'sha256 (prin1-to-string canonical))))

(defun nelisp-bytecode-native-rooted-cfg-safe-contract-create
    (input plan emitted)
  "Create a portable safe-v3 contract from verified INPUT, PLAN and EMITTED."
  (let* ((function (plist-get input :function))
         (f1 (plist-get plan :funcall-version))
         (recipe (and (nelisp-bytecode-native-rooted-cfg-safe-contract--canonical-input-p
                       input)
                      (nelisp-bytecode-native-rooted-cfg-contract-input-recipe input)))
         (canonical-plan
          (and recipe
               (nelisp-bytecode-native-rooted-cfg-plan
                input 'safe-primitives-v3)))
         (canonical-emitted
          (and canonical-plan
               (nelisp-bytecode-native-rooted-cfg-emit
                canonical-plan nelisp-bytecode-native-rooted-cfg-safe-contract-entry)))
         (imports (and canonical-emitted
                       (nelisp-bytecode-native-rooted-cfg-safe-contract--form-imports
                        (plist-get canonical-emitted :form))))
         (contract
          (and (byte-code-function-p function)
               (eq (plist-get plan :status) 'complete)
               (eq (plist-get plan :lowering-mode) 'safe-primitives-v3)
               (equal plan canonical-plan)
               (eq (plist-get emitted :status) 'complete)
               (equal emitted canonical-emitted)
               (equal (plist-get emitted :entry-name)
                      nelisp-bytecode-native-rooted-cfg-safe-contract-entry)
               imports
               (if f1
                   (equal imports '("nl_native_funcall_v2" "nl_root_pin_slot_v2"))
                 (and (cl-some (lambda (name) (member name imports))
                               '("nl_native_car_v2" "nl_native_cdr_v2"))
                      (cl-every (lambda (name)
                                  (member name '("nl_native_car_v2" "nl_native_cdr_v2"
                                                 "nl_native_cons_v2" "nl_root_pin_slot_v2")))
                                imports)))
               (list :version (if f1 nelisp-bytecode-native-rooted-cfg-safe-contract-f1-version
                                nelisp-bytecode-native-rooted-cfg-safe-contract-version)
                     :emitter-mode "safe-primitives-v3"
                     :entry nelisp-bytecode-native-rooted-cfg-safe-contract-entry
                     :abi 2 :entry-kind 'func :entry-arity 4
                     :entry-params '(u64 u64 u64 u64) :entry-return 'u64
                     :dialect
                     (nelisp-bytecode-native-rooted-cfg-safe-contract--portable-dialect)
                     :argument-count (plist-get plan :arity)
                     :root-count (plist-get plan :required-root-count)
                     :input-recipe recipe
                     :plan (nelisp-bytecode-native-rooted-cfg-safe-contract--plan-data
                            plan)
                     :entry-ast (plist-get emitted :form)
                     :initializers (append
                                    (plist-get emitted :primitive-initializers)
                                    (plist-get emitted :constant-initializers)
                                    (plist-get emitted :immediate-initializers))
                     :imports (sort (copy-sequence imports) #'string<)
                     :status-base 512 :error-base 256))))
    (when contract
      (when f1
        (setq contract (append contract
                               (list :funcall-descriptor (nelisp-native-funcall-v2-descriptor)
                                     :funcall-hash (nelisp-native-funcall-v2-hash)
                                     :exit-root-base (plist-get plan :exit-root-base)
                                     :exit-status-base 1024))))
      (plist-put contract :digest
                 (nelisp-bytecode-native-rooted-cfg-safe-contract--digest contract))
      contract)))

(defun nelisp-bytecode-native-rooted-cfg-safe-contract-valid-p (contract)
  "Rebuild and compare inert safe-v3 CONTRACT data against trusted APIs."
  (if (not (nelisp-bytecode-native-rooted-cfg-safe-contract--data-p
            contract 0 (list 20000)))
      nil
    (condition-case nil
        (let* ((recipe (plist-get contract :input-recipe))
               (function
                (apply #'make-byte-code
                       (append (list (plist-get recipe :descriptor)
                                     (plist-get recipe :code)
                                     (plist-get recipe :constants)
                                     (plist-get recipe :stack-depth))
                               (cond ((= (plist-get recipe :function-length) 4) nil)
                                     ((= (plist-get recipe :function-length) 5)
                                      (list (plist-get recipe :metadata)))
                                     ((= (plist-get recipe :function-length) 6)
                                      (list (plist-get recipe :metadata)
                                            (plist-get recipe :interactive)))
                                     (t (signal 'error nil))))))
               (input (nelisp-bytecode-compiler-input-build function))
               (plan (nelisp-bytecode-native-rooted-cfg-plan
                      input 'safe-primitives-v3))
               (emitted (and (eq (plist-get plan :status) 'complete)
                             (nelisp-bytecode-native-rooted-cfg-emit
                              plan
                              nelisp-bytecode-native-rooted-cfg-safe-contract-entry)))
               (expected (and emitted
                              (nelisp-bytecode-native-rooted-cfg-safe-contract-create
                               input plan emitted)))
               (digest (plist-get contract :digest)))
          (and expected
               (equal digest
                      (nelisp-bytecode-native-rooted-cfg-safe-contract--digest contract))
               (equal contract expected)))
      (error nil))))

(provide 'nelisp-bytecode-native-rooted-cfg-safe-contract)
;;; nelisp-bytecode-native-rooted-cfg-safe-contract.el ends here
