;;; nelisp-bytecode-native-rooted-cfg-contract.el --- generic CFG contract -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Independent, data-only validation for generic rooted-CFG raw-v2 artifacts.
;; The manifest contains a bounded byte-code recipe, never runtime addresses or
;; executable Lisp objects.  Validation reconstructs the verified plan and AST.

;;; Code:

(require 'cl-lib)
(require 'nelisp-bytecode-native-rooted-cfg-data)
;; Optimizer dependencies are loaded only when their public API is called.
(autoload 'nelisp-bytecode-compiler-input-build "nelisp-bytecode-compiler-input")
(autoload 'nelisp-bytecode-compiler-input-dialect "nelisp-bytecode-compiler-input")
(autoload 'nelisp-bytecode-compiler-input-inventory-sha256 "nelisp-bytecode-compiler-input")
(autoload 'nelisp-bytecode-native-rooted-cfg-plan "nelisp-bytecode-native-rooted-cfg-plan")
(autoload 'nelisp-bytecode-native-rooted-cfg-emit "nelisp-bytecode-native-rooted-cfg-emit")
(autoload 'nelisp-bytecode-native-rooted-cfg-shared-emit-build "nelisp-bytecode-native-rooted-cfg-shared-emit")
(autoload 'nelisp-bytecode-native-rooted-cfg-shared-emit-build-from-input "nelisp-bytecode-native-rooted-cfg-shared-emit")
(autoload 'nelisp-bytecode-ir-decode-result "nelisp-bytecode-ir")

(defconst nelisp-bytecode-native-rooted-cfg-contract-version
  "nelisp-native-rooted-cfg-v1")
(defconst nelisp-bytecode-native-rooted-cfg-contract-shared-version
  "nelisp-native-rooted-cfg-shared-v2")
(defconst nelisp-bytecode-native-rooted-cfg-contract-f1-version "nelisp-native-rooted-cfg-f1-v1")
(defconst nelisp-bytecode-native-rooted-cfg-contract-f1-shared-version "nelisp-native-rooted-cfg-f1-shared-v1")
(defconst nelisp-bytecode-native-rooted-cfg-contract-shared-entry
  "nl_native_rooted_cfg_shared_probe_v2")

(defun nelisp-bytecode-native-rooted-cfg-contract--safe-data-p (value depth)
  (and (<= depth 12)
       (cond ((or (null value) (eq value t) (integerp value) (floatp value)
                  (stringp value) (symbolp value)) t)
             ((consp value)
              (and (nelisp-bytecode-native-rooted-cfg-contract--safe-data-p
                    (car value) (1+ depth))
                   (nelisp-bytecode-native-rooted-cfg-contract--safe-data-p
                    (cdr value) (1+ depth))))
             ((vectorp value)
              (cl-loop for item across value
                       always (nelisp-bytecode-native-rooted-cfg-contract--safe-data-p
                               item (1+ depth))))
             (t nil))))

(defun nelisp-bytecode-native-rooted-cfg-contract--recipe-constants (recipe)
  "Reconstruct placeholder tables; runtime callers supply the live constants."
  (let ((constants (copy-sequence (plist-get recipe :constants))) (seen nil))
    (dolist (index (plist-get recipe :live-hash-constants))
      (unless (and (integerp index) (<= 0 index) (< index (length constants))
                   (not (memq index seen)) (null (aref constants index)))
        (error "Invalid live switch constant index"))
      (push index seen)
      (aset constants index (make-hash-table :test 'eq)))
    constants))

(defun nelisp-bytecode-native-rooted-cfg-contract-input-recipe (input)
  "Return a safe reconstruction recipe for verified INPUT, or nil."
  (let* ((function (plist-get input :function))
         (code (plist-get input :code))
         (original-constants (plist-get input :constants))
         (constants (and (vectorp original-constants) (copy-sequence original-constants)))
         (live-hashes nil)
         (switch-p (and (stringp code) (vectorp constants)
                        (cl-find 183 (plist-get (nelisp-bytecode-ir-decode-result code constants) :instructions)
                                 :key (lambda (row) (aref row 1)))))
         (descriptor (plist-get input :argument-descriptor))
         (metadata (and (> (length function) 4) (aref function 4)))
         (depth (plist-get input :declared-stack-depth)))
    (when switch-p
      (dotimes (index (length constants))
        (when (hash-table-p (aref constants index))
          (push index live-hashes) (aset constants index nil))))
    (when (and (byte-code-function-p function)
               (stringp code) (not (multibyte-string-p code))
               (vectorp constants) (integerp depth) (>= depth 0)
               (nelisp-bytecode-native-rooted-cfg-contract--safe-data-p
                descriptor 0)
               (nelisp-bytecode-native-rooted-cfg-contract--safe-data-p
                constants 0)
               (or (= (length function) 4)
                   (nelisp-bytecode-native-rooted-cfg-contract--safe-data-p
                    metadata 0))
               (or (< (length function) 6)
                   (nelisp-bytecode-native-rooted-cfg-contract--safe-data-p
                    (aref function 5) 0)))
      (append (and live-hashes (list :live-hash-constants (nreverse live-hashes)))
              (list :descriptor descriptor :code code :constants constants
            :stack-depth depth :function-length (length function)
            :metadata metadata
            :interactive (and (> (length function) 5) (aref function 5)))))))

(defvar nelisp--prn-symbol-cache nil)

(defun nelisp-bytecode-native-rooted-cfg-contract--digest (contract)
  (let ((rest contract) (canonical nil))
    (while rest
      (let ((key (pop rest)) (value (pop rest)))
        (unless (eq key :digest)
          (setq canonical (append canonical (list key value))))))
    (let ((print-circle nil)
          (nelisp--prn-symbol-cache (make-hash-table :test 'equal)))
      (secure-hash 'sha256 (nelisp-bytecode-native-rooted-cfg-data-print canonical)))))

(defun nelisp-bytecode-native-rooted-cfg-contract--plan-data (plan &optional shared-flat)
  "Return PLAN without its process-local compiler INPUT object."
  (let ((copy (copy-sequence plan)))
    (setq copy (plist-put copy :input nil))
    ;; Function identity contexts remain in the process-owned plan seal;
    ;; only canonical provider data belongs in the serialized contract.
    (when (plist-get copy :exit-root-base)
      (setq copy (plist-put copy :arithmetic-context nil)))
    ;; Preserve historical OFF/v1 serialized shapes. The process-local guard
    ;; snapshot never enters serialization; ON remains an explicit option.
    (cl-labels ((data-plist (value)
                  (let ((rest value) (data nil))
                    (while rest
                      (let ((key (car rest)) (item (cadr rest)))
                        (unless (or (and shared-flat (memq key '(:entry-ast :blocks)))
                                    (eq key :arithmetic-guard-context)
                                    (and (eq key :arithmetic-guard-mode) (eq item 'off)))
                          (setq data (append data (list key item)))))
                      (setq rest (cddr rest)))
                    data)))
      (setq copy (data-plist copy))
      (unless shared-flat
        (setq copy (plist-put copy :entry-ast (data-plist (plist-get copy :entry-ast))))))
    (nelisp-bytecode-compiler-input--snapshot copy 0)))

(defun nelisp-bytecode-native-rooted-cfg-contract-arithmetic-source-p
    (input plan emitted)
  "Authenticate the complete artifact-local arithmetic source and root plan."
  (let* ((verified-plan (nelisp-bytecode-native-rooted-cfg-plan
                         input (plist-get plan :lowering-mode)
                         (plist-get plan :arithmetic-guard-mode)))
         (verified-emitted
          (and (eq (plist-get verified-plan :status) 'complete)
               (nelisp-bytecode-native-rooted-cfg-shared-emit-build
                verified-plan nelisp-bytecode-native-rooted-cfg-contract-shared-entry))))
    (and (nelisp-bytecode-native-rooted-cfg--canonical-input-p input)
         (nelisp-bytecode-native-rooted-cfg-plan-guard-context-p plan)
         (equal plan verified-plan) (equal emitted verified-emitted)
         (integerp (plist-get plan :exit-root-base))
         (equal (plist-get emitted :additional-source)
                (nelisp-native-optimization-guard-v1-source
                 (plist-get plan :arithmetic-guard-mode))))))

(defun nelisp-bytecode-native-rooted-cfg-contract--ast-imports (form)
  "Return sorted import names, inspecting each shared FORM container once."
  (let ((seen (make-vector 4096 nil)) (pending (list form)) imports)
    (while pending
      (let ((value (pop pending)))
        (when (or (consp value) (vectorp value))
          (let ((key (logand (sxhash-eq value) 4095)))
            (unless (memq value (aref seen key))
              (aset seen key (cons value (aref seen key)))
              (if (consp value)
                  (progn
                    (when (and (eq (car value) 'extern-call) (consp (cdr value)))
                      (let ((name (cadr value)))
                        (when (or (symbolp name) (stringp name))
                          (push (if (symbolp name) (symbol-name name) name) imports))))
                    (push (cdr value) pending) (push (car value) pending))
                (dotimes (index (length value)) (push (aref value index) pending))))))))
    (sort (delete-dups imports) #'string<)))

(defun nelisp-bytecode-native-rooted-cfg-contract--canonical-empty-imports-p
    (input plan emitted)
  "Return non-nil when INPUT/PLAN/EMITTED canonically call no imports."
  (let* ((entry-name (plist-get emitted :entry-name))
         (verified-plan (nelisp-bytecode-native-rooted-cfg-plan
                         input (plist-get plan :lowering-mode)
                         (plist-get plan :arithmetic-guard-mode)))
         (verified-emitted
          (and (eq (plist-get verified-plan :status) 'complete)
               (cond
                ((equal entry-name nelisp-bytecode-native-rooted-cfg-contract-shared-entry)
                 (nelisp-bytecode-native-rooted-cfg-shared-emit-build
                  verified-plan entry-name))
                ((equal entry-name "nl_native_rooted_cfg_probe_v1")
                 (nelisp-bytecode-native-rooted-cfg-emit verified-plan entry-name))
                (t nil)))))
    (and (eq (plist-get plan :status) 'complete)
         (eq (plist-get emitted :status) 'complete)
         (equal plan verified-plan)
         (equal emitted verified-emitted)
         (null (nelisp-bytecode-native-rooted-cfg-contract--ast-imports
                (plist-get verified-emitted :form))))))

(defun nelisp-bytecode-native-rooted-cfg-contract--create-data (input plan emitted &optional shared-flat)
  "Build validated contract data before choosing its final version and digest."
  (let* ((recipe (nelisp-bytecode-native-rooted-cfg-contract-input-recipe input))
         (entry-ast (plist-get emitted :form))
         (imports (nelisp-bytecode-native-rooted-cfg-contract--ast-imports
                   entry-ast))
         (arithmetic (and (member (if (eq (plist-get plan :arithmetic-guard-mode) 'on)
                                     "nl_native_add_guard_v1" "nl_native_add_v2") imports)
                          (nelisp-bytecode-native-rooted-cfg-contract-arithmetic-source-p
                           input plan emitted)))
         (contract
          (and recipe
               (or (and imports
                        (cl-some (lambda (name)
                                   (member name (plist-get emitted :gateway-imports)))
                                 '("nl_native_car_v2" "nl_native_cdr_v2"
                                   "nl_native_cons_v2" "nl_native_funcall_v2" "nl_native_poll_v2")))
                   arithmetic
                   (and (null imports)
                        (nelisp-bytecode-native-rooted-cfg-contract--canonical-empty-imports-p
                         input plan emitted)))
               (cl-every (lambda (name)
                           (or (member name '("nl_native_car_v2" "nl_native_cdr_v2"
                                              "nl_native_cons_v2" "nl_native_funcall_v2" "nl_native_poll_v2" "nl_native_frame_v2" "nl_root_pin_slot_v2"))
                               (and arithmetic
                                    (equal name (if (eq (plist-get plan :arithmetic-guard-mode) 'on)
                                                    "nl_native_add_guard_v1" "nl_native_add_v2")))))
                         imports)
               (list :version nelisp-bytecode-native-rooted-cfg-contract-version
                     :entry "nl_native_rooted_cfg_probe_v1" :entry-arity 4
                     :argument-count (plist-get plan :arity)
                     :root-count (plist-get plan :required-root-count)
                     :input-recipe recipe
                     :plan (nelisp-bytecode-native-rooted-cfg-contract--plan-data plan shared-flat)
                     :entry-ast entry-ast
                     :initializers (append (plist-get emitted :primitive-initializers)
                                           (plist-get emitted :constant-initializers)
                                           (plist-get emitted :immediate-initializers))
                     :imports imports :status-base 512 :error-base 256))))
    (when (and contract (plist-get plan :funcall-version))
      (setq contract (append contract
                             (list :funcall-descriptor (nelisp-native-funcall-v2-descriptor)
                                   :funcall-hash (nelisp-native-funcall-v2-hash)
                                   :exit-root-base (plist-get plan :exit-root-base) :exit-base 1024)))
      (setq contract (plist-put contract :version nelisp-bytecode-native-rooted-cfg-contract-f1-version)))
    (when (and contract (plist-get plan :frame-descriptor))
      (setq contract (append contract
                             (list :frame-descriptor (nelisp-native-frame-v2-descriptor)
                                   :frame-hash (nelisp-native-frame-v2-hash)))))
    (when (and contract arithmetic)
      (let* ((source (nelisp-native-optimization-guard-v1-source
                      (plist-get plan :arithmetic-guard-mode)))
             (local-functions
              (mapcar (lambda (form) (symbol-name (nth 1 form))) (cdr source)))
             (runtime-imports (nelisp-native-arithmetic-v2-runtime-imports))
             (runtime-names
              (mapcar (lambda (descriptor) (plist-get descriptor :name))
                      runtime-imports)))
        (setq contract
              (plist-put contract :imports
                         (sort (delete-dups
                                (append (cl-remove-if
                                         (lambda (name) (member name local-functions))
                                         imports)
                                        runtime-names)) #'string<)))
        (setq contract (append contract
                               (list :additional-source source
                                     :arithmetic-guard-mode (plist-get plan :arithmetic-guard-mode)
                                     :local-functions local-functions
                                     :runtime-imports runtime-imports
                                     :exit-root-base (plist-get plan :exit-root-base)
                                     :exit-base 1024)))))
    contract))

(defun nelisp-bytecode-native-rooted-cfg-contract-create (input plan emitted)
  "Create a serializable contract from independently verified INPUT/PLAN/EMITTED."
  (let ((contract (nelisp-bytecode-native-rooted-cfg-contract--create-data
                   input plan emitted)))
    (when contract
      (plist-put contract :digest
                 (nelisp-bytecode-native-rooted-cfg-contract--digest contract))
      contract)))

(defun nelisp-bytecode-native-rooted-cfg-contract-create-shared-v2
    (input plan emitted)
  "Create the versioned shared-continuation contract for verified INPUT."
  (when (and (eq (plist-get emitted :status) 'complete)
             (equal (plist-get emitted :entry-name)
                    nelisp-bytecode-native-rooted-cfg-contract-shared-entry))
    (let ((contract (nelisp-bytecode-native-rooted-cfg-contract--create-data
                     input plan emitted t)))
      (when contract
        (setq contract
              (plist-put contract :version
                         (if (plist-get plan :funcall-version)
                             nelisp-bytecode-native-rooted-cfg-contract-f1-shared-version
                           nelisp-bytecode-native-rooted-cfg-contract-shared-version)))
        ;; The recipe reconstructs blocks and the top-level AST is executable.
        ;; Keep full plans in process-owned comparisons; serialize each tree once.
        ;; A fresh expected contract fixes this schema, so old shared caches refuse.
        (setq contract (plist-put contract :plan-schema-version "shared-flat-v1"))
        (setq contract (plist-put contract :emitter-mode "postdom-shared-v2"))
        (setq contract (plist-put contract :entry
                                  nelisp-bytecode-native-rooted-cfg-contract-shared-entry))
        (plist-put contract :digest
                   (nelisp-bytecode-native-rooted-cfg-contract--digest contract))
        contract))))

(defalias 'nelisp-bytecode-native-rooted-cfg-contract--snapshot-data
  (symbol-function 'nelisp-bytecode-native-rooted-cfg-data-snapshot))
(defun nelisp-bytecode-native-rooted-cfg-contract--stage (label)
  "Append optional compile diagnostics without changing validation decisions."
  (let ((path (getenv "NELISP_ROOTED_CFG_STAGE_LOG")))
    (when path
      (write-region (format "contract-%s seconds=%.3f\n" label (float-time))
                    nil path t 'silent))))

(defvar nelisp-bytecode-native-rooted-cfg-contract--validation-count 0)


(let ((snapshot-owner (symbol-function 'nelisp-bytecode-native-rooted-cfg-contract--snapshot-data))
      (reuse-owner (and (fboundp 'nelisp-bytecode-compiler-input--reuse-checked)
                        (symbol-function 'nelisp-bytecode-compiler-input--reuse-checked)))
      (data-equal-owner (symbol-function 'nelisp-bytecode-native-rooted-cfg-data-equal))
      ;; Lean images retain the original independent public reconstruction.
      ;; Optimizing cold preparation loads the shared emitter before this
      ;; factory, so the input-only entry is sealed here, never on first use.
      (pair-owner (and (featurep 'nelisp-bytecode-native-rooted-cfg-shared-emit)
                       (symbol-function 'nelisp-bytecode-native-rooted-cfg-shared-emit-build-from-input)))
      (lookup (symbol-function 'symbol-function)) (same (symbol-function 'eq)))
(defun nelisp-bytecode-native-rooted-cfg-contract-valid-p (contract &optional result-mode live-input)
  "Recompute and validate a serialized generic rooted-CFG CONTRACT.
Nil RESULT-MODE preserves the boolean API.  Exact `:reconstruction' returns
full input, fresh plan, emission and expected contract after every check passes.
LIVE-INPUT binds opaque table constants only after its canonical recipe matches.
A scope-owned, unchanged checked input replaces only duplicate construction;
all code, frame, plan and artifact comparisons remain required.
The result is data for comparison, never a certificate or cached authority."
  (setq nelisp-bytecode-native-rooted-cfg-contract--validation-count
        (1+ nelisp-bytecode-native-rooted-cfg-contract--validation-count))
  (and (memq result-mode '(nil :reconstruction))
       (or (null result-mode)
           (funcall same snapshot-owner
                    (funcall lookup 'nelisp-bytecode-native-rooted-cfg-contract--snapshot-data)))
  (condition-case nil
      (let* ((_copy-start (nelisp-bytecode-native-rooted-cfg-contract--stage "copy-start"))
             (copy (funcall snapshot-owner contract 0))
             (_copy-end (nelisp-bytecode-native-rooted-cfg-contract--stage "copy-end"))
             (digest (plist-get copy :digest))
             (recipe (plist-get copy :input-recipe))
             (descriptor (plist-get recipe :descriptor))
             (recipe-safe
              (or (null result-mode)
                  (cl-every (lambda (key)
                              (nelisp-bytecode-native-rooted-cfg-contract--safe-data-p
                               (plist-get recipe key) 0))
                            '(:descriptor :constants :metadata :interactive))))
             (live-canonical nil)
             (live-function
              (and live-input recipe-safe
                   (progn
                     (setq live-canonical
                           (or (and reuse-owner (funcall same reuse-owner
                                            (funcall lookup 'nelisp-bytecode-compiler-input--reuse-checked))
                                    (funcall reuse-owner live-input))
                               (nelisp-bytecode-compiler-input-build (plist-get live-input :function))))
                     (and (equal live-canonical live-input)
                          (equal recipe (nelisp-bytecode-native-rooted-cfg-contract-input-recipe live-canonical))
                          (plist-get live-canonical :function)))))
             (function
              (or live-function (and recipe-safe (apply #'make-byte-code
                     (append (list descriptor (plist-get recipe :code)
                                   (nelisp-bytecode-native-rooted-cfg-contract--recipe-constants recipe)
                                   (plist-get recipe :stack-depth))
                             (pcase (plist-get recipe :function-length)
                               (4 nil)
                               (5 (list (plist-get recipe :metadata)))
                               (6 (list (plist-get recipe :metadata)
                                        (plist-get recipe :interactive)))))))))
             ;; The live object passed either the private build-scope snapshot
             ;; or a canonical rebuild, and the same complete recipe comparison.
             (input (or (and live-function live-canonical)
                        (nelisp-bytecode-compiler-input-build function)))
             (shared-v2 (member (plist-get copy :version)
                                (list nelisp-bytecode-native-rooted-cfg-contract-shared-version
                                      nelisp-bytecode-native-rooted-cfg-contract-f1-shared-version)))
             (_plan-start (nelisp-bytecode-native-rooted-cfg-contract--stage "plan-start"))
             (paired (and shared-v2 pair-owner
                          (funcall same pair-owner
                                   (funcall lookup 'nelisp-bytecode-native-rooted-cfg-shared-emit-build-from-input))
                          (funcall pair-owner input nelisp-bytecode-native-rooted-cfg-contract-shared-entry
                           (plist-get (plist-get copy :plan) :lowering-mode)
                           (plist-get (plist-get copy :plan) :arithmetic-guard-mode))))
             (plan (if (and shared-v2 pair-owner) (plist-get paired :plan)
                     (nelisp-bytecode-native-rooted-cfg-plan
                      input (plist-get (plist-get copy :plan) :lowering-mode)
                      (if shared-v2 (plist-get (plist-get copy :plan) :arithmetic-guard-mode) 'off))))
             (v1 (member (plist-get copy :version)
                         (list nelisp-bytecode-native-rooted-cfg-contract-version
                               nelisp-bytecode-native-rooted-cfg-contract-f1-version)))
             (emitted (and (eq (plist-get plan :status) 'complete)
                           (if shared-v2
                               (if pair-owner (plist-get paired :emitted)
                                 (nelisp-bytecode-native-rooted-cfg-shared-emit-build
                                  plan nelisp-bytecode-native-rooted-cfg-contract-shared-entry))
                             (and v1
                                  (nelisp-bytecode-native-rooted-cfg-emit
                                   plan "nl_native_rooted_cfg_probe_v1")))))
             (_emit-end (nelisp-bytecode-native-rooted-cfg-contract--stage "emit-end"))
             (expected (and (eq (plist-get emitted :status) 'complete)
                            (if shared-v2
                                (nelisp-bytecode-native-rooted-cfg-contract-create-shared-v2
                                 input plan emitted)
                              (and v1
                                   (nelisp-bytecode-native-rooted-cfg-contract-create
                                    input plan emitted)))))
             (_expected-end (nelisp-bytecode-native-rooted-cfg-contract--stage "expected-end"))
             (without-digest copy))
        (and (or (null pair-owner)
                 (funcall same pair-owner
                          (funcall lookup 'nelisp-bytecode-native-rooted-cfg-shared-emit-build-from-input)))
             (or (null live-input) live-function) expected
             (equal digest
                    (progn
                      (nelisp-bytecode-native-rooted-cfg-contract--stage "digest-start")
                      (prog1 (nelisp-bytecode-native-rooted-cfg-contract--digest without-digest)
                        (nelisp-bytecode-native-rooted-cfg-contract--stage "digest-end"))))
             (funcall same data-equal-owner
                      (funcall lookup 'nelisp-bytecode-native-rooted-cfg-data-equal))
             (funcall data-equal-owner contract expected)
             (funcall same data-equal-owner
                      (funcall lookup 'nelisp-bytecode-native-rooted-cfg-data-equal))
             (or (null result-mode)
                 (funcall same snapshot-owner
                          (funcall lookup 'nelisp-bytecode-native-rooted-cfg-contract--snapshot-data)))
             (if result-mode
                 (progn
                   (nelisp-bytecode-native-rooted-cfg-contract--stage "result-copy-start")
                   (prog1 (funcall snapshot-owner
                                  (list :input input :plan plan :emitted emitted :expected-contract expected) 0)
                     (nelisp-bytecode-native-rooted-cfg-contract--stage "result-copy-end")))
               t)))
    (error nil)))))

(provide 'nelisp-bytecode-native-rooted-cfg-contract)
;;; nelisp-bytecode-native-rooted-cfg-contract.el ends here
