;;; nelisp-bytecode-native-rooted-cfg-native.el --- raw-v2 CFG artifacts -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'cl-lib)
(require 'nelisp-bytecode-native-rooted-cfg-plan)
(require 'nelisp-bytecode-native-rooted-cfg-shared-emit)
(require 'nelisp-bytecode-native-rooted-cfg-emit)
(require 'nelisp-bytecode-native-rooted-cfg-contract)
(require 'nelisp-bytecode-native-rooted-cfg-constructor-contract)
(require 'nelisp-bytecode-native-rooted-cfg-safe-contract)
(require 'nelisp-native-load)
(require 'nelisp-runtime-reload-abi)

(defconst nelisp-bytecode-native-rooted-cfg-native-entry
  "nl_native_rooted_cfg_probe_v1")
(defvar nelisp-bytecode-native-rooted-cfg-native--registry nil)

(defun nelisp-bytecode-native-rooted-cfg-native--fingerprint (result)
  (let* ((semantic (copy-sequence result))
         (plan (copy-sequence (plist-get semantic :plan)))
         (print-length nil)
        (print-level nil)
        (print-circle t)
         (nelisp--prn-symbol-cache (make-hash-table :test 'equal)))
    ;; Opaque process owners are authenticated separately through the genuine
    ;; public planner predicate. Never walk or print their lexical captures.
    (when plan
      (setq plan (plist-put plan :arithmetic-context nil))
      (setq plan (plist-put plan :arithmetic-guard-context nil))
      (setq semantic (plist-put semantic :plan plan)))
    (secure-hash 'sha256 (prin1-to-string semantic))))

(defun nelisp-bytecode-native-rooted-cfg-native--file-sha256 (path)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (secure-hash 'sha256 (current-buffer))))

(defun nelisp-bytecode-native-rooted-cfg-native-authenticated-result-p (result)
  "Return non-nil for an unchanged result made by this process and artifact."
  (and (consp result)
       (let ((record (assq result nelisp-bytecode-native-rooted-cfg-native--registry)))
         (and record
              (or (null (plist-get (plist-get result :plan) :exit-root-base))
                  (plist-get (plist-get result :plan) :funcall-version)
                  (and
                   (eq (plist-get (nelisp-bytecode-native-rooted-cfg-plan
                                   (plist-get (plist-get result :plan) :input)
                                   (plist-get (plist-get result :plan) :lowering-mode)
                                   (plist-get (plist-get result :plan) :arithmetic-guard-mode))
                                  :status) 'complete)
                   (nelisp-bytecode-native-rooted-cfg-plan-guard-context-p
                    (plist-get result :plan))))
              (equal (cdr record)
                     (nelisp-bytecode-native-rooted-cfg-native--fingerprint result))
              (stringp (plist-get result :artifact-path))
              (file-readable-p (plist-get result :artifact-path))
              (equal (plist-get result :artifact-file-sha256)
                     (nelisp-bytecode-native-rooted-cfg-native--file-sha256
                      (plist-get result :artifact-path)))
              (equal (nelisp-native-load-manifest (plist-get result :artifact-path))
                     (plist-get result :manifest))))))

(defun nelisp-bytecode-native-rooted-cfg-native--stage (artifact label &optional source)
  "Append a producer stage without traversing the producer body as a macro."
  (condition-case nil
      (let ((path (getenv "NELISP_ROOTED_CFG_STAGE_LOG")))
        (when (and (stringp path) (> (length path) 0))
          (write-region (format "producer-%s seconds=%.3f source=%s artifact=%s\n" label (float-time) source artifact)
                        nil path t 'silent)))
    ((error quit) nil)))

(let ((pair-owner (symbol-function 'nelisp-bytecode-native-rooted-cfg-shared-emit-build-from-input))
      (constructor-checker
       (and (fboundp 'nelisp-native-load-compiler-constructor-contract-p)
            (symbol-function 'nelisp-native-load-compiler-constructor-contract-p)))
      (f1-checker (and (fboundp 'nelisp-native-load-compiler-f1-runtime-p)
                       (symbol-function 'nelisp-native-load-compiler-f1-runtime-p)))
      (lookup (symbol-function 'symbol-function))
      (same (symbol-function 'eq)))
(defun nelisp-bytecode-native-rooted-cfg-native--build (input artifact-path shared-v2 &optional guard-mode skip-seal)
  "Build verified INPUT using the v1 or shared-v2 rooted-CFG emitter.
SKIP-SEAL is internal to cache compilation; its result cannot be admitted.
That path leaves file publication and memory-safety decoding to the cache."
  (let* ((paired
          (and shared-v2
               (progn
                 (unless (funcall same pair-owner
                                  (funcall lookup 'nelisp-bytecode-native-rooted-cfg-shared-emit-build-from-input))
                   (error "rooted-cfg: shared planner/emitter owner changed"))
                 (nelisp-bytecode-native-rooted-cfg-native--stage artifact-path "plan-emit-start")
                 (prog1 (funcall pair-owner input nelisp-bytecode-native-rooted-cfg-contract-shared-entry nil guard-mode)
                   (unless (funcall same pair-owner
                                    (funcall lookup 'nelisp-bytecode-native-rooted-cfg-shared-emit-build-from-input))
                     (error "rooted-cfg: shared planner/emitter owner changed"))
                   (nelisp-bytecode-native-rooted-cfg-native--stage artifact-path "plan-emit-end")))))
         (plan (if shared-v2 (plist-get paired :plan)
                 (nelisp-bytecode-native-rooted-cfg-plan input nil 'off)))
         (emitted (if shared-v2 (plist-get paired :emitted)
                    (and (eq (plist-get plan :status) 'complete)
                         (nelisp-bytecode-native-rooted-cfg-emit
                          plan nelisp-bytecode-native-rooted-cfg-native-entry))))
         (entry-name (if shared-v2
                         nelisp-bytecode-native-rooted-cfg-contract-shared-entry
                       nelisp-bytecode-native-rooted-cfg-native-entry))
         (binary (progn (nelisp-bytecode-native-rooted-cfg-native--stage artifact-path "binary-start")
                        (prog1 (nelisp-native-load-running-binary-sha256)
                          (nelisp-bytecode-native-rooted-cfg-native--stage artifact-path "binary-end"))))
         (source (progn (nelisp-bytecode-native-rooted-cfg-native--stage artifact-path "source-start")
                        (let ((path (if skip-seal (concat artifact-path ".el")
                                      (make-temp-file "nelisp-rooted-cfg-" nil ".el"))))
                          (nelisp-bytecode-native-rooted-cfg-native--stage artifact-path "source-end" path)
                          path)))
         (forms nil) (source-snapshot nil) (manifest nil) (result nil)
         (result-contract
          (and (eq (plist-get emitted :status) 'complete)
               (progn
                (nelisp-bytecode-native-rooted-cfg-native--stage artifact-path "contract-start" source)
                (prog1 (if shared-v2
                   (nelisp-bytecode-native-rooted-cfg-contract-create-shared-v2 input plan emitted)
                 (nelisp-bytecode-native-rooted-cfg-contract-create input plan emitted))
                  (nelisp-bytecode-native-rooted-cfg-native--stage artifact-path "contract-end" source))))))
    (unless (and (eq (plist-get emitted :status) 'complete)
                 (stringp artifact-path) (string-suffix-p ".nelr" artifact-path)
                 (stringp binary)
                 (or (progn
                       (nelisp-bytecode-native-rooted-cfg-native--stage artifact-path "runtime-match-start" source)
                       (prog1 (nelisp-runtime-reload-contract-matches-p)
                         (nelisp-bytecode-native-rooted-cfg-native--stage artifact-path "runtime-match-end" source)))
                     (and shared-v2 f1-checker
                          (funcall same f1-checker (funcall lookup 'nelisp-native-load-compiler-f1-runtime-p))
                          (and (plist-get plan :funcall-version) (funcall f1-checker)))
                     (and shared-v2
                          constructor-checker
                          (funcall same constructor-checker
                                   (funcall lookup 'nelisp-native-load-compiler-constructor-contract-p))
                          (progn
                            (nelisp-bytecode-native-rooted-cfg-native--stage artifact-path "constructor-check-start" source)
                            (prog1 (funcall constructor-checker result-contract)
                              (nelisp-bytecode-native-rooted-cfg-native--stage artifact-path "constructor-check-end" source))))))
      (when (and (not skip-seal) (file-exists-p source)) (delete-file source))
      (error "rooted-cfg: verified plan or runtime contract is unavailable"))
    (unwind-protect
        (progn
          (dolist (contract (nelisp-native-load-raw-v2-contract))
            (let ((name (intern (car contract))) (args nil))
              (dotimes (index (cdr contract))
                (push (intern (format "arg%d" index)) args))
              (push (list 'defun name (nreverse args) 0) forms)))
          (setq forms
                (append (nreverse forms)
                        (let ((additional (plist-get emitted :additional-source)))
                          (and additional
                               (if (eq (car additional) 'seq) (cdr additional)
                                 (list additional))))
                        (list (plist-get emitted :form))))
          (with-temp-buffer
            (let ((print-length nil) (print-level nil)
                  (nelisp--prn-symbol-cache (make-hash-table :test 'equal)))
              (dolist (form forms) (prin1 form (current-buffer)) (insert "\n"))
              (if skip-seal
                  (setq source-snapshot
                        (cons forms (encode-coding-string (buffer-string) 'utf-8-unix)))
                (write-region (point-min) (point-max) source nil 'silent)
                (setq source-snapshot (cons forms (buffer-string))))))
          ;; GNU Emacs records the encoding actually selected by write-region.
          ;; The standalone raw writer sends buffer-string bytes unchanged.
          (when (and (not skip-seal) (boundp 'last-coding-system-used) last-coding-system-used)
            (setcdr source-snapshot
                    (encode-coding-string (cdr source-snapshot) last-coding-system-used)))
          (nelisp-bytecode-native-rooted-cfg-native--stage artifact-path "compile-start" source)
          (let* ((built-contract result-contract)
                 (cfg-spec (and built-contract
                                (list :input input :plan plan :emitted emitted
                                      :contract built-contract))))
          (unless cfg-spec
            (error "rooted-cfg: input constants are not safely serializable"))
          (setq result-contract built-contract)
          ;; Compile-file owns the single semantic reconstruction.  Carry its
          ;; receipt locally; the serialized artifact remains unchanged.
          (let ((compiled
                 (if skip-seal
                     ;; The cache neither consumes nor forwards an admission
                     ;; receipt.  Compile-file retains its own AOT mutation check.
                     (nelisp-native-load-raw-v2-compile-file
                      source artifact-path
                      (if shared-v2 "gnu31-rooted-cfg-shared-v2" "gnu31-rooted-cfg-v1") binary
                      nil nil nil nil nil cfg-spec nil nil source-snapshot t)
                   (nelisp-native-load--raw-v2-compile-file-with-validation
                    source artifact-path
                    (if shared-v2 "gnu31-rooted-cfg-shared-v2" "gnu31-rooted-cfg-v1") binary
                    nil nil nil nil nil cfg-spec nil source-snapshot))))
            (setq manifest (if skip-seal compiled (plist-get compiled :manifest)))
            (nelisp-bytecode-native-rooted-cfg-native--stage artifact-path "compile-return" source)
            (unless skip-seal
            (let ((problems
                   (nelisp-native-load--raw-v2-check-after-compile
                    manifest entry-name (plist-get compiled :validated-contract)
                    (plist-get compiled :digest) (plist-get compiled :validator))))
              (when problems
                (error "rooted-cfg: raw-v2 artifact refused: %S" problems)))))
          (nelisp-bytecode-native-rooted-cfg-native--stage artifact-path "manifest-accepted" source))
          (unless skip-seal (nelisp-bytecode-native-rooted-cfg-native--stage artifact-path "result-seal-start" source))
          (setq result
                (list :status 'complete :input input :plan plan
                      :contract result-contract :form (plist-get emitted :form)
                      :entry-name entry-name
                      :argument-count (plist-get plan :arity)
                      :required-root-count (plist-get plan :required-root-count)
                      :gateway-imports (plist-get emitted :gateway-imports)
                      :primitive-initializers (plist-get emitted :primitive-initializers)
                      :constant-initializers (plist-get emitted :constant-initializers)
                      :immediate-initializers (plist-get emitted :immediate-initializers)
                      :artifact-path (expand-file-name artifact-path)
                      :artifact-file-sha256
                      (unless skip-seal
                        (nelisp-bytecode-native-rooted-cfg-native--file-sha256 artifact-path))
                      :runtime-binary-sha256 binary :manifest manifest))
          (unless skip-seal
            (push (cons result
                        (nelisp-bytecode-native-rooted-cfg-native--fingerprint result))
                  nelisp-bytecode-native-rooted-cfg-native--registry)
            (nelisp-bytecode-native-rooted-cfg-native--stage artifact-path "result-seal-end" source))
          result)
      (nelisp-bytecode-native-rooted-cfg-native--stage artifact-path "cleanup-source-start" source)
      (when (and (not skip-seal) (file-exists-p source)) (delete-file source))
      (nelisp-bytecode-native-rooted-cfg-native--stage artifact-path "cleanup-source-end" source))))

(defun nelisp-bytecode-native-rooted-cfg-native-build (input artifact-path)
  "Build verified INPUT as an authenticated v1 raw-CFG artifact."
  (nelisp-bytecode-native-rooted-cfg-native--build input artifact-path nil))

(defun nelisp-bytecode-native-rooted-cfg-native-build-shared-v2 (input artifact-path &optional guard-mode)
  "Build verified INPUT as a separately versioned shared-continuation artifact."
  (nelisp-bytecode-native-rooted-cfg-native--build input artifact-path t guard-mode))

(let ((f1-owner (and (fboundp 'nelisp-native-load-compiler-f1-runtime-p)
                      (symbol-function 'nelisp-native-load-compiler-f1-runtime-p)))
      (lookup (symbol-function 'symbol-function)) (same (symbol-function 'eq)))
(defun nelisp-bytecode-native-rooted-cfg-native-build-safe-v3
    (input artifact-path)
  "Build verified INPUT with opt-in safe-v3 lowering at ARTIFACT-PATH."
  (let* ((plan (nelisp-bytecode-native-rooted-cfg-plan
                input 'safe-primitives-v3))
         (emitted (and (eq (plist-get plan :status) 'complete)
                       (nelisp-bytecode-native-rooted-cfg-emit
                        plan nelisp-bytecode-native-rooted-cfg-safe-contract-entry)))
         (contract (and emitted
                        (nelisp-bytecode-native-rooted-cfg-safe-contract-create
                         input plan emitted)))
         (binary (and contract
                      (nelisp-native-load-running-binary-sha256))))
    (unless (and (eq (plist-get plan :status) 'complete)
                 (eq (plist-get emitted :status) 'complete)
                 contract
                 (nelisp-bytecode-native-rooted-cfg-safe-contract-valid-p contract)
                 (stringp artifact-path) (string-suffix-p ".nelr" artifact-path)
                 (stringp binary)
                 (or (nelisp-runtime-reload-contract-matches-p)
                     (and (plist-get plan :funcall-version) f1-owner
                          (funcall same f1-owner (funcall lookup 'nelisp-native-load-compiler-f1-runtime-p))
                          (funcall f1-owner))))
      (error "rooted-cfg-safe-v3: verified input or runtime contract is unavailable"))
    (let ((source (make-temp-file "nelisp-rooted-cfg-safe-v3-" nil ".el"))
          (forms nil) (manifest nil) (result nil))
      (unwind-protect
          (progn
            (dolist (entry (nelisp-native-load-raw-v2-contract))
              (let ((name (intern (car entry))) (args nil))
                (dotimes (index (cdr entry))
                  (push (intern (format "arg%d" index)) args))
                (push (list 'defun name (nreverse args) 0) forms)))
            (setq forms (append (nreverse forms) (list (plist-get emitted :form))))
            (with-temp-file source
              (let ((print-length nil) (print-level nil))
                (dolist (form forms)
                  (prin1 form (current-buffer))
                  (insert "\n"))))
            (let* ((safe-v3-spec (list :input input :plan plan :emitted emitted
                                       :contract contract))
                   (compiled
                    (nelisp-native-load-raw-v2-compile-file
                     source artifact-path "gnu31-rooted-cfg-safe-v3" binary
                     nil nil nil nil nil nil safe-v3-spec)))
              (setq manifest compiled)
              (let ((problems
                     (nelisp-native-load-raw-v2-check
                      manifest nelisp-bytecode-native-rooted-cfg-safe-contract-entry)))
                (when problems
                  (error "rooted-cfg-safe-v3: raw-v2 artifact refused: %S"
                         problems))))
            (setq result
                  (list :status 'complete :input input :plan plan
                        :form (plist-get emitted :form)
                        :contract contract
                        :contract-version (plist-get contract :version)
                        :entry-name nelisp-bytecode-native-rooted-cfg-safe-contract-entry
                        :argument-count (plist-get plan :arity)
                        :required-root-count (plist-get plan :required-root-count)
                        :gateway-imports (plist-get emitted :gateway-imports)
                        :primitive-initializers (plist-get emitted :primitive-initializers)
                        :constant-initializers
                        (plist-get emitted :constant-initializers)
                        :immediate-initializers
                        (plist-get emitted :immediate-initializers)
                        :artifact-path (expand-file-name artifact-path)
                        :artifact-file-sha256
                        (nelisp-bytecode-native-rooted-cfg-native--file-sha256
                         artifact-path)
                        :runtime-binary-sha256 binary :manifest manifest))
            (push (cons result
                        (nelisp-bytecode-native-rooted-cfg-native--fingerprint result))
                  nelisp-bytecode-native-rooted-cfg-native--registry)
            result)
        (when (file-exists-p source) (delete-file source)))))))

)
(provide 'nelisp-bytecode-native-rooted-cfg-native)
;;; nelisp-bytecode-native-rooted-cfg-native.el ends here
