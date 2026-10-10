;;; nelisp-bytecode-native-compiler.el --- Materialized byte-code compiler -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Public source-free entrypoint for the verified boxed straight-line backend.

;;; Code:

(require 'cl-lib)
(require 'nelisp-bytecode-compiler-input)
(require 'nelisp-bytecode-native-constant)
(require 'nelisp-bytecode-native-boxed-branch)
(require 'nelisp-bytecode-native-unary-chain)
(require 'nelisp-bytecode-native-rooted-conditional)
(require 'nelisp-bytecode-native-rooted-branch)
(require 'nelisp-bytecode-native-rooted-branch-join)

(defun nelisp-bytecode-native-compiler-unary-chain-operations (input)
  "Return INPUT's verified CAR/CDR chain operations, or nil.

This public predicate lets the package compiler refuse raw-v2 chains before
it creates package output."
  (let ((operations (nelisp-bytecode-native-unary-chain-operations input)))
    (and (> (length operations) 1) operations)))

(defun nelisp-bytecode-native-compiler-rooted-stack-input-p (input)
  "Return non-nil when INPUT needs the general rooted-stack producer.

Legacy exact CAR/CDR/CONS templates remain owned by their existing
frontends and package guards."
  (require 'nelisp-bytecode-native-rooted-stack)
  (and (nelisp-bytecode-native-compiler--rooted-stack-plan-p input)
       (not (null (plist-get (nelisp-bytecode-native-rooted-stack-plan input) :operations)))
       (not (and (equal (plist-get input :argument-descriptor) 514)
                 (equal (plist-get input :code) (unibyte-string 1 1 66 135))
                 (equal (plist-get input :constants) [])
                 (equal (plist-get input :declared-stack-depth) 4)))
       (not (nelisp-bytecode-native-compiler-unary-template-operation input))
       (not (nelisp-bytecode-native-compiler-unary-chain-operations input))))

(defun nelisp-bytecode-native-compiler--rooted-stack-plan-p (input)
  "Return non-nil when INPUT has any plan accepted by the rooted producer."
  (require 'nelisp-bytecode-native-rooted-stack)
  (let ((plan (nelisp-bytecode-native-rooted-stack-plan input)))
    (and (eq (plist-get plan :status) 'complete)
         (consp (plist-get plan :operations)))))

(defun nelisp-bytecode-native-compiler-rooted-branch-input-p (input)
  "Return non-nil when INPUT is the exact admitted rooted branch shape."
  (eq (plist-get (nelisp-bytecode-native-rooted-branch-plan input) :status)
      'complete))

(defun nelisp-bytecode-native-compiler-rooted-branch-join-operation (input)
  "Return the exact CAR/CDR gateway operation encoded in INPUT, or nil."
  (cond ((equal (plist-get input :code)
                (unibyte-string 2 131 8 0 1 130 9 0 137 64 135)) 'car)
        ((equal (plist-get input :code)
                (unibyte-string 2 131 8 0 1 130 9 0 137 65 135)) 'cdr)))

(defun nelisp-bytecode-native-compiler--rooted-branch-route
    (input artifact-path entry-name)
  "Route a verified branch only through its fixed raw-v2 entry."
  (let ((admitted (nelisp-bytecode-native-compiler-rooted-branch-input-p input)))
    (if (and admitted (stringp artifact-path)
             (string-suffix-p ".nelr" artifact-path)
             (equal entry-name nelisp-bytecode-native-rooted-branch-entry))
        (nelisp-bytecode-native-rooted-branch-build input artifact-path)
      (list :status 'unsupported :artifact-kind 'raw-runtime-v2
            :reason "rooted branch requires the exact GNU 31.1 shape, .nelr output, and explicit fixed entry"
            :input input))))

(defun nelisp-bytecode-native-compiler--rooted-stack-route
    (input artifact-path entry-name)
  "Route admitted INPUT only through its explicit raw-v2 destination."
  (when (nelisp-bytecode-native-compiler--rooted-stack-plan-p input)
    (when (and (stringp artifact-path) (string-suffix-p ".nelr" artifact-path)
               (equal entry-name "nl_native_stack_probe_v1"))
      (nelisp-bytecode-native-rooted-stack-build input artifact-path))))

(defvar nelisp-bytecode-native-compiler--call1-tokens (make-hash-table :test 'eq)
  "Process-local registry for opaque fixed-target CALL1 tokens.")

(defun nelisp-bytecode-native-compiler--call1-fingerprint (function)
  "Return FUNCTION's fingerprint for the bounded CALL1 template."
  (require 'nelisp-native-load)
  (nelisp-native-load-sha256
   (prin1-to-string (list (aref function 0) (aref function 1)
                          (aref function 2) (aref function 3)))))

(defun nelisp-bytecode-native-compiler--call1-file-fingerprint (path)
  "Hash PATH as literal bytes."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (require 'nelisp-native-load)
    (nelisp-native-load-sha256 (buffer-string))))

(defun nelisp-bytecode-native-compiler--call1-compile (input artifact-path)
  "Compile INPUT's exact CALL1 wrapper and return an opaque process token."
  (require 'nelisp-native-load)
  (let* ((callee (aref (plist-get input :constants) 0))
         (source (make-temp-file "nelisp-call1-template-" nil ".el"))
         (binary (nelisp-native-load-running-binary-sha256))
         (forms nil) (token nil))
    (unwind-protect
        (progn
          (dolist (entry (nelisp-native-load-raw-v2-contract))
            (push (list 'defun (intern (car entry))
                        (cl-loop for i below (cdr entry)
                                 collect (intern (format "arg%d" i))) 0)
                  forms))
          (setq forms
                (append
                 (nreverse forms)
                 '((defun wf_bytecode_call_gateway_exit
                     (env ticket slot-function slot-argument slot-status slot-exit) 0)
                   (defun nl_native_bytecode_call1_exit (env ticket)
                     (extern-call wf_bytecode_call_gateway_exit
                                  env ticket 4 5 2 0)))))
          (with-temp-file source
            (dolist (form forms) (prin1 form (current-buffer)) (insert "\n")))
          (nelisp-native-load-raw-v2-compile-call1-file
           source artifact-path "call1-symbol-template" binary)
          (let* ((manifest (nelisp-native-load-manifest artifact-path))
                 (handle (nelisp-native-load-raw-v2-artifact
                          artifact-path "nl_native_bytecode_call1_exit" binary)))
            (setq token (make-symbol "nelisp-bytecode-call1-token"))
            (puthash token (list :handle handle :path artifact-path
                                 :artifact-hash (plist-get manifest :artifact-sha256)
                                 :file-hash (nelisp-bytecode-native-compiler--call1-file-fingerprint artifact-path)
                                 :callee callee :function (plist-get input :function)
                                 :fingerprint (nelisp-bytecode-native-compiler--call1-fingerprint
                                               (plist-get input :function)))
                     nelisp-bytecode-native-compiler--call1-tokens)
            (list :status 'complete :artifact-kind 'raw-v2-call1-token
                  :token token :return-repr 'u64 :input input)))
      (when (file-exists-p source) (delete-file source)))))

(defun nelisp-bytecode-native-compiler-call1 (token argument)
  "Invoke only TOKEN's fixed interned callee with ARGUMENT."
  (let* ((record (and (symbolp token)
                      (not (eq token (intern-soft (symbol-name token))))
                      (gethash token nelisp-bytecode-native-compiler--call1-tokens)))
         (function (plist-get record :function))
         (path (plist-get record :path))
         (manifest (and record (nelisp-native-load-manifest path))))
    (unless (and record
                 (eq (plist-get record :callee) (aref (aref function 2) 0))
                 (equal (plist-get record :fingerprint)
                        (nelisp-bytecode-native-compiler--call1-fingerprint function))
                 (equal (plist-get record :artifact-hash)
                        (plist-get manifest :artifact-sha256))
                 (equal (plist-get record :file-hash)
                        (nelisp-bytecode-native-compiler--call1-file-fingerprint path)))
      (error "bytecode-native-compiler: invalid or changed CALL1 token"))
    (nelisp-native-load-raw-v2-call1 (plist-get record :handle)
                                     (plist-get record :callee) argument)))

(defun nelisp-bytecode-native-compiler-call1-close (token)
  "Invalidate TOKEN; executable mappings remain process-owned."
  (and (gethash token nelisp-bytecode-native-compiler--call1-tokens)
       (progn (remhash token nelisp-bytecode-native-compiler--call1-tokens) t)))

(defun nelisp-bytecode-native-compiler--required-arguments-p (arguments count)
  "Return non-nil when ARGUMENTS is COUNT unique required positional symbols."
  (let ((tail arguments) (seen-cells nil) (seen-names nil) (length 0)
        (valid t))
    (while (and valid (consp tail))
      (if (memq tail seen-cells)
          (setq valid nil)
        (push tail seen-cells)
        (let ((name (car tail)))
          (if (or (not (symbolp name))
                  (memq name '(&optional &rest &key &allow-other-keys &aux))
                  (memq name seen-names)
                  (not (fboundp 'special-variable-p))
                  (special-variable-p name))
              (setq valid nil)
            (push name seen-names)
            (setq length (1+ length))))
        (setq tail (cdr tail))))
    (and valid (null tail) (= length count))))

(defun nelisp-bytecode-native-compiler--constant-index-layout (count)
  "Return the backend's ordered hidden-constant indices for COUNT slots."
  (let ((index 0) (layout (make-vector count nil)))
    (while (< index count)
      (aset layout index index)
      (setq index (1+ index)))
    layout))

(defun nelisp-bytecode-native-compiler--boxed-branch-build
    (input artifact-path entry-name)
  "Build the bounded packed branch slice described by INPUT."
  (let* ((optional-variable
          (nelisp-bytecode-native-boxed-branch-optional-variable-shape-p
           input (append (plist-get (plist-get input :frame-result) :blocks)
                         nil)))
         (result
          (condition-case error-data
              (let ((built
                     (nelisp-bytecode-native-boxed-branch-build
                      (plist-get input :code) (plist-get input :constants)
                      artifact-path entry-name (plist-get input :argument-descriptor)
                      input)))
                (plist-put built :input input))
            (error
             (let ((reason (error-message-string error-data)))
               (if (string-prefix-p "bytecode-native-boxed-branch:" reason)
                   (list :status
                         (if (string-match-p "byte-code rejected" reason)
                             'malformed 'unsupported)
                         :reason reason :input input)
                 (signal (car error-data) (cdr error-data))))))))
    (when (eq (plist-get result :status) 'complete)
      (plist-put result :hidden-constant-indices
                 (if optional-variable []
                   (nelisp-bytecode-native-compiler--constant-index-layout
                    (length (plist-get input :constants)))))
      (plist-put result :hidden-constant-count
                 (length (plist-get result :hidden-constant-indices))))
    result))

(defun nelisp-bytecode-native-compiler--single-branch-p (input)
  "Return non-nil when INPUT's verified frame has exactly one branch."
  (let ((count 0))
    (dolist (block (append (plist-get (plist-get input :frame-result) :blocks)
                           nil))
      (dolist (instruction (append (plist-get block :instructions) nil))
        (when (and (eq (plist-get instruction :kind) 'branch)
                   (memq (plist-get instruction :opcode) '(131 132 133 134)))
          (setq count (1+ count)))))
    (= count 1)))

(defun nelisp-bytecode-native-compiler--cons-template-p (input)
  "Return non-nil only for GNU 31.1's pinned two-argument CONS template."
  (and (eq (plist-get input :status) 'complete)
       (equal (plist-get input :argument-descriptor) 514)
       (= (plist-get input :argument-min) 2)
       (= (plist-get input :argument-max) 2)
       (= (plist-get input :argument-count) 2)
       (= (plist-get input :initial-stack-depth) 2)
       (= (plist-get input :declared-stack-depth) 4)
       (= (plist-get input :computed-temporary-stack-depth) 4)
       (equal (plist-get input :code) (unibyte-string 1 1 66 135))
       (equal (plist-get input :constants) [])
       (let ((instructions
              (cl-loop for block in (append (plist-get
                                             (plist-get input :frame-result)
                                             :blocks)
                                            nil)
                       append (append (plist-get block :instructions) nil))))
         (equal (mapcar (lambda (instruction)
                          (plist-get instruction :opcode))
                        instructions)
                '(1 1 66 135)))))

(defun nelisp-bytecode-native-compiler-cons-template-p (input)
  "Return non-nil when INPUT is the pinned native CONS template."
  (nelisp-bytecode-native-compiler--cons-template-p input))

(defun nelisp-bytecode-native-compiler-unary-template-operation (input)
  "Return the fixed CAR/CDR operation for INPUT's exact GNU 31.1 template."
  (let ((operation
         (cond ((equal (plist-get input :code) (unibyte-string 64 135)) 'car)
               ((equal (plist-get input :code) (unibyte-string 65 135)) 'cdr))))
    (and operation
         (eq (plist-get input :status) 'complete)
         (equal (plist-get input :argument-descriptor) 257)
         (= (plist-get input :argument-min) 1)
         (= (plist-get input :argument-max) 1)
         (= (plist-get input :argument-count) 1)
         (= (plist-get input :initial-stack-depth) 1)
         (= (plist-get input :declared-stack-depth) 2)
         (= (plist-get input :computed-temporary-stack-depth) 1)
         (equal (plist-get input :constants) [])
         (let ((instructions
                (cl-loop for block in (append (plist-get
                                               (plist-get input :frame-result)
                                               :blocks)
                                              nil)
                         append (append (plist-get block :instructions) nil))))
           (equal (mapcar (lambda (instruction)
                            (plist-get instruction :opcode))
                          instructions)
                  (list (if (eq operation 'car) 64 65) 135)))
         operation)))

(defun nelisp-bytecode-native-compiler--unary-build
    (input operation artifact-path entry-name)
  "Build exact unary INPUT with fixed OPERATION to raw-v2 ARTIFACT-PATH."
  (let* ((name (symbol-name operation))
         (entry (format "nl_native_%s_probe" name))
         (gateway (format "nl_native_%s_v2" name))
         (binary-sha256 (progn
                          (require 'nelisp-runtime-reload-abi)
                          (require 'nelisp-native-load)
                          (nelisp-native-load-running-binary-sha256)))
         (source-path nil) (forms nil) (result nil))
    (unless (and (eq (nelisp-bytecode-native-compiler-unary-template-operation
                      input) operation)
                 (stringp artifact-path)
                 (string-suffix-p ".nelr" artifact-path)
                 (equal entry-name entry))
      (error "bytecode-native-unary: unsupported template or raw-v2 artifact contract"))
    (unless (and (stringp binary-sha256)
                 (nelisp-runtime-reload-contract-matches-p))
      (error "bytecode-native-unary: running v2 runtime identity is unavailable"))
    (setq source-path (make-temp-file (format "nelisp-bytecode-%s-" name) nil ".el"))
    (unwind-protect
        (progn
          (dolist (contract nelisp-runtime-reload-gc-contract)
            (let ((function (intern (car contract))) (arity (cdr contract)) args)
              (dotimes (index arity)
                (setq args (append args (list (intern (format "arg%d" index))))))
              (push (list 'defun function args 0) forms)))
          (setq forms
                (append (nreverse forms)
                        (list (list 'defun (intern entry)
                                    '(env ticket input-index output-index)
                                    (list 'extern-call (intern gateway)
                                          'env 'ticket 'input-index 'output-index
                                          0 0)))))
          (with-temp-file source-path
            (let ((print-length nil) (print-level nil))
              (dolist (form forms) (prin1 form (current-buffer)) (insert "\n"))))
          (setq result
                (nelisp-native-load-raw-v2-compile-file
                 source-path artifact-path (format "gnu31-bytecode-%s-v1" name)
                 binary-sha256))
          (list :status 'complete :artifact-kind 'raw-runtime-v2
                :artifact-path (expand-file-name artifact-path) :manifest result
                :runtime-abi (nelisp-native-load--runtime-abi-v2)
                :entry-name entry-name :arity 4 :return-repr 'u64
                :evaluator-return-repr 'sexp :gateway-import gateway :input input))
      (when (file-exists-p source-path) (delete-file source-path)))))

(defun nelisp-bytecode-native-compiler--cons-build
    (input artifact-path entry-name)
  "Build INPUT's exact CONS template to an authenticated raw-v2 artifact."
  (unless (and (nelisp-bytecode-native-compiler--cons-template-p input)
               (stringp artifact-path)
               (string-suffix-p ".nelr" artifact-path)
               (equal entry-name "nl_native_cons_probe"))
    (error "bytecode-native-cons: unsupported template or raw-v2 artifact contract"))
  (require 'nelisp-runtime-reload-abi)
  (require 'nelisp-native-load)
  (let* ((binary-sha256 (nelisp-native-load-running-binary-sha256))
         (source-path nil)
         (forms nil)
         (result nil))
    (unless (and (stringp binary-sha256)
                 (nelisp-runtime-reload-contract-matches-p))
      (error "bytecode-native-cons: running v2 runtime identity is unavailable"))
    (setq source-path (make-temp-file "nelisp-bytecode-cons-" nil ".el"))
    (unwind-protect
        (progn
          (dolist (entry nelisp-runtime-reload-gc-contract)
            (let ((name (intern (car entry))) (arity (cdr entry)) (args nil))
              (dotimes (index arity)
                (setq args (append args (list (intern (format "arg%d" index))))))
              (push (list 'defun name args 0) forms)))
          (setq forms
                (append (nreverse forms)
                        '((defun nl_native_cons_probe
                            (env ticket left-index right-index output-index)
                            (extern-call nl_native_cons_v2
                                         env ticket left-index right-index
                                         output-index 0)))))
          (with-temp-file source-path
            (let ((print-length nil) (print-level nil))
              (dolist (form forms)
                (prin1 form (current-buffer))
                (insert "\n"))))
          (setq result
                (nelisp-native-load-raw-v2-compile-file
                 source-path artifact-path "gnu31-bytecode-cons-v1"
                 binary-sha256))
          (list :status 'complete :artifact-kind 'raw-runtime-v2
                :artifact-path (expand-file-name artifact-path)
                :manifest result
                :runtime-abi (nelisp-native-load--runtime-abi-v2)
                :entry-name entry-name :arity 5 :return-repr 'u64
                :evaluator-return-repr 'sexp
                :gateway-import "nl_native_cons_v2" :input input))
      (when (file-exists-p source-path) (delete-file source-path)))))

(defun nelisp-bytecode-native-compiler--boxed-branch-input-p (input)
  "Return non-nil when INPUT is fully verified for the packed branch slice."
  (let* ((ir-unsupported
          (plist-get (plist-get input :ir-result) :unsupported))
         (boxed-constants-only
          (or (null ir-unsupported)
              (and (consp ir-unsupported)
                   (cl-every
                    (lambda (entry)
                      (eq (cdr entry) 'non-fixnum-constant))
                    ir-unsupported))))
         (status (plist-get input :status)))
    (and (memq status '(complete unsupported))
         (eq (plist-get (plist-get input :dialect-evidence) :status) 'pinned)
         (or (memq (plist-get input :argument-descriptor) '(257 514))
             (and (listp (plist-get input :argument-descriptor))
                  (eq (plist-get (plist-get input :frame-result) :status) 'complete)
                  (= (plist-get input :argument-min) 1)
                  (= (plist-get input :argument-max) 2)
                  (= (plist-get input :argument-count) 2)
                  (nelisp-bytecode-native-boxed-branch-optional-variable-shape-p
                   input
                   (append (plist-get (plist-get input :frame-result) :blocks)
                           nil)))
             (and (eql (plist-get input :argument-descriptor) 513)
                  (or (equal (plist-get input :code)
                             (unibyte-string 137 134 5 0 1 135))
                      (equal (plist-get input :code)
                             (unibyte-string 137 133 5 0 1 135)))
                  (= (length (plist-get input :constants)) 0)
                  (= (plist-get input :argument-min) 1)
                  (= (plist-get input :argument-max) 2)
                  (= (plist-get input :initial-stack-depth) 2)
                  (= (length (plist-get (plist-get input :frame-result) :blocks)) 3)))
         (or (and (memq (plist-get input :argument-descriptor) '(257 514))
                  (memq (plist-get input :argument-count) '(1 2))
                  (= (plist-get input :argument-min) (plist-get input :argument-count))
                  (= (plist-get input :argument-max) (plist-get input :argument-count)))
             (eql (plist-get input :argument-descriptor) 513)
             (and (listp (plist-get input :argument-descriptor))
                  (= (plist-get input :argument-min) 1)
                  (= (plist-get input :argument-max) 2)
                  (nelisp-bytecode-native-boxed-branch-optional-variable-shape-p
                   input
                   (append (plist-get (plist-get input :frame-result) :blocks)
                           nil))))
         (eq (plist-get (plist-get input :frame-result) :status) 'complete)
         (nelisp-bytecode-native-compiler--single-branch-p input)
         boxed-constants-only)))

(defun nelisp-bytecode-native-compiler--boxed-constant-return-input-p (input)
  "Admit verified pure fixed-arity returns with boxed constant roots.
Only the raw-number IR's non-fixnum constant diagnostics are bridged; frame
verification, argument layout, capture metadata and effects remain mandatory."
  (let* ((frame (plist-get input :frame-result))
         (ir (plist-get input :ir-result))
         (blocks (plist-get frame :blocks))
         (rows (plist-get ir :instructions))
         (constants (plist-get input :constants))
         (arity (plist-get input :argument-count))
         (descriptor (plist-get input :argument-descriptor))
         (unsupported (plist-get ir :unsupported)))
    (and (eq (plist-get input :status) 'unsupported)
         (eq (plist-get (plist-get input :dialect-evidence) :status) 'pinned)
         (eq (plist-get frame :status) 'complete)
         (eq (plist-get ir :status) 'unsupported)
         (vectorp blocks) (= (length blocks) 1)
         (vectorp rows) (vectorp constants)
         (integerp arity) (<= 0 arity 6)
         (integerp descriptor) (<= 0 descriptor 65535)
         (= (logand descriptor 128) 0)
         (= (logand descriptor 127) arity)
         (= (ash descriptor -8) arity)
         (eql (plist-get input :argument-min) arity)
         (eql (plist-get input :argument-max) arity)
         (eql (plist-get input :initial-stack-depth) arity)
         (not (plist-get input :rest-argument-p))
         (not (plist-get input :potential-capture-placeholder-p))
         (not (plist-get input :closure-template-descriptor))
         (<= 1 (+ arity (length constants)) 6)
         (consp unsupported)
         (cl-every
          (lambda (entry)
            (let* ((row (cl-find (car entry) (append rows nil)
                                 :key (lambda (row) (aref row 0))))
                   (metadata (and row (aref row 4)))
                   (index (plist-get metadata :constant-index)))
              (and (eq (cdr entry) 'non-fixnum-constant)
                   (eq (plist-get metadata :kind) 'constant)
                   (integerp index) (<= 0 index) (< index (length constants)))))
          unsupported)
         (let* ((block (aref blocks 0))
                (instructions (append (plist-get block :instructions) nil)))
           (and (null (append (plist-get block :successors) nil))
                (eql (plist-get block :entry-stack-depth) arity)
                (consp instructions)
                (eq (plist-get (car (last instructions)) :kind) 'return)
                (= (cl-count 'return instructions
                             :key (lambda (instruction) (plist-get instruction :kind))) 1)
                (cl-every
                 (lambda (instruction)
                   (memq (plist-get instruction :kind) '(constant stack-ref dup discard return)))
                 instructions))))))

(defun nelisp-bytecode-native-compiler--build-from-input
    (function artifact-path entry-name input)
  "Compile materialized byte-code FUNCTION to ARTIFACT-PATH.

ENTRY-NAME is the native entry symbol. This bounded frontend accepts required
positional arguments, a named GNU optional OR/AND truthiness join verified
through frame dataflow, packed descriptor 513 for the older pinned shape, or
packed descriptors 257/514 with one verified pure truthiness branch. It does
not evaluate FUNCTION or reconstruct source forms. The one admitted CONS
template is descriptor 514, code [1 1 66 135], no constants, and declared
stack depth 4; it returns an explicitly typed raw-runtime-v2 artifact, not a
boxed .neln unit. Return the backend result, or a plist with :status
`malformed' or `unsupported' and a :reason."
  (cond
     ((eq (plist-get input :status) 'malformed)
      (list :status 'malformed :reason (plist-get input :reason)
            :input input))
     ((plist-get input :call1-symbol-template-p)
      (if (and (stringp artifact-path)
               (string-suffix-p ".nelr" artifact-path)
               (equal entry-name "nl_native_bytecode_call1_exit"))
          (nelisp-bytecode-native-compiler--call1-compile input artifact-path)
        (list :status 'unsupported :reason "CALL1 requires fixed raw-v2 entry"
              :input input)))
     ((and (not (plist-get input :call1-symbol-template-p))
           (cl-some (lambda (block)
                      (cl-some (lambda (instruction) (eq (plist-get instruction :kind) 'call))
                               (append (plist-get block :instructions) nil)))
                    (append (plist-get (plist-get input :frame-result) :blocks) nil)))
      (require 'nelisp-bytecode-native-rooted-cfg-native)
      (if (and (stringp artifact-path) (string-suffix-p ".nelr" artifact-path)
               (equal entry-name nelisp-bytecode-native-rooted-cfg-contract-shared-entry))
          (nelisp-bytecode-native-rooted-cfg-native-build-shared-v2 input artifact-path 'off)
        (list :status 'unsupported :artifact-kind 'raw-runtime-v2
              :reason "Generic call requires the authenticated shared CFG entry and .nelr output"
              :input input)))
     ((or (nelisp-bytecode-native-compiler-rooted-branch-join-operation input)
          (equal entry-name nelisp-bytecode-native-rooted-branch-join-entry))
      (let ((operation
             (nelisp-bytecode-native-compiler-rooted-branch-join-operation input)))
        (if (and operation
                 (stringp artifact-path) (string-suffix-p ".nelr" artifact-path)
                 (equal entry-name nelisp-bytecode-native-rooted-branch-join-entry))
            (nelisp-bytecode-native-rooted-branch-join-build
             input operation artifact-path)
          (list :status 'unsupported :artifact-kind 'raw-runtime-v2
                :reason "joined branch requires its exact verified input and authenticated fixed .nelr entry"
                :input input))))
     ((or (nelisp-bytecode-native-rooted-conditional-input-p input)
          (equal (plist-get input :code) (unibyte-string 2 131 6 0 1 135 135))
          (equal entry-name "nl_native_rooted_conditional_probe_v1"))
      (if (and (nelisp-bytecode-native-rooted-conditional-input-p input)
               (stringp artifact-path) (string-suffix-p ".nelr" artifact-path)
               (equal entry-name "nl_native_rooted_conditional_probe_v1"))
          (nelisp-bytecode-native-rooted-conditional-build input artifact-path)
        (list :status 'unsupported
              :reason "fixed conditional requires the pinned shape, .nelr output, and explicit entry"
              :input input)))
     ((or (equal entry-name nelisp-bytecode-native-rooted-branch-entry)
          (equal (plist-get input :code)
                 (unibyte-string 2 131 7 0 1 64 135 65 135)))
      (nelisp-bytecode-native-compiler--rooted-branch-route
       input artifact-path entry-name))
     ((equal entry-name "nl_native_stack_probe_v1")
      (if (and (stringp artifact-path) (string-suffix-p ".nelr" artifact-path)
               (nelisp-bytecode-native-compiler--rooted-stack-plan-p input))
          (nelisp-bytecode-native-rooted-stack-build input artifact-path)
        (list :status 'unsupported :artifact-kind 'raw-runtime-v2
              :reason "rooted-stack entry requires a supported nonempty plan and .nelr output"
              :input input)))
     ((cl-some (lambda (block)
                      (cl-some (lambda (instruction)
                                 (= (plist-get instruction :opcode) 66))
                               (append (plist-get block :instructions) nil)))
                    (append (plist-get (plist-get input :frame-result) :blocks)
                            nil))
      (if (not (nelisp-bytecode-native-compiler--cons-template-p input))
          (or (nelisp-bytecode-native-compiler--rooted-stack-route
               input artifact-path entry-name)
              (list :status 'unsupported
                :reason "opcode 66 is accepted only in the pinned GNU 31.1 two-argument CONS template"
                :input input))
        (if (and (stringp artifact-path)
                 (string-suffix-p ".nelr" artifact-path)
                 (equal entry-name "nl_native_cons_probe"))
            (nelisp-bytecode-native-compiler--cons-build
             input artifact-path entry-name)
          (list :status 'unsupported :artifact-kind 'raw-runtime-v2
                :reason "CONS template uses raw-v2 .nelr ABI entry nl_native_cons_probe"
                :return-repr 'u64 :evaluator-return-repr 'sexp
                :input input))))
     ((cl-some (lambda (block)
                 (cl-some (lambda (instruction)
                            (memq (plist-get instruction :opcode) '(64 65)))
                          (append (plist-get block :instructions) nil)))
               (append (plist-get (plist-get input :frame-result) :blocks) nil))
      (let ((operations
             (nelisp-bytecode-native-compiler-unary-chain-operations input))
            (operation
             (nelisp-bytecode-native-compiler-unary-template-operation input)))
        (cond
         ((and operations
               (stringp artifact-path)
               (string-suffix-p ".nelr" artifact-path)
               (equal entry-name "nl_native_chain_probe_v2"))
          (nelisp-bytecode-native-unary-chain-build input artifact-path))
         (operations
          (or (nelisp-bytecode-native-compiler--rooted-stack-route
               input artifact-path entry-name)
              (list :status 'unsupported :artifact-kind 'raw-runtime-v2
                    :reason "CAR/CDR chain requires raw-v2 .nelr entry nl_native_chain_probe_v2"
                    :input input)))
         ((not operation)
          (or (nelisp-bytecode-native-compiler--rooted-stack-route
               input artifact-path entry-name)
              (list :status 'unsupported
                    :reason "CAR/CDR opcode is accepted only in its pinned GNU 31.1 unary template"
                    :input input)))
         (t
          (let ((entry (format "nl_native_%s_probe" operation)))
            (if (and (stringp artifact-path)
                     (string-suffix-p ".nelr" artifact-path)
                     (equal entry-name entry))
                (nelisp-bytecode-native-compiler--unary-build
                 input operation artifact-path entry-name)
              (list :status 'unsupported :artifact-kind 'raw-runtime-v2
                    :reason (format "%s template uses raw-v2 .nelr ABI entry %s"
                                    (upcase (symbol-name operation)) entry)
                    :return-repr 'u64 :evaluator-return-repr 'sexp
                    :gateway-import (format "nl_native_%s_v2" operation)
                    :input input)))))))
     ((and (not (plist-get input :rest-argument-p))
           (nelisp-bytecode-native-compiler--boxed-branch-input-p input))
      (nelisp-bytecode-native-compiler--boxed-branch-build
       input artifact-path entry-name))
     ((and (not (eq (plist-get input :status) 'complete))
           (not (nelisp-bytecode-native-compiler--boxed-constant-return-input-p input)))
      (list :status 'unsupported
            :reason (or (plist-get input :reason)
                        "byte-code verifier refused unsupported opcode semantics")
            :input input))
     ((plist-get input :rest-slot-return-template-p)
      (let* ((required (plist-get input :required-argument-count))
             (arity (1+ required)))
        (condition-case error-data
            (let ((result
                   (nelisp-bytecode-native-constant-return-build
                    (plist-get input :rest-native-code) [] artifact-path entry-name
                    arity nil arity required)))
              (when (eq (plist-get result :status) 'complete)
                (plist-put result :hidden-constant-indices [])
                (plist-put result :hidden-constant-count 0))
              (plist-put result :input input))
          (error
           (let ((reason (error-message-string error-data)))
             (list :status (if (string-prefix-p
                                "bytecode-native-constant: byte-code rejected"
                                reason)
                               'malformed 'unsupported)
                   :reason reason :input input))))))
     (t
      (let* ((arguments (plist-get input :argument-list))
             (descriptor (plist-get input :argument-descriptor))
             (optional-terminal-p
              (and (integerp descriptor)
                   (not (plist-get input :rest-argument-p))
                   (integerp (plist-get input :argument-min))
                   (integerp (plist-get input :argument-max))
                   (<= 0 (plist-get input :argument-min))
                   (< (plist-get input :argument-min) (plist-get input :argument-max))
                   ;; The existing boxed return ABI has six argument registers.
                   (<= (plist-get input :argument-max) 6)
                   (= (plist-get input :initial-stack-depth) (plist-get input :argument-max))
                   (equal (plist-get input :code) (unibyte-string 135))))
             (optional-arg-p (or optional-terminal-p
                                 (and (integerp descriptor) (= descriptor 513))))
             (count (if optional-arg-p (plist-get input :argument-max)
                      (plist-get input :argument-count))))
        (cond
         ((and optional-arg-p
               (not optional-terminal-p)
               (or (not (eq (plist-get input :status) 'complete))
                   (not (or (and (= (length (plist-get input :code)) 1)
                                 (= (aref (plist-get input :code) 0) 135))
                            (equal (plist-get input :code)
                                   (unibyte-string 137 134 5 0 1 135))
                            (equal (plist-get input :code)
                                   (unibyte-string 137 133 5 0 1 135))))
                   (/= (plist-get input :argument-min) 1)
                   (/= (plist-get input :argument-max) 2)))
          (list :status 'unsupported
                :reason "optional descriptor 513 supports only terminal return or pinned short-circuit"
                :input input))
         ((not (integerp count))
          (list :status 'unsupported
                :reason "packed argument descriptor does not have a fixed arity"
                :input input))
         ((and (not optional-arg-p) (> count 0) (null arguments))
          (if (and (memq (plist-get input :argument-descriptor) '(257 514))
                   (memq count '(1 2))
                   (= (plist-get input :argument-min) count)
                   (= (plist-get input :argument-max) count)
                   (nelisp-bytecode-native-compiler--single-branch-p input))
              (nelisp-bytecode-native-compiler--boxed-branch-build
               input artifact-path entry-name)
            (list :status 'unsupported
                  :reason "packed argument descriptor does not preserve argument names"
                  :input input)))
         ((and (not optional-arg-p)
               (not (nelisp-bytecode-native-compiler--required-arguments-p
                     arguments count)))
            (list :status 'unsupported
                  :reason "only unique required positional arguments are supported"
                  :input input))
         (t
          (condition-case error-data
              (let ((result
                     (nelisp-bytecode-native-constant-return-build
                      (plist-get input :code) (plist-get input :constants)
                      artifact-path entry-name count arguments
                      (and optional-arg-p count))))
                (plist-put result :input input)
                (when (eq (plist-get result :status) 'complete)
                  (plist-put result :hidden-constant-indices
                             (nelisp-bytecode-native-compiler--constant-index-layout
                              (length (plist-get result :constants))))
                  (plist-put result :hidden-constant-count
                             (length (plist-get result :hidden-constant-indices))))
                result)
            (error
             (let ((reason (error-message-string error-data)))
               (if (string-prefix-p "bytecode-native-constant:" reason)
                   (list :status
                         (if (string-match-p "byte-code rejected" reason)
                             'malformed 'unsupported)
                         :reason reason :input input)
                 (signal (car error-data) (cdr error-data))))))))))))

(defun nelisp-bytecode-native-compiler-build (function artifact-path entry-name)
  "Compile FUNCTION without accepting caller-supplied compiler analysis."
  (nelisp-bytecode-native-compiler--build-from-input
   function artifact-path entry-name
   (nelisp-bytecode-compiler-input-build function)))

(defconst nelisp-bytecode-native-compiler-package-preflight-api
  (let ((seals (make-hash-table :test 'eq))
        (helper-symbols nil)
        (context nil)
        (context-validated nil)
        (snapshot-value
         (lambda (value constants)
           (let ((active (make-hash-table :test 'eq))
                 (visited 0))
             (cl-labels
               ((walk (object depth)
                  (setq visited (1+ visited))
                  (when (> depth 128)
                    (error "bytecode-native-compiler: preflight metadata exceeds depth limit"))
                  (when (> visited 4096)
                    (error "bytecode-native-compiler: preflight metadata exceeds node limit"))
                  (cond
                   ((and (vectorp constants)
                         (let ((index 0) (found nil))
                           (while (and (not found) (< index (length constants)))
                             (when (eq object (aref constants index))
                               (setq found t))
                             (setq index (1+ index)))
                           found))
                    object)
                   ((stringp object) (copy-sequence object))
                   ((or (consp object) (vectorp object))
                    (when (gethash object active)
                      (error "bytecode-native-compiler: cyclic preflight metadata"))
                    (puthash object t active)
                    (unwind-protect
                        (if (consp object)
                            (cons (walk (car object) (1+ depth))
                                  (walk (cdr object) (1+ depth)))
                          (let ((copy (make-vector (length object) nil))
                                (index 0))
                            (while (< index (length object))
                              (aset copy index (walk (aref object index) (1+ depth)))
                              (setq index (1+ index)))
                            copy))
                      (remhash object active)))
                   (t object))))
               (walk value 0))))))
    (setq helper-symbols
          (append
           '(nelisp-bytecode-compiler-input-native-package-dependencies
             nelisp-bytecode-native-compiler--boxed-constant-return-input-p
             nelisp-bytecode-ir-native-package-dependencies
             nelisp-bytecode-frame-ir-native-package-dependencies)
           (nelisp-bytecode-compiler-input-native-package-dependencies)
           (nelisp-bytecode-ir-native-package-dependencies)
           (nelisp-bytecode-frame-ir-native-package-dependencies)))
    (list
     :preflight
     (lambda (function)
       "Build and seal compiler input for package-only reuse."
       (let* ((input (nelisp-bytecode-compiler-input-build function))
              (descriptor (and (byte-code-function-p function)
                               (aref function 0)))
              (code (and (byte-code-function-p function)
                         (aref function 1)))
              (constants (and (byte-code-function-p function)
                              (aref function 2)))
              (depth (and (byte-code-function-p function)
                          (aref function 3)))
              (source-witness
               (list :function function
                     :descriptor (funcall snapshot-value descriptor constants)
                     :code (if (stringp code) (copy-sequence code) code)
                     :constants (if (vectorp constants)
                                    (copy-sequence constants) constants)
                     :depth depth))
              (token (make-symbol "native-package-preflight"))
              (metadata-keys
               '(:status :reason :call1-symbol-template-p
                 :argument-descriptor :argument-list :argument-count
                 :argument-min :argument-max :required-argument-count
                 :rest-argument-p :rest-slot-return-template-p
                 :rest-native-code :initial-stack-depth
                 :computed-temporary-stack-depth :declared-stack-depth
                 :dialect-evidence))
              (metadata nil))
         (unless (and (byte-code-function-p function)
                      (eq function (plist-get input :function))
                      (equal descriptor (plist-get input :argument-descriptor))
                      (eq code (plist-get input :code))
                      (eq constants (plist-get input :constants))
                      (eql depth (plist-get input :declared-stack-depth)))
           (error "bytecode-native-compiler: input builder returned mismatched function"))
         (condition-case snapshot-error
             (progn
               (dolist (key metadata-keys)
                 (setq metadata
                       (append metadata
                               (list key (funcall snapshot-value
                                                  (plist-get input key) constants)))))
               (when (= (hash-table-count seals) 0)
                 (setq context
                       (list :dialect (funcall snapshot-value
                                              (plist-get input :dialect-evidence)
                                              constants)
                       :inventory (copy-sequence
                                   (nelisp-bytecode-compiler-input-inventory-sha256))
                       :root (copy-sequence (nelisp-bytecode-compiler-input-root))
                       :input-runtime-context
                       (nelisp-bytecode-compiler-input-native-package-runtime-context)
                       :runtime-dialect-id
                       (and (boundp 'nelisp-bytecode-runtime-dialect-id)
                            (funcall snapshot-value
                                     nelisp-bytecode-runtime-dialect-id constants))
                       :emacs-version
                       (and (boundp 'emacs-version)
                            (funcall snapshot-value emacs-version constants))
                       :opcode-vector
                       (and (boundp 'byte-code-vector)
                            (copy-sequence byte-code-vector))
                       :stack-adjust
                       (and (boundp 'byte-stack+-info)
                            (copy-sequence byte-stack+-info))))
                 (setq context-validated nil))
               (puthash
                token
                (list :function function :input input
                :descriptor (funcall snapshot-value descriptor constants)
                :code (if (stringp code) (copy-sequence code) code)
                :constants (if (vectorp constants)
                               (copy-sequence constants) constants)
                :depth depth
                :metadata metadata
                :ir (funcall snapshot-value (plist-get input :ir-result) constants)
                :frame (funcall snapshot-value (plist-get input :frame-result) constants)
                :dialect-evidence (funcall snapshot-value
                                           (plist-get input :dialect-evidence)
                                           constants)
                :context context
                :inventory (copy-sequence
                            (nelisp-bytecode-compiler-input-inventory-sha256))
                      :helpers (mapcar (lambda (symbol)
                                         (cons symbol (and (fboundp symbol)
                                                           (symbol-function symbol))))
                                       helper-symbols))
                seals)
               (list :input input :token token :source-witness source-witness))
           (error
            (let ((message (error-message-string snapshot-error)))
              (if (string-match-p "preflight metadata exceeds \\(depth\\|node\\) limit" message)
                  (list :input input :uncacheable t
                        :source-witness source-witness)
                (signal (car snapshot-error) (cdr snapshot-error))))))))
     :build
     (lambda (token artifact-path entry-name)
       "Consume sealed compiler input after checking its bytecode and provenance."
       (let ((record (gethash token seals)))
         (unless record
           (error "bytecode-native-compiler: missing or consumed package preflight token"))
         (unwind-protect
             (let* ((function (plist-get record :function))
                    (input (plist-get record :input))
                    (constants (and (byte-code-function-p function)
                                    (aref function 2)))
                    (constants-same
                     (and (vectorp constants)
                          (vectorp (plist-get record :constants))
                          (= (length constants)
                             (length (plist-get record :constants)))
                          (let ((index 0) (same t))
                            (while (and same (< index (length constants)))
                              (unless (eq (aref constants index)
                                          (aref (plist-get record :constants) index))
                                (setq same nil))
                              (setq index (1+ index)))
                            same)))
                    (helpers-same
                     (cl-every (lambda (pair)
                                 (and (fboundp (car pair))
                                      (eq (symbol-function (car pair)) (cdr pair))))
                               (plist-get record :helpers)))
                    (runtime-same
                     (and (equal (plist-get record :context) context)
                          (equal (nelisp-bytecode-compiler-input-inventory-sha256)
                                 (plist-get context :inventory))
                          (equal (nelisp-bytecode-compiler-input-root)
                                 (plist-get context :root))
                          (equal (nelisp-bytecode-compiler-input-native-package-runtime-context)
                                 (plist-get context :input-runtime-context))
                          (equal (and (boundp 'nelisp-bytecode-runtime-dialect-id)
                                      nelisp-bytecode-runtime-dialect-id)
                                 (plist-get context :runtime-dialect-id))
                          (equal (and (boundp 'emacs-version) emacs-version)
                                 (plist-get context :emacs-version))
                          (equal (and (boundp 'byte-code-vector) byte-code-vector)
                                 (plist-get context :opcode-vector))
                          (equal (and (boundp 'byte-stack+-info) byte-stack+-info)
                                 (plist-get context :stack-adjust))))
                    (metadata-keys
                     '(:status :reason :call1-symbol-template-p
                       :argument-descriptor :argument-list :argument-count
                       :argument-min :argument-max :required-argument-count
                       :rest-argument-p :rest-slot-return-template-p
                       :rest-native-code :initial-stack-depth
                       :computed-temporary-stack-depth :declared-stack-depth
                       :dialect-evidence))
                    (metadata nil))
               (dolist (key metadata-keys)
                 (setq metadata
                       (append metadata
                               (list key (funcall snapshot-value
                                                  (plist-get input key)
                                                  (aref function 2))))))
               (unless (and (byte-code-function-p function)
                            (eq function (plist-get input :function))
                            (eq (aref function 1) (plist-get input :code))
                            (eq (aref function 2) (plist-get input :constants))
                            (equal (aref function 0) (plist-get record :descriptor))
                            (equal (aref function 1) (plist-get record :code))
                            constants-same
                            (eql (aref function 3) (plist-get record :depth)))
                 (error "bytecode-native-compiler: sealed bytecode function changed"))
               (unless (equal metadata (plist-get record :metadata))
                 (error "bytecode-native-compiler: sealed compiler input metadata changed: %S"
                        (let ((keys metadata-keys) (changed nil))
                          (while keys
                            (unless (equal (plist-get metadata (car keys))
                                           (plist-get (plist-get record :metadata)
                                                      (car keys)))
                              (push (car keys) changed))
                            (setq keys (cdr keys)))
                          (nreverse changed))))
               (unless (equal (funcall snapshot-value
                                       (plist-get input :ir-result) constants)
                              (plist-get record :ir))
                 (error "bytecode-native-compiler: sealed IR result changed"))
               (unless (equal (funcall snapshot-value
                                       (plist-get input :frame-result) constants)
                              (plist-get record :frame))
                 (error "bytecode-native-compiler: sealed frame result changed"))
               (unless (equal (funcall snapshot-value
                                       (plist-get input :dialect-evidence) constants)
                              (plist-get record :dialect-evidence))
                 (error "bytecode-native-compiler: sealed dialect evidence changed"))
               (unless (and (equal (nelisp-bytecode-compiler-input-inventory-sha256)
                                   (plist-get record :inventory))
                            helpers-same runtime-same)
                 (error "bytecode-native-compiler: sealed compiler context changed"))
               (unless context-validated
                 (unless (equal (nelisp-bytecode-compiler-input-dialect)
                                (plist-get context :dialect))
                   (error "bytecode-native-compiler: byte-code dialect evidence changed after preflight"))
                 (setq context-validated t))
               (remhash token seals)
               (nelisp-bytecode-native-compiler--build-from-input
                function artifact-path entry-name input))
           (remhash token seals))))
     :source-current-p
     (lambda (function witness)
       "Check current function fields against an independent source witness."
       (let ((constants (and (byte-code-function-p function)
                             (aref function 2)))
             (saved (plist-get witness :constants)))
         (and (byte-code-function-p function)
              (eq function (plist-get witness :function))
              (equal (aref function 0) (plist-get witness :descriptor))
              (equal (aref function 1) (plist-get witness :code))
              (vectorp constants) (vectorp saved)
              (= (length constants) (length saved))
              (let ((index 0) (same t))
                (while (and same (< index (length constants)))
                  (unless (eq (aref constants index) (aref saved index))
                    (setq same nil))
                  (setq index (1+ index)))
                same)
              (eql (aref function 3) (plist-get witness :depth)))))
     :discard (lambda (token) (remhash token seals)))))

(provide 'nelisp-bytecode-native-compiler)
;;; nelisp-bytecode-native-compiler.el ends here
