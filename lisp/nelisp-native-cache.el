;;; nelisp-native-cache.el --- Private rooted-CFG native cache -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Private files have the same trust boundary as Emacs .eln files.  Compile
;; validates semantics; load checks identity and memory safety only.  A running
;; process never adopts a changed runtime or compiler.

;;; Code:
(require 'cl-lib)
(require 'nelisp-native-load)
(require 'nelisp-native-poll)
(require 'nelisp-bytecode-native-switch)
(require 'nelisp-bytecode-native-rooted-cfg-contract)
(require 'nelisp-bytecode-native-rooted-cfg-safe-contract)
(require 'nelisp-runtime-reload-abi)
(require 'nelisp-native-template)

(defconst nelisp-native-cache--format "nelisp-native-cache-v1")
(defconst nelisp-native-cache--compiler-modules
  '(nelisp-native-cache nelisp-native-poll nelisp-native-budget nelisp-native-gccjit nelisp-native-cfg-grammar nelisp-native-load nelisp-aot-compiler nelisp-standalone-arena-rewrite
    nelisp-bytecode-compiler-input nelisp-bytecode-compiler-input-dialect nelisp-bytecode-ir nelisp-bytecode-frame-ir nelisp-bytecode-handlers-u8
    nelisp-bytecode-native-rooted-cfg nelisp-bytecode-native-rooted-cfg-plan
    nelisp-bytecode-native-rooted-cfg-emit nelisp-bytecode-native-rooted-cfg-shared-emit
    nelisp-bytecode-native-rooted-cfg-postdom nelisp-bytecode-native-rooted-cfg-contract
    nelisp-bytecode-native-rooted-cfg-safe-contract nelisp-bytecode-native-rooted-cfg-native
    nelisp-bytecode-native-rooted-cfg-call nelisp-native-arithmetic-v2
    nelisp-bytecode-native-arithmetic-lowering nelisp-native-optimization-guard-v1
    nelisp-bytecode-native-guarded-lowering nelisp-bytecode-native-call1-layout
    nelisp-runtime-reload-abi nelisp-asm-x86_64 nelisp-asm-arm64
    nelisp-elf-write nelisp-sexp-layout
    ;; These compile-path dependencies also affect the generated artifact.
    nelisp-hash-custom nelisp-bytecode-native-switch nelisp-bytecode-cleanup nelisp-native-frame-v2 nelisp-native-funcall-v2 nelisp-bytecode-native-rooted-cfg-constructor-contract))
(defvar nelisp-native-cache--build-source-identity nil
  "Runtime/compiler source identity embedded by the reader build host.")
(defvar nelisp-native-cache--build-identities nil
  "Address-free ABI and backend identities computed before the cold dump.")
(defvar nelisp-native-cache--abi :unset)
(defvar nelisp-native-cache--compiler-revision :unset)
(defvar nelisp-native-cache--addresses nil)
(defvar nelisp-native-cache--unit-observer nil
  "Optional owner-thread observer of a reusable authenticated callable factory.")
(defvar nelisp-native-cache--disabled-reason nil)
(defvar nelisp-native-cache--cold-source-check nil
  "Opaque source-identity fence installed only by compiler cold preparation.")
(defvar nelisp-native-cache-backend 'in-house
  "Native code generator: in-house (default), gccjit or template.")
(defvar nelisp-native-cache-mode 'shared-v2
  "The cache artifact mode.  Only shared-v2 is currently executable.")
(defvar nelisp-native-cache-guard-mode 'off
  "The compiler guard mode included in the cache input identity.")

(defun nelisp-native-cache--print (value)
  "Print VALUE with canonical, complete settings."
  (let ((print-length nil) (print-level nil) (print-circle nil)
        (print-quoted t) (print-escape-newlines t)
        (print-escape-nonascii t) (print-escape-multibyte t))
    (prin1-to-string value)))

(defun nelisp-native-cache--hash (value)
  (secure-hash 'sha256 (nelisp-native-cache--print value)))

(defun nelisp-native-cache-compiler-revision-hash ()
  "Hash the explicit compiler source closure once, before any lookup.
A missing source disables caching rather than creating an incomplete key."
  (when (eq nelisp-native-cache--compiler-revision :unset)
    (setq nelisp-native-cache--compiler-revision
          (condition-case err
              (if nelisp-native-cache--cold-source-check
                  (funcall nelisp-native-cache--cold-source-check :source-check)
                (if nelisp-native-cache--build-source-identity
                    (copy-sequence nelisp-native-cache--build-source-identity)
              (let ((sources nil))
                (dolist (module nelisp-native-cache--compiler-modules)
                  (let ((path (locate-library (concat (symbol-name module) ".el") t)))
                    (unless path (error "Missing compiler source: %s" module))
                    (push (list module
                                (with-temp-buffer
                                  (set-buffer-multibyte nil)
                                  (insert-file-contents-literally path)
                                  (secure-hash 'sha256 (current-buffer))))
                          sources)))
                (nelisp-native-cache--hash
                 (list (if (boundp 'nelisp-bytecode-runtime-dialect-id)
                           nelisp-bytecode-runtime-dialect-id
                         (plist-get (nelisp-bytecode-compiler-input-dialect) :dialect))
                       (if (boundp 'nelisp-bytecode-runtime-opcode-inventory)
                           nelisp-bytecode-runtime-opcode-inventory
                         (nelisp-bytecode-compiler-input-inventory-sha256))
                       (nreverse sources))))))
            (error (setq nelisp-native-cache--disabled-reason err) nil))))
  nelisp-native-cache--compiler-revision)

(defun nelisp-native-cache--abi-components ()
  "Return every runtime ABI component required by the cache protocol."
  (append
   (list (nelisp-native-load-running-binary-sha256)
        (nelisp-native-load--raw-v2-contract-hash)
        nelisp-bytecode-native-rooted-cfg-contract-version
        nelisp-bytecode-native-rooted-cfg-contract-shared-version
        nelisp-native-load--rooted-production-layout
        nelisp-native-load-raw-layout-id-v2
        (nelisp-native-load--raw-v2-rooted-cfg-runtime-key)
        nelisp-native-load-bridgeable-symbols
        (nelisp-native-load--raw-v2-import-contract-hash
         (nelisp-native-load--raw-v2-symbols))
        (nelisp-native-load--raw-v2-call1-contract-hash)
        (nelisp-native-load--rooted-stack-contract-hash
         nelisp-native-load-raw-v2-bridgeable-imports)
        (nelisp-native-load--rooted-branch-contract-hash)
        (nelisp-native-load--rooted-branch-join-contract-hash 'cons)
        (nelisp-native-load--rooted-branch-join-contract-hash 'car)
        (nelisp-native-load-rooted-production-contract-hash)
        (nelisp-native-funcall-v2-descriptor)
        (nelisp-native-funcall-v2-hash)
        nelisp-bytecode-native-rooted-cfg-safe-contract-version
        nelisp-bytecode-native-rooted-cfg-safe-contract-f1-version
        nelisp-native-cache--format
        nelisp-native-load-raw-artifact-format-v2)
   (when (nelisp-native-load--windows-p)
     (list (nelisp-native-load--target-v2) (nelisp-native-load--runtime-abi-v2)))))

(defun nelisp-native-cache-prepare-cold-template ()
  "Prepare Tier 0 identities without loading any optimizing compiler module."
  (when (cl-some #'featurep '(nelisp-aot-compiler nelisp-bytecode-ir
                              nelisp-bytecode-native-rooted-cfg-plan
                              nelisp-bytecode-native-rooted-cfg-emit))
    (error "Lean template image already contains Tier 1 modules"))
  (let ((nelisp-native-cache-backend 'template))
    (setq nelisp-native-template--build-abi (nelisp-native-template-abi-hash)))
  (setq nelisp-native-template--abi :unset
        nelisp-native-cache--addresses nil)
  t)

(defun nelisp-native-cache-prepare-cold-compiler ()
  "Preload compiler Lisp, fencing its source closure before a cold dump.
Native artifacts are not loaded. Built readers freeze address-free identities
for their authenticated cold image; restored processes resolve fresh addresses.
Source-loaded compilers compare source bytes before using their fingerprint."
  (when (or nelisp-native-cache--cold-source-check
            (featurep 'nelisp-aot-compiler))
    (error "Native compiler cold preparation requires a fresh source loader"))
  (require 'nelisp-bytecode-compiler-input)
  (let ((before (nelisp-native-cache-compiler-revision-hash)))
    (unless before (error "Native compiler cold source fingerprint unavailable"))
    ;; The reader advertises the producer feature before loading the complete
    ;; source implementation. Load its actual source, not that feature marker.
    (load (locate-library "nelisp-bytecode-native-rooted-cfg-native.el" t) nil t t)
    (require 'nelisp-aot-compiler)
    (require 'nelisp-standalone-arena-rewrite)
    ;; Materialize the private structural indexes without emitting code or
    ;; adopting any runtime addresses. The dialect gate pins this recipe.
    (unless (eq (plist-get
                 (nelisp-bytecode-native-rooted-cfg-plan
                  (nelisp-bytecode-compiler-input-build
                   (make-byte-code 257 (unibyte-string 135) [] 1)) nil 'off)
                 :status) 'complete)
      (error "Native compiler cold structural preparation failed"))
    (setq nelisp-native-cache--compiler-revision :unset)
    (unless (equal before (nelisp-native-cache-compiler-revision-hash))
      (error "Native compiler sources changed during cold preparation"))
    (let* ((frozen (copy-sequence before)) (same (symbol-function 'equal))
           (dialect (copy-sequence nelisp-bytecode-runtime-dialect-id))
           (inventory (copy-sequence nelisp-bytecode-runtime-opcode-inventory))
           (modules (copy-sequence nelisp-native-cache--compiler-modules))
           (files
            (unless nelisp-native-cache--build-source-identity
            (mapcar (lambda (module)
                      (cons module
                            (with-temp-buffer
                              (set-buffer-multibyte nil)
                              (insert-file-contents-literally
                               (locate-library (concat (symbol-name module) ".el") t))
                              (buffer-string))))
                    modules))))
      ;; Prove the private bytes correspond to the already verified revision.
      ;; The consumer compares complete bytes, not mtime, size or public data.
      (unless (or nelisp-native-cache--build-source-identity
                  (equal frozen
                     (nelisp-native-cache--hash
                      (list dialect inventory
                            (mapcar (lambda (file)
                                      (list (car file) (secure-hash 'sha256 (cdr file))))
                                    files)))))
        (error "Native compiler sources changed while freezing their bytes"))
      (setq nelisp-native-cache--cold-source-check
            (lambda (current)
              (if (eq current :source-check)
                  (progn
                    (unless (and (or (null nelisp-native-cache--build-source-identity)
                                     (funcall same frozen nelisp-native-cache--build-source-identity))
                                 (funcall same dialect nelisp-bytecode-runtime-dialect-id)
                                 (funcall same inventory nelisp-bytecode-runtime-opcode-inventory)
                                 (funcall same modules nelisp-native-cache--compiler-modules)
                                 ;; A built reader executes its immutable source
                                 ;; closure. Disk edits take effect at rebuild;
                                 ;; hosted/source-loaded compilers retain the fence.
                                 (or nelisp-native-cache--build-source-identity
                                     (cl-every
                                  (lambda (file)
                                    (with-temp-buffer
                                      (set-buffer-multibyte nil)
                                      (insert-file-contents-literally
                                       (locate-library (concat (symbol-name (car file)) ".el") t))
                                      (funcall same (cdr file) (buffer-string))))
                                  files)))
                      (error "Native compiler cold source identity changed; rebuild the cold image"))
                    (copy-sequence frozen))
                (funcall same frozen current)))))
    ;; The image trailer already authenticates the exact reader. Freeze only
    ;; address-free identities; resolve process-local addresses after restore.
    (when nelisp-native-cache--build-source-identity
      (setq nelisp-native-cache--abi :unset)
      (let ((in-house (nelisp-native-cache-abi-hash))
            (gccjit (let ((nelisp-native-cache-backend 'gccjit))
                      (nelisp-native-cache-abi-hash))))
        (unless (and in-house gccjit) (error "Build ABI identity unavailable"))
        (setq nelisp-native-cache--build-identities
              (list nelisp-native-cache--abi before in-house gccjit))))
    (when nelisp-native-cache--build-source-identity
      (let ((nelisp-native-cache-backend 'template))
        (setq nelisp-native-template--build-abi (nelisp-native-template-abi-hash))))
    (setq nelisp-native-template--abi :unset)
    (setq nelisp-native-cache--compiler-revision :unset
          nelisp-native-cache--abi :unset
          nelisp-native-cache--addresses nil
          nelisp-native-cache--disabled-reason nil)
    t))

(defun nelisp-native-cache--stage (label)
  "Append bounded cache identity phases to the existing opt-in compiler trace."
  (let ((path (getenv "NELISP_ROOTED_CFG_STAGE_LOG")))
    (when (and (stringp path) (> (length path) 0))
      (write-region (format "cache-%s seconds=%.3f\n" label (float-time))
                    nil path t 'silent))))

(defun nelisp-native-cache-abi-hash ()
  "Return the once-per-process runtime and compiler cache identity, or nil.
Root address resolution itself checks raw support and the reload contract
exactly once.  Failure permanently disables this process's cache."
  (if (eq nelisp-native-cache-backend 'template)
      (nelisp-native-template-abi-hash)
  (when (eq nelisp-native-cache--abi :unset)
    (setq nelisp-native-cache--abi
          (condition-case err
              (let ((revision (progn (nelisp-native-cache--stage "revision-start")
                                      (prog1 (nelisp-native-cache-compiler-revision-hash)
                                        (nelisp-native-cache--stage "revision-end")))))
                (unless revision (error "Compiler fingerprint unavailable"))
                (when (and nelisp-native-cache--cold-source-check
                           (not (funcall nelisp-native-cache--cold-source-check revision)))
                  (error "Native compiler cold source identity changed; rebuild the cold image"))
                (setq nelisp-native-cache--addresses
                      (progn (nelisp-native-cache--stage "addresses-start")
                             (prog1 (nelisp-native-load-root-v2-addresses)
                               (nelisp-native-cache--stage "addresses-end"))))
                (unless (fboundp 'nelisp--native-pin-copy-v2)
                  (error "Native pin-copy unavailable"))
                (if nelisp-native-cache--build-identities
                    (progn
                      (unless (equal revision (nth 1 nelisp-native-cache--build-identities))
                        (error "Build compiler identity changed"))
                      (copy-sequence (car nelisp-native-cache--build-identities)))
                (let ((components (progn (nelisp-native-cache--stage "components-start")
                                       (prog1 (nelisp-native-cache--abi-components)
                                         (nelisp-native-cache--stage "components-end")))))
                  (unless (and (stringp (car components))
                               (= (length (car components)) 64))
                    (error "Running binary identity unavailable"))
                  (nelisp-native-cache--hash (list components revision)))))
            (error (setq nelisp-native-cache--disabled-reason err) nil))))
  (unless (memq nelisp-native-cache-backend '(in-house gccjit template))
    (error "Unsupported native cache backend: %S" nelisp-native-cache-backend))
  (and nelisp-native-cache--abi
       (if (and nelisp-native-cache--build-identities
                (equal nelisp-native-cache--abi
                       (car nelisp-native-cache--build-identities)))
           (copy-sequence
            (nth (if (eq nelisp-native-cache-backend 'gccjit) 3 2)
                 nelisp-native-cache--build-identities))
         (nelisp-native-cache--hash
          (list nelisp-native-cache--abi nelisp-native-cache-backend))))))

(defun nelisp-native-cache--private-directory (directory)
  "Create DIRECTORY privately, refusing symlinks, foreign owners and non-0700 modes."
  (if (nelisp-native-load--windows-p)
      (progn (require 'nelisp-native-windows)
             (nelisp-native-windows-private-directory directory))
  (unless (file-exists-p directory)
    (make-directory directory t)
    (set-file-modes directory #o700))
  (let ((attrs (file-attributes directory 'integer))
        (mode (file-modes directory)))
    (unless (and (not (file-symlink-p directory)) (eq (car attrs) t)
                 (eql (nth 2 attrs)
                      (if (fboundp 'user-uid) (user-uid)
                        (syscall-direct 102 0 0 0 0 0 0))) mode
                 (= (logand mode #o7777) #o700))
      (error "nelisp-native-cache: directory must be owned by user and mode 0700: %s"
             directory)))
  directory))

(defun nelisp-native-cache--root ()
  (expand-file-name
   (or (getenv "NELISP_NATIVE_CACHE")
       (if (nelisp-native-load--windows-p)
           (expand-file-name "NeLisp/native-cache"
                             (or (getenv "LOCALAPPDATA") (error "LOCALAPPDATA unavailable")))
         (expand-file-name "nelisp/native-cache"
                           (or (getenv "XDG_CACHE_HOME") (expand-file-name ".cache" "~")))))))

(defun nelisp-native-cache--function (function)
  (if (symbolp function) (symbol-function function) function))

(defun nelisp-native-cache--recipe (function)
  "Snapshot only canonical byte-code fields, without compiling or planning."
  (let ((fn (nelisp-native-cache--function function)))
    (if (eq nelisp-native-cache-backend 'template) (nelisp-native-template-recipe fn)
    (unless (byte-code-function-p fn) (error "Cache requires materialized byte-code"))
    (or (nelisp-bytecode-native-rooted-cfg-contract-input-recipe
         (list :function fn :argument-descriptor (aref fn 0)
               :code (aref fn 1) :constants (aref fn 2)
               :declared-stack-depth (aref fn 3)))
        (error "Cache relocation refused: unreadable or unsupported constant/metadata (buffer and marker objects cannot be serialized); function remains byte code")))))

(defun nelisp-native-cache--input-hash (function)
  (funcall (if (eq nelisp-native-cache-backend 'template)
               #'nelisp-native-template--hash #'nelisp-native-cache--hash)
           (list (nelisp-native-cache--recipe function)
                 nelisp-native-cache-mode nelisp-native-cache-guard-mode)))

(defun nelisp-native-cache-file (function)
  "Return the content-addressed private cache file for FUNCTION, or nil if disabled."
  (let ((abi (nelisp-native-cache-abi-hash)))
    (when abi
      (let* ((root (nelisp-native-cache--private-directory (nelisp-native-cache--root)))
             (dir (nelisp-native-cache--private-directory
                   (expand-file-name (substring abi 0 16) root)))
             (name (if (symbolp function) (symbol-name function) "lambda")))
        (expand-file-name
         (concat (replace-regexp-in-string "[^[:alnum:]_-]" "_" name)
                 "-" (substring (nelisp-native-cache--input-hash function) 0 32)
                 (if (eq nelisp-native-cache-backend 'gccjit) ".so" ".nelr"))
         dir)))))

(defun nelisp-native-cache--cstring (filename)
  "Copy FILENAME into a NUL-terminated native byte buffer.
The caller must inhibit mid-form collection until the syscall returns."
  (when (string-match-p "\0" filename)
    (error "Native cache filename contains NUL"))
  (let* ((bytes (if (fboundp 'string-byte) filename
                  (encode-coding-string filename 'utf-8-unix)))
         (size (string-bytes bytes))
         (buffer (alloc-bytes (1+ size) 1)))
    (unless (and (integerp buffer) (> buffer 0))
      (error "Native cache filename allocation failed"))
    (dotimes (index size)
      (ptr-write-u8 buffer index (nelisp-native-load--byte bytes index)))
    (ptr-write-u8 buffer size 0)
    buffer))

(defun nelisp-native-cache--publish (temporary final)
  "Publish TEMPORARY with an atomic no-clobber hard link to FINAL."
  (unwind-protect
      (if (nelisp-native-load--windows-p)
          (nelisp-native-windows-publish temporary final)
      (if (fboundp 'add-name-to-file)
          (condition-case nil
              (progn (add-name-to-file temporary final nil) t)
            (file-already-exists nil))
        (nelisp-native-load--without-midform-collect
         (lambda ()
           (>= (syscall-direct 86
                               (nelisp-native-cache--cstring temporary)
                               (nelisp-native-cache--cstring final)
                               0 0 0 0)
               0)))))
    (when (file-exists-p temporary) (delete-file temporary))))

(defun nelisp-native-cache--compile-in-house (function)
  "Compile and fully validate FUNCTION, publishing atomically on a cache miss."
  (when (and (nelisp-native-load--windows-p) (not (eq nelisp-native-cache-backend 'in-house)))
    (error "Windows supports only the in-house native backend"))
  (let ((file (nelisp-native-cache-file function)))
    (unless file (error "Native cache disabled: %S" nelisp-native-cache--disabled-reason))
    (unless (and (eq nelisp-native-cache-mode 'shared-v2)
                 (eq nelisp-native-cache-guard-mode 'off))
      (error "Unsupported native cache compilation mode"))
    (unless (file-exists-p file)
      (require 'nelisp-bytecode-native-rooted-cfg-native)
      (let* ((temporary nil) (serialized nil)
             (nelisp-native-load--serialization-receiver
              (lambda (manifest bytes) (setq serialized (cons manifest bytes)))))
        (unwind-protect
            (let* ((input (nelisp-bytecode-compiler-input-build
                           (nelisp-native-cache--function function)))
                   (result (nelisp-bytecode-native-rooted-cfg-native--build
                            input (concat file ".compile.nelr")
                            t nelisp-native-cache-guard-mode t))
                   (header
                    (list :nelisp-native-cache 1 :canonical-manifest 'prebuilt-v1 :backend 'in-house :abi (nelisp-native-cache-abi-hash)
                          :input (nelisp-native-cache--input-hash function)
                          :entry (plist-get result :entry-name)
                          :arity (plist-get result :argument-count)
                          :argument-min (plist-get input :argument-min)
                          :argument-max (plist-get input :argument-max)
                          :rest-argument-p (plist-get input :rest-argument-p)
                          :root-count (plist-get result :required-root-count)
                          :exit-root-base (plist-get (plist-get result :plan) :exit-root-base)
                          :initializers (append (plist-get result :primitive-initializers)
                                                (plist-get result :constant-initializers)
                                                (plist-get result :immediate-initializers)))))
              (unless (eq (plist-get result :status) 'complete)
                (error "Native cache compilation incomplete"))
              (setq temporary
                    (funcall (if (nelisp-native-load--windows-p)
                                 #'nelisp-native-windows-temporary #'make-temp-file)
                             (expand-file-name ".publish-" (file-name-directory file))))
              (let ((coding-system-for-write 'utf-8-unix))
                (write-region
                 (concat (nelisp-native-cache--print header) "\n"
                         (if (eq (car serialized) (plist-get result :manifest))
                             (cdr serialized)
                           (nelisp-native-cache--print (plist-get result :manifest))) "\n")
                 nil temporary nil 'silent))
              (nelisp-native-cache--publish temporary file))
          (when (and temporary (file-exists-p temporary)) (delete-file temporary)))))
    file))

(defvar nelisp-native-cache--gccjit-handles nil
  "Retain dlopen owners of callable gccjit cache units.")

(defun nelisp-native-cache--file-hash (file)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (current-buffer))))

(defun nelisp-native-cache--gccjit-imports (names)
  "Resolve NAMES with the existing T1 trusted runtime resolver."
  (mapcar (lambda (name)
            (cons name (nelisp-native-load--raw-v2-symbol-addr-trusted name))) names))

(defun nelisp-native-cache--compile-gccjit (function)
  "Compile through the shared front end; validate its contract exactly once."
  (require 'nelisp-native-gccjit)
  (when (and (nelisp-native-load--windows-p) (not (eq nelisp-native-cache-backend 'in-house)))
    (error "Windows supports only the in-house native backend"))
  (let* ((file (nelisp-native-cache-file function))
         (sidecar (and file (concat file ".nelh"))))
    (unless file (error "Native cache disabled: %S" nelisp-native-cache--disabled-reason))
    (unless (and (eq nelisp-native-cache-mode 'shared-v2)
                 (eq nelisp-native-cache-guard-mode 'off))
      (error "Unsupported native cache compilation mode"))
    ;; The sidecar is the commit marker: an interrupted .so publication remains
    ;; a miss, and may be completed by another publisher without clobbering it.
    (unless (and (file-exists-p file) (file-exists-p sidecar))
      (let ((library (make-temp-file (expand-file-name ".gccjit-" (file-name-directory file)) nil ".so"))
            (temporary nil))
        (unwind-protect
            (let* ((input (nelisp-bytecode-compiler-input-build (nelisp-native-cache--function function)))
                   (paired (nelisp-bytecode-native-rooted-cfg-shared-emit-build-from-input
                            input nelisp-bytecode-native-rooted-cfg-contract-shared-entry nil nelisp-native-cache-guard-mode))
                   (plan (plist-get paired :plan))
                   (emitted (plist-get paired :emitted))
                   (contract (and (eq (plist-get emitted :status) 'complete)
                                  (nelisp-bytecode-native-rooted-cfg-contract-create-shared-v2 input plan emitted))))
              ;; Authenticate all returned metadata, including fields omitted
              ;; from the serialized contract, before using it in the header.
              (let ((verified (and contract
                                   (nelisp-bytecode-native-rooted-cfg-contract-valid-p
                                    contract :reconstruction input))))
                (unless (and verified
                             (equal input (plist-get verified :input))
                             (equal plan (plist-get verified :plan))
                             (equal emitted (plist-get verified :emitted)))
                  (error "gccjit: shared-v2 compile contract refused"))
                ;; Compilation consumes the independent snapshot, not values
                ;; that the producer can mutate after the comparisons.
                (setq input (plist-get verified :input)
                      plan (plist-get verified :plan)
                      emitted (plist-get verified :emitted)
                      contract (plist-get verified :expected-contract)))
              (when (plist-get plan :funcall-version)
                (unless (and (fboundp 'nelisp-native-load-compiler-f1-runtime-p)
                             (nelisp-native-load-compiler-f1-runtime-p))
                  (error "gccjit: source-pinned F1 runtime unavailable")))
              (let* ((names (plist-get contract :imports))
                     (imports (nelisp-native-cache--gccjit-imports names)))
                (nelisp-native-gccjit-compile-to-file (plist-get emitted :form) imports library)
                (nelisp-native-cache--publish library file)
                ;; If another process won, bind this header to its published bytes.
                (let ((header (list :nelisp-native-cache 1 :backend 'gccjit
                                    :abi (nelisp-native-cache-abi-hash)
                                    :input (nelisp-native-cache--input-hash function)
                                    :entry (plist-get emitted :entry-name)
                                    :arity (plist-get emitted :argument-count)
                          :argument-min (plist-get input :argument-min)
                          :argument-max (plist-get input :argument-max)
                          :rest-argument-p (plist-get input :rest-argument-p)
                                    :root-count (plist-get emitted :required-root-count)
                                    :exit-root-base (plist-get plan :exit-root-base)
                                    :initializers (append (plist-get emitted :primitive-initializers)
                                                          (plist-get emitted :constant-initializers)
                                                          (plist-get emitted :immediate-initializers))
                                    :imports names :library-sha256 (nelisp-native-cache--file-hash file))))
                  (setq temporary (make-temp-file (expand-file-name ".header-" (file-name-directory file))))
                  (let ((coding-system-for-write 'utf-8-unix))
                    (write-region (concat (nelisp-native-cache--print header) "\n") nil temporary nil 'silent))
                  (nelisp-native-cache--publish temporary sidecar))))
          (when (file-exists-p library) (delete-file library))
          (when (and temporary (file-exists-p temporary)) (delete-file temporary)))))
    file))

;;;###autoload
(defun nelisp-native-cache-compile (function)
  "Compile FUNCTION once with the selected backend, publishing without clobber."
  (when (and (nelisp-native-load--windows-p) (not (memq nelisp-native-cache-backend '(in-house template))))
    (error "Windows supports only in-house and template native backends"))
  (nelisp-native-budget-check (if (eq nelisp-native-cache-backend 'gccjit) 4096 8192))
  (pcase nelisp-native-cache-backend
    ('in-house (nelisp-native-cache--compile-in-house function))
    ('gccjit (nelisp-native-cache--compile-gccjit function))
    ('template (nelisp-native-template-compile function))
    (_ (error "Unsupported native cache backend: %S" nelisp-native-cache-backend))))

(defun nelisp-native-cache--load-gccjit (file header &optional constants)
  "Bind runtime address cells and share the T1 frame callable, without validation."
  (require 'nelisp-native-gccjit)
  (require 'nl-ffi)
  (nelisp-native-budget-reserve (nelisp-native-budget-elf-bytes file))
  (nelisp-native-load--without-midform-collect
   (lambda ()
     (let* ((imports (nelisp-native-cache--gccjit-imports (plist-get header :imports)))
            (handle (nl-ffi--dlopen file))
            (entry (nelisp-native-gccjit--symbol handle (plist-get header :entry))))
       (dolist (import imports)
         (ptr-write-u64 (nelisp-native-gccjit--symbol
                        handle (nelisp-native-gccjit-import-cell-name (car import)))
                        0 (cdr import)))
       (push handle nelisp-native-cache--gccjit-handles)
       (let* ((addresses nelisp-native-cache--addresses)
              (factory (lambda (live-constants)
                         (let ((callable (nelisp-native-cache--callable-from-entry
                                          entry header addresses live-constants handle)))
                           (nelisp-native-cache--retain-callable callable handle)))))
         (when nelisp-native-cache--unit-observer
           (funcall nelisp-native-cache--unit-observer factory))
         (funcall factory constants))))))

(defun nelisp-native-cache--resume-exit (addresses env ticket base)
  "Resume the public caller's signal/throw protocol using cached ADDRESSES."
  (let* ((slot (plist-get addresses :slot))
         (roots (mapcar (lambda (index) (ptr-call slot env ticket index 0 0 0))
                        (list 0 base (1+ base) (+ base 2)))))
    (unless (cl-every (lambda (address) (and (integerp address) (> address 0))) roots)
      (error "Native cache exit roots unavailable"))
    (let ((kind (nelisp-native-load-unbox (nth 1 roots) env (car roots)))
          (tag (nelisp-native-load-unbox (nth 2 roots) env (car roots)))
          (value (nelisp-native-load-unbox (nth 3 roots) env (car roots))))
      (cond ((eql kind 1)
             (unless (and (symbolp tag) tag) (error "Malformed native cache signal"))
             (signal tag value))
            ((eql kind 2) (throw tag value))
            (t (error "Malformed native cache exit kind"))))))

(defun nelisp-native-cache--callable (handle header addresses &optional constants)
  "Construct a callable retaining HANDLE and its cached frame protocol."
  (let ((callable (nelisp-native-cache--callable-from-entry
                   (nelisp-native-load-raw-export-address handle (plist-get header :entry))
                   header addresses constants handle)))
    (nelisp-native-cache--retain-callable callable handle)))

(defun nelisp-native-cache--retain-callable (callable owner)
  "Retain OWNER for CALLABLE without an interpreted dispatch wrapper."
  (if (and (fboundp 'subrp) (subrp callable)) callable
    (lambda (&rest arguments)
      (unless owner (error "Native cache mapping unavailable"))
      (apply callable arguments))))

;; Constant vector of FUNCTION's byte code, or nil for non-byte-code input.
(defun nelisp-native-cache--constants (function)
  (let ((code (nelisp-native-cache--function function)))
    (and (byte-code-function-p code) (aref code 2))))

(defun nelisp-native-cache--callable-from-entry (entry header addresses &optional constants owner)
  "Construct the shared raw-v2 frame callable from ENTRY, HEADER and ADDRESSES."
  (catch 'nelisp-native-cache-callable
  (let ((env (plist-get addresses :environment))
        (arity (plist-get header :arity)) (count (plist-get header :root-count))
        (initializers (plist-get header :initializers))
        (begin (plist-get addresses :begin)) (reserve (plist-get addresses :reserve))
        (slot-address (plist-get addresses :slot)) (end (plist-get addresses :end))
        (entry-name (plist-get header :entry))
        (template-p (eq (plist-get header :backend) 'template))
        (primitive-initializer (symbol-function 'nelisp-native-funcall-v2-initializer))
        (poll-function (nelisp-native-poll-state))
        (switch-function (nelisp-bytecode-native-switch-function))
        (exit-base (plist-get header :exit-root-base))
        (broken nil))
    ;; Resolve immutable providers once. Constant roots remain live on EACH
    ;; invocation, and frame policy creates a fresh activation in the gateway.
    (setq initializers
          (mapcar (lambda (init)
                    (if (plist-member init :constant-index) init
                      (list :root (plist-get init :root) :poll-state (plist-get init :poll) :value
                            (cond ((plist-get init :primitive)
                                   (funcall primitive-initializer (plist-get init :primitive)))
                                  ((plist-get init :poll) poll-function)
                                  ((plist-get init :switch) switch-function)
                                  ((plist-get init :frame) (nelisp-native-frame-v2-initializer))
                                  (t (plist-get init :value)))))) initializers))
    ;; The private tag-18 descriptor is constructed only after authentication.
    ;; Runtime checks below protect activation ownership, not artifact trust.
    (when (fboundp 'nelisp--native-subr-create)
      (let ((descriptor
             (vector entry (or (plist-get header :argument-min) arity)
                     (if (plist-get header :rest-argument-p) -1
                       (or (plist-get header :argument-max) arity))
                     arity count
                     (vconcat (mapcar
                               (lambda (init)
                                 (vector (plist-get init :root)
                                         (cond ((plist-member init :constant-index) 1)
                                               ((plist-get init :poll-state) 2) (t 0))
                                         (if (plist-member init :constant-index)
                                             (plist-get init :constant-index)
                                           (plist-get init :value)))) initializers))
                     constants (or exit-base -1) (list owner addresses header))))
        (throw 'nelisp-native-cache-callable
          (let ((native (nelisp--native-subr-create descriptor (intern entry-name) 0 t)))
            (if template-p
                (lambda (&rest arguments)
                  (setq nelisp-native-template--entry-count (1+ nelisp-native-template--entry-count))
                  (apply native arguments))
              native)))))
    (lambda (&rest arguments)
      ;; Keep the entire mapping reachable for the lifetime of the closure.
      (unless (and entry (not broken)) (error "Native cache unit is broken"))
      (let ((minimum (or (plist-get header :argument-min) arity))
            (maximum (if (plist-get header :rest-argument-p) nil
                       (or (plist-get header :argument-max) arity)))
            (argc (length arguments)))
        (unless (and (>= argc minimum) (or (null maximum) (<= argc maximum)))
          (signal 'wrong-number-of-arguments (list entry-name argc)))
        (let ((normalized nil) (tail arguments) (index 0))
          (while (< index arity)
            (push (if (and (null maximum) (= index (1- arity))) (copy-sequence tail)
                    (prog1 (car tail) (setq tail (cdr tail)))) normalized)
            (setq index (1+ index)))
          (setq arguments (nreverse normalized))))
      (let ((ticket nil) (slots nil))
        (unwind-protect
            (progn
              (setq ticket (ptr-call begin env 0 0 0 0 0))
              (unless (and (integerp ticket) (> ticket 0))
                (error "Native cache root frame begin failed"))
              (dotimes (_ count)
                (let ((slot (ptr-call reserve env ticket 0 0 0 0)))
                  (unless (and (integerp slot) (> slot 0))
                    (error "Native cache root reservation failed"))
                  (push slot slots)))
              (setq slots (nreverse slots))
              ;; nl_root_pin_reserve_v2 initializes all four words to nil.
              ;; Clearing again through Lisp boxing is redundant and allocates.

              (cl-loop for arg in arguments for index from 1 do
                       (unless (eql (nelisp--native-pin-copy-v2 env ticket index arg)
                                    (nth index slots))
                         (error "Native cache argument root mismatch")))
              (dolist (init initializers)
                (let ((index (plist-get init :root)))
                  (unless (eql (nelisp--native-pin-copy-v2
                                env ticket index (if (plist-member init :constant-index)
                                   (aref constants (plist-get init :constant-index))
                                 (if (plist-get init :poll-state)
                                     (copy-sequence (plist-get init :value))
                                   (plist-get init :value))))
                               (nth index slots))
                    (error "Native cache initializer root mismatch"))))
              (cl-loop for slot in slots for index from 0 do
                       (unless (eql slot (ptr-call slot-address
                                                  env ticket index 0 0 0))
                         (error "Native cache roots changed before entry")))
              (when template-p
                (setq nelisp-native-template--entry-count (1+ nelisp-native-template--entry-count)))
              (let ((status (ptr-call entry env ticket arity count 0 0)))
                (cl-loop for slot in slots for index from 0 do
                         (unless (eql slot (ptr-call slot-address
                                                    env ticket index 0 0 0))
                           (error "Native cache roots changed across entry")))
                (cond ((and exit-base (eql status (+ 1024 exit-base)))
                       (nelisp-native-cache--resume-exit addresses env ticket exit-base))
                      ((and (integerp status) (<= 512 status) (< status (+ 512 count)))
                       (nelisp-native-load-unbox (nth (- status 512) slots) env (car slots)))
                      ((and (integerp status) (<= 256 status) (< status (+ 256 count)))
                       (signal 'wrong-type-argument
                               (list 'listp (nelisp-native-load-unbox
                                             (nth (- status 256) slots) env (car slots)))))
                      (t (error "Native cache infrastructure status: %S" status)))))
          (when (and (integerp ticket) (> ticket 0))
            (condition-case err
                (unless (eql (ptr-call end env ticket 0 0 0 0) 1)
                  (error "Native cache root frame ownership lost"))
              (error (setq broken t) (signal (car err) (cdr err)))))))))))

(defun nelisp-native-cache--unsigned-snapshot (snapshot start end manifest)
  "Recover the producer's unsigned bytes from one bounded SNAPSHOT.
Return nil for legacy layouts, preserving their ordinary canonical check."
  (let* ((digest (plist-get manifest :artifact-sha256))
         (suffix (and (stringp digest)
                      (= (length digest) 64)
                      (concat " :artifact-sha256 " (prin1-to-string digest) ")")))
         (cut (and suffix (- end (length suffix)))))
    (while (and (< start end) (memq (aref snapshot start) '(32 9 10 13)))
      (setq start (1+ start)))
    (when (and cut (> cut start) (= (aref snapshot start) 40)
               (equal (substring snapshot cut end) suffix))
      (concat (substring snapshot start cut) ")"))))

(defun nelisp-native-cache-load (function)
  "Load FUNCTION from one private-file snapshot, without semantic revalidation."
  (let* ((file (nelisp-native-cache-file function))
         (sidecar (and file (if (eq nelisp-native-cache-backend 'gccjit)
                               (concat file ".nelh") file))))
    (when (and file (file-exists-p file) (file-exists-p sidecar))
      (let* ((snapshot (if (nelisp-native-load--windows-p)
                           (nelisp-native-windows-file-bytes sidecar t)
                         (with-temp-buffer (insert-file-contents sidecar) (buffer-string))))
             (read-eval nil) (read-circle nil)
             (first (read-from-string snapshot))
             (header (car first))
             (code (nelisp-native-cache--function function))
             (descriptor (and (byte-code-function-p code) (aref code 0)))
             (minimum (if (integerp descriptor) (logand descriptor 127) 0))
             (restp (and (integerp descriptor) (/= (logand descriptor 128) 0)))
             (maximum (if (integerp descriptor) (ash descriptor -8) 0))
             (arity (plist-get header :arity)) (count (plist-get header :root-count))
             (base (plist-get header :exit-root-base)))
        (unless (and (nelisp-native-load--trusted-list-p header)
                     (eql (plist-get header :nelisp-native-cache) 1)
                     (eq (or (plist-get header :backend) 'in-house) nelisp-native-cache-backend)
                     (equal (plist-get header :abi) (nelisp-native-cache-abi-hash))
                     (equal (plist-get header :input) (nelisp-native-cache--input-hash function))
                     (stringp (plist-get header :entry))
                     (integerp arity) (<= 0 arity)
                     (= arity (+ maximum (if restp 1 0)))
                     (= minimum (or (plist-get header :argument-min) arity))
                     (eq (not (null restp)) (not (null (plist-get header :rest-argument-p))))
                     (if restp (null (plist-get header :argument-max))
                       (eql maximum (or (plist-get header :argument-max) arity)))
                     (integerp count) (< arity count) (< 0 count 256)
                     (or (null base) (and (integerp base) (> base 0) (< (+ base 2) count)))
                     (nelisp-native-load--trusted-list-p (plist-get header :initializers))
                     (cl-every (lambda (init)
                                 (let ((index (plist-get init :root)))
                                   (and (integerp index) (< 0 index count))))
                               (plist-get header :initializers)))
          (error "Native cache header identity or structure mismatch"))
        (if (eq nelisp-native-cache-backend 'gccjit)
            (progn
              (unless (and (string-match-p "\\`[ \t\r\n]*\\'" (substring snapshot (cdr first)))
                           (nelisp-native-load--trusted-list-p (plist-get header :imports))
                           (cl-every #'stringp (plist-get header :imports))
                           (equal (plist-get header :library-sha256)
                                  (nelisp-native-cache--file-hash file)))
                (error "Native cache gccjit sidecar or library mismatch"))
              (nelisp-native-cache--load-gccjit file header (nelisp-native-cache--constants function)))
          (let* ((second (read-from-string snapshot (cdr first)))
               (manifest (car second)))
          (unless (string-match-p "\\`[ \t\r\n]*\\'" (substring snapshot (cdr second)))
            (error "Trailing native cache data"))
          (when (eq nelisp-native-cache-backend 'template)
            (let ((certificate (plist-get (plist-get manifest :native-template-proof) :certificate)))
              (unless (and (equal (plist-get header :entry) nelisp-native-template-entry)
                           (eql arity (plist-get certificate :arity))
                           (eql count (plist-get certificate :root-count))
                           (eql base (plist-get certificate :exit-root-base))
                           (equal (plist-get header :initializers) (plist-get certificate :initializers)))
                (error "Template header/root certificate mismatch"))))
          (let* ((unsigned (and (eq (plist-get header :canonical-manifest) 'prebuilt-v1)
                           (nelisp-native-cache--unsigned-snapshot
                            snapshot (cdr first) (cdr second) manifest)))
                 (nelisp-native-load--trusted-serialization
                  (and unsigned (cons manifest unsigned))))
            (let* ((handle (nelisp-native-load-raw-v2-artifact-trusted manifest (plist-get header :entry) file))
                   (addresses nelisp-native-cache--addresses)
                   (factory (lambda (live-constants)
                              (nelisp-native-cache--callable handle header addresses live-constants))))
              (when nelisp-native-cache--unit-observer
                (funcall nelisp-native-cache--unit-observer factory))
              (funcall factory (nelisp-native-cache--constants function))))))))))

;;;###autoload
(defun nelisp-native-cache-install (symbol function)
  "Load or compile FUNCTION and install its native callable in SYMBOL."
  (unless (symbolp symbol) (error "Native cache installation requires a symbol"))
  (let ((callable (or (nelisp-native-cache-load function)
                      (progn (nelisp-native-cache-compile function)
                             (nelisp-native-cache-load function)))))
    (unless callable (error "Native cache unit unavailable"))
    (fset symbol callable)
    callable))

(provide 'nelisp-native-cache)
;;; nelisp-native-cache.el ends here
