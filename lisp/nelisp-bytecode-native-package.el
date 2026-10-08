;;; nelisp-bytecode-native-package.el --- cold-loadable native package slice -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Package a bounded set of GNU .elc byte-code definitions into separately
;; authenticated .neln entries.  Compilation reads .elc as data; opening the
;; package loads its copied .elc once for load-time effects and definitions.
;; The admitted load profile is intentionally narrow: readable top-level forms
;; must end with exactly (provide FEATURE).  This does not implement GNU's full
;; .elc load-history, autoload, advice, or reader compatibility behavior.

;;; Code:

(require 'cl-lib)
(require 'nelisp-bytecode-native-consumer)
(require 'nelisp-bytecode-native-compiler)
(require 'nelisp-native-boxed-unit)

(defconst nelisp-bytecode-native-package--marker
  'nelisp-bytecode-native-package-v2)
(defvar nelisp-bytecode-native-package-native-enabled t
  "When nil, package calls use their current Lisp function cell.")

(defun nelisp-bytecode-native-package-raw-read-elc-forms (path)
  "Public raw-package facade for reading PATH's .elc data without evaluation."
  (nelisp-bytecode-native-package--read-elc-forms path))

(defun nelisp-bytecode-native-package-raw-final-provider-p (forms feature)
  "Public raw-package facade checking FORMS' final FEATURE provider."
  (nelisp-bytecode-native-package--final-provider-p forms feature))

(defun nelisp-bytecode-native-package-raw-eval-elc-forms (forms)
  "Public raw-package facade evaluating previously validated FORMS."
  (nelisp-bytecode-native-package--eval-elc-forms forms))

(defun nelisp-bytecode-native-package-raw-file-sha256 (path)
  "Public raw-package facade returning PATH's literal-byte SHA-256."
  (nelisp-bytecode-native-package--sha256-file path))

(defun nelisp-bytecode-native-package-raw-copy-file-bytes (source destination)
  "Public raw-package facade copying SOURCE bytes to DESTINATION."
  (nelisp-bytecode-native-package--copy-file-bytes source destination))

(defun nelisp-bytecode-native-package--abi-fingerprint ()
  "Return this package reader's fingerprint, or reject an unknown dialect."
  (let* ((dialect (nelisp-bytecode-compiler-input-dialect))
         (native-abi (nelisp-artifact-native-runtime-abi)))
    (unless (and (eq (plist-get dialect :status) 'pinned)
                 (stringp (plist-get dialect :dialect))
                 (stringp (plist-get dialect :inventory-sha256))
                 (stringp native-abi))
      (error "bytecode-native-package: ABI fingerprint unavailable: %s"
             (or (plist-get dialect :reason) "native ABI identity is missing")))
    (secure-hash
     'sha256
     (prin1-to-string
      (list :package-format nelisp-bytecode-native-package--marker
            :native-runtime-abi native-abi
            :bytecode-dialect (plist-get dialect :dialect)
            :bytecode-inventory-sha256
            (plist-get dialect :inventory-sha256))))))

(defun nelisp-bytecode-native-package-abi-fingerprint ()
  "Return the package dialect/runtime ABI fingerprint for raw producers."
  (nelisp-bytecode-native-package--abi-fingerprint))

(defun nelisp-bytecode-native-package--sha256-file (path)
  "Return the SHA-256 digest of literal bytes in PATH."
  (with-temp-buffer
    (insert-file-contents-literally path)
    (secure-hash 'sha256 (current-buffer))))

(defun nelisp-bytecode-native-package--copy-file-bytes (source destination)
  "Copy SOURCE to DESTINATION as literal bytes."
  (with-temp-buffer
    (insert-file-contents-literally source)
    (write-region (buffer-string) nil destination nil 'silent)))

(defun nelisp-bytecode-native-package--read-elc-forms (path)
  "Forward to the producer-independent compiled input reader."
  (nelisp-bytecode-native-consumer-read-elc-forms path))

(defun nelisp-bytecode-native-package--elc-definitions (forms)
  "Forward to the producer-independent compiled input reader."
  (nelisp-bytecode-native-consumer-read-elc-definitions forms))

(defun nelisp-bytecode-native-package-read-elc-functions (path)
  "Forward to the producer-independent compiled input reader."
  (nelisp-bytecode-native-consumer-read-elc-functions path))

(defun nelisp-bytecode-native-package-read-elc-function (path name)
  "Forward to the producer-independent compiled input reader."
  (nelisp-bytecode-native-consumer-read-elc-function path name))

(defun nelisp-bytecode-native-package--eval-elc-forms (forms)
  "Evaluate the forms already read from a verified GNU .elc, in order."
  (dolist (form forms)
    (eval form)))

(defun nelisp-bytecode-native-package--final-provider-p (forms feature)
  "Return non-nil when FORMS ends with its exact FEATURE provider."
  (let ((last-form (car (last forms))))
    (and (consp last-form) (eq (car last-form) 'provide)
         (consp (cdr last-form)) (null (cddr last-form))
         (consp (cadr last-form)) (eq (caadr last-form) 'quote)
         (eq (cadadr last-form) feature) (null (cddadr last-form)))))

(defun nelisp-bytecode-native-package--safe-entry-name (symbol)
  "Return the safe filename stem for SYMBOL or signal an error."
  (let ((name (symbol-name symbol)))
    (unless (string-match-p "\\`[A-Za-z0-9_-]+\\'" name)
      (error "bytecode-native-package: unsafe function name: %S" symbol))
    name))

(defun nelisp-bytecode-native-package--native-entry-name (symbol)
  "Return an injective C-compatible entry name for SYMBOL."
  (concat "nl_pkg"
          (mapconcat (lambda (character)
                       (format "_%04X" (aref (symbol-name symbol) character)))
                     (number-sequence 0 (1- (length (symbol-name symbol))))
                     "")))

(defun nelisp-bytecode-native-package-compile-elc
    (elc-path feature function-names package-directory)
  "Compile named byte-code functions in GNU ELC-PATH into PACKAGE-DIRECTORY.

ELC-PATH is read as data and never evaluated here. FEATURE is provided by the
ELC as its final top-level form, and FUNCTION-NAMES is a nonempty list of its
supported definitions. Each
function is emitted through `nelisp-bytecode-native-compiler-build'. The
resulting package contains a copied .elc, hash-pinned .neln entries, and a
readable package manifest. Load-time forms run only when the package is opened.
The output directory must not already exist."
  (unless (and (stringp elc-path) (string-suffix-p ".elc" elc-path)
               (file-readable-p elc-path) (symbolp feature)
               (consp function-names) (cl-every #'symbolp function-names)
               (stringp package-directory) (not (file-exists-p package-directory)))
    (error "bytecode-native-package: invalid compile contract"))
  (unless (= (length function-names)
             (length (delete-dups (copy-sequence function-names))))
    (error "bytecode-native-package: duplicate function name"))
  (let* ((abi-fingerprint
          (nelisp-bytecode-native-package--abi-fingerprint))
         (forms (nelisp-bytecode-native-package--read-elc-forms elc-path))
         (definitions (nelisp-bytecode-native-package--elc-definitions forms))
         (directory (expand-file-name package-directory))
         (elc-copy "module.elc")
         (compiler-inputs (make-hash-table :test 'eq))
         (preflight-api nelisp-bytecode-native-compiler-package-preflight-api)
         (discard-preflights
          (lambda ()
            (maphash (lambda (_name preflight)
                       (funcall (plist-get preflight-api :discard)
                                (nth 2 preflight)))
                     compiler-inputs)))
         (entries nil)
         (created nil)
         (published nil)
         (manifest-path (expand-file-name "package.npkg" directory))
         (manifest-temp (expand-file-name "package.npkg.tmp" directory)))
    (unless (nelisp-bytecode-native-package--final-provider-p forms feature)
      (error "bytecode-native-package: .elc is truncated or does not end in (provide %S)"
             feature))
    (dolist (name function-names)
      (unless (assq name definitions)
        (error "bytecode-native-package: .elc has no byte-code definition for %S" name)))
    (condition-case preflight-error
        (dolist (name function-names)
          (let* ((function (cdr (assq name definitions)))
                 (sealed (funcall (plist-get preflight-api :preflight) function))
                 (input (plist-get sealed :input))
                 (token (plist-get sealed :token))
                 (uncacheable (plist-get sealed :uncacheable))
                 (witness (plist-get sealed :source-witness)))
            (puthash name (list function input token uncacheable witness) compiler-inputs)
        (when (plist-get input :call1-symbol-template-p)
          (error "bytecode-native-package: %S lowers to an opaque raw-v2 CALL1 token and cannot enter a boxed .neln package"
                 name))
        (when (and (eq (plist-get input :status) 'complete)
                   (nelisp-bytecode-native-compiler-cons-template-p input))
          (error "bytecode-native-package: %S lowers to raw-runtime-v2 and cannot enter a boxed .neln package"
                 name))
        (when (and (eq (plist-get input :status) 'complete)
                   (nelisp-bytecode-native-compiler-unary-template-operation
                    input))
          (error "bytecode-native-package: %S lowers to raw-runtime-v2 and cannot enter a boxed .neln package"
                 name))
        (when (and (eq (plist-get input :status) 'complete)
                   (nelisp-bytecode-native-compiler-unary-chain-operations input))
          (error "bytecode-native-package: %S lowers to raw-runtime-v2 unary chain and cannot enter a boxed .neln package"
                 name))
        (when (and (eq (plist-get input :status) 'complete)
                   (nelisp-bytecode-native-compiler-rooted-stack-input-p input))
          (error "bytecode-native-package: %S lowers to rooted raw-runtime-v2 and cannot enter a boxed .neln package"
                 name))
        (when (and (eq (plist-get input :status) 'complete)
                   (nelisp-bytecode-native-rooted-conditional-input-p input))
          (error "bytecode-native-package: %S lowers to the fixed rooted conditional raw-v2 probe and cannot enter a boxed .neln package"
                 name))
        (when (and (eq (plist-get input :status) 'complete)
                   (nelisp-bytecode-native-compiler-rooted-branch-input-p input))
          (error "bytecode-native-package: %S lowers to the fixed rooted branch raw-v2 probe and cannot enter a boxed .neln package"
                 name))
            (when (nelisp-bytecode-native-compiler-rooted-branch-join-operation input)
              (error "bytecode-native-package: %S is a joined CAR/CDR raw-v2 entry and cannot enter a boxed .neln package"
                     name))))
      (error
       (funcall discard-preflights)
       (signal (car preflight-error) (cdr preflight-error))))
    (unwind-protect
        (progn
          ;; Create the parent if needed, then atomically claim this package
          ;; directory.  The earlier absence check is only advisory: another
          ;; publisher may win between that check and this mkdir.
          (make-directory (file-name-directory directory) t)
          (make-directory directory)
          (setq created t)
          (dolist (name function-names)
            (let* ((function (cdr (assq name definitions)))
                   (preflight (gethash name compiler-inputs))
                   (input (nth 1 preflight))
                   (_ (unless (and preflight
                                   (eq function (car preflight))
                                   (eq function (plist-get input :function))
                                   (eq (aref function 1) (plist-get input :code))
                                   (eq (aref function 2) (plist-get input :constants))
                                   (equal (aref function 0)
                                          (plist-get input :argument-descriptor))
                                   (eql (aref function 3)
                                        (plist-get input :declared-stack-depth))
                                   (funcall (plist-get preflight-api :source-current-p)
                                            function (nth 4 preflight)))
                        (error "bytecode-native-package: %S changed after preflight" name)))
                   (stem (nelisp-bytecode-native-package--safe-entry-name name))
                   (relative (concat stem ".neln"))
                   (entry-name
                    (nelisp-bytecode-native-package--native-entry-name name))
                   (artifact (expand-file-name relative directory))
                   (result (if (nth 3 preflight)
                               (nelisp-bytecode-native-compiler-build
                                function artifact entry-name)
                             (funcall (plist-get preflight-api :build)
                                      (nth 2 preflight) artifact entry-name))))
              (unless (and (eq (plist-get result :status) 'complete)
                           (file-readable-p artifact))
                (error "bytecode-native-package: %S refused: %s"
                       name (or (plist-get result :reason) "unsupported byte-code")))
              (let ((layout (plist-get result :hidden-constant-indices))
                    (hidden-count (plist-get result :hidden-constant-count)))
                (unless (and (vectorp layout) (integerp hidden-count)
                             (= hidden-count (length layout)))
                  (error "bytecode-native-package: %S compiler omitted hidden-constant layout"
                         name))
              (push (append
                     (list :name name :artifact relative
                           :sha256 (nelisp-bytecode-native-package--sha256-file artifact)
                           :minimum (plist-get input :argument-min)
                           :maximum (plist-get input :argument-max)
                           :hidden-constant-count hidden-count
                           :hidden-constant-indices layout
                           :entry entry-name)
                     (when (plist-get input :rest-slot-return-template-p)
                       (list :rest-required-count
                             (plist-get input :required-argument-count))))
                    entries))))
          (nelisp-bytecode-native-package--copy-file-bytes
           elc-path (expand-file-name elc-copy directory))
          (unless (equal (nelisp-bytecode-native-package--sha256-file elc-path)
                         (nelisp-bytecode-native-package--sha256-file
                          (expand-file-name elc-copy directory)))
            (error "bytecode-native-package: copied .elc hash mismatch"))
          (setq entries (nreverse entries))
          (let ((manifest
                 (list :format nelisp-bytecode-native-package--marker
                       :abi-fingerprint abi-fingerprint
                       :feature feature :elc elc-copy
                       :elc-sha256
                       (nelisp-bytecode-native-package--sha256-file
                        (expand-file-name elc-copy directory))
                       :entries entries)))
            (with-temp-file manifest-temp (prin1 manifest (current-buffer)))
            (rename-file manifest-temp manifest-path)
            (setq published t)
            (list :status 'complete :manifest manifest-path
                  :package-directory directory :entries entries)))
      (when (and created (not published))
        (delete-directory directory t))
      (funcall discard-preflights))))

(defun nelisp-bytecode-native-package--read-manifest (path)
  "Read and validate the basic package manifest at PATH."
  (let ((manifest (with-temp-buffer
                    (insert-file-contents-literally path)
                    (goto-char (point-min))
                    (read (current-buffer)))))
    (unless (and (listp manifest)
                 (eq (plist-get manifest :format)
                     nelisp-bytecode-native-package--marker)
                 (symbolp (plist-get manifest :feature))
                 (stringp (plist-get manifest :elc))
                 (stringp (plist-get manifest :elc-sha256))
                 (stringp (plist-get manifest :abi-fingerprint))
                 (consp (plist-get manifest :entries)))
      (error "bytecode-native-package: malformed package manifest: %s" path))
    manifest))

(defun nelisp-bytecode-native-package-open (manifest-path)
  "Validate package identity, load its .elc once, and return a lazy handle."
  (let* ((manifest-path (expand-file-name manifest-path))
         (directory (file-name-directory manifest-path))
         (manifest (nelisp-bytecode-native-package--read-manifest manifest-path))
         (abi-fingerprint (plist-get manifest :abi-fingerprint))
         (elc-path (expand-file-name (plist-get manifest :elc) directory))
         (feature (plist-get manifest :feature))
         (entries (make-hash-table :test 'eq))
         (names-seen (make-hash-table :test 'eq))
         (artifacts-seen (make-hash-table :test 'equal))
         (originals (make-hash-table :test 'eq))
         (native-eligible (make-hash-table :test 'eq))
         (units (make-hash-table :test 'eq))
         (calls (make-hash-table :test 'eq))
         (witnesses (make-hash-table :test 'eq)))
    ;; This check precedes reading/evaluating module.elc and precedes any
    ;; native artifact load.  A manifest from another dialect/runtime fails
    ;; closed without running package load-time forms.
    (unless (equal abi-fingerprint
                   (nelisp-bytecode-native-package--abi-fingerprint))
      (error "bytecode-native-package: ABI fingerprint mismatch"))
    (unless (and (equal (plist-get manifest :elc) "module.elc")
                 (file-readable-p elc-path)
                 (equal (plist-get manifest :elc-sha256)
                        (nelisp-bytecode-native-package--sha256-file elc-path)))
      (error "bytecode-native-package: .elc identity mismatch"))
    ;; Validate every artifact before executing any package top-level form.
    (dolist (entry (plist-get manifest :entries))
      (let* ((name (plist-get entry :name))
             (artifact-name (plist-get entry :artifact))
             (native-entry (plist-get entry :entry))
             (artifact-path (and (stringp artifact-name)
                                 (expand-file-name artifact-name directory))))
        (unless (and (symbolp (plist-get entry :name))
                     (string-match-p "\\`[A-Za-z0-9_-]+\\.neln\\'" artifact-name)
                     (string-match-p "\\`[A-Za-z_][A-Za-z0-9_]*\\'" native-entry)
                     (not (gethash name names-seen))
                     (not (gethash artifact-name artifacts-seen))
                     (stringp (plist-get entry :sha256))
                     (integerp (plist-get entry :minimum))
                     (or (and (integerp (plist-get entry :maximum))
                              (<= 0 (plist-get entry :minimum)
                                  (plist-get entry :maximum))
                              (not (memq :rest-required-count entry)))
                         (and (null (plist-get entry :maximum))
                              (integerp (plist-get entry :rest-required-count))
                              (= (plist-get entry :minimum)
                                 (plist-get entry :rest-required-count))))
                     (file-readable-p artifact-path)
                     (equal (plist-get entry :sha256)
                            (nelisp-bytecode-native-package--sha256-file artifact-path)))
                 (error "bytecode-native-package: native artifact identity mismatch for %S"
                 name))
        (puthash name t names-seen)
        (puthash artifact-name t artifacts-seen)
        (puthash name entry entries)))
    (let* ((forms (nelisp-bytecode-native-package--read-elc-forms elc-path))
           (expected (nelisp-bytecode-native-package--elc-definitions forms)))
      (unless (nelisp-bytecode-native-package--final-provider-p forms feature)
        (error "bytecode-native-package: .elc is truncated or has no final provider"))
      (maphash
       (lambda (name entry)
         (let* ((compiled (cdr (assq name expected)))
                (input (and compiled
                            (nelisp-bytecode-compiler-input-build compiled)))
                (source-constants
                 (and input (vectorp (plist-get input :constants))
                      (plist-get input :constants)))
                (layout (plist-get entry :hidden-constant-indices))
                (hidden-count (plist-get entry :hidden-constant-count))
                (rest-p (memq :rest-required-count entry))
                (rest-count (plist-get entry :rest-required-count))
                (native-manifest
                 (nelisp-native-load-manifest
                  (expand-file-name (plist-get entry :artifact) directory)))
                (native-entry
                 (cl-find-if
                  (lambda (definition)
                    (equal (plist-get definition :name)
                           (plist-get entry :entry)))
                  (plist-get (plist-get native-manifest :native) :defuns)))
                (transport-arity
                 (if rest-p (1+ rest-count) (plist-get entry :maximum))))
           (unless (and input
                        (= (plist-get entry :minimum)
                           (plist-get input :argument-min))
                        (equal (plist-get entry :maximum)
                               (plist-get input :argument-max))
                        (vectorp source-constants)
                        (vectorp layout) (integerp hidden-count)
                        (= hidden-count (length layout))
                        (or (= hidden-count 0)
                            (= hidden-count (length source-constants)))
                        (equal layout
                               (if (= hidden-count 0) []
                                 (vconcat (number-sequence 0
                                                           (1- hidden-count)))))
                        (= (plist-get native-entry :arity)
                           (+ hidden-count transport-arity))
                        (if (memq :rest-required-count entry)
                            (and (plist-get input :rest-slot-return-template-p)
                                 (= rest-count
                                    (plist-get input :required-argument-count)))
                          (not (plist-get input :rest-slot-return-template-p))))
             (error "bytecode-native-package: .elc descriptor mismatch for %S (%S)"
                    name
                    (list :minimum (plist-get entry :minimum)
                          :input-minimum (and input (plist-get input :argument-min))
                          :maximum (plist-get entry :maximum)
                          :input-maximum (and input (plist-get input :argument-max))
                          :hidden-count hidden-count :layout layout
                          :source-constant-count
                          (and (vectorp source-constants)
                               (length source-constants))
                          :native-arity (and native-entry
                                             (plist-get native-entry :arity))
                          :transport-arity transport-arity
                          :rest-p rest-p)))
           (puthash name entry entries)))
       entries)
      ;; `featurep' gives .elc's ordinary require-once load behavior.
      (unless (featurep feature)
        (nelisp-bytecode-native-package--eval-elc-forms forms))
      (unless (featurep feature)
        (error "bytecode-native-package: .elc did not provide %S" feature))
      (maphash
       (lambda (name entry)
         (unless (fboundp name)
           (error "bytecode-native-package: .elc did not define %S" name))
         (let ((current (symbol-function name))
               (compiled (cdr (assq name expected)))
               (hidden-count (plist-get entry :hidden-constant-count))
               (layout (plist-get entry :hidden-constant-indices)))
           (puthash name current originals)
           (puthash name
                    (nelisp-bytecode-native-package--function-witness current)
                    witnesses)
           (let* ((constants (and (byte-code-function-p current)
                                  (> (length current) 2)
                                  (aref current 2)))
                  (layout-valid
                   (and (vectorp constants) (vectorp layout)
                        (= hidden-count (length layout))
                        (let ((index 0) (valid t))
                          (while (and valid (< index (length layout)))
                            (let ((slot (aref layout index)))
                              (unless (and (integerp slot) (<= 0 slot)
                                           (< slot (length constants)))
                                (setq valid nil)))
                            (setq index (1+ index)))
                          valid)))
                  (same-shape
                   (and (byte-code-function-p current)
                        (byte-code-function-p compiled)
                        (> (length current) 3) (> (length compiled) 3)
                        (equal (aref current 0) (aref compiled 0))
                        (equal (aref current 1) (aref compiled 1))
                        (equal (aref current 3) (aref compiled 3))
                        (vectorp constants)
                        (vectorp (aref compiled 2))
                        (= (length constants) (length (aref compiled 2)))
                        (let ((index 0) (same t))
                          (while (and same (< index (length constants)))
                            (unless (equal (aref constants index)
                                           (aref (aref compiled 2) index))
                              (setq same nil))
                            (setq index (1+ index)))
                          same)))
                  (eligible (and same-shape
                                 (or (= hidden-count 0) layout-valid))))
             ;; Use constant objects from the installed function. ELC is read
             ;; as data before evaluation, so separately read mutable literal
             ;; vectors/conses need not retain EQ identity after `defun'.
             (plist-put entry :hidden-constants
                        (if (and eligible (> hidden-count 0))
                            (vconcat
                             (mapcar (lambda (slot) (aref constants slot))
                                     (append layout nil)))
                          []))
             (puthash name eligible native-eligible))
           (puthash name 0 calls)))
       entries))
    (vector nelisp-bytecode-native-package--marker manifest-path directory
            entries originals native-eligible units calls 'open witnesses)))

(defun nelisp-bytecode-native-package--handle (package)
  (unless (and (vectorp package) (= (length package) 10)
               (eq (aref package 0) nelisp-bytecode-native-package--marker)
               (eq (aref package 8) 'open))
    (error "bytecode-native-package: invalid or closed package"))
  package)

(defun nelisp-bytecode-native-package--function-witness (function)
  "Capture FUNCTION's mutable byte-code fields without copying objects.
The code string and constant-slot vector are shallow copies. Constant objects
remain shared so nested mutable objects preserve their identity."
  (when (and (byte-code-function-p function)
             (> (length function) 3)
             (stringp (aref function 1))
             (vectorp (aref function 2)))
    (vector (if (sequencep (aref function 0))
                (copy-sequence (aref function 0))
              (aref function 0))
            (copy-sequence (aref function 1))
            (copy-sequence (aref function 2))
            (aref function 3))))

(defun nelisp-bytecode-native-package--function-matches-witness-p
    (function witness)
  "Return non-nil when FUNCTION retains WITNESS's descriptor/code/slots/depth."
  (and (vectorp witness) (= (length witness) 4)
       (byte-code-function-p function) (> (length function) 3)
       (equal (aref function 0) (aref witness 0))
       (equal (aref function 1) (aref witness 1))
       (equal (aref function 3) (aref witness 3))
       (let ((current (aref function 2))
             (snapshot (aref witness 2))
             (index 0)
             (same t))
         (unless (and (vectorp current) (vectorp snapshot)
                      (= (length current) (length snapshot)))
           (setq same nil))
         (while (and same (< index (length snapshot)))
           (unless (eq (aref current index) (aref snapshot index))
             (setq same nil))
           (setq index (1+ index)))
         same)))

(defun nelisp-bytecode-native-package-call (package function-name arguments)
  "Call FUNCTION-NAME in PACKAGE with ARGUMENTS, using native code lazily.

If the current function cell was redefined after package open, call that
current Lisp function instead. This preserves redefinition without installing
a wrapper that can recurse into itself."
  (let* ((package (nelisp-bytecode-native-package--handle package))
         (entries (aref package 3))
         (originals (aref package 4))
         (native-eligible (aref package 5))
         (units (aref package 6))
         (calls (aref package 7))
         (witnesses (aref package 9))
         (entry (gethash function-name entries))
         (current (and (fboundp function-name)
                       (symbol-function function-name))))
    (unless (and entry (listp arguments) current)
      (error "bytecode-native-package: unknown function or invalid call: %S"
             function-name))
    ;; Native code is tied to the installed function's byte-code descriptor,
    ;; code bytes, and constant slot identities. Retire it if any of those
    ;; fields changed in place, then follow the current interpreter function.
    (when (and (eq current (gethash function-name originals))
               (gethash function-name native-eligible)
               (not (nelisp-bytecode-native-package--function-matches-witness-p
                     current (gethash function-name witnesses))))
      (let ((unit (gethash function-name units)))
        (when unit
          (nelisp-native-boxed-unit-close unit)
          (remhash function-name units)))
      (puthash function-name nil native-eligible))
    (if (or (not nelisp-bytecode-native-package-native-enabled)
            (not (gethash function-name native-eligible))
            (not (eq current (gethash function-name originals))))
        (apply current arguments)
      (let* ((rest-count (plist-get entry :rest-required-count))
             (rest-p (memq :rest-required-count entry))
             (unit (gethash function-name units)))
        (when (and rest-p
                   (or (not (listp arguments))
                       (< (length arguments) rest-count)))
          (error "bytecode-native-package: expected at least %d argument(s), got %s"
                 rest-count (if (listp arguments) (length arguments) "non-list")))
        (unless unit
          (setq unit
                (if rest-p
                    (nelisp-native-boxed-unit-open-rest
                     (expand-file-name (plist-get entry :artifact) (aref package 2))
                     (plist-get entry :entry)
                     (or (plist-get entry :hidden-constants) [])
                     (plist-get entry :rest-required-count))
                  (nelisp-native-boxed-unit-open-with-constants
                   (expand-file-name (plist-get entry :artifact) (aref package 2))
                   (plist-get entry :entry)
                   (or (plist-get entry :hidden-constants) [])
                   (plist-get entry :maximum) (plist-get entry :minimum))))
          (puthash function-name unit units))
        (puthash function-name (1+ (gethash function-name calls 0)) calls)
        (if (memq :rest-required-count entry)
            (nelisp-native-boxed-unit-call-rest unit arguments)
          (nelisp-native-boxed-unit-call unit arguments))))))

(defun nelisp-bytecode-native-package-native-call-count (package function-name)
  "Return the number of native calls made for FUNCTION-NAME in PACKAGE."
  (let* ((package (nelisp-bytecode-native-package--handle package))
         (entries (aref package 3)))
    (unless (gethash function-name entries)
      (error "bytecode-native-package: unknown function: %S" function-name))
    (gethash function-name (aref package 7) 0)))

(defun nelisp-bytecode-native-package-close (package)
  "Close native units opened by PACKAGE without changing function cells."
  (let* ((package (nelisp-bytecode-native-package--handle package))
         (units (aref package 6)))
    (maphash (lambda (_name unit)
               (nelisp-native-boxed-unit-close unit))
             units)
    (clrhash units)
    (aset package 8 'closed)
    t))

(provide 'nelisp-bytecode-native-package)
;;; nelisp-bytecode-native-package.el ends here
