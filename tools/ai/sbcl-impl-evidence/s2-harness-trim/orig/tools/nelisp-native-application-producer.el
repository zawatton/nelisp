;;; nelisp-native-application-producer.el --- forms-only AOT intake -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Install only a bounded, authenticated subset of pinned lexical definitions
;; into the current disposable NeLisp image. Never load application modules.

;;; Code:
(require 'cl-lib)
(require 'nelisp-cc-fxir-rows)
(require 'nelisp-native-application-protocol)

(defconst nelisp-native-application-producer--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name)))))
(defconst nelisp-native-application-producer--helper-sha256
  "55e2524b8a4ca339a54508c8e5c6f80bfd50b14c5229e937d04ac62effa2b791")
(defconst nelisp-native-application-producer--prelude-sha256
  "0e0b197d8cb588cdc1fb9a5249eeb35dc9cb42125e8cc4b1bb977ea8ba32cb36")

(defun nelisp-native-application-producer--read-one (bytes label)
  "Read exactly one literal data form from BYTES, named by LABEL."
  (let ((forms (nelisp-native-application-protocol-forms bytes)))
    (unless (= (length forms) 1)
      (error "Expected one %s form" label))
    (car forms)))

(defun nelisp-native-application-producer--named-hashes (manifest)
  "Return MANIFEST's accepted and excluded definition hashes by name."
  (let ((table (make-hash-table :test 'eq)))
    (dolist (entry (append (plist-get manifest :definitions)
                           (plist-get manifest :rejections)))
      (let ((name (plist-get entry :name))
            (hash (plist-get entry :form-sha256)))
        (unless (and (symbolp name) (stringp hash)
                     (not (gethash name table)))
          (error "Malformed or duplicate manifest definition"))
        (puthash name hash table)))
    table))

(defun nelisp-native-application-producer--selection-records
    (definitions rejections modules)
  "Validate selected DEFINITION and excluded REJECTION records against MODULES."
  (unless (and (proper-list-p definitions) (proper-list-p rejections))
    (error "Malformed selected or excluded definition records"))
  (let ((records (make-hash-table :test 'eq)) excluded)
    (dolist (pair (append (mapcar (lambda (row) (cons :selected row)) definitions)
                          (mapcar (lambda (row) (cons :excluded row)) rejections)))
      (let* ((kind (car pair)) (row (cdr pair))
             (name (plist-get row :name))
             (source (plist-get row :source))
             (hash (plist-get row :form-sha256)))
        (unless (and (symbolp name) name (not (keywordp name))
                     (member source modules)
                     (stringp hash)
                     (string-match-p "\\`[[:xdigit:]]\\{64\\}\\'" hash)
                     (not (gethash name records))
                     (or (eq kind :selected)
                         (eq (plist-get row :reason) 'excluded-by-request)))
          (error "Malformed, duplicate, or unsupported selection record: %S" name))
        (puthash name row records)
        (when (eq kind :excluded) (push row excluded))))
    (list records (nreverse excluded))))

(defun nelisp-native-application-producer-preflight
    (manifest-file names-file expected-manifest-sha256 expected-names-sha256
                   &optional mode)
  "Validate pinned discovery inputs without evaluating or installing forms.
EXPECTED hashes authenticate the two generated intake artifacts. The result
is only a validated source plan; it is not evidence of producer installation.
The source-form-count counts all selected and excluded defuns, not skipped forms.
MODE nil or :strict preserves strict topform validation; :selected-definitions
admits only individually authenticated defuns and records skipped literals."
  (unless (memq mode '(nil :strict :selected-definitions))
    (error "Unknown application preflight mode: %S" mode))
  (unless (and (stringp expected-manifest-sha256)
               (stringp expected-names-sha256))
    (error "Expected manifest and names hashes are required"))
  (let* ((manifest-path (file-truename manifest-file))
         (names-path (file-truename names-file))
         (manifest-bytes
          (nelisp-native-application-protocol-bytes
           manifest-path nelisp-native-application-protocol-max-file-bytes))
         (names-bytes
          (nelisp-native-application-protocol-bytes
           names-path nelisp-native-application-protocol-max-file-bytes)))
    (when (equal manifest-path names-path)
      (error "Manifest and names paths alias"))
    (unless (and (equal expected-manifest-sha256
                        (secure-hash 'sha256 manifest-bytes))
                 (equal expected-names-sha256
                        (secure-hash 'sha256 names-bytes)))
      (error "Discovery artifact hash mismatch"))
    (let* ((manifest (nelisp-native-application-producer--read-one
                      manifest-bytes "discovery manifest"))
           (names-text (decode-coding-string names-bytes 'utf-8-unix))
           (names (plist-get manifest :names))
           (modules (plist-get manifest :modules))
           (definitions (plist-get manifest :definitions))
           (rejections (plist-get manifest :rejections))
           (expected-lines
            (and (proper-list-p names)
                 (concat (mapconcat #'symbol-name names "\n") "\n")))
           (form-hashes
            (nelisp-native-application-producer--named-hashes manifest))
           (selection (and (eq mode :selected-definitions)
                           (nelisp-native-application-producer--selection-records
                            definitions rejections modules)))
           (selection-records (car-safe selection))
           (excluded (cadr selection))
           (module-bytes 0) (forms-seen (make-hash-table :test 'eq))
           (forms-by-name (make-hash-table :test 'eq))
           skipped)
      (unless (and (eq (plist-get manifest :kind) :application-intake)
                   (= (plist-get manifest :schema) 1)
                   (proper-list-p modules)
                   (= (length modules) (length (delete-dups (copy-tree modules))))
                   (<= 1 (length modules)
                       nelisp-native-application-protocol-max-files)
                   (proper-list-p definitions)
                   (<= 1 (length definitions)
                       nelisp-native-application-protocol-max-definitions)
                   (or (not (eq mode :selected-definitions))
                       (<= (+ (length definitions) (length rejections))
                           nelisp-native-application-protocol-max-definitions))
                   (equal names (mapcar (lambda (d) (plist-get d :name)) definitions))
                   (equal expected-lines names-text))
        (error "Malformed or mismatched discovery manifest and names"))
      (dolist (pin modules)
        (let* ((path (file-truename (plist-get pin :file)))
               (bytes (nelisp-native-application-protocol-bytes
                       path nelisp-native-application-protocol-max-file-bytes))
               (forms (nelisp-native-application-protocol-forms bytes)))
          (when (member path (list manifest-path names-path))
            (error "Discovery output aliases a pinned source"))
          (setq module-bytes (+ module-bytes (string-bytes bytes)))
          (when (> module-bytes nelisp-native-application-protocol-max-total-bytes)
            (error "Application module total-byte bound"))
          (unless (and (equal path (plist-get pin :file))
                       (equal (plist-get pin :sha256)
                              (secure-hash 'sha256 bytes))
                       (nelisp-native-application-protocol-lexical-header-p bytes))
            (error "Stale or unsupported lexical module: %s" path))
          (let ((ordinal 0))
            (dolist (form forms)
              (setq ordinal (1+ ordinal))
              (if (eq (car-safe form) 'defun)
                  (let* ((name (cadr form))
                         (record (if selection
                                     (gethash name selection-records)
                                   (gethash name form-hashes)))
                         (digest (nelisp-native-application-protocol-digest form)))
                    (unless (and (symbolp name) record
                                 (equal (if selection
                                            (plist-get record :source) pin)
                                        pin)
                                 (equal (if selection
                                            (plist-get record :form-sha256) record)
                                        digest))
                      (if selection
                          (error "Unaccounted or changed source defun in %s" path)
                        (error "Unsupported top-level form or changed defun in %s"
                               path)))
                    (when (gethash name (or forms-seen
                                            (setq forms-seen
                                                  (make-hash-table :test 'eq))))
                      (error "Duplicate source definition: %S" name))
                    (puthash name t forms-seen)
                    (puthash name form forms-by-name))
                (if selection
                    (push (list :source pin :ordinal ordinal
                                :form-sha256
                                (nelisp-native-application-protocol-digest form)
                                :head (let ((head (car-safe form)))
                                        (and (symbolp head) (symbol-name head)))
                                :kind (if (consp form) :list :atom))
                          skipped)
                  (error "Unsupported top-level form or changed defun in %s"
                         path)))))))
      (unless (and forms-seen
                   (= (hash-table-count forms-seen) (hash-table-count form-hashes))
                   (or (not selection)
                       (= (hash-table-count forms-seen)
                          (hash-table-count selection-records))))
        (error "Source definitions do not match discovery manifest"))
      (unless (equal (plist-get manifest :baseline-sha256)
                     (nelisp-native-application-protocol-digest
                      (nelisp-cc-fxir-native-all-rows)))
        (error "Admission baseline changed after discovery"))
      (dolist (name names)
        (when (fboundp name)
          (error "Producer provider collision: %S" name)))
      (setq skipped (nreverse skipped))
      (append
       (list :status :validated-inputs
            :manifest manifest :manifest-file manifest-path
            :names-file names-path :names (copy-sequence names)
            :forms (mapcar (lambda (name) (cons name (gethash name forms-by-name))) names)
            :form-hashes (mapcar (lambda (entry)
                                   (cons (plist-get entry :name)
                                         (plist-get entry :form-sha256)))
                                 definitions)
            :source-form-count (hash-table-count forms-seen))
       (when selection
         (list :mode :selected-definitions
               :context-policy :pinned-producer-baseline
               :omitted-definition-names (mapcar (lambda (row) (plist-get row :name))
                                                 excluded)
               :excluded-definitions (copy-tree excluded)
               :skipped-topforms skipped
               :selected-definition-count (length names)
               :excluded-definition-count (length excluded)
               :skipped-topform-count (length skipped)
               :module-initialization-executed nil
               :whole-module-initialization-equivalence nil))))))

(defun nelisp-native-application-producer--supported-form-p (form names)
  "Check selected defun shape and fixed argument declarations only."
  (ignore names)
  (let ((args (nth 2 form)))
    (and (proper-list-p form) (>= (length form) 4)
         (eq (car form) 'defun) (memq (cadr form) names)
         (proper-list-p args) (<= (length args) 64)
         (cl-every (lambda (arg)
                     (and (symbolp arg) arg (not (keywordp arg))
                          (not (string-prefix-p "&" (symbol-name arg)))))
                   args)
         (= (length args) (length (delete-dups (copy-sequence args)))))))

(defun nelisp-native-application-producer--closure-authentic-p
    (closure form &optional constructor)
  "Check CLOSURE's lexical marker, arguments, body and structural reconstruction."
  (and (consp closure) (eq (car closure) 'closure)
       (equal (cadr closure) '(t))
       (equal (nth 2 closure) (nth 2 form))
       (equal (nthcdr 3 closure) (nthcdr 3 form))
       (equal closure
              (funcall (or constructor
                           #'nelisp-native-application-producer--construct)
                       form))))

(defun nelisp-native-application-producer--construct (form)
  "Construct, but never invoke, the lexical closure described by FORM."
  (eval (list 'function
              (cons 'lambda (cons (nth 2 form) (nthcdr 3 form))))
        t))

(defun nelisp-native-application-producer--prepare-candidates
    (plan &optional closure-constructor)
  "Prepare every exact lexical closure from validated PLAN without binding it."
  (let* ((names (plist-get plan :names))
         (forms (plist-get plan :forms))
         (hashes (plist-get plan :form-hashes))
         (constructor (or closure-constructor
                          #'nelisp-native-application-producer--construct))
         candidates)
    (unless (and (eq (plist-get plan :status) :validated-inputs)
                 (proper-list-p names) names (proper-list-p forms)
                 (proper-list-p hashes)
                 (= (length names) (length forms) (length hashes))
                 (= (length names) (length (delete-dups (copy-sequence names))))
                 (equal names (mapcar #'car forms))
                 (equal names (mapcar #'car hashes))
                 (cl-every (lambda (name) (and (symbolp name) name
                                               (not (keywordp name)))) names)
                 (cl-every (lambda (entry) (and (consp entry) (stringp (cdr entry))))
                           hashes))
      (error "Malformed or duplicate preparation plan"))
    (dolist (name names)
      (when (fboundp name) (error "Selected name is already bound: %S" name)))
    (dolist (entry forms)
      (let* ((name (car entry)) (form (cdr entry))
             (args (nth 2 form)) (body (nthcdr 3 form))
             (hash (cdr (assq name hashes))))
        (unless (and (proper-list-p form) (>= (length form) 4)
                     (eq (car form) 'defun) (eq (cadr form) name)
                     (proper-list-p args)
                     (equal hash (nelisp-native-application-protocol-digest form)))
          (error "Selected source metadata is incomplete or stale: %S" name))
        (let ((closure (funcall constructor form)))
          (unless (nelisp-native-application-producer--closure-authentic-p
                   closure form constructor)
            (error "Constructed lexical closure does not match source: %S" name))
          (push (list :name name :form form :args args :body body
                      :form-sha256 hash :closure closure
                      :closure-constructor constructor)
                candidates))))
    (dolist (name names)
      (when (fboundp name) (error "Preparation changed selected binding: %S" name)))
    (nreverse candidates)))

(define-error 'nelisp-native-application-producer-discard-required
  "Producer rollback failed; discard the owned process")

(defun nelisp-native-application-producer--provider-witness (name)
  "Authenticate NAME's actual current callable cell, without resolving aliases."
  (let ((cell (and (fboundp name) (symbol-function name))))
    (unless (and cell (not (symbolp cell)) (functionp cell)
                 (not (macrop name)) (not (autoloadp cell)))
      (error "Unknown, aliased, macro or noncallable provider: %S" name))
    (list :name name :kind (if (subrp cell) :subr :function-cell) :identity cell)))

(defun nelisp-native-application-producer--check-providers (providers)
  "Require every exact callable identity in PROVIDERS to remain current."
  (dolist (provider providers)
    (let ((current (nelisp-native-application-producer--provider-witness
                    (plist-get provider :name))))
      (unless (eq (plist-get current :identity) (plist-get provider :identity))
        (error "Provider context changed: %S" (plist-get provider :name))))))

(defun nelisp-native-application-producer--compiler-admission
    (name args names compiled &optional omitted candidates lookup)
  "Validate actual compiler IR and exact selected or external callee identities."
  (require 'nelisp-cc-fxir-analysis)
  (let ((code (car compiled)) (nlocals (nth 1 compiled)) (ir (nth 2 compiled)))
    (unless (and (vectorp code) (integerp nlocals) (>= nlocals 0) ir
                 (null (nelisp-fxir-problems ir code nlocals)))
      (error "FX compiler returned invalid IR operands: %S" name))
    (let* ((callees (nelisp-fxir-callees ir code))
           (providers
            (delq nil
                  (mapcar
                   (lambda (callee)
                     (unless (and (symbolp callee) callee)
                       (error "Invalid compiler callee: %S" callee))
                     (when (memq callee omitted)
                       (error "Compiler callee is an omitted source definition: %S" callee))
                     (if (memq callee names)
                         (let ((owned (cl-find callee candidates
                                               :key (lambda (c) (plist-get c :name)))))
                           (unless (and owned lookup
                                        (eq (funcall lookup callee)
                                            (plist-get owned :closure)))
                             (error "Selected callee identity changed: %S" callee))
                           nil)
                       (nelisp-native-application-producer--provider-witness callee)))
                   callees)))
           (row (list name 0 nlocals callees ir
                      (list (length args) (length args) -1)))
           (report (nelisp-cc-fxir-analysis-row row code)))
      (unless (plist-get report :valid)
        (error "FX compiler produced invalid analysis row: %S (%S)"
               name (plist-get report :refusals)))
      (when (cl-some (lambda (reason)
                       (memq reason '(:mutable-global-or-alias :dynamic-binding)))
                     (plist-get report :refusals))
        (error "Unsupported mutable or dynamic compiler context: %S" name))
      (nelisp-native-application-producer--check-providers providers)
      (list :callees callees :providers providers :analysis report))))

(defun nelisp-native-application-producer--initialize-fx-helper ()
  "Load the pinned repository helper before checking the FX registration APIs."
  (let* ((root nelisp-native-application-producer--root)
         (helper (expand-file-name "test/nelisp-fxir-eval.el" root))
         (prelude (expand-file-name "scripts/nelisp-stdlib-prelude.el" root))
         (hotpatch (not (boundp 'nelisp--fx-ir-table))))
    (unless (equal (secure-hash 'sha256
                                (nelisp-native-application-protocol-bytes helper 1048576))
                   nelisp-native-application-producer--helper-sha256)
      (error "Pinned FXIR helper source changed"))
    (unless (equal (secure-hash 'sha256
                                (nelisp-native-application-protocol-bytes prelude 8388608))
                   nelisp-native-application-producer--prelude-sha256)
      (error "Pinned FXIR prelude source changed"))
    (let ((default-directory root)
          (process-environment (copy-sequence process-environment)))
      (setenv "NELISP_FXIR_ROOT" root)
      (load helper nil t))
    (unless (and (fboundp 'nelisp--fx-register)
                 (fboundp 'nelisp--fx-env-ok-p)
                 (fboundp 'nelisp-fxir-recompile)
                 (fboundp 'nelisp-fxir-code-same-p)
                 (fboundp 'nelisp-fxir-callees)
                 (boundp 'nelisp--fx-table)
                 (hash-table-p nelisp--fx-table))
      (error "Required FX registration APIs unavailable after helper initialization"))
    (list :helper-sha256 nelisp-native-application-producer--helper-sha256
          :prelude-sha256 nelisp-native-application-producer--prelude-sha256
          :prelude-hotpatched hotpatch
          :root root)))

(defun nelisp-native-application-producer--check-candidates (plan candidates &optional lookup)
  "Authenticate complete precomputed CANDIDATES against PLAN and optional live LOOKUP."
  (unless (and (eq (plist-get plan :status) :validated-inputs)
               (proper-list-p (plist-get plan :names)) (plist-get plan :names)
               (= (length (plist-get plan :names))
                  (length (delete-dups (copy-sequence (plist-get plan :names)))))
               (proper-list-p candidates)
               (equal (mapcar (lambda (c) (plist-get c :name)) candidates)
                      (plist-get plan :names)))
    (error "Candidate set does not match the selected plan"))
  (dolist (candidate candidates)
    (let* ((name (plist-get candidate :name))
           (form (cdr (assq name (plist-get plan :forms))))
           (closure (plist-get candidate :closure)))
      (unless (and (equal form (plist-get candidate :form))
                   (equal (nth 2 form) (plist-get candidate :args))
                   (equal (nthcdr 3 form) (plist-get candidate :body))
                   (equal (plist-get candidate :form-sha256)
                          (cdr (assq name (plist-get plan :form-hashes))))
                   (equal (plist-get candidate :form-sha256)
                          (nelisp-native-application-protocol-digest form))
                   (nelisp-native-application-producer--closure-authentic-p
                    closure form (plist-get candidate :closure-constructor))
                   (or (null lookup) (eq (funcall lookup name) closure)))
        (error "Candidate source or live identity changed: %S" name)))))

(defun nelisp-native-application-producer--compile-candidates (plan candidates &optional lookup)
  "Admit exact prepared CANDIDATES after all their real cells are published.
LOOKUP belongs to the transaction adapter; no closure is reconstructed for compilation."
  (setq lookup (or lookup (plist-get (nelisp-native-application-producer--host-adapter) :lookup)))
  (nelisp-native-application-producer--check-candidates plan candidates lookup)
  (let ((names (plist-get plan :names)) result)
    (dolist (candidate candidates)
      (let* ((name (plist-get candidate :name)) (form (plist-get candidate :form)))
        (unless (and (nelisp-native-application-producer--supported-form-p form names)
                     (fboundp 'nelisp--fx-macros-used)
                     (null (nelisp--fx-macros-used (plist-get candidate :body))))
          (error "Unsupported arity, macro or unknown compiler context: %S" name))
        (let* ((compiled (nelisp-fxir-recompile (plist-get candidate :closure)))
               (admission (and compiled
                               (nelisp-native-application-producer--compiler-admission
                                name (plist-get candidate :args) names compiled
                                (plist-get plan :omitted-definition-names) candidates lookup)))
               (row (copy-sequence candidate)))
          (unless admission (error "FX compiler rejected context: %S" name))
          (setq row (plist-put row :compiler-analysis (plist-get admission :analysis))
                row (plist-put row :compiler-callees (plist-get admission :callees))
                row (plist-put row :provider-witnesses (plist-get admission :providers)))
          (push row result))))
    (nelisp-native-application-producer--check-candidates plan candidates lookup)
    (dolist (candidate result)
      (nelisp-native-application-producer--check-providers
       (plist-get candidate :provider-witnesses)))
    (nreverse result)))

(defun nelisp-native-application-producer--host-adapter ()
  "Return the real function-cell operations used after target runtime checks."
  (list :lookup (lambda (name) (if (fboundp name) (symbol-function name) :unbound))
        :bind (lambda (name closure) (fset name closure))
        :unbind (lambda (name) (when (fboundp name) (fmakunbound name)))))

(defun nelisp-native-application-producer--receipt (plan candidates pins)
  "Assemble the returning receipt inside the transaction, without source reads."
  (let ((result
         (list :status :installed :native-qualified nil
               :names (copy-sequence (plist-get plan :names))
               :manifest-file (plist-get plan :manifest-file)
               :manifest-sha256 (plist-get pins :manifest-sha256)
               :names-file (plist-get plan :names-file)
               :names-sha256 (plist-get pins :names-sha256)
               :source-modules (copy-tree (plist-get (plist-get plan :manifest) :modules))
               :baseline-sha256 (plist-get (plist-get plan :manifest) :baseline-sha256)
               :mode (or (plist-get plan :mode) :strict)
               :context-policy (plist-get plan :context-policy)
               :omitted-definition-names (plist-get plan :omitted-definition-names)
               :skipped-topforms (plist-get plan :skipped-topforms)
               :module-initialization-executed nil
               :whole-module-initialization-equivalence nil
               :helper-sha256 (plist-get pins :helper-sha256)
               :prelude-sha256 (plist-get pins :prelude-sha256)
               :definitions
               (mapcar (lambda (candidate)
                         (list :name (plist-get candidate :name)
                               :source-form-sha256 (plist-get candidate :form-sha256)
                               :lexical-environment '(t) :arguments-authenticated t
                               :body-authenticated t :installed-identity-match t
                               :installed-structure-match t
                               :compiler-analysis (plist-get candidate :compiler-analysis)
                               :compiler-callees (plist-get candidate :compiler-callees)))
                       candidates))))
    (setq result (plist-put result :producer-image-sha256 (plist-get pins :producer-image-sha256)))
    (plist-put result :producer-source-sha256 (plist-get pins :producer-source-sha256))))

(defun nelisp-native-application-producer--rollback (owned adapter failure)
  "Remove all OWNED cells; preserve FAILURE or require owned-process discard."
  (let (cleanup-errors)
    (dolist (name owned)
      (condition-case cleanup
          (progn
            (funcall (plist-get adapter :unbind) name)
            (unless (eq (funcall (plist-get adapter :lookup) name) :unbound)
              (error "Owned cell survived cleanup: %S" name)))
        ((error quit) (push (list name cleanup) cleanup-errors))))
    (if cleanup-errors
        (signal 'nelisp-native-application-producer-discard-required
                (list :original-error failure :cleanup-errors (nreverse cleanup-errors)))
      (signal (car failure) (cdr failure)))))

(defun nelisp-native-application-producer--install-candidates (plan candidates adapter pins)
  "Publish all exact closures, admit and build the receipt in one rollback boundary."
  (nelisp-native-application-producer--check-candidates plan candidates)
  (let ((lookup (plist-get adapter :lookup)) (bind (plist-get adapter :bind))
        owned)
    (dolist (candidate candidates)
      (unless (eq (funcall lookup (plist-get candidate :name)) :unbound)
        (error "Producer provider collision: %S" (plist-get candidate :name))))
    (condition-case failure
        (progn
          (dolist (candidate candidates)
            (let ((name (plist-get candidate :name)))
              ;; Own the fresh name before a bind that may write and then signal.
              (push name owned)
              (funcall bind name (plist-get candidate :closure))))
          (nelisp-native-application-producer--check-candidates plan candidates lookup)
          (let* ((admitted (nelisp-native-application-producer--compile-candidates
                            plan candidates lookup))
                 (receipt (nelisp-native-application-producer--receipt plan admitted pins)))
            (nelisp-native-application-producer--check-candidates plan candidates lookup)
            (nelisp-native-application-producer--check-providers (plist-get pins :context-witnesses))
            (dolist (candidate admitted)
              (nelisp-native-application-producer--check-providers
               (plist-get candidate :provider-witnesses)))
            receipt))
      ((error quit) (nelisp-native-application-producer--rollback owned adapter failure)))))

;; The plan is a deterministic function of the manifest and names bytes, the
;; pinned module bytes, and the image's native-row table.  After helper setup
;; it is enough to prove those inputs unchanged; re-parsing and re-digesting
;; every module (a full second preflight) recomputes the same plan.
(defun nelisp-native-application-producer--recheck-inputs
    (plan expected-manifest-sha256 expected-names-sha256)
  "Signal unless PLAN's inputs are byte-identical and the baseline still holds."
  (let ((manifest (plist-get plan :manifest)))
    (unless (and (equal expected-manifest-sha256
                        (secure-hash 'sha256
                                     (nelisp-native-application-protocol-bytes
                                      (plist-get plan :manifest-file)
                                      nelisp-native-application-protocol-max-file-bytes)))
                 (equal expected-names-sha256
                        (secure-hash 'sha256
                                     (nelisp-native-application-protocol-bytes
                                      (plist-get plan :names-file)
                                      nelisp-native-application-protocol-max-file-bytes))))
      (error "Source context changed during helper setup"))
    (dolist (pin (plist-get manifest :modules))
      (unless (equal (plist-get pin :sha256)
                     (secure-hash 'sha256
                                  (nelisp-native-application-protocol-bytes
                                   (plist-get pin :file)
                                   nelisp-native-application-protocol-max-file-bytes)))
        (error "Source context changed during helper setup")))
    (unless (equal (plist-get manifest :baseline-sha256)
                   (nelisp-native-application-protocol-digest
                    (nelisp-cc-fxir-native-all-rows)))
      (error "Source context changed during helper setup"))
    plan))

(defun nelisp-native-application-producer-install
    (manifest-file names-file expected-manifest-sha256 expected-names-sha256
                   &optional expected-image-sha256 mode supplied-plan)
  "Install exact definitions in a disposable NeLisp image, never invoking their bodies.
MODE is forwarded explicitly to source preflight. The owning process wrapper must
publish the returned receipt and discard its owned process/outputs on later failure.
SUPPLIED-PLAN, when non-nil, is a plan the caller already obtained from
`nelisp-native-application-producer-preflight' in this process with the same
arguments; it replaces the first preflight.  The post-helper preflight below still
re-reads and re-hashes every input and must be `equal' to the plan, so any change
since the supplied plan was computed (including a forged plan) is rejected."
  (let* ((plan (or supplied-plan
                   (nelisp-native-application-producer-preflight
                    manifest-file names-file expected-manifest-sha256 expected-names-sha256 mode)))
         (image (getenv "NELISP_PRODUCER_IMAGE_SHA256")))
    (unless (and (stringp expected-image-sha256)
                 (string-match-p "\\`[[:xdigit:]]\\{64\\}\\'" expected-image-sha256)
                 (equal image expected-image-sha256))
      (error "Trusted launcher image pin missing or mismatched"))
    (when (and (fboundp 'emacs-version)
               (string-match-p "\\`GNU Emacs\\b" (emacs-version)))
      (error "Production installation refuses a GNU Emacs host"))
    (unless (and (boundp 'nelisp-version) (stringp nelisp-version))
      (error "Production installation requires the NeLisp runtime marker"))
    (let* ((pins (nelisp-native-application-producer--initialize-fx-helper))
           (_analysis (require 'nelisp-cc-fxir-analysis))
           (_inputs (nelisp-native-application-producer--recheck-inputs
                     plan expected-manifest-sha256 expected-names-sha256)))
      (setq pins (plist-put pins :producer-image-sha256 image)
            pins (plist-put pins :manifest-sha256 expected-manifest-sha256)
            pins (plist-put pins :names-sha256 expected-names-sha256)
            pins (plist-put pins :producer-source-sha256
                            (secure-hash 'sha256
                                         (nelisp-native-application-protocol-bytes
                                          (expand-file-name "tools/nelisp-native-application-producer.el"
                                                            nelisp-native-application-producer--root)
                                          1048576)))
            pins (plist-put pins :context-witnesses
                            (mapcar #'nelisp-native-application-producer--provider-witness
                                    '(nelisp-fxir-recompile nelisp-fxir-problems nelisp-fxir-callees
                                      nelisp--fx-macros-used nelisp-cc-fxir-analysis-row))))
      (let ((candidates (nelisp-native-application-producer--prepare-candidates plan))
            (adapter (nelisp-native-application-producer--host-adapter)))
        (dolist (name (plist-get plan :names))
          (when (fboundp name) (error "Provider collision after helper setup: %S" name)))
        (nelisp-native-application-producer--install-candidates plan candidates adapter pins)))))

(provide 'nelisp-native-application-producer)
;;; nelisp-native-application-producer.el ends here
