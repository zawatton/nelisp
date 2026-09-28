;;; nelisp-vendor-bytecode-jit-coverage.el --- Measure vendor JIT coverage -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Load a small, pinned set of GNU Emacs 31.1 compiler sources as source,
;; compile selected vendor function objects, and classify JIT IR coverage.
;; This probe never calls the selected functions.

;;; Code:

(require 'cl-lib)
(require 'json)

(defconst nelisp-vendor-bytecode-jit-coverage--functions
  '((macroexp . (macroexp--all-forms macroexpand-1 macroexp-parse-body))
    (cconv . (cconv-closure-convert cconv--convert-function cconv--set-diff))
    (bytecomp . (byte-compile-lambda byte-compile-form
                 byte-compile-make-closure byte-compile-if byte-compile-setq
                 byte-compile-funcall byte-compile-constant)))
  "Reproducible, bounded vendor function sample.")

(defconst nelisp-vendor-bytecode-jit-coverage--small-functions
  '(subr . (zerop caar cadr fixnump bignump frame-configuration-p))
  "Small GNU subr.el predicates/accessors, kept separate from compiler tier.")

(defconst nelisp-vendor-bytecode-jit-coverage--emacs-version "31.1")

(defconst nelisp-vendor-bytecode-jit-coverage--source-pins
  '((macroexp . "10df0fe326e6f436a3ec3a74fdaa79b06272e5a35556383d8e2741ce3c16a0c2")
    (cconv . "1639c5812c18837332ad90f3d7f835e7abeab1af22f49f2c391c813e74ab03cc")
    (bytecomp . "094fa608bed9d9feffd4364b8df3288c1eb2bf1efd5dfa57fcd6d9bd13cdd099")))

(defconst nelisp-vendor-bytecode-jit-coverage--bytecode-pins
  '((macroexp--all-forms . "d5fb6cb4da360407ecc92efe907c953ce8424569af083474d9e64a59af806ad4")
    (macroexpand-1 . "f563e04d4c970a1aaa33e83bdd422e83380833522ed31458c4f6e14b5ca6a640")
    (macroexp-parse-body . "103af4e221c460a5f65f4982988dd0a9c91de8e9f76740557fb7e58f76267216")
    (cconv-closure-convert . "2bf05487ce4ea7e369cd833a99b7c0034697f1ec58ec071ebffb274c2e839eb2")
    (cconv--convert-function . "67b97718daa01a4069830e118d5274f85e097536b8a21b7e55093e6081a69fd5")
    (cconv--set-diff . "3cfeb5df8e38ae1fdbe99a9789a5131202ac3802fe4603c8e6444c0c28947caa")
    (byte-compile-lambda . "eecd7d8d5431d1b1563da422fdc50338fde8463b406946c142e2da7ba2d6c790")
    (byte-compile-form . "34d502a53616cbff0d4985be5e5e1a409f54081aea5f97a311971fd59163727d")
    (byte-compile-make-closure . "bc887f86534ddbc8cff28ac88dece4e3e4b6ed41d9907875b222b1af24b05d59")
    (byte-compile-if . "3655c1dddb2d8f057d2c5952979cbbb9ec097ff9947620fd01eb7b9aee316c50")
    (byte-compile-setq . "1e405076254a6d7632dc6e3e330969a3ac88e637dbcd5c60c04892f8f5916c54")
    (byte-compile-funcall . "87441123a4ac5583fb7945384fbd569efa0c032d18d80e772a2f91ac2580e3f1")
    (byte-compile-constant . "6cf7ba2b4bc2ad59ae49667d6c2a962336ca917f763d37070df687b0789fd66d")
    (zerop . "6683de2c492f3752bd51b517569fc85085c6faabd98e8ec9efbfc331cc7aeafb")
    (caar . "54657f49c4d902c5a7c19d4a30e977cb6c49f11c8add17ae6ab0166f845f2643")
    (cadr . "122913e0b5f7d30c803c78773dc279f3c053af5f5202cf8562c14d2148ad0c78")
    (fixnump . "102e639e742351efbc457d8517db951ead880750dbc9eb1b96409f5fe063762d")
    (bignump . "28ab0a68fdfdb2ad17665d5e25c15fca14b03ac24a1a3beee8a5f0f6228b71cc")
    (frame-configuration-p . "3d94ac76fce88b09b2f4ea29b815040031990e1e9ec6103072c78373952473f0")))

(defconst nelisp-vendor-bytecode-jit-coverage--repository-root
  (file-name-directory
   (directory-file-name (file-name-directory load-file-name))))

(defun nelisp-vendor-bytecode-jit-coverage--root ()
  nelisp-vendor-bytecode-jit-coverage--repository-root)

(defun nelisp-vendor-bytecode-jit-coverage--source-root ()
  (or (getenv "NELISP_VENDOR_ROOT")
      (expand-file-name "vendor/emacs-lisp/emacs-lisp"
                        (nelisp-vendor-bytecode-jit-coverage--root))))

(defun nelisp-vendor-bytecode-jit-coverage--bytecode-fingerprint (function)
  "Return a stable SHA-256 fingerprint for byte-code FUNCTION's payload."
  (secure-hash 'sha256
               (prin1-to-string (list (aref function 0) (aref function 1)
                                      (aref function 2) (aref function 3)))))

(defun nelisp-vendor-bytecode-jit-coverage--verify-fingerprint (name function)
  "Signal if NAME's compiled FUNCTION differs from the pinned byte-code."
  (let ((expected (cdr (assq name
                             nelisp-vendor-bytecode-jit-coverage--bytecode-pins)))
        (actual (nelisp-vendor-bytecode-jit-coverage--bytecode-fingerprint function)))
    (unless (and expected (equal actual expected))
      (error "Pinned byte-code fingerprint mismatch for %s: %s (expected %s)"
             name actual expected))
    actual))

(defun nelisp-vendor-bytecode-jit-coverage--source-fingerprint (file)
  "Return FILE's SHA-256 fingerprint without decoding its contents."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (current-buffer))))

(defun nelisp-vendor-bytecode-jit-coverage--verify-sources ()
  "Check GNU version and all pinned source files, returning source rows."
  (unless (equal emacs-version nelisp-vendor-bytecode-jit-coverage--emacs-version)
    (error "Pinned corpus requires GNU Emacs %s; found %s"
           nelisp-vendor-bytecode-jit-coverage--emacs-version emacs-version))
  (let ((root (nelisp-vendor-bytecode-jit-coverage--source-root)) rows)
    (dolist (pin nelisp-vendor-bytecode-jit-coverage--source-pins)
      (let* ((name (car pin))
             (file (expand-file-name (concat (symbol-name name) ".el") root))
             (actual (nelisp-vendor-bytecode-jit-coverage--source-fingerprint file)))
        (unless (equal actual (cdr pin))
          (error "Pinned vendor source fingerprint mismatch for %s: %s"
                 name actual))
        (push (cons name actual) rows)))
    (require 'nelisp-vendor-source)
    (nelisp-vendor-source-form "vendor/staged-emacs-lisp/subr.el" 'zerop)
    (push (cons 'subr
                (nelisp-vendor-bytecode-jit-coverage--source-fingerprint
                 (expand-file-name "vendor/staged-emacs-lisp/subr.el"
                                   nelisp-vendor-source--root)))
          rows)
    (nreverse rows)))

(defun nelisp-vendor-bytecode-jit-coverage--blocker (function)
  "Classify FUNCTION's first structural, semantic, or JIT lowering blocker."
  (let* ((code (aref function 1))
         (constants (aref function 2))
         (entry-depth (ash (aref function 0) -8))
         (validation (nelisp-bytecode-ir-validate code constants entry-depth))
         (rows (plist-get validation :instructions))
         (bad-row (cl-loop for row across rows
                           when (not (plist-get (aref row 4) :lowerable))
                           return row))
         (semantic-opcodes
          (delete-dups
           (cl-loop for row across rows
                    unless (plist-get (aref row 4) :lowerable)
                    collect (aref row 1))))
         (reason (plist-get validation :reason))
         (offset (or (and (stringp reason)
                          (string-match " at \\([0-9]+\\)" reason)
                          (string-to-number (match-string 1 reason)))
                     (and (eq (plist-get validation :status) 'malformed)
                          (if (> (length rows) 0)
                              (aref (aref rows (1- (length rows))) 2)
                            0))))
         (opcode (and (integerp offset) (< offset (length code))
                      (aref code offset)))
         (decoded (nelisp-bytecode-jit--decode-instructions code constants)))
    (cond
     ((eq (plist-get validation :status) 'malformed)
      (append (list :stage 'structural :reason 'malformed-bytecode
                    :detail reason)
              (when (integerp offset) (list :offset offset :opcode opcode))))
     (bad-row
      (list :stage 'semantics :reason 'unsupported-semantics
            :offset (aref bad-row 0) :opcode (aref bad-row 1)
            :semantic-opcodes semantic-opcodes))
     ((not decoded)
      (list :stage 'jit-lowering :reason 'unsupported-by-jit-lowering
            :semantic-opcodes nil))
     (t (list :stage 'jit-lowering :reason 'unsupported-arity-stack-or-shape
              :semantic-opcodes nil)))))

(defun nelisp-vendor-bytecode-jit-coverage--classify (function)
  "Return generic-IR blockers plus current JIT eligibility for FUNCTION."
  (let* ((validation
          (nelisp-bytecode-ir-validate
           (aref function 1) (aref function 2) (ash (aref function 0) -8)))
         (generic-status (plist-get validation :status))
         (unsupported (car (plist-get validation :unsupported)))
         (blocker (nelisp-vendor-bytecode-jit-coverage--blocker function))
         (ir (nelisp-bytecode-jit--decode-ir function))
         (legacy-decoded
          (nelisp-bytecode-jit--decode-instructions
           (aref function 1) (aref function 2)))
         (result (list :generic-ir-status generic-status
                       :semantic-opcodes (plist-get blocker :semantic-opcodes)
                       :first-ir-rejection-reason
                       (and (memq generic-status '(unsupported malformed))
                            (if (and (eq generic-status 'unsupported) unsupported)
                                (symbol-name (cdr unsupported))
                              (plist-get validation :reason)))
                       :generic-ir-decoded (and ir t)
                       :legacy-instruction-decoder-decoded (and legacy-decoded t))))
    (when (and (eq generic-status 'malformed)
               (plist-get blocker :opcode))
      (setq result (append result (list :first-opcode (plist-get blocker :opcode)
                                        :first-offset (plist-get blocker :offset)))))
    (when (eq (plist-get blocker :stage) 'semantics)
      (setq result (append result
                           (list :first-opcode (plist-get blocker :opcode)
                                 :first-offset (plist-get blocker :offset)))))
    (setq result (append result
                         (list :rejection-stage (plist-get blocker :stage)
                               :reason (plist-get blocker :reason))))
    (if ir
        (let* ((arity (plist-get ir :arity))
               (arguments (make-list arity 1))
               (environment
                (cl-loop for index below arity
                         collect (cons (intern (format "x%d" index)) 1)))
               (value (nelisp-bytecode-jit--ir-safe-form-p
                       (plist-get ir :expression) environment))
               (guard-ok (and (nelisp-bytecode-jit--safe-ir-p ir arguments)
                              value)))
          (setq result
                (append result
                        (list :guard-accepts-fixnum-probe (and guard-ok t)
                              :static-jit-eligible-for-fixnum-inputs (and guard-ok t)
                              :arity arity
                              :opcodes (delete-dups
                                        (mapcar (lambda (instruction)
                                                  (aref instruction 1))
                                                (plist-get ir :instructions))))))
          (when guard-ok
            (setq result (plist-put result :rejection-stage 'static-jit-eligible))
            (setq result (plist-put result :reason 'static-jit-eligible-for-fixnum-inputs)))
          result)
      (append result (list :guard-accepts-fixnum-probe nil
                           :static-jit-eligible-for-fixnum-inputs nil)))))

(defun nelisp-vendor-bytecode-jit-coverage--load-sources ()
  (let ((source-root (nelisp-vendor-bytecode-jit-coverage--source-root)))
    (dolist (name '(macroexp cconv bytecomp))
      (let ((file (expand-file-name (concat (symbol-name name) ".el")
                                    source-root)))
        (unless (file-readable-p file)
          (error "Missing pinned vendor source: %s" file))
        (load-file file)))))

(defun nelisp-vendor-bytecode-jit-coverage--source-function-object (name)
  "Compile exact pinned source form for small-tier function NAME."
  (require 'nelisp-vendor-source)
  (let* ((form (read (nelisp-vendor-source-form
                      "vendor/staged-emacs-lisp/subr.el" name)))
         (lambda-form (cons 'lambda (cddr form))))
    (unless (eq (car form) 'defun)
      (error "Expected source defun for %s" name))
    (byte-compile lambda-form)))

(defun nelisp-vendor-bytecode-jit-coverage--opcode-marginals (rows)
  "Summarize first blockers and incremental unlocks in ROWS.
Marginals simulate adding semantic support in ascending opcode order."
  (let ((opcodes
         (sort (delete-dups
                (cl-loop for row in rows
                         append (copy-sequence
                                 (plist-get row :semantic-opcodes))))
               #'<)))
    (let ((enabled nil) (already-unlocked nil) result)
      (dolist (opcode opcodes)
        (push opcode enabled)
        (let ((first nil) (unlocked nil) (single-unlocked nil))
          (dolist (row rows)
            (when (and (eq (plist-get row :rejection-stage) 'semantics)
                       (equal (plist-get row :first-opcode) opcode))
              (push (plist-get row :function) first))
            (when (and (eq (plist-get row :rejection-stage) 'semantics)
                       (not (memq (plist-get row :function) already-unlocked))
                       (cl-every (lambda (blocked) (memq blocked enabled))
                                 (plist-get row :semantic-opcodes)))
              (push (plist-get row :function) unlocked))
            (when (and (eq (plist-get row :rejection-stage) 'semantics)
                       (equal (plist-get row :semantic-opcodes) (list opcode)))
              (push (plist-get row :function) single-unlocked)))
          (setq first (nreverse first)
                unlocked (nreverse unlocked)
                single-unlocked (nreverse single-unlocked)
                already-unlocked (append already-unlocked unlocked))
          (when (or first unlocked single-unlocked)
            (push (list :opcode opcode
                        :first-blocker-functions first
                        :first-blocker-count (length first)
                        :unlocked-functions unlocked
                        :marginal-unlocked-count (length unlocked)
                        :single-opcode-unlocked-functions single-unlocked
                        :single-opcode-unlocked-count (length single-unlocked))
                  result))))
      (nreverse result))))

(defun nelisp-vendor-bytecode-jit-coverage-report ()
  "Return pinned function classifications and opcode marginal counts."
  (let* ((root (nelisp-vendor-bytecode-jit-coverage--root))
         (jit-file (or (getenv "NELISP_BYTECODE_JIT_SOURCE")
                       (expand-file-name "lisp/nelisp-bytecode-jit.el" root)))
         (load-path (cons (expand-file-name "lisp" root) load-path))
         (sources (nelisp-vendor-bytecode-jit-coverage--verify-sources))
         rows)
    (nelisp-vendor-bytecode-jit-coverage--load-sources)
    (unless (file-readable-p jit-file)
      (error "Missing JIT source: %s" jit-file))
    (load-file jit-file)
    (dolist (spec (append
                   (mapcar (lambda (group)
                             (list 'compiler group
                                   (concat "vendor/emacs-lisp/emacs-lisp/"
                                           (symbol-name (car group)) ".el")))
                           nelisp-vendor-bytecode-jit-coverage--functions)
                   (list (list 'small
                               nelisp-vendor-bytecode-jit-coverage--small-functions
                               "vendor/staged-emacs-lisp/subr.el"))))
      (let ((tier (nth 0 spec)) (group (nth 1 spec)) (source (nth 2 spec)))
        (dolist (name (cdr group))
          (let* ((object (if (eq tier 'small)
                             (nelisp-vendor-bytecode-jit-coverage--source-function-object name)
                           (progn
                             (unless (fboundp name)
                               (error "Missing pinned function definition: %s" name))
                             (byte-compile (symbol-function name)))))
                 (fingerprint
                  (nelisp-vendor-bytecode-jit-coverage--verify-fingerprint
                   name object))
                 (classification
                  (nelisp-vendor-bytecode-jit-coverage--classify object)))
            (push (append (list :tier tier :source source :function name
                                :bytecode-sha256 fingerprint)
                          classification)
                  rows)))))
    (setq rows (nreverse rows))
    (list :schema "nelisp-vendor-bytecode-jit-coverage-v3"
          :emacs-version emacs-version
          :source-fingerprints sources
          :selected (length rows)
          :generic-ir-valid
          (cl-count 'valid rows :key (lambda (row)
                                       (plist-get row :generic-ir-status)))
          :generic-ir-semantic-blocked
          (cl-count 'semantics rows :key (lambda (row)
                                          (plist-get row :rejection-stage)))
          :structural-blocked
          (cl-count 'structural rows :key (lambda (row)
                                            (plist-get row :rejection-stage)))
          :generic-ir-decoded (cl-count t rows :key (lambda (row)
                                                     (plist-get row :generic-ir-decoded)))
          :legacy-instruction-decode-success
          (cl-count t rows :key (lambda (row)
                                  (plist-get row :legacy-instruction-decoder-decoded)))
          :static-jit-eligible-for-fixnum-inputs
          (cl-count t rows :key (lambda (row)
                                  (plist-get row :static-jit-eligible-for-fixnum-inputs)))
          :smallest-blocker-sets
          (let ((blocked (cl-remove-if-not
                          (lambda (row) (plist-get row :semantic-opcodes))
                          (copy-sequence rows))))
            (cl-subseq
             (cl-stable-sort blocked
                             (lambda (a b)
                               (let ((na (length (plist-get a :semantic-opcodes)))
                                     (nb (length (plist-get b :semantic-opcodes))))
                                 (if (= na nb)
                                     (string< (symbol-name (plist-get a :function))
                                              (symbol-name (plist-get b :function)))
                                   (< na nb)))))
             0 (min 10 (length blocked))))
          :functions rows
          :opcode-marginals
          (nelisp-vendor-bytecode-jit-coverage--opcode-marginals rows))))

(defun nelisp-vendor-bytecode-jit-coverage--json-object (report)
  "Convert REPORT's internal plists into a compact JSON object."
  (let ((functions
         (mapcar
          (lambda (row)
            `((tier . ,(symbol-name (plist-get row :tier)))
              (source . ,(plist-get row :source))
              (function . ,(symbol-name (plist-get row :function)))
              (bytecode_sha256 . ,(plist-get row :bytecode-sha256))
              (generic_ir_status . ,(symbol-name (plist-get row :generic-ir-status)))
              (rejection_stage . ,(symbol-name (plist-get row :rejection-stage)))
              (first_rejected_opcode . ,(or (plist-get row :first-opcode) :null))
              (first_rejected_offset . ,(or (plist-get row :first-offset) :null))
              (first_ir_rejection_reason
               . ,(or (plist-get row :first-ir-rejection-reason) :null))
              (semantic_blocker_opcodes . ,(vconcat (plist-get row :semantic-opcodes)))
              (generic_ir_decoded
               . ,(if (plist-get row :generic-ir-decoded) t :json-false))
              (legacy_instruction_decoder_decoded
               . ,(if (plist-get row :legacy-instruction-decoder-decoded) t :json-false))
              (static_jit_eligible_for_fixnum_inputs
               . ,(if (plist-get row :static-jit-eligible-for-fixnum-inputs) t :json-false))))
          (plist-get report :functions)))
        (sources
         (mapcar (lambda (row)
                   `((file . ,(if (eq (car row) 'subr)
                                  "vendor/staged-emacs-lisp/subr.el"
                                (concat "vendor/emacs-lisp/emacs-lisp/"
                                        (symbol-name (car row)) ".el")))
                     (sha256 . ,(cdr row))))
                 (plist-get report :source-fingerprints)))
        (marginals
         (mapcar (lambda (row)
                   `((opcode . ,(plist-get row :opcode))
                     (first_blocker_functions
                      . ,(vconcat (mapcar #'symbol-name
                                          (plist-get row :first-blocker-functions))))
                     (first_blocker_count . ,(plist-get row :first-blocker-count))
                     (marginal_unlocked_functions
                      . ,(vconcat (mapcar #'symbol-name
                                          (plist-get row :unlocked-functions))))
                     (marginal_unlocked_count
                      . ,(plist-get row :marginal-unlocked-count))
                     (single_opcode_unlocked_functions
                      . ,(vconcat (mapcar #'symbol-name
                                          (plist-get row :single-opcode-unlocked-functions))))
                     (single_opcode_unlocked_count
                      . ,(plist-get row :single-opcode-unlocked-count))))
                 (plist-get report :opcode-marginals))))
    `((schema . ,(plist-get report :schema))
      (emacs_version . ,(plist-get report :emacs-version))
      (selected . ,(plist-get report :selected))
      (generic_ir_valid . ,(plist-get report :generic-ir-valid))
      (generic_ir_semantic_blocked . ,(plist-get report :generic-ir-semantic-blocked))
      (structural_blocked . ,(plist-get report :structural-blocked))
      (generic_ir_decoded . ,(plist-get report :generic-ir-decoded))
      (legacy_instruction_decode_success
       . ,(plist-get report :legacy-instruction-decode-success))
      (marginal_order . "ascending bytecode opcode")
      (marginal_definition
       . "new generic-IR-valid functions after enabling each opcode in order")
      (single_opcode_definition
       . "baseline functions whose only unsupported semantic opcode is this opcode")
      (native_execution_measured . :json-false)
      (generic_ir_decode_definition . "success of nelisp-bytecode-jit--decode-ir")
      (legacy_instruction_decoder_definition
       . "success of nelisp-bytecode-jit--decode-instructions; this is a separate legacy decode pass")
      (static_jit_eligibility_definition
       . "generic IR and safe IR evaluator accept all-fixnum inputs; output type is unrestricted; this is not native execution")
      (static_jit_eligible_for_fixnum_inputs
       . ,(plist-get report :static-jit-eligible-for-fixnum-inputs))
      (tier_summary
       . ,(vconcat
           (mapcar (lambda (tier)
                     `((tier . ,(symbol-name tier))
                       (selected . ,(cl-count tier (plist-get report :functions)
                                              :key (lambda (row) (plist-get row :tier))))
                       (generic_ir_valid . ,(cl-count-if
                                             (lambda (row) (and (eq tier (plist-get row :tier))
                                                                (eq 'valid (plist-get row :generic-ir-status))))
                                             (plist-get report :functions)))
                       (semantic_blocked . ,(cl-count-if
                                             (lambda (row) (and (eq tier (plist-get row :tier))
                                                                (eq 'semantics (plist-get row :rejection-stage))))
                                             (plist-get report :functions)))
                       (static_jit_eligible_for_fixnum_inputs . ,(cl-count-if
                                                          (lambda (row) (and (eq tier (plist-get row :tier))
                                                                             (plist-get row :static-jit-eligible-for-fixnum-inputs)))
                                                          (plist-get report :functions)))))
                   '(compiler small))))
      (smallest_blocker_sets
       . ,(vconcat
           (mapcar (lambda (row)
                     `((tier . ,(symbol-name (plist-get row :tier)))
                       (function . ,(symbol-name (plist-get row :function)))
                       (opcode_count . ,(length (plist-get row :semantic-opcodes)))
                       (opcodes . ,(vconcat (plist-get row :semantic-opcodes)))))
                   (plist-get report :smallest-blocker-sets))))
      (sources . ,(vconcat sources))
      (functions . ,(vconcat functions))
      (opcode_marginals . ,(vconcat marginals)))))

(defun nelisp-vendor-bytecode-jit-coverage-run ()
  "Write the deterministic JSON report; execute no sample function."
  (let* ((json (json-serialize
                (nelisp-vendor-bytecode-jit-coverage--json-object
                 (nelisp-vendor-bytecode-jit-coverage-report))
                :null-object :null :false-object :json-false))
         (file (expand-file-name
                "tools/nelisp-vendor-bytecode-jit-coverage.json"
                (nelisp-vendor-bytecode-jit-coverage--root))))
    (with-temp-file file (insert json "\n"))
    (princ json)
    (terpri)
    file))

(provide 'nelisp-vendor-bytecode-jit-coverage)
;;; nelisp-vendor-bytecode-jit-coverage.el ends here
