;;; nelisp-vendor-bytecode-triparity.el --- Host/VM/JIT parity runner -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Run a bounded set of byte-compiled GNU Emacs 31.1 vendor functions in
;; separate Host, standalone VM, and standalone JIT subprocesses.

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'nelisp-vendor-source)
(require 'nelisp-bytecode-corpus-parity)
(require 'nelisp-vendor-bytecode-jit-coverage)

(defconst nelisp-vendor-bytecode-triparity--zerop-bytecode-sha256
  "55b766b78af0a19a0dfea8ba55cf5536a6fe8b9d5ef135e9935c3e74fbfba7c8"
  "Expected fingerprint of the GNU Emacs 31.1 vendor `zerop' bytecode.")

(defconst nelisp-vendor-bytecode-triparity--cadr-bytecode-sha256
  "c6c15a50ceb4415464aa59e1039a615620c69efc13ecc001f8485d6fd89ba13f"
  "Expected fingerprint of the GNU Emacs 31.1 vendor `cadr' bytecode.")

(defconst nelisp-vendor-bytecode-triparity--fixtures
  '((macroexp--all-forms ((+ 1 2)) nil)
    (macroexpand-1 triparity-not-a-macro)
    (macroexp-parse-body ((declare (ignore x)) (+ x 1) "doc"))
    (cconv-closure-convert (lambda (x) (+ x 1)))
    (cconv--convert-function (x) ((+ x 1)) nil nil)
    (cconv--set-diff (a b) (b))
    (byte-compile-lambda (lambda () 17))
    (byte-compile-form (+ 1 2))
    (byte-compile-make-closure (lambda (x) (+ x 1)))
    (byte-compile-if (if t 1 2))
    (byte-compile-setq (setq triparity-x 1))
    (byte-compile-funcall (funcall #'identity 1))
    (byte-compile-constant 1)
    (caar (((triparity-cons tail) inner) outer))
    (cadr (head middle tail))
    (fixnump 42)
    (bignump 42)
    (frame-configuration-p nil)
    (zerop 0))
  "Fixed, bounded argument forms for the pinned vendor sample.")

(defconst nelisp-vendor-bytecode-triparity--coverage-bytecode-pins
  '((zerop . "6683de2c492f3752bd51b517569fc85085c6faabd98e8ec9efbfc331cc7aeafb")
    (caar . "54657f49c4d902c5a7c19d4a30e977cb6c49f11c8add17ae6ab0166f845f2643")
    (cadr . "122913e0b5f7d30c803c78773dc279f3c053af5f5202cf8562c14d2148ad0c78")
    (fixnump . "102e639e742351efbc457d8517db951ead880750dbc9eb1b96409f5fe063762d")
    (bignump . "28ab0a68fdfdb2ad17665d5e25c15fca14b03ac24a1a3beee8a5f0f6228b71cc")
    (frame-configuration-p . "3d94ac76fce88b09b2f4ea29b815040031990e1e9ec6103072c78373952473f0"))
  "GNU Emacs 31.1 coverage corpus fingerprints (raw byte-code payload format).")

(defconst nelisp-vendor-bytecode-triparity--native-expected
  '((triparity-add1 . 1) (zerop . 1) (caar . 1) (cadr . 1) (fixnump . 1))
  "Measured native-call counts for the focused scalar and accessor cases.")

(defconst nelisp-vendor-bytecode-triparity--repository-root
  (file-name-directory
   (directory-file-name (file-name-directory load-file-name))))

(defun nelisp-vendor-bytecode-triparity--root ()
  nelisp-vendor-bytecode-triparity--repository-root)

(defun nelisp-vendor-bytecode-triparity--read-result (file)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-max))
    (when (bolp) (forward-line -1))
    (beginning-of-line)
    (read (current-buffer))))

(defun nelisp-vendor-bytecode-triparity--code-expression (code)
  "Encode byte string CODE using the reader-safe unibyte-string constructor."
  (concat "(unibyte-string"
          (mapconcat (lambda (byte) (format " %d" byte))
                     (append code nil) "") ")"))

(defun nelisp-vendor-bytecode-triparity--argument-expressions (arguments)
  (mapconcat (lambda (arg)
               (concat "'" (nelisp-bytecode-corpus--print arg)))
             arguments " "))

(defun nelisp-vendor-bytecode-triparity--process-failure (status stderr-file)
  (let ((detail (with-temp-buffer
                  (insert-file-contents stderr-file)
                  (buffer-string))))
    (list :status (if (equal status 124) "not-executable" "process-error")
          :reason (cond ((equal status 124) "timeout")
                        ((string-match-p "invalid-read-syntax" detail)
                         "unencodable-constant")
                        ((string-match-p "void-function:" detail)
                         "missing-function")
                        (t "process-error"))
          :detail (substring detail 0 (min 240 (length detail))))))

(defun nelisp-vendor-bytecode-triparity--run-child (program args &optional timeout-seconds)
  (let ((out (make-temp-file "nelisp-triparity-out-"))
        (err (make-temp-file "nelisp-triparity-err-"))
        (started (float-time)))
    (unwind-protect
        (let ((status (apply #'process-file "timeout" nil
                             (list (list :file out) err) nil
                             (number-to-string (or timeout-seconds 15))
                             program args)))
          (let ((result
                 (if (and (integerp status) (= status 0))
                     (condition-case e
                         (nelisp-vendor-bytecode-triparity--read-result out)
                       (error (list :status "process-error" :reason "unreadable-result"
                                    :detail (format "%s" e))))
                   (nelisp-vendor-bytecode-triparity--process-failure status err))))
            (when (plist-member result :native_after)
              (setq result (plist-put result :native
                                      (plist-get result :native_after))))
            (when (plist-member result :fallback_after)
              (setq result (plist-put result :fallback
                                      (plist-get result :fallback_after))))
            (plist-put result :process_elapsed_ms
                       (* 1000.0 (- (float-time) started)))))
      (delete-file out) (delete-file err))))

(defun nelisp-vendor-bytecode-triparity--child-form
    (object arguments lane &optional sources)
  "Return an eval form constructing OBJECT via make-byte-code and calling it."
  (let* ((vendor-root (nelisp-vendor-bytecode-jit-coverage--source-root))
         (setup (append
                 (mapcar (lambda (file)
                           `(load-file ,(expand-file-name file vendor-root)))
                         (if (eq sources :none)
                             nil
                           (or sources '("macroexp.el" "cconv.el"))))
                 (when (eq lane 'jit)
                   `((load ,(or (getenv "NELISP_BYTECODE_JIT_SOURCE")
                                (expand-file-name
                                 "lisp/nelisp-bytecode-jit.el"
                                 (nelisp-vendor-bytecode-triparity--root)))
                           nil nil t)
                     (setq nelisp-bytecode-jit-threshold 1)))))
         (descriptor (aref object 0))
         (code-form (read (nelisp-vendor-bytecode-triparity--code-expression
                           (aref object 1))))
         (constants (aref object 2))
         (depth (aref object 3))
         (call `(funcall fn
                         ,@(mapcar (lambda (arg)
                                     (read (concat "'"
                                                   (nelisp-bytecode-corpus--print arg))))
                                   arguments)))
         (counter-form
          (lambda (variable)
            `(if (boundp ',variable) ,variable 0)))
         (hash-form
          '(secure-hash 'sha256
                        (prin1-to-string
                         (list (aref fn 0)
                               (mapconcat (lambda (byte)
                                            (number-to-string byte))
                                          (append (aref fn 1) nil) ",")
                               (aref fn 2) (aref fn 3))))))
    (prin1-to-string
     `(progn ,@setup
             (let* ((fn (make-byte-code ,descriptor ,code-form
                                        ',constants ,depth))
                    (native-before ,(funcall counter-form
                                             'nelisp-bytecode-jit--native-call-count))
                    (fallback-before ,(funcall counter-form
                                               'nelisp-bytecode-jit--interpreter-fallback-count))
                    (call-start (float-time)))
               (condition-case err
                   (let* ((value ,call)
                          (native-after ,(funcall counter-form
                                                 'nelisp-bytecode-jit--native-call-count))
                          (fallback-after ,(funcall counter-form
                                                    'nelisp-bytecode-jit--interpreter-fallback-count))
                          (native-delta (- native-after native-before))
                          (fallback-delta (- fallback-after fallback-before)))
                     (prin1 (list :status "ok" :value value
                                  :bytecode_sha256 ,hash-form
                                  :native native-after :native_before native-before
                                  :native_after native-after :native_delta native-delta
                                  :fallback fallback-after
                                  :fallback_before fallback-before
                                  :fallback_after fallback-after
                                  :fallback_delta fallback-delta
                                  :compile_phase
                                  (cond ((> native-delta 0) "native-executed")
                                        ((> fallback-delta 0) "vm-fallback")
                                        (t "no-dispatch-observed"))
                                  :runtime_call_elapsed_ms
                                  (* 1000.0 (- (float-time) call-start)))))
                 (error
                  (let* ((native-after ,(funcall counter-form
                                                 'nelisp-bytecode-jit--native-call-count))
                         (fallback-after ,(funcall counter-form
                                                    'nelisp-bytecode-jit--interpreter-fallback-count)))
                    (prin1 (list :status "runtime-error" :reason "runtime-error"
                                 :condition (car err) :bytecode_sha256 ,hash-form
                                 :native native-after :native_before native-before
                                 :native_after native-after
                                 :native_delta (- native-after native-before)
                                 :fallback fallback-after
                                 :fallback_before fallback-before
                                 :fallback_after fallback-after
                                 :fallback_delta (- fallback-after fallback-before)
                                 :compile_phase "runtime-error"
                                 :runtime_call_elapsed_ms
                                 (* 1000.0 (- (float-time) call-start))))))))))))

(defun nelisp-vendor-bytecode-triparity--equal-p (row)
  "Fail closed unless each lane ran and returned the same printed value."
  (let ((lanes (mapcar (lambda (key) (plist-get row key)) '(:host :vm :jit))))
    (and (cl-every (lambda (x) (equal (plist-get x :status) "ok")) lanes)
         (equal (mapcar (lambda (x) (nelisp-bytecode-corpus--print
                                    (plist-get x :value))) lanes)
                (make-list 3 (nelisp-bytecode-corpus--print
                              (plist-get (car lanes) :value))))
         (= (or (plist-get (plist-get row :jit) :native) -1)
            (plist-get row :native_expected))
         (or (= (plist-get row :native_expected) 0)
             (not (plist-member (plist-get row :jit) :native_delta))
             (= (plist-get (plist-get row :jit) :native_delta)
                (plist-get row :native_expected)))
         (and (stringp (plist-get row :bytecode_sha256))
              (cl-every (lambda (lane)
                          (equal (plist-get lane :bytecode_sha256)
                                 (plist-get row :bytecode_sha256)))
                        lanes)))))

(defun nelisp-vendor-bytecode-triparity--bytecode-hash (object)
  "Return a stable fingerprint for the descriptor, byte stream and constants."
  (secure-hash
   'sha256
   (nelisp-bytecode-corpus--print
    (list (aref object 0)
          (mapconcat (lambda (byte) (number-to-string byte))
                     (append (aref object 1) nil) ",")
          (aref object 2) (aref object 3)))))

(defun nelisp-vendor-bytecode-triparity--coverage-bytecode-hash (object)
  "Return the coverage corpus fingerprint over the raw byte-code payload."
  (secure-hash
   'sha256
   (prin1-to-string (list (aref object 0) (aref object 1)
                          (aref object 2) (aref object 3)))))

(defun nelisp-vendor-bytecode-triparity--parity (row)
  "Classify ROW as pass, fail, or not-comparable."
  (if (not (cl-every (lambda (lane)
                       (equal (plist-get (plist-get row lane) :status) "ok"))
                     '(:host :vm :jit)))
      "not-comparable"
    (if (nelisp-vendor-bytecode-triparity--equal-p row) "pass" "fail")))

(defun nelisp-vendor-bytecode-triparity--self-test (row)
  "Return non-nil iff ROW agrees and a changed expected value is rejected."
  (let ((good (nelisp-vendor-bytecode-triparity--equal-p row))
        (bad (copy-tree row)))
    (let* ((host (plist-get bad :host))
           (wrong (copy-sequence host)))
      (setq wrong (plist-put wrong :value :forced-mismatch))
      (setq bad (plist-put bad :host wrong)))
    (and good (not (nelisp-vendor-bytecode-triparity--equal-p bad)))))

(defun nelisp-vendor-bytecode-triparity--negative-control-p ()
  (nelisp-vendor-bytecode-triparity--self-test
   '(:native_expected 0
     :bytecode_sha256 "control"
     :host (:status "ok" :value 42 :bytecode_sha256 "control")
     :vm (:status "ok" :value 42 :bytecode_sha256 "control")
     :jit (:status "ok" :value 42 :native 0 :bytecode_sha256 "control"))))

(defun nelisp-vendor-bytecode-triparity--sources (name)
  "Return the minimal vendor sources needed by fixture NAME."
  (cond ((memq name '(zerop caar cadr fixnump bignump frame-configuration-p))
         :none)
        ((eq name 'byte-compile-lambda)
         '("macroexp.el" "cconv.el" "bytecomp.el"))
        ((memq name '(macroexp--all-forms macroexpand-1 macroexp-parse-body))
         '("macroexp.el"))
        ((memq name '(cconv-closure-convert cconv--convert-function cconv--set-diff))
         '("macroexp.el" "cconv.el"))
        (t '("macroexp.el" "cconv.el"))))

(defun nelisp-vendor-bytecode-triparity--object (name)
  "Return the bytecode object for fixture NAME, compiling pinned source as needed."
  (if (memq name '(zerop caar cadr fixnump bignump frame-configuration-p))
      (let* ((form (read (nelisp-vendor-source-form
                          "vendor/staged-emacs-lisp/subr.el" name)))
             (object (byte-compile (cons 'lambda (cddr form))))
             (coverage-pin (cdr (assq name
                                      nelisp-vendor-bytecode-triparity--coverage-bytecode-pins))))
        (unless (equal (nelisp-vendor-bytecode-triparity--coverage-bytecode-hash object)
                       coverage-pin)
          (error "Pinned GNU %s coverage fingerprint changed" name))
        (when (and (eq name 'zerop)
                   (not (equal (nelisp-vendor-bytecode-triparity--bytecode-hash object)
                               nelisp-vendor-bytecode-triparity--zerop-bytecode-sha256)))
          (error "Pinned triparity zerop fingerprint changed"))
        (when (and (eq name 'cadr)
                   (not (equal (nelisp-vendor-bytecode-triparity--bytecode-hash object)
                               nelisp-vendor-bytecode-triparity--cadr-bytecode-sha256)))
          (error "Pinned triparity cadr fingerprint changed"))
        object)
    (let ((function (symbol-function name)))
      (if (byte-code-function-p function)
          function
        (byte-compile function)))))

(defun nelisp-vendor-bytecode-triparity--timeout-budget (name lane)
  "Return the bounded child timeout for fixture NAME and execution LANE."
  (cond ((and (memq name '(caar cadr fixnump)) (eq lane 'jit)) 90)
        ((and (eq name 'macroexpand-1) (eq lane 'jit)) 45)
        (t 15)))

(defun nelisp-vendor-bytecode-triparity--counter-summary (rows key)
  "Summarize observed counter KEY without counting missing measurements as zero."
  (let* ((missing (cl-remove-if
                   (lambda (row)
                     (integerp (plist-get (plist-get row :jit) key)))
                   rows))
         (count (- (length rows) (length missing))))
    (list :status (cond ((null missing) "complete")
                        ((= count 0) "unavailable")
                        (t "partial"))
          :value (and (null missing)
                      (cl-loop for row in rows
                               sum (plist-get (plist-get row :jit) key)))
          :measured-count count
          :missing-functions (mapcar (lambda (row) (plist-get row :name))
                                     missing))))

(defun nelisp-vendor-bytecode-triparity--selected-specs ()
  "Return the default corpus or the comma-separated names in the environment."
  (let* ((all (append '((triparity-add1 41))
                      nelisp-vendor-bytecode-triparity--fixtures))
         (requested (getenv "NELISP_TRIPARITY_NAMES")))
    (if (not (and requested (not (equal requested ""))))
        all
      (let ((names (mapcar #'intern (split-string requested "," t))))
        (unless (memq 'triparity-add1 names)
          (push 'triparity-add1 names))
        (cl-remove-if-not (lambda (spec) (memq (car spec) names)) all)))))

(defun nelisp-vendor-bytecode-triparity--lane
    (program object args lane &optional sources timeout-seconds)
  (let ((form (nelisp-vendor-bytecode-triparity--child-form
               object args lane sources)))
    (let ((result (nelisp-vendor-bytecode-triparity--run-child
                   program (list "-Q" "--batch" "--eval" form)
                   (or timeout-seconds 15))))
      (plist-put result :timeout_seconds (or timeout-seconds 15)))))

(defun nelisp-vendor-bytecode-triparity-run ()
  "Run bounded Host/VM/JIT fixtures and write JSON report.
Set NELISP_TRIPARITY_OUTPUT to choose report path."
  (interactive)
  (let* ((root (nelisp-vendor-bytecode-triparity--root))
         (emacs (or (getenv "NELISP_EMACS") "emacs"))
         (vm (or (getenv "NELISP_BIN") (expand-file-name "target/nelisp" root)))
         (source-root (nelisp-vendor-bytecode-jit-coverage--source-root))
         (out (or (getenv "NELISP_TRIPARITY_OUTPUT")
                  (expand-file-name "target/vendor-bytecode-triparity.json" root)))
         rows ranking)
    (unless (and (file-executable-p vm) (file-directory-p source-root))
      (error "Required standalone binary or pinned vendor sources missing"))
    (nelisp-vendor-bytecode-jit-coverage--verify-sources)
    (nelisp-vendor-bytecode-jit-coverage--load-sources)
    (let ((load-path (cons (expand-file-name "lisp" root) load-path)))
      (require 'nelisp-bytecode-ir))
    (load-file (or (getenv "NELISP_BYTECODE_JIT_SOURCE")
                   (expand-file-name "lisp/nelisp-bytecode-jit.el" root)))
    (setq ranking (nelisp-vendor-bytecode-jit-coverage-report))
      (dolist (spec (nelisp-vendor-bytecode-triparity--selected-specs))
      (let* ((name (car spec))
             (args (cdr spec))
             (object (if (eq name 'triparity-add1)
                         (make-byte-code 257 (unibyte-string 84 135) [] 2)
                       (nelisp-vendor-bytecode-triparity--object name)))
             (row (list :name (symbol-name name)
                        :native_expected
                        (or (cdr (assq name
                                       nelisp-vendor-bytecode-triparity--native-expected))
                            0)
                        :bytecode_sha256 (nelisp-vendor-bytecode-triparity--bytecode-hash object)
                        :coverage_bytecode_sha256
                        (nelisp-vendor-bytecode-triparity--coverage-bytecode-hash object)
                        :jit_decoder
                        (let ((start (float-time))
                              (classification
                               (nelisp-vendor-bytecode-jit-coverage--classify object)))
                          (append classification
                                  (list :elapsed_ms
                                        (* 1000.0 (- (float-time) start)))))
                        :host (nelisp-vendor-bytecode-triparity--lane
                               emacs object args 'host
                               (nelisp-vendor-bytecode-triparity--sources name))
                        :vm (nelisp-vendor-bytecode-triparity--lane
                             vm object args 'vm
                             (nelisp-vendor-bytecode-triparity--sources name))
                        :jit (nelisp-vendor-bytecode-triparity--lane
                              vm object args 'jit
                              (nelisp-vendor-bytecode-triparity--sources name)
                              (nelisp-vendor-bytecode-triparity--timeout-budget
                               name 'jit)))))
        (setq row (plist-put row :parity
                             (nelisp-vendor-bytecode-triparity--parity row)))
        (setq row (plist-put row :negative_control "global-control"))
        (push row rows)))
    (setq rows (nreverse rows))
    (let* ((native-summary
            (nelisp-vendor-bytecode-triparity--counter-summary rows :native_delta))
           (fallback-summary
            (nelisp-vendor-bytecode-triparity--counter-summary rows :fallback_delta))
           (report `((schema . "nelisp-vendor-bytecode-triparity-v1")
                     (host . ,emacs) (emacs_version . ,emacs-version)
                     (standalone . ,vm)
                     (native_counter_status . ,(plist-get native-summary :status))
                     (observed_native_calls . ,(or (plist-get native-summary :value) :null))
                     (native_counter_measured_cases . ,(plist-get native-summary :measured-count))
                     (native_counter_missing_functions
                      . ,(vconcat (plist-get native-summary :missing-functions)))
                     (fallback_counter_status . ,(plist-get fallback-summary :status))
                     (observed_vm_fallbacks . ,(or (plist-get fallback-summary :value) :null))
                     (fallback_counter_measured_cases . ,(plist-get fallback-summary :measured-count))
                     (fallback_counter_missing_functions
                      . ,(vconcat (plist-get fallback-summary :missing-functions)))
                     (counterfactual_scope
                      . "generic-IR semantics only; singleton unsupported-opcode rows; does not imply native JIT lowering")
                     (counterfactual_unlocks
                      . ,(vconcat
                          (mapcar #'nelisp-vendor-bytecode-triparity--json-counterfactual
                                  (plist-get ranking :opcode-marginals))))
                     (negative_control . ,(if (nelisp-vendor-bytecode-triparity--negative-control-p)
                                              "pass" "fail"))
                     (cases . ,(vconcat (mapcar #'nelisp-vendor-bytecode-triparity--json-row rows)))))
           (json-encoding-pretty-print t))
      (make-directory (file-name-directory out) t)
      (with-temp-file out (insert (json-encode report)))
      (princ (format "triparity: %d cases; value-parity=%d; mismatch=%d; not-comparable=%d; native-calls=%d; vm-fallbacks=%d; negative-control=%s; report=%s\n"
                     (length rows)
                     (cl-count "pass" rows :key (lambda (r) (plist-get r :parity)) :test #'equal)
                     (cl-count "fail" rows :key (lambda (r) (plist-get r :parity)) :test #'equal)
                     (cl-count "not-comparable" rows :key (lambda (r) (plist-get r :parity)) :test #'equal)
                     (plist-get native-summary :value)
                     (plist-get fallback-summary :value)
                     (if (nelisp-vendor-bytecode-triparity--negative-control-p) "pass" "fail")
                     out)))))

(defun nelisp-vendor-bytecode-triparity--json-row (row)
  (let ((obj (make-hash-table :test 'equal)))
    (dolist (key '(:name :bytecode_sha256 :coverage_bytecode_sha256
                   :parity :negative_control
                   :native_expected))
      (puthash (substring (symbol-name key) 1) (plist-get row key) obj))
    (let ((decoder (make-hash-table :test 'equal)))
      (dolist (key '(:generic-ir-status :semantic-opcodes :generic-ir-decoded
                     :legacy-instruction-decoder-decoded :first-opcode
                     :first-offset :first-ir-rejection-reason :rejection-stage :reason
                     :guard-accepts-fixnum-probe
                     :static-jit-eligible-for-fixnum-inputs :arity :opcodes
                     :elapsed_ms))
        (when (plist-member (plist-get row :jit_decoder) key)
          (puthash (substring (symbol-name key) 1)
                   (plist-get (plist-get row :jit_decoder) key) decoder)))
      (puthash "jit_decoder" decoder obj))
    (dolist (lane '(:host :vm :jit))
      (let ((value (make-hash-table :test 'equal)))
        (dolist (key '(:status :reason :detail :condition :value :native :fallback
                       :native_before :native_after :native_delta
                       :fallback_before :fallback_after :fallback_delta
                       :compile_phase :runtime_call_elapsed_ms :process_elapsed_ms
                       :bytecode_sha256 :timeout_seconds))
          (when (plist-member (plist-get row lane) key)
            (puthash (substring (symbol-name key) 1)
                     (plist-get (plist-get row lane) key) value)))
        (puthash (substring (symbol-name lane) 1) value obj)))
    obj))

(defun nelisp-vendor-bytecode-triparity--json-counterfactual (row)
  `((opcode . ,(plist-get row :opcode))
    (functions_unlocked_by_single_opcode
     . ,(vconcat (mapcar #'symbol-name
                         (plist-get row :single-opcode-unlocked-functions))))
    (function_count . ,(plist-get row :single-opcode-unlocked-count))))

(provide 'nelisp-vendor-bytecode-triparity)
;;; nelisp-vendor-bytecode-triparity.el ends here
