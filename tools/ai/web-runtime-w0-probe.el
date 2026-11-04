;;; web-runtime-w0-probe.el --- Source-first Wasm runtime diagnosis -*- lexical-binding: t; -*-

;; This is a feasibility probe, not a second evaluator or an NLWB loader.
;; Invoke via web-runtime-w0-probe.py; a blocked runtime returns nonzero.

(require 'cl-lib)
(require 'json)
(require 'nelisp-aot-compiler)
(require 'nelisp-heap-image)
(require 'nelisp-eval)

(defconst web-runtime-w0-probe--application
  '((car (cons 7 nil))
    (concat "Ne" "Lisp")
    (let ((x 9)) (funcall (lambda () x))))
  "Application forms kept as data, never translated to JavaScript.")

(defconst web-runtime-w0-probe--providers
  '((nelisp-cc-eval-inner nelisp-cc-eval-inner--source)
    (nelisp-cc-evalport-combiner-cons nelisp-cc-evalport-combiner-cons--source)
    (nelisp-cc-evalport-combiner-apply nelisp-cc-evalport-combiner-apply--source)
    (nelisp-cc-evalport-aot-builtin-calln nelisp-cc-evalport-aot-builtin-calln--source)
    (nelisp-cc-evalport-capture-lexical nelisp-cc-evalport-capture-lexical--source))
  "Genuine native-runtime providers; not a closed runtime dependency set.")

(defun web-runtime-w0-probe--contains-p (form operator)
  "Return non-nil if FORM contains an OPERATOR call."
  (and (consp form)
       (or (eq (car form) operator)
           (cl-some (lambda (child)
                      (web-runtime-w0-probe--contains-p child operator))
                    form))))

(defun web-runtime-w0-probe--operation-owners (source operator)
  "Return top-level defuns of SOURCE containing OPERATOR."
  (mapcar #'cadr
          (cl-remove-if-not
           (lambda (form)
             (and (eq (car-safe form) 'defun)
                  (web-runtime-w0-probe--contains-p (cdddr form) operator)))
           (cdr source))))

(defun web-runtime-w0-probe--compile (provider directory)
  "Compile a genuine PROVIDER into DIRECTORY and return diagnostics."
  (let* ((feature (car provider))
         (source-symbol (cadr provider))
         (file nil)
         (provenance nil)
         (output (expand-file-name (concat (symbol-name feature) ".wasm")
                                   directory))
         (start (float-time))
         (source nil)
         (failure nil))
    (condition-case err
        (progn
          (require feature)
          (setq source (symbol-value source-symbol))
          (setq file (symbol-file source-symbol 'defvar)
                provenance (if file "symbol-file" "unavailable"))
          (nelisp-aot-compile-to-object source output :arch 'wasm32 :format 'wasm))
      (error (setq failure err)))
    `((provider . ,(symbol-name feature))
      (path . ,(when file (file-relative-name file default-directory)))
      (source_provenance . ,provenance)
      (source_sha256 . ,(when (and file (file-readable-p file))
                          (with-temp-buffer
                            (insert-file-contents-literally file)
                            (secure-hash 'sha256 (current-buffer)))))
      (definition_count . ,(length (cdr source)))
      (status . ,(cond ((and failure (null source)) "missing-provider")
                       (failure "blocked") (t "compiled-only")))
      (elapsed_seconds . ,(- (float-time) start))
      (condition . ,(when failure (prin1-to-string failure)))
      (unsupported_operation . ,(when (eq (cadr failure) :wasm-unsupported-ir)
                                  (symbol-name (caddr failure))))
      (operation_owners . ,(vconcat
                            (when (eq (cadr failure) :wasm-unsupported-ir)
                              (mapcar #'symbol-name
                                      (web-runtime-w0-probe--operation-owners
                                       source (caddr failure)))))))))

(defun web-runtime-w0-probe-run ()
  "Write a machine-readable source-first report; keep W0 red until execution."
  (let* ((directory (expand-file-name
                     (or (getenv "WEB_RUNTIME_W0_OUTPUT") "target/web-runtime-w0")))
         (image (nelisp-heap-image-encode-roots
                 (list (cons "application" web-runtime-w0-probe--application))))
         (decoded (cdr (assoc "application" (nelisp-heap-image-decode-form image))))
         (reference (mapcar #'nelisp-eval web-runtime-w0-probe--application))
         (rows nil))
    (make-directory directory t)
    (unless (and (equal decoded web-runtime-w0-probe--application)
                 (equal reference '(7 "NeLisp" 9)))
      (error "W0 fixture codec/reference mismatch"))
    (with-temp-file (expand-file-name "application.heap-v0.sexp" directory)
      (let ((print-length nil) (print-level nil)) (prin1 image (current-buffer))))
    (dolist (provider web-runtime-w0-probe--providers)
      (push (web-runtime-w0-probe--compile provider directory) rows))
    (setq rows (nreverse rows))
    (with-temp-file (expand-file-name "source-first.json" directory)
      (insert
       (json-encode
        `((schema_version . 1)
          (w0_exit . "blocked")
          (wasm_application_executed . :json-false)
          (javascript_builtin_dispatcher . :json-false)
          (fixture . ((codec . "heap-v0 (diagnostic input, not NLWB v1)")
                      (host_reference_only . t)
                      (forms . ,(vconcat (mapcar #'prin1-to-string decoded)))
                      (expected . ,(prin1-to-string reference))))
          (first_blocker . ,(cl-find-if
                             (lambda (row)
                               (not (equal (alist-get 'status row) "compiled-only"))) rows))
          (providers . ,(vconcat rows))
          (remaining . ["Close genuine runtime dependencies and relocate native absolute addresses"
                        "Implement target-compatible image loader and evaluator boot"
                        "Execute all three forms inside Wasm with no JS builtin imports"
                        "Remove actual Wasm builtin provider and demonstrate execution failure"]))))
      (insert "\n"))
    (princ (format "W0 blocked; providers=%d; host fixture=(7 \"NeLisp\" 9); report=%s\n"
                   (length rows) (expand-file-name "source-first.json" directory)))
    (kill-emacs 1)))

(provide 'web-runtime-w0-probe)
;;; web-runtime-w0-probe.el ends here
