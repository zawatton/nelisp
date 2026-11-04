;;; wasm-rootstack-controls.el --- Negative and full-provider diagnostics -*- lexical-binding: t; -*-
(require 'wasm-rootstack-fixture)

(defun wasm-rootstack-controls-run ()
  "Build a broken-clear control and report bounded full-provider limits."
  (let* ((directory "target/wasm-rootstack/")
         (authority (wasm-rootstack-authority))
         (owners (plist-get authority :owners))
         (resolved (cl-remove-if-not (lambda (row) (plist-get row :size)) owners))
         (references (delete-dups
                      (mapcar #'cadr (wasm-rootstack--forms
                                     nelisp-cc-rootstack--source
                                     (lambda (node) (eq (car-safe node) 'data-addr))))))
         (original (symbol-function 'nelisp-aot-compile-to-object))
         (process-environment (copy-sequence process-environment))
         (full-condition nil))
    (make-directory directory t)
    (unless (equal (sort (copy-sequence references) (lambda (a b) (string< (symbol-name a) (symbol-name b))))
                   (sort (mapcar (lambda (row) (plist-get row :name)) owners)
                         (lambda (a b) (string< (symbol-name a) (symbol-name b)))))
      (error "Full rootstack ownership reference set changed"))
    (setenv "WASM_ROOTSTACK_OUTPUT" (concat directory "negative-clear.wasm"))
    ;; Corrupt only the accepted source at the compiler boundary.  Production
    ;; ownership/genuine-definition validation remains active and unchanged.
    (cl-letf (((symbol-function 'nelisp-aot-compile-to-object)
               (lambda (source path &rest options)
                 (apply original
                        (cl-subst '(ptr-write-u64 (+ slot 24) 0 (ptr-read-u64 (+ slot 24) 0))
                                  '(ptr-write-u64 (+ slot 24) 0 0) source :test #'equal)
                        path options))))
      (wasm-rootstack-fixture-build))
    (condition-case err
        (nelisp-aot-compile-to-object (wasm-rootstack-compose :owners resolved :full t)
                                      (concat directory "full-provider.wasm") :arch 'wasm32 :format 'wasm)
      (error (setq full-condition (prin1-to-string err))))
    (with-temp-file (concat directory "full-provider-report.json")
      (insert (json-encode
               `((provider_source . ,(file-relative-name (symbol-file 'nelisp-cc-rootstack--source 'defvar)))
                 (provider_source_sha256 . ,(wasm-rootstack--hash (symbol-file 'nelisp-cc-rootstack--source 'defvar)))
                 (definition_count . ,(cl-count-if (lambda (form) (eq (car-safe form) 'defun))
                                                  (cdr nelisp-cc-rootstack--source)))
                 (data_references . ,(vconcat (mapcar #'symbol-name references)))
                 (authority_source_sha256 . ,(plist-get authority :file-sha256))
                 (ownership . ,(vconcat
                                 (mapcar (lambda (row)
                                           `((name . ,(symbol-name (plist-get row :name)))
                                             (size . ,(plist-get row :size))
                                             (start_expression . ,(prin1-to-string (plist-get row :offset-expression)))
                                             (end_expression . ,(prin1-to-string (plist-get row :end-expression)))
                                             (unresolved . ,(plist-get row :unresolved)))) owners)))
                 (resolved_declaration_count . ,(length resolved))
                 (full_ownership_closed . :json-false)
                 (compile_status . ,(if full-condition "blocked" "compiled-only"))
                 (first_condition . ,full-condition)
                 (generator_limits . ,(plist-get authority :limit))
                 (w0_application_executed . :json-false)))))
    (princ (format "full-provider=%s resolved-owners=%d/6\n" full-condition (length resolved)))))

(provide 'wasm-rootstack-controls)
