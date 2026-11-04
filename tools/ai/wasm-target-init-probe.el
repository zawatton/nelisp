;;; wasm-target-init-probe.el --- Build and inspect target init -*- lexical-binding: t; -*-
(require 'wasm-target-init)

(defun wasm-target-init--node (&rest args)
  "Run bounded Node inside this Emacs process's heavy slot."
  (with-temp-buffer
    (let ((rc (apply #'call-process "bash" nil t nil
                     "-c" "ulimit -v 8388608; exec \"$@\"" "node"
                     "timeout" "120" "node" "--disable-wasm-trap-handler"
                     "tools/ai/wasm-target-init-check.mjs" args)))
      (cons rc (buffer-string)))))

(defun wasm-target-init-build (&optional skip path no-full)
  "Build init fixture at PATH; SKIP is the uninitialized negative control."
  (let* ((path (or path "target/wasm-target-init/init.wasm"))
         (contract (wasm-target-init-contract))
         (authority (wasm-target-init-authority))
         (owners (plist-get authority :owners))
         (source (wasm-target-init--source contract owners skip))
         (layout (wasm-rootstack-layout source))
         (record (plist-get contract :record))
         (forms (plist-get contract :forms))
         (definition (cl-find 'nelisp-standalone--arena-init-metadata-forms-dynamic
                              (plist-get record :definitions) :key #'cadr))
         (print-length nil) (print-level nil) (print-circle nil)
         (print-quoted t) (print-escape-newlines t)
         (files (delete-dups
                 (append (mapcar #'car (plist-get record :files))
                         (mapcar #'car (plist-get (plist-get authority :native-oracle) :files))
                         (mapcar (lambda (name) (expand-file-name name wasm-rootstack--root))
                                 '("lisp/nelisp-cc-rootstack.el" "lisp/nelisp-aot-compiler.el"
                                   "lisp/nelisp-asm-wasm.el" "lisp/nelisp-standalone-arena-rewrite.el"
                                   "tools/ai/wasm-rootstack-compose.el"
                                   "tools/ai/wasm-rootstack-full-authority.eld"
                                   "tools/ai/wasm-target-init.el" "tools/ai/wasm-target-init-oracle.el"
                                   "tools/ai/wasm-target-init-probe.el" "tools/ai/wasm-target-init-check.mjs")))))
         (full-path "target/wasm-target-init/full.wasm") full-sha full-imports)
    (princ (format "generator-sha256=%s forms-sha256=%s\n"
                   (plist-get contract :generator-sha256) (plist-get contract :forms-sha256)))
    (make-directory (file-name-directory path) t)
    (nelisp-aot-compile-to-object source path :arch 'wasm32 :format 'wasm)
    (unless no-full
      (let* ((rewritten (wasm-target-init-rewrite nelisp-cc-rootstack--source))
             (count (cl-count 'defun (cdr rewritten) :key #'car-safe))
             (full `(seq ,@(cl-remove-if-not (lambda (form) (eq (car-safe form) 'data-blob))
                                             (cdr source)) ,@(cdr rewritten))))
        (wasm-target-init--assert-rewritten rewritten)
        (unless (= count 61) (error "Full definition count changed: %d" count))
        (princ (format "raw-arena-atoms=0 definitions=%d\n" count))
        (nelisp-aot-compile-to-object full full-path :arch 'wasm32 :format 'wasm)
        (setq full-sha (wasm-rootstack--hash full-path))
        (let ((result (wasm-target-init--node "--inspect" full-path)))
          (unless (equal (car result) 0) (error "Full inspect: %s" (cdr result)))
          (setq full-imports (json-read-from-string (cdr result)))
          (princ (format "full-remaining-imports=%s\n" (cdr result))))))
    (with-temp-file (concat path ".json")
      (insert (json-encode
               `((schema_version . 1)
                 (wasm_sha256 . ,(wasm-rootstack--hash path))
                 (source_sha256 . ,(vconcat (mapcar (lambda (file)
                                                     (vector (file-relative-name file wasm-rootstack--root)
                                                             (wasm-rootstack--hash file))) files)))
                 (generator_definition . ,(prin1-to-string definition))
                 (generator_sha256 . ,(plist-get contract :generator-sha256))
                 (seed_forms . ,(prin1-to-string forms))
                 (seed_forms_sha256 . ,(plist-get contract :forms-sha256))
                 (genuine_init_sha256 . ,(wasm-rootstack--definition-hash
                                         (cl-find 'nl_rootstack_init (cdr nelisp-cc-rootstack--source) :key #'cadr)))
                 (inputs . ((target . "wasm32") (arena_size . ,(number-to-string (plist-get (plist-get contract :inputs) :arena-size)))
                            (intern_region . "excluded") (runtime_reload . :json-false) (callsite . :json-false)))
                 (data_start . ,(number-to-string (plist-get contract :data-start)))
                 (oracle_data_start . ((value . ,(number-to-string (plist-get record :data-start)))
                                       (definition . ,(prin1-to-string (plist-get record :data-start-definition)))
                                       (sha256 . ,(wasm-rootstack--definition-hash
                                                   (plist-get record :data-start-definition)))))
                 (contract_words . ,(vconcat
                                     (mapcar (lambda (row)
                                               (let ((copy (copy-tree row)))
                                                 (setf (alist-get 'offset copy)
                                                       (number-to-string (alist-get 'offset row)))
                                                 copy)) (plist-get contract :words))))
                 (zero_fill . ,(plist-get contract :zero-fill))
                 (post_init_words . [((owner . "nl_arena_base") (offset . "0") (expected . ["base" "0"]))])
                 (post_rootstack_words . [((owner . "nl_rootstack_top") (offset . "0")
                                          (expected . ["owner" "nl_rootstack_region"]))])
                 (rootstack . ,(plist-get contract :rootstack))
                 ;; Original declarations are unchecked provenance in Node.
                 (owners . ,(vconcat
                             (mapcar (lambda (row)
                                       `((name . ,(symbol-name (plist-get row :name)))
                                         (address . ,(number-to-string (cdr (assoc (symbol-name (plist-get row :name))
                                                                                  (plist-get layout :symbol-addrs)))))
                                         (size . ,(number-to-string (plist-get row :size)))
                                         (native_offset . ,(number-to-string (plist-get row :offset)))
                                         (declaration_offset_expression . ,(prin1-to-string (plist-get row :offset-expression)))
                                         (declaration_end_expression . ,(prin1-to-string (plist-get row :end-expression))))) owners)))
                 (rewritten_offsets . ,(vconcat (mapcar #'number-to-string (wasm-target-init--raw-offsets nelisp-cc-rootstack--source))))
                 (rewrite_source_sha256 . ,(wasm-rootstack--definition-hash
                                            (wasm-target-init-rewrite nelisp-cc-rootstack--source)))
                 (full_module . ,(if no-full nil
                                   `((path . ,full-path) (sha256 . ,full-sha) (definitions . 61)
                                     (raw_arena_atoms . 0) (imports . ,(vconcat full-imports)))))
                 (limit . ,(plist-get contract :limit))))))
    (princ (format "target-init=%s seed-words=%d arena-size=%d\n" path (length forms)
                   (plist-get (plist-get contract :inputs) :arena-size)))
    path))

(unless (bound-and-true-p wasm-target-init--library-only)
  (let ((print-length 6) (print-level 3))
    (condition-case err (wasm-target-init-build)
      (error (princ (format "target-init-diagnostic=%s\n"
                            (truncate-string-to-width (prin1-to-string err) 500)))
             (kill-emacs 1)))))
(provide 'wasm-target-init-probe)
