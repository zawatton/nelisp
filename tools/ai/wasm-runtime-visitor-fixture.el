;;; wasm-runtime-visitor-fixture.el --- Operand traversal conformance -*- lexical-binding: t; -*-

(require 'nelisp-aot-compiler)

(defconst wasm-runtime-visitor-fixture--cases
  '(("atomic-import" . (atomic-fetch-add (extern-call audit_ptr) (extern-call audit_delta)))
    ("tag-import" . (sexp-tag (extern-call audit_word)))
    ("atomic-ptr-catch" . (atomic-fetch-add (catch 3 (throw 3 1040)) 1))
    ("atomic-delta-catch" . (atomic-fetch-add 1040 (catch 3 (throw 3 2))))
    ("tag-catch" . (sexp-tag (catch 3 (throw 3 169))))))

(defun wasm-runtime-visitor-fixture-build ()
  "Compile one fixture whose child expressions require visitor traversal."
  (let* ((name (getenv "WASM_RUNTIME_VISITOR_CASE"))
         (body (cdr (assoc name wasm-runtime-visitor-fixture--cases)))
         (path (format "target/wasm-runtime-ops/visitor-%s.wasm" name)))
    (unless body (error "Unknown visitor fixture %s" name))
    (make-directory (file-name-directory path) t)
    (nelisp-aot-compile-to-object
     `(seq (defun ensure-memory () (alloc-bytes 8 8))
           (defun run-operand () ,body)) path :arch 'wasm32 :format 'wasm)
    (princ (format "fixture=%s\n" path))))

(provide 'wasm-runtime-visitor-fixture)
;;; wasm-runtime-visitor-fixture.el ends here
