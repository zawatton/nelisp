;;; wasm-rootstack-fixture.el --- Compile genuine rootstack composition -*- lexical-binding: t; -*-
(require 'json)
(require 'wasm-rootstack-compose)

(defun wasm-rootstack-fixture-build ()
  "Build one module containing four genuine functions and diagnostic exports."
  (let* ((path (or (getenv "WASM_ROOTSTACK_OUTPUT") "target/wasm-rootstack/fixture.wasm"))
         (source (wasm-rootstack-compose))
         (layout (wasm-rootstack-layout source))
         (authority (wasm-rootstack-authority))
         (provider (symbol-file 'nelisp-cc-rootstack--source 'defvar)))
    (make-directory (file-name-directory path) t)
    (setq source
          (append source
                  '((defun rootstack-top-address () (data-addr nl_rootstack_top))
                    (defun rootstack-region-address () (data-addr nl_rootstack_region))
                    (defun managed-cursor-probe () (alloc-bytes 8 8))
                    (defun literal-address-word () 268435544))))
    (nelisp-aot-compile-to-object source path :arch 'wasm32 :format 'wasm)
    (with-temp-file (concat path ".json")
      (insert (json-encode
               `((schema_version . 1)
                 (wasm_sha256 . ,(wasm-rootstack--hash path))
                 (source_sha256 . ,(vconcat
                                    (mapcar (lambda (file)
                                              (vector (file-relative-name file wasm-rootstack--root)
                                                      (wasm-rootstack--hash file)))
                                            (list provider (plist-get authority :file)
                                                  "lisp/nelisp-aot-compiler.el"
                                                  "lisp/nelisp-asm-wasm.el"
                                                  "tools/ai/wasm-rootstack-compose.el"
                                                  "tools/ai/wasm-rootstack-fixture.el"))))
                 (genuine_definition_sha256 . ,(secure-hash 'sha256 (prin1-to-string (wasm-rootstack-definitions))))
                 (authority_definition_sha256 . ,(plist-get authority :definition-sha256))
                 (top . ,(cdr (assoc "nl_rootstack_top" (plist-get layout :symbol-addrs))))
                 (region . ,(cdr (assoc "nl_rootstack_region" (plist-get layout :symbol-addrs))))
                 (region_bytes . ,(plist-get (cadr (plist-get authority :owners)) :size))
                 (managed_cursor . ,(plist-get layout :stack-top))
                 (genuine_functions . ,(vconcat (mapcar #'symbol-name wasm-rootstack--names)))
                 (scope . "Four genuine functions; two owned BSS regions; no allocator/GC/image compatibility claim")))))
    (princ (format "rootstack-fixture=%s\n" path))))

(provide 'wasm-rootstack-fixture)
