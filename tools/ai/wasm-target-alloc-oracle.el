;;; wasm-target-alloc-oracle.el --- Isolated allocator source -*- lexical-binding: t; -*-
(require 'cl-lib)
(defvar nelisp-standalone--target)
(defun wasm-target-alloc-oracle (file)
  "Return genuine sources under an explicit native target binding."
  (load (file-name-sans-extension file) nil t)
  ;; Linux uses the shared CAS allocator without Windows page-commit hooks.
  ;; Only its OS growth/failure boundary is replaced for the one-chunk harness.
  (let* ((nelisp-standalone--target 'linux-x86_64)
         (arena (nelisp-standalone--target-arena-source))
         (support nelisp-standalone--gc-source)
         (files (sort (delete-dups (delq nil (mapcar #'car load-history))) #'string<))
         (print-length nil) (print-level nil) (print-circle nil))
    (princ "WASM-TARGET-ALLOC=")
    (prin1 (list :allocator-source 'linux-x86_64 :arena arena :support support
                 :files (mapcar (lambda (path)
                                  (cons path (with-temp-buffer
                                               (set-buffer-multibyte nil)
                                               (insert-file-contents-literally path)
                                               (secure-hash 'sha256 (current-buffer))))) files)))
    (terpri)))
(provide 'wasm-target-alloc-oracle)
