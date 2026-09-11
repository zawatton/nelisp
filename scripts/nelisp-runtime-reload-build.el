;;; nelisp-runtime-reload-build.el --- opt-in bounded native reload build -*- lexical-binding: t; -*-

;;; Commentary:
;; Keep the production standalone entry point unchanged.  This small entry
;; point opts into the Linux x86_64 runtime-reload source variant before
;; invoking the existing incremental builder.

;;; Code:

(require 'nelisp-standalone-build)

;;;###autoload
(defun nelisp-runtime-reload-build ()
  "Build the opt-in Linux x86_64 runtime-reload standalone binary."
  (let ((process-environment (copy-sequence process-environment))
        ;; The normal builder may already be loaded in this host.  Its
        ;; registration list was computed at load time, so extend it only
        ;; for this build without changing subsequent normal builds/tests.
        (nelisp-standalone--reader-builtins
         (delete-dups
          (append nelisp-standalone--reader-builtins
                  '("nelisp--native-runtime-symbol-addr"
                    "nelisp--native-runtime-contract-word"))))
        (nelisp-standalone--reader-out
         (expand-file-name "target/nelisp-runtime-reload"
                           nelisp-standalone--repo-root)))
    (setenv "NELISP_RUNTIME_RELOAD" "1")
    (unless (eq nelisp-standalone--target 'linux-x86_64)
      (user-error "runtime reload supports Linux x86_64 only"))
    (nelisp-standalone-build-reader)))

(defun nelisp-runtime-reload-production-source ()
  "Return the complete allocator and collector from this checkout.
The candidate contains original bodies, with internal direct calls bound
inside the candidate unit.  Native metadata addresses use the exact same
arena relocation pass as a normal standalone build.  The live loader supplies
the process's existing BSS addresses; it must never create a second heap."
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "NELISP_RUNTIME_RELOAD" "0")
    (nelisp-standalone--chunk-arena-rewrite
     (nelisp-standalone--rebase-arena-source
      (cons 'seq
            (append (cdr (nelisp-standalone--target-arena-source))
                    (cdr (nelisp-runtime-reload-instrument-gc
                          nelisp-standalone--gc-source))))))))

(defun nelisp-runtime-reload-export-source (output)
  "Write the complete native production source to OUTPUT for inspection.
Edit the canonical bodies in scripts/nelisp-standalone-build.el and export
again, so a later normal build includes the same correction.  OUTPUT contains
raw defuns suitable for the development native-unit compiler."
  (let ((parent (file-name-directory (expand-file-name output))))
    (make-directory parent t))
  (with-temp-file output
    (insert ";;; Generated from the current checkout's allocator and GC.\n")
    (let ((print-length nil) (print-level nil))
      (dolist (form (cdr (nelisp-runtime-reload-production-source)))
        (pp form (current-buffer))
        (insert "\n"))))
  output)

(provide 'nelisp-runtime-reload-build)

;;; nelisp-runtime-reload-build.el ends here
