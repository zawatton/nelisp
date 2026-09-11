;;; nelisp-runtime-development.el --- Rebuild native GC from a live REPL -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Commentary:
;; Edit the canonical allocator/GC bodies in this checkout, then rebuild from
;; the persistent REPL.  Host compilation keeps the compiler's allocations
;; outside the heap being repaired.  Publication happens only in the parent.
;;; Code:
(require 'nelisp-native-load)

(defun nelisp-runtime-rebuild-and-reload (&optional repository)
  "Rebuild REPOSITORY's complete allocator/GC and publish in this REPL.
REPOSITORY defaults to the current directory.  The host Emacs executable is
selected by EMACS, or `emacs'.  This requires the opt-in Linux x86_64 binary.
The returned plist names the phase, candidate artifact, and generation.
Failed staging or validation leaves the existing generation installed."
  (let* ((root (file-name-as-directory (expand-file-name
                                       (or repository default-directory))))
         (script (expand-file-name "scripts/nelisp-runtime-reload-compile.el" root))
         (phase :arguments)
         (artifact nil)
         (output nil)
         (binary nil))
    (condition-case err
        (progn
          (unless (file-readable-p script)
            (error "No runtime compiler in checkout: %s" root))
          (unless (eq (plist-get (nelisp-runtime-reload-status) :status) 'ready)
            (error "Start an opt-in native runtime development binary first"))
          (setq phase :identity
                binary (nelisp-native-load--running-binary-sha256))
          (unless binary (error "Cannot identify the running native binary"))
          (setq phase :compile
                artifact (make-temp-file "nelisp-runtime-checkout-" nil ".nelr")
                output (generate-new-buffer " *nelisp-runtime-compiler*"))
          (unwind-protect
              (let ((code (call-process
                           (or (getenv "EMACS") "emacs") nil output nil
                           "-Q" "--batch"
                           "--eval" "(setq load-prefer-newer t)"
                           "-L" (expand-file-name "lisp" root)
                           "-L" (expand-file-name "src" root)
                           "-L" (expand-file-name "scripts" root)
                           "-l" script
                           "-f" "nelisp-runtime-reload-compile-command"
                           artifact binary)))
                (unless (equal code 0)
                  (error "Native compiler failed (%S): %s" code
                         (with-current-buffer output (buffer-string)))))
            (when (buffer-live-p output) (kill-buffer output)))
          (setq phase :load)
          (let* ((alloc (nelisp-native-load-raw-artifact
                         artifact "nl_alloc_bytes_uncheck" binary))
                 ;; Both entries belong to the same validated unit and GC
                 ;; table.  Mapping the artifact twice wastes a generation's
                 ;; code pages and repeats all metadata decoding/validation.
                 (gc (copy-sequence alloc)))
            (setq gc (plist-put gc :entry-name
                                "nl_gc_collect_recorded_mark_sweep_body"))
            (setq gc (plist-put gc :entry
                                (nelisp-native-load-raw-export-address
                                 alloc "nl_gc_collect_recorded_mark_sweep_body")))
            (setq gc (plist-put gc :arity 1))
            (setq phase :publish)
            (append (nelisp-native-load-raw-install alloc gc)
                    (list :repository root :artifact artifact
                          :source (concat artifact ".el")
                          :binary-sha256 binary))))
      (error (list :status 'rejected :phase phase
                   :reason (error-message-string err)
                   :repository root :artifact artifact)))))

(provide 'nelisp-runtime-development)
;;; nelisp-runtime-development.el ends here
