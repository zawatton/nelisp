;;; nelisp-runtime-development.el --- Rebuild native GC from a live REPL -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Commentary:
;; Edit the canonical allocator/GC bodies in this checkout, then rebuild from
;; the persistent REPL.  Host compilation keeps the compiler's allocations
;; outside the heap being repaired.  Staging never publishes; the compatibility
;; command below performs the explicit publication step.
;;; Code:
(require 'nelisp-native-load)

(defun nelisp-runtime-build-and-stage (&optional repository)
  "Build and load REPOSITORY's allocator/GC without publishing it.
REPOSITORY defaults to the current directory.  Return a plist containing the
staged raw handles under `:alloc-handle' and `:gc-handle'.  The runtime
generation and running binary identity are captured before the build and
checked again after loading; a change rejects the candidate.  No source hash
or reload plan is implied by this API."
  (let* ((root (file-name-as-directory
                (expand-file-name (or repository default-directory))))
         (script (expand-file-name "scripts/nelisp-runtime-reload-compile.el"
                                   root))
         (phase :arguments)
         (artifact nil)
         (output nil)
         (binary nil)
         (generation nil)
         (before nil)
         (alloc nil)
         (gc nil))
    (condition-case err
        (progn
          (unless (file-readable-p script)
            (error "No runtime compiler in checkout: %s" root))
          (setq phase :identity
                before (nelisp-runtime-reload-status))
          (unless (eq (plist-get before :status) 'ready)
            (error "Start an opt-in native runtime development binary first"))
          (setq generation (plist-get before :generation)
                binary (nelisp-native-load--running-binary-sha256))
          (unless (integerp generation)
            (error "Cannot identify the current runtime generation"))
          (unless (stringp binary)
            (error "Cannot identify the running native binary"))
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
          (setq alloc (nelisp-native-load-raw-artifact
                       artifact "nl_alloc_bytes_uncheck" binary))
          ;; Both entries belong to the same validated unit and GC table.
          ;; Mapping the artifact twice wastes a generation's code pages and
          ;; repeats all metadata decoding/validation.
          (setq gc (copy-sequence alloc))
          (setq gc (plist-put gc :entry-name
                              "nl_gc_collect_recorded_mark_sweep_body"))
          (setq gc (plist-put gc :entry
                              (nelisp-native-load-raw-export-address
                               alloc "nl_gc_collect_recorded_mark_sweep_body")))
          (setq gc (plist-put gc :arity 1))
          (let* ((after (nelisp-runtime-reload-status))
                 (after-generation (plist-get after :generation))
                 (after-binary (nelisp-native-load--running-binary-sha256)))
            (unless (and (eq (plist-get after :status) 'ready)
                         (equal after-generation generation))
              (error "Runtime generation changed while staging (%S -> %S)"
                     generation after-generation))
            (unless (equal after-binary binary)
              (error "Running native binary changed while staging")))
          (list :status 'staged :phase :stage
                :repository root :artifact artifact
                :source (concat artifact ".el")
                :binary-sha256 binary :generation generation
                :alloc-handle alloc :gc-handle gc))
      (error (list :status 'rejected :phase phase
                   :reason (error-message-string err)
                   :repository root :artifact artifact
                   :binary-sha256 binary :generation generation)))))

(defun nelisp-runtime-rebuild-and-reload (&optional repository)
  "Build, stage, and publish REPOSITORY's complete allocator/GC.
This compatibility convenience preserves the historical return plist and
publishes the two handles returned by `nelisp-runtime-build-and-stage' exactly
once.  Failed staging or validation leaves the existing generation installed."
  (let ((staged (nelisp-runtime-build-and-stage repository)))
    (if (not (eq (plist-get staged :status) 'staged))
        staged
      (condition-case err
          (let* ((repository (plist-get staged :repository))
                 (artifact (plist-get staged :artifact))
                 (source (plist-get staged :source))
                 (binary (plist-get staged :binary-sha256))
                 (generation (plist-get staged :generation))
                 (current (nelisp-runtime-reload-status))
                 (current-binary (nelisp-native-load--running-binary-sha256)))
            (if (or (not (eq (plist-get current :status) 'ready))
                    (not (equal (plist-get current :generation) generation))
                    (not (equal current-binary binary)))
                (list :status 'rejected :phase :publish
                      :reason :staged-runtime-identity-changed
                      :repository repository :artifact artifact :source source
                      :binary-sha256 binary :generation generation)
              (append
               (nelisp-native-load-raw-install
                (plist-get staged :alloc-handle)
                (plist-get staged :gc-handle))
               (list :repository repository :artifact artifact :source source
                     :binary-sha256 binary))))
        (error
         (list :status 'rejected :phase :publish
               :reason (error-message-string err)
               :repository (plist-get staged :repository)
               :artifact (plist-get staged :artifact)
               :source (plist-get staged :source)
               :binary-sha256 (plist-get staged :binary-sha256)
               :generation (plist-get staged :generation)))))))

(provide 'nelisp-runtime-development)
;;; nelisp-runtime-development.el ends here
