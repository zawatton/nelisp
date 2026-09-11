;;; nelisp-native-unit-development.el --- rebuild user native functions in the REPL -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'nelisp-native-unit)

(defun nelisp-native-unit-development--bytes (path)
  "Read at most 2 MiB of raw source bytes."
  (unless (and (file-regular-p path) (file-readable-p path))
    (error "Unreadable native source: %s" path))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path nil 0 (1+ (* 2 1024 1024)))
    (when (> (- (point-max) (point-min)) (* 2 1024 1024))
      (error "Native source exceeds 2 MiB"))
    (buffer-string)))

(defun nelisp-native-unit-rebuild-and-reload
    (source &optional unit-id exports repository)
  "Recompile SOURCE and publish UNIT-ID, or create a unit when UNIT-ID is nil.
EXPORTS names the stable public entries at creation; nil selects all entries.
Existing unit contracts cannot change. REPOSITORY defaults to the current
directory. SOURCE is compiled in a host subprocess from an immutable snapshot;
the original file is checked again before publication. This command is explicit
and does not retry application calls. Return the publication result plist."
  (let ((phase :source) artifact snapshot)
    (condition-case err
        (let* ((root (file-name-as-directory
                      (expand-file-name (or repository default-directory))))
               (path (expand-file-name source root))
               (bytes (nelisp-native-unit-development--bytes path))
               (hash (secure-hash 'sha256 bytes))
               (binary (nelisp-native-load--running-binary-sha256))
               (script (expand-file-name "scripts/nelisp-native-unit-compile.el" root)))
          (unless (and (stringp binary) (file-readable-p script))
            (error "Native identity or checkout compiler is unavailable"))
          (setq artifact (make-temp-file "nelisp-native-unit-" nil ".nelr")
                snapshot (concat artifact ".el") phase :compile)
          (let ((coding-system-for-write 'no-conversion))
            (with-temp-file snapshot
              (set-buffer-multibyte nil) (insert bytes)))
          (let ((output (generate-new-buffer " *native-unit-compiler*")))
            (unwind-protect
                (let ((code (call-process
                             (or (getenv "EMACS") "emacs") nil output nil
                             "-Q" "--batch" "--eval" "(setq load-prefer-newer t)"
                             "-L" (expand-file-name "lisp" root)
                             "-L" (expand-file-name "src" root)
                             "-L" (expand-file-name "scripts" root)
                             "-l" script "-f" "nelisp-native-unit-compile-command"
                             snapshot artifact binary)))
                  (unless (equal code 0)
                    (error "Compiler failed (%S): %s" code
                           (with-current-buffer output (buffer-string)))))
              (when (buffer-live-p output) (kill-buffer output))))
          (unless (equal hash (secure-hash 'sha256
                                          (nelisp-native-unit-development--bytes path)))
            (error "Native source changed while compiling"))
          (setq phase :stage)
          (let ((staged (nelisp-native-unit-stage artifact unit-id exports)))
            (if (not (eq (plist-get staged :status) 'staged)) staged
              (setq phase :publish)
              (unless (equal hash (secure-hash 'sha256
                                              (nelisp-native-unit-development--bytes path)))
                (nelisp-native-unit-discard (plist-get staged :candidate-id))
                (error "Native source changed while staging"))
              (append (plist-put (nelisp-native-unit-publish (plist-get staged :candidate-id))
                                 :source-sha256 hash)
                      (list :source path
                            :artifact artifact :snapshot snapshot)))))
      (error (list :status 'rejected :phase phase
                   :reason (error-message-string err)
                   :artifact artifact :snapshot snapshot)))))

(provide 'nelisp-native-unit-development)
