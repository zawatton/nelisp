;;; gui-daily-magit-profile.el --- Headless real Magit timing -*- lexical-binding: t; -*-
;; Reload only the observation harness; packages must already exist in the heap.
(when (fboundp 'nelisp--write-stdout-bytes)
  (nelisp-gui-packages-assert-image)
  (load (expand-file-name "../packages/nelisp-gui-xcb/fixtures/packages.el"
                          (file-name-directory load-file-name)) nil t))
(load (getenv "NELISP_GUI_PACKAGE_PROFILE_FUNCTIONS") nil t)
(if (fboundp 'nelisp--write-stdout-bytes)
    (nelisp-gui-packages-fixture)
  (setq native-comp-jit-compilation nil)
  (require 'comp nil t)
  (require 'json)
  (require 'cl-lib)
  (load (expand-file-name "../packages/nelisp-gui-xcb/fixtures/packages.el"
                          (file-name-directory load-file-name)) nil t)
  (load (expand-file-name "../packages/nelisp-gui-xcb/fixtures/skk-evil.el"
                          (file-name-directory load-file-name)) nil t)
  (nelisp-gui-packages-preload)
  (setq default-directory (concat (getenv "NELISP_GUI_PACKAGES_ROOT") "/repo/")
        nelisp-gui-packages-state-file (getenv "NELISP_GUI_PACKAGE_STATE")
        exec-path (cons (getenv "NELISP_GUI_PACKAGE_GIT_BIN") exec-path)
        magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1
        with-editor-emacsclient-executable nil)
  (dolist (function (delete-dups (append nelisp-gui-packages-profile-functions
                            magit-status-sections-hook magit-status-headers-hook
                            '(magit-status process-file call-process))))
    (when (fboundp function)
      (advice-add function :around
                  (apply-partially #'nelisp-gui-packages-trace function)))))
(let ((start (float-time)))
  (garbage-collect)
  (princ (format "GUI-MAGIT-GC|phase=before|seconds=%.6f|\n"
                 (- (float-time) start))))
(let ((start (float-time)) (failure nil) (seconds nil) (gc-seconds nil))
  (condition-case err
      (progn
        (magit-status (concat (getenv "NELISP_GUI_PACKAGES_ROOT") "/repo/"))
        (nelisp-gui-packages-observe))
    (error (setq failure err)))
  (setq seconds (- (float-time) start))
  (let ((gc-start (float-time)))
    (garbage-collect)
    (setq gc-seconds (- (float-time) gc-start)))
  (princ (format "GUI-MAGIT-GC|phase=after|seconds=%.6f|\n" gc-seconds))
  (princ (format "GUI-MAGIT-PROFILE|seconds=%.6f|error=%S|\n" seconds failure))
  (with-temp-file (concat (getenv "NELISP_GUI_PACKAGE_STATE") ".profile.json")
    (insert (json-encode `((seconds . ,seconds) (gc_after_seconds . ,gc-seconds)
                          (error . ,(format "%S" failure)))))))
