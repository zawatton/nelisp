;;; calendar.el --- Calendar loader for NeLisp  -*- lexical-binding: t; -*-

;;; Code:

;; Standalone source normalization drops top-level `easy-menu-define' UI
;; wiring.  GNU calendar.el nevertheless passes these cal-menu variables to
;; `easy-menu-binding' while constructing `calendar-mode-map'.  Keep them
;; bound to a harmless placeholder before the vendor load; a full cal-menu
;; implementation can still replace them because `defvar' does not overwrite
;; an existing value.
(defvar cal-menu-context-mouse-menu nil)
(defvar cal-menu-global-mouse-menu nil)

(defun calendar--standalone-runtime-p ()
  "Return non-nil on the standalone NeLisp reader."
  (or (not (boundp 'emacs-version))
      (fboundp 'nl-write-file)
      (fboundp 'nl-syscall-write-file)
      (fboundp 'nelisp--eval-source-string)))

(defun calendar--vendor-file ()
  "Return the vendored `calendar.el' path for standalone loads, or nil."
  (let* ((root (or (and (boundp 'nelisp-emacs-vendor-root)
                        nelisp-emacs-vendor-root)
                   (let ((here (or (and (boundp 'load-file-name) load-file-name)
                                   (and (boundp 'buffer-file-name) buffer-file-name))))
                     (and here
                          (expand-file-name "../vendor"
                                            (file-name-directory here))))))
         (file (and root
                    (expand-file-name "emacs-lisp/calendar/calendar.el"
                                      root))))
    (and file (file-readable-p file) file)))

(defun calendar--host-load-standard ()
  "Load the vendored `calendar' library on host Emacs when available.
This keeps bootstrap and runtime-image host-side builds from capturing
whatever system `calendar.el' happens to be installed on the machine."
  (let ((vendor-file (calendar--vendor-file)))
    (if vendor-file
        (load vendor-file nil t)
      (let ((shim-dir (expand-file-name
                       (file-name-as-directory
                        (file-name-directory
                         (or (and (boundp 'load-file-name) load-file-name)
                             (and (boundp 'buffer-file-name) buffer-file-name)
                             default-directory)))))
            filtered)
        (dolist (dir load-path)
          (unless (equal (expand-file-name (file-name-as-directory dir))
                         shim-dir)
            (push dir filtered)))
        (let ((load-path (nreverse filtered)))
          (load "calendar" nil t))))))

(if (calendar--standalone-runtime-p)
    (let ((file (calendar--vendor-file)))
      (when file
        (require 'emacs-keymap-builtins)
        (require 'emacs-faces-builtins)
        (require 'emacs-mode-builtins)
        ;; Add the vendored `calendar/' directory to `load-path' globally
        ;; rather than binding it around the load.  GNU `calendar.el' does
        ;; `(load "cal-loaddefs" nil t)' at top level, and that sibling has to
        ;; be findable from inside the nested load.  A `let' cannot do it on
        ;; the standalone reader: `load-path' is bound there but not
        ;; `special-variable-p', so the binding is lexical and the loader,
        ;; which reads the global value, never sees the added directory --
        ;; measured 2026-09-04, the nested load failed with
        ;; (file-missing "cal-loaddefs") while `load-path' printed inside the
        ;; `let' did contain the directory.  `src/nemacs-tramp.el' adds the
        ;; same directory the same way, for the same reason.
        (let ((dir (file-name-directory file)))
          (unless (member dir load-path)
            (setq load-path (cons dir load-path))))
        (load file nil t)))
  (calendar--host-load-standard))

;;; calendar.el ends here
