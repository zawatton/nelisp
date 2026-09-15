;;; nelisp-project-build.el --- Embedded application ELF -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Reuse the unmodified reader driver and content-addressed runtime units.
;; A small native entry adapter supplies --eval plus embedded application
;; source. Application forms are interpreted, not claimed to be AOT compiled.

;;; Code:

(require 'nelisp-standalone-build)

(defun nelisp-project-build-main ()
  "Build using environment paths provided by the project frontend."
  (condition-case err
      (progn
        (nelisp-standalone-build-application
         (getenv "NELISP_PROJECT_SOURCE")
         (getenv "NELISP_PROJECT_ENTRY")
         (getenv "NELISP_PROJECT_OUTPUT")
         (equal (getenv "NELISP_PROJECT_PROFILE") "release")
         (equal (getenv "NELISP_PROJECT_PROFILE") "profile")
         (let ((value (getenv "NELISP_PROJECT_DEBUG_SHA256")))
           (and value (not (equal value "")) value))
         (let ((value (getenv "NELISP_PROJECT_BUILD_CACHE")))
           (and value (not (equal value "")) value)))
        (kill-emacs 0))
    (error (message "nelisp build: %s" (error-message-string err))
           (kill-emacs 1))))

(provide 'nelisp-project-build)
;;; nelisp-project-build.el ends here
