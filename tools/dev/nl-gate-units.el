;;; nl-gate-units.el --- record the first GU_MAX unit cache events  -*- lexical-binding: t; -*-
;; Loaded into the real reader build.  Appends one TSV row per unit that was
;; compiled (miss) or read (hit): KIND NAME KEY PAYLOAD-SHA256 CLOSURE, then
;; stops the process once GU_MAX rows exist.  No link, no bake, no full build.
(require 'nelisp-standalone-build)
(defvar nlgu--out (getenv "GU_OUT"))
(defvar nlgu--max (string-to-number (or (getenv "GU_MAX") "1")))
(defvar nlgu--n 0)
(defun nlgu--row (kind file key payload closure)
  (let ((name (replace-regexp-in-string "\\(\\.[0-9a-f]+\\)?\\.unit\\'" "" (file-name-nondirectory file))))
    (write-region (format "%s\t%s\t%s\t%s\t%s\n" kind name key (secure-hash 'sha256 payload) (or closure "-"))
                  nil nlgu--out t 'silent)
    (when (>= (setq nlgu--n (1+ nlgu--n)) nlgu--max) (kill-emacs 0))))
(advice-add 'nelisp-standalone-cachekey-entry-write :after
            (lambda (file key payload &optional closure) (nlgu--row "miss" file key payload closure)))
(advice-add 'nelisp-standalone-cachekey-entry-read :around
            (lambda (orig file key)
              (let ((r (funcall orig file key)))
                (when (car r)
                  (nlgu--row "hit" file key (cdr r)
                             (plist-get (nelisp-standalone-cachekey-entry-header file) :closure)))
                r)))
