;;; emacs-file-name-handler.el --- find-file-name-handler dispatch substrate  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 37 (2026-07) -- Layer 2 (IO/runtime adapters).
;;
;; Promotes the `find-file-name-handler' dispatch logic that previously
;; lived only inside `emacs-process.el' (the `process-file' remote-dispatch
;; hook point shipped for Doc 36) into a small standalone module every
;; file-I/O primitive can share.  This is the shared substrate the
;; ssh-only Tramp lane (task #16) and Magit's `process-file' path
;; (task #17) both depend on.
;;
;; Under host Emacs, `find-file-name-handler' is a real C primitive and
;; `file-name-handler-alist' is the genuine variable Tramp/jka-compr/etc.
;; already populate.  Lookups use the public operation on both hosts.
;; Standalone bridges use the dispatch wrappers for the operations whose
;; underlying `nelisp-ec-*' function is local-only.
;;
;; Public surface:
;;   - `emacs-fnh-find-file-name-handler' (FILENAME OPERATION)
;;   - `emacs-fnh-dispatch' (OPERATION LOCAL-FUNCTION &rest ARGS)
;;   - `emacs-fnh-wrap' (OPERATION LOCAL-FUNCTION) -> closure
;;   - handler-aware standalone fallbacks for `file-remote-p',
;;     `file-local-name', and `unhandled-file-name-directory'.

;; Shim audit 2026-09-29: intentionally shadows native NeLisp definitions -- file-remote-p dispatches through the library handler table.
;;; Code:

(defvar file-name-handler-alist nil)

(defun emacs-fnh-find-file-name-handler (filename operation)
  "Return FILENAME's file name handler for OPERATION."
  (find-file-name-handler filename operation))

(defun emacs-fnh-dispatch (operation local-function &rest args)
  "Run OPERATION, honouring a `file-name-handler-alist' match on (car ARGS).
When a handler matches the first argument (the FILENAME/DIRECTORY
convention shared by the classic Emacs magic-file-name operations),
apply it as `(HANDLER OPERATION . ARGS)' -- the Tramp/`files.el' calling
convention.  Otherwise apply LOCAL-FUNCTION to ARGS unchanged."
  (let ((handler (emacs-fnh-find-file-name-handler (car args) operation)))
    (if handler
        (apply handler operation args)
      (apply local-function args))))

(defun emacs-fnh-wrap (operation local-function)
  "Return a closure implementing OPERATION with handler dispatch.
The closure inspects its first argument for a `file-name-handler-alist'
match before falling back to LOCAL-FUNCTION; see `emacs-fnh-dispatch'."
  (lambda (&rest args)
    (apply #'emacs-fnh-dispatch operation local-function args)))

;;;; --- handler-aware standalone fallbacks -----------------------------
;;
;; These three operations are not in `emacs-fileio-builtins.el's batch
;; because they have no `nelisp-ec-*' local counterpart to wrap -- Emacs
;; defines all three as pure handler-dispatch functions with a trivial
;; "assume local" default.  An earlier module (`emacs-stub.el') may
;; already have bound an always-nil `file-remote-p' stub; the standalone
;; check below force-installs the handler-aware version over that stub
;; the same way `emacs-fileio-builtins--standalone-overrides' does for
;; the fileio primitives, so a Tramp name is recognised once Tramp has
;; registered its handler.

(defun emacs-fnh-file-remote-p (file &optional identification connected)
  "Standalone `file-remote-p': ask FILE's handler, default nil (local)."
  (let ((handler (emacs-fnh-find-file-name-handler file 'file-remote-p)))
    (if handler
        (funcall handler 'file-remote-p file identification connected)
      nil)))

(defun emacs-fnh-file-local-name (file)
  "Standalone `file-local-name': strip FILE's handler prefix, if any."
  (let ((handler (emacs-fnh-find-file-name-handler file 'file-local-name)))
    (if handler
        (funcall handler 'file-local-name file)
      file)))

(defun emacs-fnh-unhandled-file-name-directory (filename)
  "Return a directly usable directory name associated with FILENAME.
Ask FILENAME's handler when present; a non-string answer means that no
local directory is available.  Otherwise convert FILENAME to directory
syntax, honoring `file-name-as-directory' handlers."
  (unless (stringp filename)
    (signal 'wrong-type-argument (list 'stringp filename)))
  (let ((handler (find-file-name-handler
                  filename 'unhandled-file-name-directory)))
    (if handler
        (let ((directory
               (funcall handler 'unhandled-file-name-directory filename)))
          (and (stringp directory) directory))
      (let ((directory-handler
             (find-file-name-handler filename 'file-name-as-directory)))
        (if directory-handler
            (let ((directory
                   (funcall directory-handler 'file-name-as-directory filename)))
              (unless (stringp directory)
                (error "Invalid handler in ‘file-name-handler-alist’"))
              directory)
          ;; Keep this local conversion independent of the file-I/O shim's
          ;; handler lookup and empty-name behavior.
          (cond
           ((= (length filename) 0) "./")
           ((eq (aref filename (1- (length filename))) ?/) filename)
           (t (concat filename "/"))))))))

(defun emacs-fnh--standalone-p ()
  "Return non-nil when running on the standalone NeLisp reader.
Mirrors `emacs-fileio-builtins--standalone-p': a bound `emacs-version'
is unreliable (the reader binds it to a sentinel), so this prefers
`files-standalone-runtime-p' when it is loaded and otherwise falls back
to the `emacs-version'-unbound heuristic."
  (if (fboundp 'files-standalone-runtime-p)
      (files-standalone-runtime-p)
    (not (boundp 'emacs-version))))

(unless (fboundp 'file-remote-p)
  (defalias 'file-remote-p #'emacs-fnh-file-remote-p))
(unless (fboundp 'file-local-name)
  (defalias 'file-local-name #'emacs-fnh-file-local-name))
(unless (fboundp 'unhandled-file-name-directory)
  (defalias 'unhandled-file-name-directory
    #'emacs-fnh-unhandled-file-name-directory))

(when (emacs-fnh--standalone-p)
  ;; Force-install over any earlier always-nil/always-local stub.
  (defalias 'file-remote-p #'emacs-fnh-file-remote-p)
  (defalias 'file-local-name #'emacs-fnh-file-local-name)
  (defalias 'unhandled-file-name-directory
    #'emacs-fnh-unhandled-file-name-directory))

(defun emacs-fnh-install-quoting ()
  "Register the GNU files.el handler for explicitly quoted names."
  (when (and (emacs-fnh--standalone-p) (fboundp 'file-name-non-special))
    (add-to-list 'file-name-handler-alist '("\\`/:" . file-name-non-special) t)))

(provide 'emacs-file-name-handler)

;;; emacs-file-name-handler.el ends here
