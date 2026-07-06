;;; emacs-startup-screen.el --- Startup splash screen semantics  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Editor-semantics owner for the startup splash screen (the `*GNU Emacs*'
;; welcome buffer shown by Emacs when no init file and no file arguments
;; are present).  The buffer contents are drafted for nemacs with reference
;; to `normal-splash-screen' in `vendor/emacs-lisp/startup.el' (read-only
;; vendor reference; no vendor text is copied).
;;
;; This module owns:
;;   - the splash buffer name and body text,
;;   - the display gate (`emacs-startup-screen-use-p'), and
;;   - buffer creation (`emacs-startup-screen-create').
;;
;; It does not select the buffer or render anything; making the splash
;; buffer current is the caller's decision (`nemacs-loadup''s splash step
;; for the interactive bootstrap, `nemacs-next-session' for protocol
;; frontends).  For a future GUI frontend,
;; `emacs-startup-screen-image-path' reports the vendored splash image
;; asset without drawing it.

;;; Code:

(require 'emacs-buffer-builtins)
(require 'emacs-buffer)

(defvar inhibit-startup-screen)
(defvar user-init-file)
(defvar buffer-read-only)
(defvar inhibit-read-only)

(defgroup emacs-startup-screen nil
  "Startup splash screen for the nemacs runtime."
  :group 'initialization
  :prefix "emacs-startup-screen-")

(defconst emacs-startup-screen-buffer-name "*GNU Emacs*"
  "Name of the startup splash buffer (Emacs-compatible).")

(defcustom emacs-startup-screen-text
  "Welcome to nemacs, an Emacs-compatible editor runtime.

Get help          C-h        (Hold down CTRL and press h)
Exit nemacs       C-x C-c    Undo changes      C-x u
Find a file       C-x C-f    Save a file       C-x C-s
Switch buffer     C-x b      Run a command     M-x

(`C-' means use the CTRL key.  `M-' means use the Meta (or Alt) key.
If you have no Meta key, you may instead type ESC followed by the
character.)

Useful tasks:
  Visit a new file          C-x C-f, then type a new file name
  Browse a directory        M-x dired RET
  Switch to *scratch*       C-x b *scratch* RET

nemacs comes with ABSOLUTELY NO WARRANTY.  It is free software, and
you are welcome to redistribute it under the GNU General Public License.
"
  "Body text inserted into the startup splash buffer.
The about line appended by `emacs-startup-screen-create' reports the
runtime version separately; this text stays version-neutral."
  :type 'string
  :group 'emacs-startup-screen)

(defvar emacs-startup-screen-file-arguments nil
  "CLI file arguments visible to the splash gate.
`nemacs-main--apply-startup-gate' reflects the `:args' options-plist key
here before `nemacs-init' runs; a non-nil value suppresses the splash
screen just like invoking Emacs with file arguments does.")

(defconst emacs-startup-screen-image-names
  '("splash.svg" "splash.png" "splash.pbm" "splash.xpm")
  "Vendored splash image file names in preference order.
The assets live under `vendor/emacs-etc/images/' (vendored from GNU
Emacs 30.1 etc/images).  TUI frontends use the text splash; a future
GUI frontend may resolve one of these through
`emacs-startup-screen-image-path'.")

(defconst emacs-startup-screen--source-directory
  (and (boundp 'load-file-name)
       (stringp load-file-name)
       (fboundp 'file-name-directory)
       (file-name-directory load-file-name))
  "Directory this module was loaded from (src/), or nil.")

(defun emacs-startup-screen-image-path (&optional directory)
  "Return the preferred existing splash image path, or nil.
DIRECTORY overrides the default `vendor/emacs-etc/images/' lookup root,
which is resolved relative to this module's own source directory when
known.  This helper only reports the asset path; it never renders the
image."
  (let ((dir (or directory
                 (and emacs-startup-screen--source-directory
                      (concat emacs-startup-screen--source-directory
                              "../vendor/emacs-etc/images/"))
                 "vendor/emacs-etc/images/"))
        found)
    (when (fboundp 'file-exists-p)
      (dolist (name emacs-startup-screen-image-names)
        (let ((path (concat (file-name-as-directory dir) name)))
          (when (and (not found) (file-exists-p path))
            (setq found path)))))
    found))

(defun emacs-startup-screen-use-p (&optional file-args)
  "Return non-nil when the startup splash screen should be shown.
The splash is suppressed when any of the following holds:
  - `inhibit-startup-screen' is non-nil (`-Q' / `--no-splash'),
  - a user init file was loaded (`user-init-file' non-nil),
  - FILE-ARGS or `emacs-startup-screen-file-arguments' is non-nil
    (the session starts on a visited file instead).
All globals are consulted through `boundp' so the gate is usable on
substrates that never loaded `nemacs-loadup'."
  (and (not (and (boundp 'inhibit-startup-screen) inhibit-startup-screen))
       (not (and (boundp 'user-init-file) user-init-file))
       (not (or file-args emacs-startup-screen-file-arguments))))

(defun emacs-startup-screen--core-substrate-p ()
  "Return non-nil when the NeLisp core buffer substrate should be used.
Matches `nemacs--ensure-scratch-buffer''s substrate choice: whenever the
`nelisp-ec-*' compat layer is loaded, the splash buffer must live in the
same registry as the bootstrap's *scratch* buffer (`nelisp-ec-set-buffer'
only accepts that substrate's buffer objects)."
  (and (fboundp 'nelisp-ec-generate-new-buffer)
       (fboundp 'nelisp-ec-insert)))

(defun emacs-startup-screen--about-line ()
  "Return the about line appended below `emacs-startup-screen-text'."
  (concat "\nThis is nemacs"
          (if (and (boundp 'nemacs-version)
                   (stringp (symbol-value 'nemacs-version)))
              (concat " " (symbol-value 'nemacs-version))
            "")
          ", an Emacs-compatible runtime on the NeLisp substrate.\n"))

(defun emacs-startup-screen--contents ()
  "Return the full splash buffer contents."
  (concat emacs-startup-screen-text (emacs-startup-screen--about-line)))

(defun emacs-startup-screen-buffer ()
  "Return the existing splash buffer, or nil when absent."
  (cond
   ((and (emacs-startup-screen--core-substrate-p)
         (boundp 'nelisp-ec--buffers))
    (cdr (assoc emacs-startup-screen-buffer-name
                (symbol-value 'nelisp-ec--buffers))))
   ((fboundp 'get-buffer)
    (get-buffer emacs-startup-screen-buffer-name))
   (t nil)))

(defun emacs-startup-screen--standalone-p ()
  "Return non-nil when running under the standalone NeLisp reader."
  (or (fboundp 'nl-write-file)
      (not (boundp 'emacs-version))))

(defun emacs-startup-screen--create-core ()
  "Create or refresh the splash buffer on the NeLisp core substrate.
Read-only marking follows the substrate's MVP convention (same as
`emacs-special-buffers'): the global `buffer-read-only' flag describes
the current buffer, so it is only set on the standalone reader, where
`emacs-startup-screen-select' keeps it aligned with the splash being
current.  Under host Emacs the compat substrate has no per-buffer
read-only cell, and touching the host-global flag would leak onto
unrelated host buffers."
  (let ((buf (or (emacs-startup-screen-buffer)
                 (nelisp-ec-generate-new-buffer
                  emacs-startup-screen-buffer-name))))
    (nelisp-ec-with-current-buffer buf
      (let ((buffer-read-only nil)
            (inhibit-read-only t))
        (when (fboundp 'nelisp-ec-erase-buffer)
          (nelisp-ec-erase-buffer))
        (nelisp-ec-insert (emacs-startup-screen--contents)))
      (when (fboundp 'nelisp-ec-goto-char)
        (nelisp-ec-goto-char 1))
      (when (emacs-startup-screen--standalone-p)
        (setq buffer-read-only t))
      (when (fboundp 'emacs-buffer-set-buffer-modified-p)
        (emacs-buffer-set-buffer-modified-p nil buf)))
    buf))

(defun emacs-startup-screen--create-host ()
  "Create or refresh the splash buffer using host Emacs buffers."
  (let ((buf (get-buffer-create emacs-startup-screen-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (emacs-startup-screen--contents)))
      (goto-char (point-min))
      (setq buffer-read-only t)
      (when (fboundp 'set-buffer-modified-p)
        (set-buffer-modified-p nil)))
    buf))

;;;###autoload
(defun emacs-startup-screen-create ()
  "Create (or refresh) the startup splash buffer and return it.
The buffer is named `emacs-startup-screen-buffer-name', filled from
`emacs-startup-screen-text' plus an about line, made read-only, and left
with point at the beginning.  The current buffer is not changed; callers
decide whether to select the splash buffer (see
`emacs-startup-screen-select').  Returns nil when no buffer substrate is
available."
  (cond
   ((emacs-startup-screen--core-substrate-p)
    (emacs-startup-screen--create-core))
   ((fboundp 'get-buffer-create)
    (emacs-startup-screen--create-host))
   (t nil)))

;;;###autoload
(defun emacs-startup-screen-select (&optional buffer)
  "Make the splash BUFFER current and return it.
BUFFER defaults to a fresh `emacs-startup-screen-create'.  On the
standalone NeLisp substrate the global `buffer-read-only' flag
describes the current buffer (same MVP fiction the bootstrap uses for
*scratch*), so selecting the splash also sets it non-nil there.
Returns nil when no buffer substrate is available."
  (let ((buf (or buffer (emacs-startup-screen-create))))
    (when buf
      (cond
       ((and (emacs-startup-screen--core-substrate-p)
             (fboundp 'nelisp-ec-set-buffer))
        (nelisp-ec-set-buffer buf)
        (when (emacs-startup-screen--standalone-p)
          (setq buffer-read-only t)))
       ((fboundp 'set-buffer)
        (set-buffer buf))))
    buf))

(provide 'emacs-startup-screen)

;;; emacs-startup-screen.el ends here
