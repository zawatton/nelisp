;;; emacs-fileio.el --- Interactive file I/O layer for nelisp-emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 02 v0.1 daily-driver §3.1 M1.
;;
;; Builds the interactive `find-file' / `save-buffer' / `write-file'
;; layer on top of the lower-level bridges in `emacs-fileio-builtins.el'.
;; The builtins module gives us file primitives plus a minimal visited-file
;; mapping; this module adds:
;;
;; - interactive minibuffer entry points
;; - same-file dedup/switch semantics
;; - per-buffer default-directory bookkeeping
;; - major-mode dispatch through a minimum `auto-mode-alist'
;; - global `C-x C-f' / `C-x C-s' / `C-x C-w' bindings

;;; Code:

(require 'cl-lib)
(require 'emacs-buffer)
(require 'emacs-buffer-builtins)
(require 'emacs-fileio-builtins)
(require 'emacs-fileio-gui)
(require 'emacs-keymap-builtins)
(require 'emacs-minibuffer-builtins)
(require 'emacs-mode-builtins)

(eval-and-compile
  ;; Host Emacs 30 native-comp can try to build subr trampolines when this
  ;; module wraps C primitives such as `rename-buffer'.  That path currently
  ;; fails before org-agenda tests can even load, while standalone NeLisp does
  ;; not need host trampolines.
  (when (boundp 'native-comp-enable-subr-trampolines)
    (setq native-comp-enable-subr-trampolines nil))
  (when (boundp 'comp-enable-subr-trampolines)
    (setq comp-enable-subr-trampolines nil)))

(defvar default-directory "/"
  "Current buffer's working directory.

In the standalone runtime this is not buffer-local yet, so this module
mirrors the value from `emacs-fileio--buffer-default-directories' when a
buffer becomes current via `find-file'.")

(defvar emacs-fileio--buffer-default-directories nil
  "Alist mapping live buffers to their `default-directory'.")

(defvar emacs-fileio--buffer-major-modes nil
  "Alist mapping live buffers to their chosen major-mode symbol.")

(defvar emacs-fileio--buffer-mode-names nil
  "Alist mapping live buffers to their `mode-name' string.")

(defvar emacs-fileio-auto-mode-alist
  '(("\\.el\\'" . emacs-lisp-mode)
    ("\\.org\\'" . org-mode))
  "Minimum file-extension → major-mode associations for M1.")

(defvar emacs-fileio--primitive-kill-buffer
  (and (fboundp 'kill-buffer) (symbol-function 'kill-buffer))
  "Underlying `kill-buffer' implementation captured before this module wraps it.")

(defvar emacs-fileio--primitive-rename-buffer
  (and (fboundp 'rename-buffer) (symbol-function 'rename-buffer))
  "Underlying `rename-buffer' implementation captured before this module wraps it.")

(defun emacs-fileio--buffer-live-p (buffer)
  "Return non-nil when BUFFER is live in either host or standalone mode."
  (cond
   ((and (fboundp 'nelisp-ec-buffer-p)
         (nelisp-ec-buffer-p buffer))
    (not (nelisp-ec-buffer-killed-p buffer)))
   ((and (fboundp 'buffer-live-p)
         (condition-case nil
             (buffer-live-p buffer)
           (error nil))))
   (t nil)))

(defun emacs-fileio--clean-side-tables ()
  "Drop killed buffers from this module's side tables."
  (let ((pred (lambda (cell)
                (emacs-fileio--buffer-live-p (car cell)))))
    (setq emacs-fileio--buffer-default-directories
          (cl-remove-if-not pred emacs-fileio--buffer-default-directories))
    (setq emacs-fileio--buffer-major-modes
          (cl-remove-if-not pred emacs-fileio--buffer-major-modes))
    (setq emacs-fileio--buffer-mode-names
          (cl-remove-if-not pred emacs-fileio--buffer-mode-names))))

(defun emacs-fileio--alist-set (table-symbol buffer value)
  "In TABLE-SYMBOL, set BUFFER's entry to VALUE and return VALUE."
  (set table-symbol
       (cons (cons buffer value)
             (assq-delete-all buffer (symbol-value table-symbol))))
  value)

(defun emacs-fileio--forget-buffer-state (buffer)
  "Forget all file I/O side-table state for BUFFER."
  (setq emacs-fileio--buffer-files
        (assq-delete-all buffer emacs-fileio--buffer-files))
  (setq emacs-fileio--buffer-default-directories
        (assq-delete-all buffer emacs-fileio--buffer-default-directories))
  (setq emacs-fileio--buffer-major-modes
        (assq-delete-all buffer emacs-fileio--buffer-major-modes))
  (setq emacs-fileio--buffer-mode-names
        (assq-delete-all buffer emacs-fileio--buffer-mode-names))
  buffer)

(defun emacs-fileio--buffer-name (buffer)
  "Return BUFFER's name across host and standalone modes."
  (cond
   ((and (fboundp 'nelisp-ec-buffer-p)
         (nelisp-ec-buffer-p buffer))
    (nelisp-ec-buffer-name buffer))
   ((and (fboundp 'buffer-name)
         (condition-case nil
             (buffer-name buffer)
           (error nil))))
   (t nil)))

(defun emacs-fileio--buffer-object (buffer-or-name)
  "Return a live buffer for BUFFER-OR-NAME, or nil."
  (cond
   ((null buffer-or-name) nil)
   ((emacs-fileio--buffer-live-p buffer-or-name) buffer-or-name)
   ((and (stringp buffer-or-name) (fboundp 'get-buffer))
    (get-buffer buffer-or-name))
   (t nil)))

(defun emacs-fileio--get-buffer-create (buffer-or-name)
  "Return a live buffer for BUFFER-OR-NAME, creating named buffers."
  (or (emacs-fileio--buffer-object buffer-or-name)
      (and (stringp buffer-or-name)
           (if (fboundp 'get-buffer-create)
               (get-buffer-create buffer-or-name)
             (generate-new-buffer buffer-or-name)))))

(defun emacs-fileio--registry-buffer-name-in-use-p (name &optional except)
  "Return non-nil when NAME is used by a live buffer other than EXCEPT."
  (catch 'found
    (dolist (buffer (buffer-list))
      (when (and (not (eq buffer except))
                 (emacs-fileio--buffer-live-p buffer)
                 (equal name (emacs-fileio--buffer-name buffer)))
        (throw 'found t)))
    nil))

(defun emacs-fileio--generate-buffer-name (name &optional except)
  "Return a buffer name based on NAME that is not used, ignoring EXCEPT."
  (if (not (emacs-fileio--registry-buffer-name-in-use-p name except))
      name
    (let ((n 2)
          (candidate ""))
      (setq candidate (format "%s<%d>" name n))
      (while (emacs-fileio--registry-buffer-name-in-use-p candidate except)
        (setq n (1+ n))
        (setq candidate (format "%s<%d>" name n)))
      candidate)))

(defun emacs-fileio--rename-standalone-buffer (buffer name unique)
  "Rename standalone BUFFER to NAME, uniquifying when UNIQUE is non-nil."
  (let* ((old-name (nelisp-ec-buffer-name buffer))
         (new-name (if unique
                       (emacs-fileio--generate-buffer-name name buffer)
                     name)))
    (when (and (not unique)
               (emacs-fileio--registry-buffer-name-in-use-p new-name buffer))
      (signal 'error (list "Buffer name is in use" new-name)))
    (setq nelisp-ec--buffers
          (cons (cons new-name buffer)
                (assoc-delete-all old-name nelisp-ec--buffers)))
    (nelisp-ec-buffer-name--setter buffer new-name)
    new-name))

(defun emacs-fileio--buffer-default-directory (&optional buffer)
  "Return BUFFER's recorded default directory, or nil."
  (cdr (assq (or buffer (current-buffer))
             emacs-fileio--buffer-default-directories)))

(defun emacs-fileio--visited-file-name (&optional buffer)
  "Return BUFFER's visited filename across host and standalone modes."
  (let ((buf (or buffer (current-buffer))))
    (or (cdr (assq buf emacs-fileio--buffer-files))
        (condition-case nil
            (buffer-file-name buf)
          (error nil)))))

(defun emacs-fileio-buffer-file-name (&optional buffer)
  "Return BUFFER's visited filename.
Thin helper so callers do not need to know the builtins' state table."
  (emacs-fileio--visited-file-name buffer))

(defun emacs-fileio--remember-default-directory (buffer filename)
  "Record BUFFER's `default-directory' from FILENAME."
  (let ((dir (or (file-name-directory filename) default-directory "/")))
    (emacs-fileio--alist-set 'emacs-fileio--buffer-default-directories
                             buffer
                             (file-name-as-directory dir))))

(defun emacs-fileio--set-major-mode-state (buffer mode)
  "Remember BUFFER's MODE and current `mode-name'."
  (emacs-fileio--alist-set 'emacs-fileio--buffer-major-modes buffer mode)
  (emacs-fileio--alist-set 'emacs-fileio--buffer-mode-names
                           buffer
                           (if (boundp 'mode-name) mode-name "Fundamental")))

(defun emacs-fileio--resolve-major-mode (filename)
  "Resolve the major-mode symbol for FILENAME."
  (let ((alist (append emacs-fileio-auto-mode-alist
                       (or auto-mode-alist nil)))
        (match nil))
    (catch 'done
      (dolist (cell alist)
        (when (and (stringp (car cell))
                   (string-match (car cell) filename))
          (setq match (cdr cell))
          (throw 'done match))))
    (cond
     ((and (eq match 'org-mode) (not (fboundp 'org-mode)))
      'fundamental-mode)
     ((and match (fboundp match))
      match)
     (t 'fundamental-mode))))

(defun emacs-fileio--remember-major-mode (buffer mode)
  "Remember BUFFER's MODE without activating it."
  (emacs-fileio--alist-set 'emacs-fileio--buffer-major-modes buffer mode)
  mode)

(defun emacs-fileio--activate-major-mode (buffer &optional filename)
  "Activate BUFFER's major mode for FILENAME and remember it."
  (with-current-buffer buffer
    (let ((mode (or (cdr (assq buffer emacs-fileio--buffer-major-modes))
                    (and filename (emacs-fileio--resolve-major-mode filename))
                    (and (boundp 'major-mode) major-mode)
                    'fundamental-mode)))
      (when (fboundp mode)
        (funcall mode))
      (emacs-fileio--set-major-mode-state buffer mode)
      mode)))

(defun emacs-fileio--apply-buffer-state (buffer)
  "Make BUFFER current and mirror its recorded state globally."
  (set-buffer buffer)
  (let ((dir (emacs-fileio--buffer-default-directory buffer)))
    (when dir
      (setq default-directory dir)))
  (emacs-fileio--activate-major-mode buffer))

(defun emacs-fileio--find-existing-buffer (filename)
  "Return the live buffer already visiting FILENAME, or nil."
  (emacs-fileio--clean-killed)
  (emacs-fileio--clean-side-tables)
  (or
   (catch 'found
     (dolist (cell emacs-fileio--buffer-files)
       (when (and (equal filename (cdr cell))
                  (emacs-fileio--buffer-live-p (car cell)))
         (throw 'found (car cell))))
     nil)
   (catch 'found
     (dolist (buf (buffer-list))
       (let ((visited (emacs-fileio--visited-file-name buf)))
         (when (and visited
                    (equal filename (expand-file-name visited)))
           (throw 'found buf))))
     nil)))

(defun emacs-fileio--prepare-buffer-name (filename)
  "Return a sensible buffer name for FILENAME."
  (let ((name (file-name-nondirectory filename)))
    (if (and name (> (length name) 0))
        name
      " *find-file*")))

(defun emacs-fileio--ensure-global-bindings ()
  "Install the M1 file I/O bindings into the global map."
  (let ((map (or (and (fboundp 'current-global-map) (current-global-map))
                 (and (fboundp 'make-sparse-keymap) (make-sparse-keymap)))))
    (when (and map (fboundp 'use-global-map))
      (use-global-map map)
      (define-key map (kbd "C-x C-f") #'find-file)
      (define-key map (kbd "C-x C-s") #'save-buffer)
      (define-key map (kbd "C-x C-w") #'write-file))))

;;;###autoload
(defun switch-to-buffer (buffer-or-name &optional norecord force-same-window)
  "Make BUFFER-OR-NAME current and return the selected buffer."
  (interactive
   (list (if (fboundp 'read-buffer)
             (read-buffer "Switch to buffer: "
                          (and (fboundp 'buffer-name)
                               (buffer-name)))
           "*scratch*")))
  (ignore norecord force-same-window)
  (let ((buffer (emacs-fileio--get-buffer-create
                 (or buffer-or-name "*scratch*"))))
    (unless buffer
      (signal 'error (list "No buffer selected")))
    (emacs-fileio--apply-buffer-state buffer)
    buffer))

;;;###autoload
(defun rename-buffer (newname &optional unique)
  "Rename the current buffer to NEWNAME and return NEWNAME.
When UNIQUE is non-nil, append a numeric suffix if NEWNAME is already
in use."
  (interactive "sRename buffer: ")
  (let ((buffer (current-buffer)))
    (cond
     ((and (fboundp 'nelisp-ec-buffer-p)
           (nelisp-ec-buffer-p buffer))
      (emacs-fileio--rename-standalone-buffer buffer newname unique))
     (emacs-fileio--primitive-rename-buffer
      (funcall emacs-fileio--primitive-rename-buffer newname unique))
     (t
      (signal 'error '("rename-buffer is unavailable"))))))

;;;###autoload
(defun kill-buffer (&optional buffer-or-name)
  "Kill BUFFER-OR-NAME, defaulting to the current buffer."
  (interactive)
  (let* ((target (or (emacs-fileio--buffer-object buffer-or-name)
                     (current-buffer))))
    (unless target
      (signal 'error '("No buffer to kill")))
    (emacs-fileio--forget-buffer-state target)
    (cond
     ((and (fboundp 'nelisp-ec-buffer-p)
           (nelisp-ec-buffer-p target)
           (fboundp 'nelisp-ec-kill-buffer))
      (nelisp-ec-kill-buffer target)
      (when (and (null (current-buffer))
                 (buffer-list))
        (set-buffer (car (buffer-list))))
      t)
     (emacs-fileio--primitive-kill-buffer
      (funcall emacs-fileio--primitive-kill-buffer target))
     (t
      (signal 'error '("kill-buffer is unavailable"))))))

;;;###autoload
(defun kill-buffer-and-window ()
  "Kill the current buffer and delete its selected window when possible."
  (interactive)
  (let ((result (kill-buffer (current-buffer))))
    (when (and (fboundp 'delete-window)
               (condition-case nil
                   (not (one-window-p))
                 (error nil)))
      (delete-window))
    result))

;;;###autoload
(defun list-buffers (&optional files-only)
  "Create and select a `*Buffer List*' buffer.
FILES-ONLY limits rows to buffers visiting files."
  (interactive "P")
  (let ((current (current-buffer))
        (out "Buffer\tFile\n"))
    (dolist (buffer (buffer-list))
      (when (emacs-fileio--buffer-live-p buffer)
        (let ((file (or (emacs-fileio-buffer-file-name buffer) ""))
              (name (or (emacs-fileio--buffer-name buffer) "")))
          (when (or (not files-only) (not (equal file "")))
            (setq out
                  (concat out
                          (if (eq buffer current) "* " "  ")
                          name
                          "\t"
                          file
                          "\n"))))))
    (let ((buffer (emacs-fileio--get-buffer-create "*Buffer List*")))
      (with-current-buffer buffer
        (erase-buffer)
        (insert out)
        (set-buffer-modified-p nil))
      (switch-to-buffer buffer)
      buffer)))

;;;###autoload
(defun find-file-noselect (filename &optional nowarn rawfile wildcards)
  "Return a buffer visiting FILENAME, without selecting it."
  (ignore nowarn rawfile wildcards)
  (let* ((abs (expand-file-name filename))
         (existing (emacs-fileio--find-existing-buffer abs)))
    (if existing
        existing
      (let ((buf (generate-new-buffer
                  (emacs-fileio--prepare-buffer-name abs))))
        (with-current-buffer buf
          (when (file-exists-p abs)
            (insert-file-contents abs))
          (set-visited-file-name abs)
          (emacs-fileio--remember-default-directory buf abs)
          (emacs-fileio--remember-major-mode
           buf
           (emacs-fileio--resolve-major-mode abs))
          (set-buffer-modified-p nil))
        buf))))

;;;###autoload
(defun find-file (filename &optional wildcards)
  "Visit FILENAME in the current window and return its buffer."
  (interactive
   (list (read-file-name "Find file: " default-directory nil nil)
         nil))
  (ignore wildcards)
  (let ((buf (find-file-noselect filename)))
    (emacs-fileio--apply-buffer-state buf)
    buf))

;;;###autoload
(defun save-buffer (&optional arg)
  "Write the current buffer to its visited file.
When the buffer is unmodified, emit a message and do nothing."
  (interactive "P")
  (ignore arg)
  (let ((filename (buffer-file-name)))
    (cond
     ((null filename)
      (signal 'error '("save-buffer: buffer is not visiting a file")))
     ((not (buffer-modified-p))
      (message "(No changes need to be saved)")
      nil)
     (t
      (write-region (point-min) (point-max) filename nil nil)
      (set-buffer-modified-p nil)
      filename))))

;;;###autoload
(defun write-file (filename &optional confirm)
  "Write the current buffer to FILENAME and visit it."
  (interactive
   (list (read-file-name "Write file: "
                         (or default-directory "/")
                         nil nil
                         (buffer-name))))
  (ignore confirm)
  (let ((abs (expand-file-name filename))
        (buf (current-buffer)))
    (set-visited-file-name abs)
    (emacs-fileio--remember-default-directory buf abs)
    (setq default-directory
          (emacs-fileio--buffer-default-directory buf))
    (emacs-fileio--activate-major-mode buf abs)
    (set-buffer-modified-p t)
    (save-buffer)
    abs))

(emacs-fileio--ensure-global-bindings)

(provide 'emacs-fileio)

;;; emacs-fileio.el ends here
