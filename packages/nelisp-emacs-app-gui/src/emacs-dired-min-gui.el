;;; emacs-dired-min-gui.el --- GUI bridge Dired adapter  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Lightweight GUI bridge adapter for Dired commands.  The backend owns
;; transport stores; this module owns the Emacs-facing Dired command
;; sequencing.  It intentionally has no heavy runtime requires so it can be
;; baked into the standalone GUI bridge image.

;;; Code:

(defvar emacs-dired-min-gui-backend nil
  "PLIST of GUI bridge Dired backend callbacks.
The callbacks own bridge transport stores; this module owns the
Dired command sequencing used by GUI bridge commands.")

(defvar emacs-dired-min-gui-directory ""
  "Directory argument supplied by the GUI bridge Dired command.")

(defvar emacs-dired-min-gui-target ""
  "Target file used by GUI bridge `dired-jump'.")

(defvar emacs-dired-min-gui-current-file-name ""
  "Current file name supplied by the GUI bridge Dired command.")

(defvar emacs-dired-min-gui-status "ok"
  "Status string returned by the GUI bridge Dired command.")

(defvar emacs-dired-min-gui-buffer-name ""
  "Buffer name reported by the GUI bridge Dired backend.")

;;;###autoload
(defun emacs-dired-min-gui-register-backend (&rest backend)
  "Register BACKEND as the GUI bridge Dired adapter.
BACKEND is a plist.  Passing nil clears the adapter."
  (setq emacs-dired-min-gui-backend backend))

(defun emacs-dired-min-gui--backend-call (key &rest args)
  "Call GUI Dired backend function KEY with ARGS, if registered."
  (let ((fn (and emacs-dired-min-gui-backend
                 (plist-get emacs-dired-min-gui-backend key))))
    (when fn
      (apply fn args))))

(defun emacs-dired-min-gui--backend-function (key)
  "Return GUI Dired backend function KEY, or nil."
  (and emacs-dired-min-gui-backend
       (plist-get emacs-dired-min-gui-backend key)))

;;;###autoload
(defun emacs-dired-min-gui-set-context (&rest plist)
  "Update GUI bridge Dired context from PLIST.
Recognized keys are `:directory', `:target', `:current-file-name',
`:status', and `:buffer-name'."
  (when (plist-member plist :directory)
    (setq emacs-dired-min-gui-directory (plist-get plist :directory)))
  (when (plist-member plist :target)
    (setq emacs-dired-min-gui-target (plist-get plist :target)))
  (when (plist-member plist :current-file-name)
    (setq emacs-dired-min-gui-current-file-name
          (plist-get plist :current-file-name)))
  (when (plist-member plist :status)
    (setq emacs-dired-min-gui-status (plist-get plist :status)))
  (when (plist-member plist :buffer-name)
    (setq emacs-dired-min-gui-buffer-name (plist-get plist :buffer-name)))
  plist)

;;;###autoload
(defun emacs-dired-min-gui-refresh-context-from-backend ()
  "Refresh GUI bridge Dired context from registered backend callbacks."
  (let ((directory (emacs-dired-min-gui--backend-function :current-directory))
        (target (emacs-dired-min-gui--backend-function :current-target))
        (current-file-name
         (emacs-dired-min-gui--backend-function :current-file-name))
        (status (emacs-dired-min-gui--backend-function :current-status))
        (buffer-name (emacs-dired-min-gui--backend-function :buffer-name)))
    (when directory
      (setq emacs-dired-min-gui-directory (funcall directory)))
    (when target
      (setq emacs-dired-min-gui-target (funcall target)))
    (when current-file-name
      (setq emacs-dired-min-gui-current-file-name
            (funcall current-file-name)))
    (when status
      (setq emacs-dired-min-gui-status (funcall status)))
    (when buffer-name
      (setq emacs-dired-min-gui-buffer-name (funcall buffer-name))))
  (list :directory emacs-dired-min-gui-directory
        :target emacs-dired-min-gui-target
        :current-file-name emacs-dired-min-gui-current-file-name
        :status emacs-dired-min-gui-status
        :buffer-name emacs-dired-min-gui-buffer-name))

(defun emacs-dired-min-gui--set-buffer-name-from-backend (&optional result)
  "Refresh GUI Dired buffer name from backend, falling back to RESULT."
  (setq emacs-dired-min-gui-buffer-name
        (or (emacs-dired-min-gui--backend-call :buffer-name)
            result
            emacs-dired-min-gui-buffer-name)))

(defun emacs-dired-min-gui--set-status (status)
  "Set GUI Dired STATUS."
  (setq emacs-dired-min-gui-status status)
  (emacs-dired-min-gui--backend-call :set-status status)
  status)

(defun emacs-dired-min-gui--apply-display-prefix (action)
  "Ask the backend to apply display ACTION."
  (emacs-dired-min-gui--backend-call :apply-display-prefix action))

(defun emacs-dired-min-gui--target-directory ()
  "Return the directory for `dired-jump' from current file/target state."
  (let ((target (or emacs-dired-min-gui-current-file-name "")))
    (when (equal target "")
      (setq target (or emacs-dired-min-gui-target "")))
    (if (equal target "")
        "."
      (or (file-name-directory target) "."))))

(defun emacs-dired-min-gui--project-directory ()
  "Return the current GUI project directory from the backend."
  (or (emacs-dired-min-gui--backend-call :project-directory) "."))

(defun emacs-dired-min-gui--absolute-file-name-p (name)
  "Return non-nil when NAME is an absolute file name."
  (and (stringp name)
       (> (length name) 0)
       (= (aref name 0) 47)))

(defun emacs-dired-min-gui-project-directory-name (&optional name)
  "Return NAME resolved relative to the current GUI project.
Absolute NAME values are returned unchanged.  Relative values are
resolved under `emacs-dired-min-gui--project-directory'."
  (let ((name (or name emacs-dired-min-gui-directory "")))
    (if (emacs-dired-min-gui--absolute-file-name-p name)
        name
      (let ((directory (emacs-dired-min-gui--project-directory)))
        (if (equal directory "/")
            (concat "/" name)
          (concat directory "/" name))))))

(defun emacs-dired-min-gui-directory-entry-kind (path)
  "Return the one-character listing kind for PATH."
  (cond
   ((file-directory-p path) "d")
   ((file-exists-p path) "-")
   (t "?")))

(defun emacs-dired-min-gui-directory-listing (directory)
  "Return a plist describing DIRECTORY as simple GUI listing text.
The return value contains `:directory', `:entries', `:count', and
`:text'.  Each entry is a plist with `:name', `:path', and `:kind'."
  (let* ((dir (if (or (null directory) (equal directory ""))
                  "."
                directory))
         (abs (expand-file-name dir))
         (names (sort (directory-files abs) #'string<))
         (entries nil)
         (text (format "Directory: %s\n\n" abs)))
    (dolist (name names)
      (let* ((path (expand-file-name name abs))
             (kind (emacs-dired-min-gui-directory-entry-kind path))
             (entry (list :name name :path path :kind kind)))
        (push entry entries)
        (setq text (concat text (format "  %s  %s\n" kind name)))))
    (setq entries (nreverse entries))
    (list :directory abs
          :entries entries
          :count (length entries)
          :text text)))

(defun emacs-dired-min-gui-simple-directory-name (directory)
  "Return DIRECTORY as displayed by the simple Dired bridge listing."
  (let ((dir (if (or (null directory) (equal directory ""))
                 "."
               directory)))
    (if (and (> (length dir) 1)
             (= (aref dir (1- (length dir))) ?/))
        (substring dir 0 (1- (length dir)))
      dir)))

(defun emacs-dired-min-gui-simple-listing (directory names)
  "Return a plist for a simple Dired bridge listing.
NAMES is a directory entry list in display order.  The return value
contains `:directory', `:entries', `:count', and `:text'.  Dot entries
`.` and `..' are omitted from `:entries' and `:text'."
  (let* ((display-dir (emacs-dired-min-gui-simple-directory-name directory))
         (entries nil)
         (text (concat "Directory " display-dir "\n")))
    (dolist (name names)
      (unless (member name '("." ".."))
        (push name entries)
        (setq text (concat text "  " name "\n"))))
    (setq entries (nreverse entries))
    (list :directory display-dir
          :entries entries
          :count (length entries)
          :text text)))

;;;###autoload
(defun emacs-dired-min-gui-render-directory-buffer (directory &rest plist)
  "Render DIRECTORY as a Dired buffer through frontend callbacks.
PLIST accepts `:default-directory', `:directory-files', `:emit-text',
`:display-buffer', `:set-directory', `:set-buffer-name', and
`:buffer-name'.  Return the displayed buffer name."
  (let* ((default-directory-fn (plist-get plist :default-directory))
         (directory-files-fn (plist-get plist :directory-files))
         (emit-text (plist-get plist :emit-text))
         (display-buffer (plist-get plist :display-buffer))
         (set-directory (plist-get plist :set-directory))
         (set-buffer-name (plist-get plist :set-buffer-name))
         (buffer-name (or (plist-get plist :buffer-name) "*Dired*"))
         (dir (if (or (not directory) (equal directory ""))
                  (if default-directory-fn
                      (funcall default-directory-fn)
                    ".")
                directory))
         (listing (emacs-dired-min-gui-simple-listing
                   dir
                   (and directory-files-fn
                        (funcall directory-files-fn dir))))
         (text (plist-get listing :text)))
    (when set-directory
      (funcall set-directory dir))
    (when set-buffer-name
      (funcall set-buffer-name buffer-name))
    (when emit-text
      (funcall emit-text text))
    (when display-buffer
      (funcall display-buffer buffer-name text))
    buffer-name))

(defun emacs-dired-min-gui-list-directory-core ()
  "Open `emacs-dired-min-gui-directory' through the GUI backend.
This is the core directory listing operation.  Command variants layer
display-prefix policy on top."
  (let ((result (emacs-dired-min-gui--backend-call
                 :list-directory emacs-dired-min-gui-directory)))
    (emacs-dired-min-gui--set-buffer-name-from-backend result)
    emacs-dired-min-gui-buffer-name))

(defun emacs-dired-min-gui-dired (&optional action)
  "Open `emacs-dired-min-gui-directory' through the GUI backend.
ACTION is nil/same, `other', `frame', or `tab'."
  (let ((result (emacs-dired-min-gui-list-directory-core)))
    (when (equal emacs-dired-min-gui-status "ok")
      (emacs-dired-min-gui--apply-display-prefix (or action "same")))
    result))

(defun emacs-dired-min-gui-dired-jump (&optional action)
  "Open the directory containing the current GUI bridge target."
  (let ((old emacs-dired-min-gui-directory))
    (setq emacs-dired-min-gui-directory
          (emacs-dired-min-gui--target-directory))
    (unwind-protect
        (emacs-dired-min-gui-dired action)
      (setq emacs-dired-min-gui-directory old))))

(defun emacs-dired-min-gui-project-find-dir (&optional action)
  "Open a project-relative directory through the GUI backend."
  (let ((old emacs-dired-min-gui-directory))
    (setq emacs-dired-min-gui-directory
          (emacs-dired-min-gui-project-directory-name))
    (unwind-protect
        (emacs-dired-min-gui-dired action)
      (setq emacs-dired-min-gui-directory old))))

(defun emacs-dired-min-gui-project-dired (&optional action)
  "Open the current project directory, or a project-relative directory."
  (let ((old emacs-dired-min-gui-directory))
    (setq emacs-dired-min-gui-directory
          (if (equal (or emacs-dired-min-gui-directory "") "")
              (emacs-dired-min-gui--project-directory)
            (emacs-dired-min-gui-project-directory-name)))
    (unwind-protect
        (emacs-dired-min-gui-dired action)
      (setq emacs-dired-min-gui-directory old))))

(defun emacs-dired-min-gui-mark (mark)
  "Apply Dired MARK through the GUI backend."
  (let ((fn (emacs-dired-min-gui--backend-function :mark)))
    (if fn
        (let ((result (funcall fn mark)))
          (emacs-dired-min-gui--set-buffer-name-from-backend result)
          emacs-dired-min-gui-buffer-name)
      (emacs-dired-min-gui-apply-mark-core mark))))

(defun emacs-dired-min-gui-apply-mark-core (mark)
  "Apply MARK to the current GUI bridge Dired line.
The backend owns transport storage; this runtime owns mark command
sequencing."
  (if (emacs-dired-min-gui--backend-call :directory-buffer-p)
      (let ((name (or (emacs-dired-min-gui--backend-call
                       :name-at-point)
                      "")))
        (if (equal name "")
            (emacs-dired-min-gui--backend-call :next-line)
          (progn
            (if (equal mark " ")
                (emacs-dired-min-gui--backend-call :remove-mark name)
              (emacs-dired-min-gui--backend-call :set-mark name mark))
            (emacs-dired-min-gui--backend-call :write-marks-state)
            (emacs-dired-min-gui--backend-call :rerender)
            (emacs-dired-min-gui--backend-call :next-line))))
    (emacs-dired-min-gui--set-status "unsupported"))
  (emacs-dired-min-gui--set-buffer-name-from-backend)
  emacs-dired-min-gui-buffer-name)

(defun emacs-dired-min-gui--marked-lines ()
  "Return newline-split GUI Dired mark lines."
  (let ((text (or (emacs-dired-min-gui--backend-call :marks-text) ""))
        (index 0)
        (start 0)
        (lines nil))
    (while (<= index (length text))
      (if (or (= index (length text))
              (= (aref text index) ?\n))
          (let ((line (substring text start index)))
            (unless (equal line "")
              (push line lines))
            (setq start (1+ index)))
        nil)
      (setq index (1+ index)))
    (nreverse lines)))

(defun emacs-dired-min-gui--marked-name-for (line mark)
  "Return marked name from LINE when it ends with MARK, or nil."
  (and (> (length line) 2)
       (= (aref line (- (length line) 2)) ?\t)
       (equal (substring line (- (length line) 1)) mark)
       (substring line 0 (- (length line) 2))))

(defun emacs-dired-min-gui-do-flagged-delete-core ()
  "Delete files flagged in the GUI bridge Dired buffer."
  (if (emacs-dired-min-gui--backend-call :directory-buffer-p)
      (let ((deleted 0))
        (dolist (line (emacs-dired-min-gui--marked-lines))
          (let ((name (emacs-dired-min-gui--marked-name-for line "D")))
            (when name
              (let ((path (emacs-dired-min-gui--backend-call
                           :expand-name name)))
                (when (and path
                           (not (emacs-dired-min-gui--backend-call
                                 :directory-p path))
                           (emacs-dired-min-gui--backend-call
                            :delete-file path))
                  (setq deleted (1+ deleted))
                  (emacs-dired-min-gui--backend-call
                   :remove-mark name))))))
        (emacs-dired-min-gui--backend-call :write-marks-state)
        (emacs-dired-min-gui--backend-call :rerender)
        (emacs-dired-min-gui--backend-call
         :set-modeline
         (concat "Deleted " (number-to-string deleted) " files")))
    (emacs-dired-min-gui--set-status "unsupported"))
  (emacs-dired-min-gui--set-buffer-name-from-backend)
  emacs-dired-min-gui-buffer-name)

(defun emacs-dired-min-gui-do-rename-core (target)
  "Rename the current GUI bridge Dired entry to TARGET."
  (if (emacs-dired-min-gui--backend-call :directory-buffer-p)
      (let ((name (or (emacs-dired-min-gui--backend-call
                       :name-at-point)
                      "")))
        (if (or (equal name "") (equal target ""))
            (emacs-dired-min-gui--set-status "unsupported")
          (let ((source (emacs-dired-min-gui--backend-call
                         :expand-name name))
                (dest (emacs-dired-min-gui--backend-call
                       :expand-name target)))
            (if (emacs-dired-min-gui--backend-call :file-exists-p source)
                (if (emacs-dired-min-gui--backend-call
                     :rename-file source dest)
                    (progn
                      (emacs-dired-min-gui--backend-call
                       :remove-mark name)
                      (emacs-dired-min-gui--backend-call
                       :write-marks-state)
                      (emacs-dired-min-gui--backend-call :rerender))
                  (emacs-dired-min-gui--set-status "unsupported"))
              (emacs-dired-min-gui--set-status "file-not-found")))))
    (emacs-dired-min-gui--set-status "unsupported"))
  (emacs-dired-min-gui--set-buffer-name-from-backend)
  emacs-dired-min-gui-buffer-name)

(defun emacs-dired-min-gui-do-copy-core (target)
  "Copy the current GUI bridge Dired entry to TARGET."
  (if (emacs-dired-min-gui--backend-call :directory-buffer-p)
      (let ((name (or (emacs-dired-min-gui--backend-call
                       :name-at-point)
                      "")))
        (if (or (equal name "") (equal target ""))
            (emacs-dired-min-gui--set-status "unsupported")
          (let ((source (emacs-dired-min-gui--backend-call
                         :expand-name name))
                (dest (emacs-dired-min-gui--backend-call
                       :expand-name target)))
            (if (emacs-dired-min-gui--backend-call :file-exists-p source)
                (progn
                  (emacs-dired-min-gui--backend-call
                   :write-file
                   dest
                   (emacs-dired-min-gui--backend-call :read-file source))
                  (emacs-dired-min-gui--backend-call :rerender))
              (emacs-dired-min-gui--set-status "file-not-found")))))
    (emacs-dired-min-gui--set-status "unsupported"))
  (emacs-dired-min-gui--set-buffer-name-from-backend)
  emacs-dired-min-gui-buffer-name)

(defun emacs-dired-min-gui-do-flagged-delete ()
  "Delete files flagged in the GUI bridge Dired buffer."
  (let ((fn (emacs-dired-min-gui--backend-function :flagged-delete)))
    (if fn
        (let ((result (funcall fn)))
          (emacs-dired-min-gui--set-buffer-name-from-backend result)
          emacs-dired-min-gui-buffer-name)
      (emacs-dired-min-gui-do-flagged-delete-core))))

(defun emacs-dired-min-gui-do-rename (target)
  "Rename the current GUI bridge Dired entry to TARGET."
  (let ((fn (emacs-dired-min-gui--backend-function :rename)))
    (if fn
        (let ((result (funcall fn target)))
          (emacs-dired-min-gui--set-buffer-name-from-backend result)
          emacs-dired-min-gui-buffer-name)
      (emacs-dired-min-gui-do-rename-core target))))

(defun emacs-dired-min-gui-do-copy (target)
  "Copy the current GUI bridge Dired entry to TARGET."
  (let ((fn (emacs-dired-min-gui--backend-function :copy)))
    (if fn
        (let ((result (funcall fn target)))
          (emacs-dired-min-gui--set-buffer-name-from-backend result)
          emacs-dired-min-gui-buffer-name)
      (emacs-dired-min-gui-do-copy-core target))))

(defun emacs-dired-min-gui-dired-command (&optional action)
  "Run the GUI bridge `dired' command variant."
  (emacs-dired-min-gui-dired (or action "same")))

(defun emacs-dired-min-gui-dired-jump-command (&optional action)
  "Run the GUI bridge `dired-jump' command variant."
  (emacs-dired-min-gui-dired-jump (or action "same")))

(defun emacs-dired-min-gui-project-find-dir-command (&optional action)
  "Run the GUI bridge `project-find-dir' command variant."
  (emacs-dired-min-gui-project-find-dir (or action "same")))

(defun emacs-dired-min-gui-project-dired-command (&optional action)
  "Run the GUI bridge `project-dired' command variant."
  (emacs-dired-min-gui-project-dired (or action "same")))

(defun emacs-dired-min-gui-dired-current-context-command (&optional action)
  "Refresh GUI Dired context from backend, then run `dired'."
  (emacs-dired-min-gui-refresh-context-from-backend)
  (emacs-dired-min-gui-dired-command action))

(defun emacs-dired-min-gui-dired-jump-current-context-command
    (&optional action)
  "Refresh GUI Dired context from backend, then run `dired-jump'."
  (emacs-dired-min-gui-refresh-context-from-backend)
  (emacs-dired-min-gui-dired-jump-command action))

(defun emacs-dired-min-gui-project-find-dir-current-context-command
    (&optional action)
  "Refresh GUI Dired context from backend, then run `project-find-dir'."
  (emacs-dired-min-gui-refresh-context-from-backend)
  (emacs-dired-min-gui-project-find-dir-command action))

(defun emacs-dired-min-gui-project-dired-current-context-command
    (&optional action)
  "Refresh GUI Dired context from backend, then run `project-dired'."
  (emacs-dired-min-gui-refresh-context-from-backend)
  (emacs-dired-min-gui-project-dired-command action))

;;;###autoload
(defun emacs-dired-min-gui-run-directory-command (&rest plist)
  "Run a frontend Dired directory command through the shared Dired core.
PLIST accepts `:install-function', `:read-string', `:default-directory',
`:prompt', `:buffer-name', `:command', and `:action'.  The frontend owns
how a directory is read; this helper owns the reusable context setup and
current-context dispatch."
  (let* ((install-function (plist-get plist :install-function))
         (read-string (plist-get plist :read-string))
         (default-directory-function (plist-get plist :default-directory))
         (prompt (or (plist-get plist :prompt) "Dired (directory): "))
         (buffer-name (or (plist-get plist :buffer-name) "*Dired*"))
         (command (or (plist-get plist :command) 'dired))
         (action (or (plist-get plist :action) "same"))
         (directory (and read-string (funcall read-string prompt))))
    (when install-function
      (funcall install-function))
    (when (or (not directory) (equal directory ""))
      (setq directory
            (if default-directory-function
                (funcall default-directory-function)
              ".")))
    (emacs-dired-min-gui-set-context
     :directory directory
     :status "ok"
     :buffer-name buffer-name)
    (emacs-dired-min-gui-current-context-command command action)))

(defun emacs-dired-min-gui-mark-command ()
  "Run the GUI bridge `dired-mark' command."
  (emacs-dired-min-gui-mark "*"))

(defun emacs-dired-min-gui-unmark-command ()
  "Run the GUI bridge `dired-unmark' command."
  (emacs-dired-min-gui-mark " "))

(defun emacs-dired-min-gui-flag-file-deletion-command ()
  "Run the GUI bridge `dired-flag-file-deletion' command."
  (emacs-dired-min-gui-mark "D"))

(defun emacs-dired-min-gui-do-flagged-delete-command ()
  "Run the GUI bridge `dired-do-flagged-delete' command."
  (emacs-dired-min-gui-do-flagged-delete))

(defun emacs-dired-min-gui-do-rename-command ()
  "Run the GUI bridge `dired-do-rename' command."
  (emacs-dired-min-gui-do-rename emacs-dired-min-gui-directory))

(defun emacs-dired-min-gui-do-copy-command ()
  "Run the GUI bridge `dired-do-copy' command."
  (emacs-dired-min-gui-do-copy emacs-dired-min-gui-directory))

;;;###autoload
(defun emacs-dired-min-gui-current-context-command
    (command &optional action)
  "Refresh backend context and run GUI Dired COMMAND.
ACTION is nil/same, `other', `frame', or `tab' for display variants."
  (emacs-dired-min-gui-refresh-context-from-backend)
  (cond
   ((eq command 'dired)
    (emacs-dired-min-gui-dired-command action))
   ((eq command 'dired-jump)
    (emacs-dired-min-gui-dired-jump-command action))
   ((eq command 'project-find-dir)
    (emacs-dired-min-gui-project-find-dir-command action))
   ((eq command 'project-dired)
    (emacs-dired-min-gui-project-dired-command action))
   ((eq command 'dired-mark)
    (emacs-dired-min-gui-mark-command))
   ((eq command 'dired-unmark)
    (emacs-dired-min-gui-unmark-command))
   ((eq command 'dired-flag-file-deletion)
    (emacs-dired-min-gui-flag-file-deletion-command))
   ((eq command 'dired-do-flagged-delete)
    (emacs-dired-min-gui-do-flagged-delete-command))
   ((eq command 'dired-do-rename)
    (emacs-dired-min-gui-do-rename-command))
   ((eq command 'dired-do-copy)
    (emacs-dired-min-gui-do-copy-command))
   (t nil)))

;;;###autoload
(defun emacs-dired-min-gui-writeback-spec (&optional command)
  "Return GUI transport writeback spec for Dired COMMAND.
The Dired runtime owns command-to-state policy; bridge adapters
interpret the returned plist by writing their transport stores."
  (let ((command (cond
                  ((symbolp command) (symbol-name command))
                  ((stringp command) command)
                  (t ""))))
    (cond
     ((member command
              '("list-directory" "dired" "dired-jump"
                "dired-jump-other-window" "dired-other-window"
                "project-find-dir" "project-dired"))
      '(:buffer t :file t :buffer-name t :window t
        :point t :mark t :window-start t))
     ((equal command "dired-other-frame")
      '(:buffer t :file t :buffer-name t :window t :frame t
        :point t :mark t :window-start t))
     ((equal command "dired-other-tab")
      '(:buffer t :file t :buffer-name t :window t :tab t
        :point t :mark t :window-start t))
     ((member command
              '("dired-mark" "dired-unmark" "dired-flag-file-deletion"
                "dired-do-rename" "dired-do-copy"))
      '(:buffer t :buffer-name t :point t :mark t :window-start t))
     ((equal command "dired-do-flagged-delete")
      '(:buffer t :buffer-name t :modeline t
        :point t :mark t :window-start t))
     (t nil))))

;;;###autoload
(defun emacs-dired-min-gui-writeback-spec-flag (spec key)
  "Return non-nil when Dired GUI writeback SPEC enables KEY."
  (and spec (plist-get spec key)))

;;;###autoload
(defun emacs-dired-min-gui-writeback-state (&optional command)
  "Write GUI transport state for Dired COMMAND.
The Dired runtime owns writeback spec interpretation and callback ordering.
The registered backend owns concrete transport writes.  Return non-nil when
COMMAND has a Dired writeback spec."
  (let ((spec (emacs-dired-min-gui-writeback-spec command)))
    (when spec
      (when (emacs-dired-min-gui-writeback-spec-flag spec :buffer)
        (emacs-dired-min-gui--backend-call :write-buffer-state))
      (when (emacs-dired-min-gui-writeback-spec-flag spec :file)
        (emacs-dired-min-gui--backend-call :write-file-state))
      (when (emacs-dired-min-gui-writeback-spec-flag spec :buffer-name)
        (emacs-dired-min-gui--backend-call :write-buffer-name-state))
      (when (emacs-dired-min-gui-writeback-spec-flag spec :window)
        (emacs-dired-min-gui--backend-call :write-window-state))
      (when (emacs-dired-min-gui-writeback-spec-flag spec :frame)
        (emacs-dired-min-gui--backend-call :write-frame-state))
      (when (emacs-dired-min-gui-writeback-spec-flag spec :tab)
        (emacs-dired-min-gui--backend-call :write-tab-state))
      (when (emacs-dired-min-gui-writeback-spec-flag spec :modeline)
        (emacs-dired-min-gui--backend-call :write-modeline-state))
      (when (emacs-dired-min-gui-writeback-spec-flag spec :point)
        (emacs-dired-min-gui--backend-call :write-point-state))
      (when (emacs-dired-min-gui-writeback-spec-flag spec :mark)
        (emacs-dired-min-gui--backend-call :write-mark-state))
      (when (emacs-dired-min-gui-writeback-spec-flag spec :window-start)
        (emacs-dired-min-gui--backend-call :write-window-start-state))
      (emacs-dired-min-gui--backend-call :mark-written-state)
      t)))

(provide 'emacs-dired-min-gui)

;;; emacs-dired-min-gui.el ends here
