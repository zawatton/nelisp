;;; emacs-fileio-gui.el --- GUI bridge file/buffer adapter  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Lightweight GUI bridge adapter for file and buffer commands.  The
;; backend owns transport stores; this module owns the Emacs-facing command
;; sequencing.  It intentionally has no heavy runtime requires so it can be
;; baked into the standalone GUI bridge image.

;;; Code:

(defvar emacs-fileio-gui-backend nil
  "PLIST of GUI bridge file/buffer backend callbacks.
The callbacks own transport-specific stores; this module owns the
Emacs-facing command sequencing.")

(defvar emacs-fileio-gui-arg ""
  "Argument string supplied by the GUI bridge file/buffer command.")

(defvar emacs-fileio-gui-status "ok"
  "Status string returned by the GUI bridge file/buffer command.")

(defvar emacs-fileio-gui-current-file-name nil
  "Visited filename reported by the GUI bridge backend.")

(defvar emacs-fileio-gui-buffer-name ""
  "Current buffer name reported by the GUI bridge backend.")

(defvar emacs-fileio-gui-read-only-p nil
  "Non-nil when the GUI bridge current buffer should be read-only.")

(defvar emacs-fileio-gui-display-action ""
  "Display action requested by a GUI bridge command: same, other, frame, tab.")

;;;###autoload
(defun emacs-fileio-gui-register-backend (&rest backend)
  "Register BACKEND as the GUI bridge file/buffer adapter.
BACKEND is a plist.  Passing nil clears the adapter."
  (setq emacs-fileio-gui-backend backend))

(defun emacs-fileio-gui--backend-call (key &rest args)
  "Call GUI file/buffer backend function KEY with ARGS, if registered."
  (let ((fn (and emacs-fileio-gui-backend
                 (plist-get emacs-fileio-gui-backend key))))
    (when fn
      (apply fn args))))

(defun emacs-fileio-gui--backend-function (key)
  "Return GUI file/buffer backend function KEY, or nil."
  (and emacs-fileio-gui-backend
       (plist-get emacs-fileio-gui-backend key)))

(defun emacs-fileio-gui--set-status (status)
  "Set GUI bridge STATUS."
  (setq emacs-fileio-gui-status status)
  (emacs-fileio-gui--backend-call :set-status status)
  status)

(defun emacs-fileio-gui--set-current-file-name (filename)
  "Set GUI bridge current FILENAME."
  (setq emacs-fileio-gui-current-file-name filename)
  (emacs-fileio-gui--backend-call :set-current-file-name filename)
  filename)

(defun emacs-fileio-gui--set-buffer-name (name)
  "Set GUI bridge buffer NAME."
  (setq emacs-fileio-gui-buffer-name name)
  (emacs-fileio-gui--backend-call :set-buffer-name name)
  name)

(defun emacs-fileio-gui--set-read-only (flag)
  "Set GUI bridge read-only FLAG."
  (setq emacs-fileio-gui-read-only-p flag)
  (emacs-fileio-gui--backend-call :set-read-only flag)
  flag)

;;;###autoload
(defun emacs-fileio-gui-set-context (&rest plist)
  "Update GUI bridge file/buffer context from PLIST.
Recognized keys are `:arg', `:status', `:current-file-name',
`:buffer-name', `:read-only-p', and `:display-action'."
  (when (plist-member plist :arg)
    (setq emacs-fileio-gui-arg (plist-get plist :arg)))
  (when (plist-member plist :status)
    (setq emacs-fileio-gui-status (plist-get plist :status)))
  (when (plist-member plist :current-file-name)
    (setq emacs-fileio-gui-current-file-name
          (plist-get plist :current-file-name)))
  (when (plist-member plist :buffer-name)
    (setq emacs-fileio-gui-buffer-name (plist-get plist :buffer-name)))
  (when (plist-member plist :read-only-p)
    (setq emacs-fileio-gui-read-only-p (plist-get plist :read-only-p)))
  (when (plist-member plist :display-action)
    (setq emacs-fileio-gui-display-action
          (plist-get plist :display-action)))
  plist)

;;;###autoload
(defun emacs-fileio-gui-refresh-context-from-backend ()
  "Refresh GUI file/buffer context from backend current-value callbacks.
Missing callbacks leave the existing runtime context unchanged."
  (when (emacs-fileio-gui--backend-function :current-arg)
    (setq emacs-fileio-gui-arg
          (emacs-fileio-gui--backend-call :current-arg)))
  (when (emacs-fileio-gui--backend-function :current-status)
    (setq emacs-fileio-gui-status
          (emacs-fileio-gui--backend-call :current-status)))
  (when (emacs-fileio-gui--backend-function :current-file-name)
    (setq emacs-fileio-gui-current-file-name
          (emacs-fileio-gui--backend-call :current-file-name)))
  (when (emacs-fileio-gui--backend-function :buffer-name)
    (setq emacs-fileio-gui-buffer-name
          (emacs-fileio-gui--backend-call :buffer-name)))
  (when (emacs-fileio-gui--backend-function :current-read-only-p)
    (setq emacs-fileio-gui-read-only-p
          (emacs-fileio-gui--backend-call :current-read-only-p)))
  (when (emacs-fileio-gui--backend-function :current-display-action)
    (setq emacs-fileio-gui-display-action
          (emacs-fileio-gui--backend-call :current-display-action)))
  (list :arg emacs-fileio-gui-arg
        :status emacs-fileio-gui-status
        :current-file-name emacs-fileio-gui-current-file-name
        :buffer-name emacs-fileio-gui-buffer-name
        :read-only-p emacs-fileio-gui-read-only-p
        :display-action emacs-fileio-gui-display-action))

(defun emacs-fileio-gui--apply-display-prefix (action)
  "Ask the backend to apply display ACTION."
  (emacs-fileio-gui--backend-call :apply-display-prefix action))

(defun emacs-fileio-gui--string-prefix-p (prefix string)
  "Return non-nil when PREFIX is a prefix of STRING."
  (let ((prefix (or prefix ""))
        (string (or string "")))
    (and (<= (length prefix) (length string))
         (equal prefix (substring string 0 (length prefix))))))

(defun emacs-fileio-gui--buffer-list-source ()
  "Return newline-separated live GUI buffer names from the backend."
  (or (emacs-fileio-gui--backend-call :buffer-list-source)
      (emacs-fileio-gui--backend-call :buffer-candidates)
      ""))

(defun emacs-fileio-gui--buffer-file-name (name)
  "Return NAME's visited file path from the backend, or the empty string."
  (or (emacs-fileio-gui--backend-call :buffer-file-name name)
      ""))

(defun emacs-fileio-gui--current-buffer-name ()
  "Return the current GUI buffer name, defaulting to main."
  (let ((name (or (emacs-fileio-gui--backend-call :buffer-name)
                  emacs-fileio-gui-buffer-name
                  "")))
    (if (equal name "") "main" name)))

(defun emacs-fileio-gui--normalize-buffer-name (name fallback)
  "Return NAME unless it is empty, then FALLBACK, then main."
  (let ((name (or name "")))
    (if (equal name "")
        (let ((fallback (or fallback "")))
          (if (equal fallback "") "main" fallback))
      name)))

(defun emacs-fileio-gui-first-buffer-name ()
  "Return the first live GUI buffer name, defaulting to main."
  (let ((source (emacs-fileio-gui--buffer-list-source))
        (index 0)
        (start 0)
        (name ""))
    (when (equal source "")
      (setq source "main\n"))
    (while (and (equal name "")
                (<= index (length source)))
      (if (or (= index (length source))
              (= (aref source index) 10))
          (let ((line (substring source start index)))
            (when (not (equal line ""))
              (setq name line))
            (setq start (+ index 1)))
        nil)
      (setq index (+ index 1)))
    (if (equal name "") "main" name)))

(defun emacs-fileio-gui--low-level-buffer-backend-p ()
  "Return non-nil when transport exposes low-level buffer callbacks."
  (and (emacs-fileio-gui--backend-call :low-level-buffer-backend-p)
       t))

(defun emacs-fileio-gui--load-buffer-state (name)
  "Ask the backend to make NAME current from transport stores."
  (emacs-fileio-gui--backend-call :load-buffer-state name))

(defun emacs-fileio-gui--switch-to-buffer-state (name)
  "Switch to NAME using low-level backend callbacks."
  (emacs-fileio-gui--backend-call :save-current-buffer-state)
  (emacs-fileio-gui--load-buffer-state name)
  name)

(defun emacs-fileio-gui--clear-buffer-state (name)
  "Ask the backend to clear NAME's persisted buffer state."
  (emacs-fileio-gui--backend-call :clear-buffer-state name))

(defun emacs-fileio-gui--remove-buffer (name)
  "Ask the backend to remove NAME from the transport buffer list."
  (emacs-fileio-gui--backend-call :remove-buffer name))

(defun emacs-fileio-gui--add-buffer (name)
  "Ask the backend to add NAME to the transport buffer list."
  (emacs-fileio-gui--backend-call :add-buffer name))

(defun emacs-fileio-gui--kill-buffer-state (target)
  "Kill TARGET using low-level backend callbacks and return current buffer."
  (let ((current (emacs-fileio-gui--current-buffer-name))
        (fallback "main"))
    (emacs-fileio-gui--remove-buffer target)
    (emacs-fileio-gui--clear-buffer-state target)
    (if (equal target current)
        (progn
          (if (equal target "main")
              (setq fallback "main")
            (setq fallback "main"))
          (emacs-fileio-gui--load-buffer-state fallback)
          fallback)
      (progn
        (emacs-fileio-gui--add-buffer current)
        current))))

(defun emacs-fileio-gui--project-kill-buffer-states ()
  "Kill all project buffers using low-level backend callbacks."
  (let ((targets (emacs-fileio-gui-project-buffer-candidates))
        (index 0)
        (start 0)
        (name ""))
    (emacs-fileio-gui--backend-call :save-current-buffer-state)
    (while (<= index (length targets))
      (if (or (= index (length targets))
              (= (aref targets index) 10))
          (progn
            (setq name (substring targets start index))
            (when (not (equal name ""))
              (emacs-fileio-gui--remove-buffer name)
              (emacs-fileio-gui--clear-buffer-state name))
            (setq start (+ index 1)))
        nil)
      (setq index (+ index 1)))
    (let ((fallback (emacs-fileio-gui-first-buffer-name)))
      (emacs-fileio-gui--load-buffer-state fallback)
      fallback)))

(defun emacs-fileio-gui--project-directory ()
  "Return the current GUI project directory from the backend."
  (or (emacs-fileio-gui--backend-call :project-directory) "/"))

(defun emacs-fileio-gui--project-prefix ()
  "Return the absolute filename prefix for the current GUI project."
  (let ((directory (emacs-fileio-gui--project-directory)))
    (if (equal directory "/")
        "/"
      (concat directory "/"))))

(defun emacs-fileio-gui--absolute-file-name-p (name)
  "Return non-nil when NAME is an absolute file name."
  (and (stringp name)
       (> (length name) 0)
       (= (aref name 0) 47)))

(defun emacs-fileio-gui-project-file-name (&optional name)
  "Return NAME resolved relative to the current GUI project.
Absolute NAME values are returned unchanged.  Relative values are
resolved under `emacs-fileio-gui--project-directory'."
  (let ((name (or name emacs-fileio-gui-arg "")))
    (if (emacs-fileio-gui--absolute-file-name-p name)
        name
      (let ((directory (emacs-fileio-gui--project-directory)))
        (if (equal directory "/")
            (concat "/" name)
          (concat directory "/" name))))))

(defun emacs-fileio-gui--file-exists-p (name)
  "Return non-nil when NAME exists according to the GUI backend."
  (if (emacs-fileio-gui--backend-function :file-exists-p)
      (emacs-fileio-gui--backend-call :file-exists-p name)
    nil))

(defun emacs-fileio-gui--project-buffer-p (name)
  "Return non-nil when buffer NAME belongs to the current GUI project."
  (let ((file (emacs-fileio-gui--buffer-file-name name))
        (prefix (emacs-fileio-gui--project-prefix)))
    (and (not (equal file ""))
         (emacs-fileio-gui--string-prefix-p prefix file))))

;;;###autoload
(defun emacs-fileio-gui-buffer-candidates ()
  "Return newline-separated buffer names for GUI minibuffer completion."
  (emacs-fileio-gui--buffer-list-source))

;;;###autoload
(defun emacs-fileio-gui-project-buffer-candidates ()
  "Return newline-separated buffer names in the current GUI project."
  (let ((source (emacs-fileio-gui--buffer-list-source))
        (index 0)
        (start 0)
        (out ""))
    (while (<= index (length source))
      (if (or (= index (length source))
              (= (aref source index) 10))
          (let ((name (substring source start index)))
            (when (and (not (equal name ""))
                       (emacs-fileio-gui--project-buffer-p name))
              (setq out (concat out name "\n")))
            (setq start (+ index 1)))
        nil)
      (setq index (+ index 1)))
    out))

(defun emacs-fileio-gui--render-buffer-list (source project-only)
  "Render SOURCE buffer names as an Emacs-style buffer list.
PROJECT-ONLY limits rows to buffers under the current project."
  (let ((current (or (emacs-fileio-gui--backend-call :buffer-name)
                     emacs-fileio-gui-buffer-name
                     ""))
        (index 0)
        (start 0)
        (out "Buffer\tFile\n"))
    (when (equal source "")
      (setq source "main\n"))
    (while (<= index (length source))
      (if (or (= index (length source))
              (= (aref source index) 10))
          (let ((name (substring source start index)))
            (when (and (not (equal name ""))
                       (or (not project-only)
                           (emacs-fileio-gui--project-buffer-p name)))
              (let ((file (emacs-fileio-gui--buffer-file-name name)))
                (setq out
                      (concat out
                              (if (equal name current) "* " "  ")
                              name
                              "\t"
                              file
                              "\n"))))
            (setq start (+ index 1)))
        nil)
      (setq index (+ index 1)))
    out))

;;;###autoload
(defun emacs-fileio-gui-buffer-list-text (&optional project-only)
  "Return rendered GUI buffer list text.
PROJECT-ONLY limits rows to buffers under the current project."
  (emacs-fileio-gui--render-buffer-list
   (emacs-fileio-gui--buffer-list-source)
   project-only))

;;;###autoload
(defun emacs-fileio-gui-find-file-core ()
  "Visit `emacs-fileio-gui-arg' through the GUI backend.
This is the core file visit operation.  Command variants layer display
actions and read-only policy on top."
  (let ((result (emacs-fileio-gui--backend-call
                 :find-file-core emacs-fileio-gui-arg)))
    (setq emacs-fileio-gui-current-file-name
          (or (emacs-fileio-gui--backend-call :current-file-name)
              result
              emacs-fileio-gui-current-file-name))
    emacs-fileio-gui-current-file-name))

;;;###autoload
(defun emacs-fileio-gui-find-file (&optional action read-only)
  "Visit `emacs-fileio-gui-arg' through the GUI backend.
ACTION is nil/same, `other', `frame', or `tab'.  READ-ONLY marks the
visited buffer read-only after a successful visit."
  (let ((result (emacs-fileio-gui-find-file-core)))
    (when (equal emacs-fileio-gui-status "ok")
      (emacs-fileio-gui--apply-display-prefix (or action "same"))
      (when read-only
        (emacs-fileio-gui--set-read-only t)))
    result))

;;;###autoload
(defun emacs-fileio-gui-save-buffer-core ()
  "Save the current GUI bridge buffer through the backend."
  (let ((result (emacs-fileio-gui--backend-call :save-buffer)))
    (setq emacs-fileio-gui-current-file-name
          (or (emacs-fileio-gui--backend-call :current-file-name)
              result
              emacs-fileio-gui-current-file-name))
    emacs-fileio-gui-current-file-name))

;;;###autoload
(defun emacs-fileio-gui-save-buffer ()
  "Save the current GUI bridge buffer through the backend."
  (emacs-fileio-gui-save-buffer-core))

;;;###autoload
(defun emacs-fileio-gui-save-some-buffers ()
  "Save modified GUI bridge buffers through the backend."
  (emacs-fileio-gui--backend-call :save-some-buffers))

;;;###autoload
(defun emacs-fileio-gui-write-file ()
  "Write the current GUI bridge buffer to `emacs-fileio-gui-arg'."
  (let ((result (emacs-fileio-gui--backend-call
                 :write-file emacs-fileio-gui-arg)))
    (setq emacs-fileio-gui-current-file-name
          (or (emacs-fileio-gui--backend-call :current-file-name)
              result
              emacs-fileio-gui-current-file-name))
    emacs-fileio-gui-current-file-name))

;;;###autoload
(defun emacs-fileio-gui-insert-file ()
  "Insert `emacs-fileio-gui-arg' into the current GUI bridge buffer."
  (emacs-fileio-gui--backend-call :insert-file emacs-fileio-gui-arg))

;;;###autoload
(defun emacs-fileio-gui-insert-buffer ()
  "Insert `emacs-fileio-gui-arg' buffer into the current GUI bridge buffer."
  (let ((target (emacs-fileio-gui--normalize-buffer-name
                 emacs-fileio-gui-arg "main")))
    (emacs-fileio-gui--backend-call :insert-buffer target)))

;;;###autoload
(defun emacs-fileio-gui-switch-to-buffer (&optional action)
  "Switch to `emacs-fileio-gui-arg' through the GUI backend.
ACTION is nil/same, `other', `frame', or `tab'."
  (let* ((target (emacs-fileio-gui--normalize-buffer-name
                  emacs-fileio-gui-arg "main"))
         (result
          (if (emacs-fileio-gui--low-level-buffer-backend-p)
              (emacs-fileio-gui--switch-to-buffer-state target)
            (emacs-fileio-gui--backend-call
             :switch-to-buffer target))))
    (setq emacs-fileio-gui-buffer-name
          (or (emacs-fileio-gui--backend-call :buffer-name)
              result
              emacs-fileio-gui-buffer-name))
    (emacs-fileio-gui--apply-display-prefix (or action "same"))
    emacs-fileio-gui-buffer-name))

;;;###autoload
(defun emacs-fileio-gui-revert-buffer ()
  "Revert the current GUI bridge buffer through the backend."
  (let ((result (emacs-fileio-gui--backend-call :revert-buffer)))
    (setq emacs-fileio-gui-current-file-name
          (or (emacs-fileio-gui--backend-call :current-file-name)
              result
              emacs-fileio-gui-current-file-name))
    emacs-fileio-gui-current-file-name))

;;;###autoload
(defun emacs-fileio-gui-rename-buffer ()
  "Rename the current GUI bridge buffer through the backend."
  (let ((result (emacs-fileio-gui--backend-call
                 :rename-buffer emacs-fileio-gui-arg)))
    (setq emacs-fileio-gui-buffer-name
          (or (emacs-fileio-gui--backend-call :buffer-name)
              result
              emacs-fileio-gui-buffer-name))
    emacs-fileio-gui-buffer-name))

;;;###autoload
(defun emacs-fileio-gui-kill-buffer ()
  "Kill a GUI bridge buffer through the backend."
  (let* ((target (emacs-fileio-gui--normalize-buffer-name
                  emacs-fileio-gui-arg
                  (emacs-fileio-gui--current-buffer-name)))
         (result
          (if (emacs-fileio-gui--low-level-buffer-backend-p)
              (emacs-fileio-gui--kill-buffer-state target)
            (emacs-fileio-gui--backend-call
             :kill-buffer target))))
    (setq emacs-fileio-gui-buffer-name
          (or (emacs-fileio-gui--backend-call :buffer-name)
              result
              emacs-fileio-gui-buffer-name))
    emacs-fileio-gui-buffer-name))

;;;###autoload
(defun emacs-fileio-gui-kill-buffer-and-window ()
  "Kill the current GUI bridge buffer and its selected window."
  (let* ((target (emacs-fileio-gui--normalize-buffer-name
                  emacs-fileio-gui-arg
                  (emacs-fileio-gui--current-buffer-name)))
         (result
          (if (emacs-fileio-gui--low-level-buffer-backend-p)
              (let ((buffer (emacs-fileio-gui--kill-buffer-state target)))
                (emacs-fileio-gui--backend-call :delete-window)
                buffer)
            (emacs-fileio-gui--backend-call
             :kill-buffer-and-window target))))
    (setq emacs-fileio-gui-buffer-name
          (or (emacs-fileio-gui--backend-call :buffer-name)
              result
              emacs-fileio-gui-buffer-name))
    emacs-fileio-gui-buffer-name))

;;;###autoload
(defun emacs-fileio-gui-list-buffers ()
  "Display the GUI bridge buffer list through the backend."
  (let ((result nil))
    (emacs-fileio-gui--add-buffer "*Buffer List*")
    (if (emacs-fileio-gui--backend-call
         :show-buffer-list (emacs-fileio-gui-buffer-list-text nil))
        nil
      (setq result (emacs-fileio-gui--backend-call :list-buffers)))
    (setq emacs-fileio-gui-buffer-name
          (or (emacs-fileio-gui--backend-call :buffer-name)
              result
              emacs-fileio-gui-buffer-name))
    emacs-fileio-gui-buffer-name))

;;;###autoload
(defun emacs-fileio-gui-project-list-buffers ()
  "Display buffers for the current GUI bridge project."
  (let ((result nil))
    (emacs-fileio-gui--add-buffer "*Buffer List*")
    (if (emacs-fileio-gui--backend-call
         :show-buffer-list (emacs-fileio-gui-buffer-list-text t))
        nil
      (setq result
            (emacs-fileio-gui--backend-call :project-list-buffers)))
    (setq emacs-fileio-gui-buffer-name
          (or (emacs-fileio-gui--backend-call :buffer-name)
              result
              emacs-fileio-gui-buffer-name))
    emacs-fileio-gui-buffer-name))

;;;###autoload
(defun emacs-fileio-gui-project-kill-buffers ()
  "Kill buffers for the current GUI bridge project."
  (let ((result
         (if (emacs-fileio-gui--low-level-buffer-backend-p)
             (emacs-fileio-gui--project-kill-buffer-states)
           (emacs-fileio-gui--backend-call :project-kill-buffers))))
    (setq emacs-fileio-gui-buffer-name
          (or (emacs-fileio-gui--backend-call :buffer-name)
              result
              emacs-fileio-gui-buffer-name))
    emacs-fileio-gui-buffer-name))

;;;###autoload
(defun emacs-fileio-gui-find-file-command (&optional action read-only)
  "Run the GUI bridge `find-file' command variant.
ACTION is nil/same, `other', `frame', or `tab'.  READ-ONLY marks the
visited buffer read-only after a successful visit."
  (emacs-fileio-gui-find-file (or action "same") read-only))

;;;###autoload
(defun emacs-fileio-gui-find-file-read-only-command (&optional action)
  "Run the GUI bridge read-only `find-file' command variant."
  (emacs-fileio-gui-find-file-command (or action "same") t))

;;;###autoload
(defun emacs-fileio-gui-find-alternate-file-command ()
  "Run the GUI bridge `find-alternate-file' command."
  (emacs-fileio-gui-find-file-command "same" nil))

;;;###autoload
(defun emacs-fileio-gui-project-find-file-command ()
  "Run `project-find-file' using GUI file/buffer runtime policy."
  (let ((original emacs-fileio-gui-arg)
        (target (emacs-fileio-gui-project-file-name)))
    (setq emacs-fileio-gui-arg target)
    (unwind-protect
        (emacs-fileio-gui-find-file-command "same" nil)
      (setq emacs-fileio-gui-arg original))))

;;;###autoload
(defun emacs-fileio-gui-project-or-external-find-file-command ()
  "Run `project-or-external-find-file' using GUI runtime policy.
Relative input is resolved under the current project when that target
exists.  Otherwise the original input is visited."
  (let* ((original emacs-fileio-gui-arg)
         (target (emacs-fileio-gui-project-file-name))
         (chosen (if (emacs-fileio-gui--file-exists-p target)
                     target
                   original)))
    (setq emacs-fileio-gui-arg chosen)
    (unwind-protect
        (emacs-fileio-gui-find-file-command "same" nil)
      (setq emacs-fileio-gui-arg original))))

;;;###autoload
(defun emacs-fileio-gui-save-buffer-command ()
  "Run the GUI bridge `save-buffer' command."
  (emacs-fileio-gui-save-buffer))

;;;###autoload
(defun emacs-fileio-gui-save-some-buffers-command ()
  "Run the GUI bridge `save-some-buffers' command."
  (emacs-fileio-gui-save-some-buffers))

;;;###autoload
(defun emacs-fileio-gui-write-file-command ()
  "Run the GUI bridge `write-file' command."
  (emacs-fileio-gui-write-file))

;;;###autoload
(defun emacs-fileio-gui-insert-file-command ()
  "Run the GUI bridge `insert-file' command."
  (emacs-fileio-gui-insert-file))

;;;###autoload
(defun emacs-fileio-gui-insert-buffer-command ()
  "Run the GUI bridge `insert-buffer' command."
  (emacs-fileio-gui-insert-buffer))

;;;###autoload
(defun emacs-fileio-gui-revert-buffer-command ()
  "Run the GUI bridge `revert-buffer' command."
  (emacs-fileio-gui-revert-buffer))

;;;###autoload
(defun emacs-fileio-gui-switch-to-buffer-command (&optional action)
  "Run the GUI bridge `switch-to-buffer' command variant.
ACTION is nil/same, `other', `frame', or `tab'."
  (emacs-fileio-gui-switch-to-buffer (or action "same")))

;;;###autoload
(defun emacs-fileio-gui-display-buffer-command (&optional action)
  "Run the GUI bridge `display-buffer' command variant."
  (emacs-fileio-gui-switch-to-buffer-command (or action "other")))

;;;###autoload
(defun emacs-fileio-gui-rename-buffer-command ()
  "Run the GUI bridge `rename-buffer' command."
  (emacs-fileio-gui-rename-buffer))

;;;###autoload
(defun emacs-fileio-gui-kill-buffer-command ()
  "Run the GUI bridge `kill-buffer' command."
  (emacs-fileio-gui-kill-buffer))

;;;###autoload
(defun emacs-fileio-gui-kill-buffer-and-window-command ()
  "Run the GUI bridge `kill-buffer-and-window' command."
  (emacs-fileio-gui-kill-buffer-and-window))

;;;###autoload
(defun emacs-fileio-gui-list-buffers-command ()
  "Run the GUI bridge `list-buffers' command."
  (emacs-fileio-gui-list-buffers))

;;;###autoload
(defun emacs-fileio-gui-project-list-buffers-command ()
  "Run the GUI bridge `project-list-buffers' command."
  (emacs-fileio-gui-project-list-buffers))

;;;###autoload
(defun emacs-fileio-gui-project-kill-buffers-command ()
  "Run the GUI bridge `project-kill-buffers' command."
  (emacs-fileio-gui-project-kill-buffers))

;;;###autoload
(defun emacs-fileio-gui-find-file-current-context-command
    (&optional action read-only)
  "Refresh backend context and run `find-file' GUI command.
ACTION is nil/same, `other', `frame', or `tab'.  READ-ONLY marks the
visited buffer read-only after a successful visit."
  (emacs-fileio-gui-refresh-context-from-backend)
  (emacs-fileio-gui-find-file-command action read-only))

;;;###autoload
(defun emacs-fileio-gui-find-file-read-only-current-context-command
    (&optional action)
  "Refresh backend context and run read-only `find-file' GUI command."
  (emacs-fileio-gui-refresh-context-from-backend)
  (emacs-fileio-gui-find-file-read-only-command action))

;;;###autoload
(defun emacs-fileio-gui-find-alternate-file-current-context-command ()
  "Refresh backend context and run `find-alternate-file' GUI command."
  (emacs-fileio-gui-refresh-context-from-backend)
  (emacs-fileio-gui-find-alternate-file-command))

;;;###autoload
(defun emacs-fileio-gui-project-find-file-current-context-command ()
  "Refresh backend context and run `project-find-file' GUI command."
  (emacs-fileio-gui-refresh-context-from-backend)
  (emacs-fileio-gui-project-find-file-command))

;;;###autoload
(defun emacs-fileio-gui-project-or-external-find-file-current-context-command ()
  "Refresh backend context and run `project-or-external-find-file'."
  (emacs-fileio-gui-refresh-context-from-backend)
  (emacs-fileio-gui-project-or-external-find-file-command))

;;;###autoload
(defun emacs-fileio-gui-save-buffer-current-context-command ()
  "Refresh backend context and run `save-buffer' GUI command."
  (emacs-fileio-gui-refresh-context-from-backend)
  (emacs-fileio-gui-save-buffer-command))

;;;###autoload
(defun emacs-fileio-gui-save-some-buffers-current-context-command ()
  "Refresh backend context and run `save-some-buffers' GUI command."
  (emacs-fileio-gui-refresh-context-from-backend)
  (emacs-fileio-gui-save-some-buffers-command))

;;;###autoload
(defun emacs-fileio-gui-write-file-current-context-command ()
  "Refresh backend context and run `write-file' GUI command."
  (emacs-fileio-gui-refresh-context-from-backend)
  (emacs-fileio-gui-write-file-command))

;;;###autoload
(defun emacs-fileio-gui-insert-file-current-context-command ()
  "Refresh backend context and run `insert-file' GUI command."
  (emacs-fileio-gui-refresh-context-from-backend)
  (emacs-fileio-gui-insert-file-command))

;;;###autoload
(defun emacs-fileio-gui-insert-buffer-current-context-command ()
  "Refresh backend context and run `insert-buffer' GUI command."
  (emacs-fileio-gui-refresh-context-from-backend)
  (emacs-fileio-gui-insert-buffer-command))

;;;###autoload
(defun emacs-fileio-gui-revert-buffer-current-context-command ()
  "Refresh backend context and run `revert-buffer' GUI command."
  (emacs-fileio-gui-refresh-context-from-backend)
  (emacs-fileio-gui-revert-buffer-command))

;;;###autoload
(defun emacs-fileio-gui-switch-to-buffer-current-context-command
    (&optional action)
  "Refresh backend context and run `switch-to-buffer' GUI command.
ACTION is nil/same, `other', `frame', or `tab'."
  (emacs-fileio-gui-refresh-context-from-backend)
  (emacs-fileio-gui-switch-to-buffer-command action))

;;;###autoload
(defun emacs-fileio-gui-display-buffer-current-context-command
    (&optional action)
  "Refresh backend context and run `display-buffer' GUI command."
  (emacs-fileio-gui-refresh-context-from-backend)
  (emacs-fileio-gui-display-buffer-command action))

;;;###autoload
(defun emacs-fileio-gui-rename-buffer-current-context-command ()
  "Refresh backend context and run `rename-buffer' GUI command."
  (emacs-fileio-gui-refresh-context-from-backend)
  (emacs-fileio-gui-rename-buffer-command))

;;;###autoload
(defun emacs-fileio-gui-kill-buffer-current-context-command ()
  "Refresh backend context and run `kill-buffer' GUI command."
  (emacs-fileio-gui-refresh-context-from-backend)
  (emacs-fileio-gui-kill-buffer-command))

;;;###autoload
(defun emacs-fileio-gui-kill-buffer-and-window-current-context-command ()
  "Refresh backend context and run `kill-buffer-and-window' GUI command."
  (emacs-fileio-gui-refresh-context-from-backend)
  (emacs-fileio-gui-kill-buffer-and-window-command))

;;;###autoload
(defun emacs-fileio-gui-list-buffers-current-context-command ()
  "Refresh backend context and run `list-buffers' GUI command."
  (emacs-fileio-gui-refresh-context-from-backend)
  (emacs-fileio-gui-list-buffers-command))

;;;###autoload
(defun emacs-fileio-gui-project-list-buffers-current-context-command ()
  "Refresh backend context and run `project-list-buffers' GUI command."
  (emacs-fileio-gui-refresh-context-from-backend)
  (emacs-fileio-gui-project-list-buffers-command))

;;;###autoload
(defun emacs-fileio-gui-project-kill-buffers-current-context-command ()
  "Refresh backend context and run `project-kill-buffers' GUI command."
  (emacs-fileio-gui-refresh-context-from-backend)
  (emacs-fileio-gui-project-kill-buffers-command))

;;;###autoload
(defun emacs-fileio-gui-command-spec (&optional command)
  "Return normalized GUI file/buffer command spec for COMMAND.
The result is a plist with `:command', `:action', and `:read-only'.
Return nil when COMMAND is not owned by the file/buffer GUI runtime."
  (let ((command (cond
                  ((symbolp command) command)
                  ((stringp command) (intern command))
                  (t nil))))
    (cond
     ((eq command 'find-file)
      '(:command find-file :action "same" :read-only nil))
     ((eq command 'find-file-other-window)
      '(:command find-file :action "other" :read-only nil))
     ((eq command 'find-file-other-frame)
      '(:command find-file :action "frame" :read-only nil))
     ((eq command 'find-file-other-tab)
      '(:command find-file :action "tab" :read-only nil))
     ((eq command 'find-file-read-only)
      '(:command find-file :action "same" :read-only t))
     ((eq command 'find-file-read-only-other-window)
      '(:command find-file :action "other" :read-only t))
     ((eq command 'find-file-read-only-other-frame)
      '(:command find-file :action "frame" :read-only t))
     ((eq command 'find-file-read-only-other-tab)
      '(:command find-file :action "tab" :read-only t))
     ((memq command '(find-alternate-file project-find-file
                      project-or-external-find-file save-some-buffers
                      write-file insert-file insert-buffer rename-buffer
                      kill-buffer kill-buffer-and-window list-buffers
                      project-list-buffers project-kill-buffers))
      (list :command command :action nil :read-only nil))
     ((memq command '(save-buffer basic-save-buffer))
      '(:command save-buffer :action nil :read-only nil))
     ((memq command '(revert-buffer revert-buffer-quick))
      '(:command revert-buffer :action nil :read-only nil))
     ((memq command '(switch-to-buffer project-switch-to-buffer))
      '(:command switch-to-buffer :action "same" :read-only nil))
     ((eq command 'switch-to-buffer-other-window)
      '(:command switch-to-buffer :action "other" :read-only nil))
     ((eq command 'switch-to-buffer-other-frame)
      '(:command switch-to-buffer :action "frame" :read-only nil))
     ((eq command 'switch-to-buffer-other-tab)
      '(:command switch-to-buffer :action "tab" :read-only nil))
     ((eq command 'display-buffer)
      '(:command display-buffer :action "other" :read-only nil))
     ((eq command 'display-buffer-other-frame)
      '(:command display-buffer :action "frame" :read-only nil))
     (t nil))))

;;;###autoload
(defun emacs-fileio-gui-current-context-command
    (command &optional action read-only)
  "Refresh backend context and run GUI file/buffer COMMAND.
ACTION is nil/same, `other', `frame', or `tab' for display variants.
READ-ONLY is meaningful for `find-file' variants."
  (let* ((spec (emacs-fileio-gui-command-spec command))
         (command (or (plist-get spec :command) command))
         (action (or action (plist-get spec :action)))
         (read-only (or read-only (plist-get spec :read-only))))
    (cond
     ((eq command 'find-file)
      (emacs-fileio-gui-find-file-current-context-command action read-only))
     ((eq command 'find-alternate-file)
      (emacs-fileio-gui-find-alternate-file-current-context-command))
     ((eq command 'project-find-file)
      (emacs-fileio-gui-project-find-file-current-context-command))
     ((eq command 'project-or-external-find-file)
      (emacs-fileio-gui-project-or-external-find-file-current-context-command))
     ((eq command 'save-buffer)
      (emacs-fileio-gui-save-buffer-current-context-command))
     ((eq command 'save-some-buffers)
      (emacs-fileio-gui-save-some-buffers-current-context-command))
     ((eq command 'write-file)
      (emacs-fileio-gui-write-file-current-context-command))
     ((eq command 'insert-file)
      (emacs-fileio-gui-insert-file-current-context-command))
     ((eq command 'insert-buffer)
      (emacs-fileio-gui-insert-buffer-current-context-command))
     ((eq command 'revert-buffer)
      (emacs-fileio-gui-revert-buffer-current-context-command))
     ((eq command 'switch-to-buffer)
      (emacs-fileio-gui-switch-to-buffer-current-context-command action))
     ((eq command 'display-buffer)
      (emacs-fileio-gui-display-buffer-current-context-command action))
     ((eq command 'rename-buffer)
      (emacs-fileio-gui-rename-buffer-current-context-command))
     ((eq command 'kill-buffer)
      (emacs-fileio-gui-kill-buffer-current-context-command))
     ((eq command 'kill-buffer-and-window)
      (emacs-fileio-gui-kill-buffer-and-window-current-context-command))
     ((eq command 'list-buffers)
      (emacs-fileio-gui-list-buffers-current-context-command))
     ((eq command 'project-list-buffers)
      (emacs-fileio-gui-project-list-buffers-current-context-command))
     ((eq command 'project-kill-buffers)
      (emacs-fileio-gui-project-kill-buffers-current-context-command))
     (t nil))))

;;;###autoload
(defun emacs-fileio-gui-writeback-spec (&optional command status)
  "Return GUI transport writeback spec for file/buffer COMMAND.
The file/buffer runtime owns command-to-state policy; bridge adapters
interpret the returned plist by writing their transport stores.  STATUS
defaults to `emacs-fileio-gui-status'.  Return nil when COMMAND is not a
file/buffer writeback command or when STATUS should suppress writeback."
  (let* ((command (cond
                   ((symbolp command) (symbol-name command))
                   ((stringp command) command)
                   (t "")))
         (status (or status emacs-fileio-gui-status "ok")))
    (cond
     ((member command
              '("find-file" "project-find-file"
                "project-or-external-find-file"
                "find-file-other-window" "find-file-read-only"
                "find-file-read-only-other-window" "find-alternate-file"))
      '(:buffer t :file t :read-only t :window t :point t))
     ((member command
              '("find-file-other-frame"
                "find-file-read-only-other-frame"))
      '(:buffer t :file t :read-only t :window t :frame t :point t))
     ((member command
              '("find-file-other-tab"
                "find-file-read-only-other-tab"))
      '(:buffer t :file t :read-only t :window t :tab t :point t))
     ((member command '("insert-file" "insert-buffer"))
      '(:buffer t :point t :mark t))
     ((equal command "write-file")
      '(:file t :point t))
     ((and (member command '("save-buffer" "basic-save-buffer"))
           (equal status "ok"))
      '(:file t :point t))
     ((equal command "save-some-buffers")
      '(:file t :read-only t :point t :mark t :window-start t))
     ((member command '("revert-buffer" "revert-buffer-quick"))
      '(:buffer t :file t :point t))
     ((member command
              '("switch-to-buffer" "scratch-buffer" "messages-buffer"
                "warnings-buffer" "project-switch-to-buffer"
                "switch-to-buffer-other-window" "display-buffer"
                "rename-buffer" "rename-uniquely" "clone-buffer"
                "clone-indirect-buffer-other-window" "kill-buffer"
                "kill-buffer-and-window" "project-kill-buffers"
                "list-buffers" "project-list-buffers"))
      '(:buffer t :file t :buffer-name t :window t
        :point t :mark t :window-start t))
     ((member command
              '("switch-to-buffer-other-frame"
                "display-buffer-other-frame"))
      '(:buffer t :file t :buffer-name t :window t :frame t
        :point t :mark t :window-start t))
     ((equal command "switch-to-buffer-other-tab")
      '(:buffer t :file t :buffer-name t :window t :tab t
        :point t :mark t :window-start t))
     (t nil))))

;;;###autoload
(defun emacs-fileio-gui-writeback-spec-flag (spec key)
  "Return non-nil when file/buffer GUI writeback SPEC enables KEY."
  (and spec (plist-get spec key)))

;;;###autoload
(defun emacs-fileio-gui-writeback-state (&optional command status)
  "Write GUI transport state for file/buffer COMMAND.
The file/buffer runtime owns writeback spec interpretation and callback
ordering.  The registered backend owns concrete transport writes.  Return
non-nil when COMMAND has a file/buffer writeback spec."
  (let ((spec (emacs-fileio-gui-writeback-spec command status)))
    (when spec
      (when (emacs-fileio-gui-writeback-spec-flag spec :buffer)
        (emacs-fileio-gui--backend-call :write-buffer-state))
      (when (emacs-fileio-gui-writeback-spec-flag spec :file)
        (emacs-fileio-gui--backend-call :write-file-state))
      (when (emacs-fileio-gui-writeback-spec-flag spec :buffer-name)
        (emacs-fileio-gui--backend-call :write-buffer-name-state))
      (when (emacs-fileio-gui-writeback-spec-flag spec :read-only)
        (emacs-fileio-gui--backend-call :write-read-only-state))
      (when (emacs-fileio-gui-writeback-spec-flag spec :window)
        (emacs-fileio-gui--backend-call :write-window-state))
      (when (emacs-fileio-gui-writeback-spec-flag spec :frame)
        (emacs-fileio-gui--backend-call :write-frame-state))
      (when (emacs-fileio-gui-writeback-spec-flag spec :tab)
        (emacs-fileio-gui--backend-call :write-tab-state))
      (when (emacs-fileio-gui-writeback-spec-flag spec :point)
        (emacs-fileio-gui--backend-call :write-point-state))
      (when (emacs-fileio-gui-writeback-spec-flag spec :mark)
        (emacs-fileio-gui--backend-call :write-mark-state))
      (when (emacs-fileio-gui-writeback-spec-flag spec :window-start)
        (emacs-fileio-gui--backend-call :write-window-start-state))
      (emacs-fileio-gui--backend-call :mark-written-state)
      t)))

(provide 'emacs-fileio-gui)

;;; emacs-fileio-gui.el ends here
