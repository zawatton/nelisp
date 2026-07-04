;;; emacs-buffer-ui.el --- Interactive buffer commands on top of Nelisp buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 02 v0.1 Daily-Driver §3.1 M1 (item 4).
;;
;; Provides the interactive UI layer for buffer switching / killing /
;; listing without modifying `emacs-buffer.el'.  Name-collision policy:
;; host Emacs already defines `switch-to-buffer', `kill-buffer', and
;; `list-buffers', while this repo already uses `kill-buffer' as the
;; primitive NeLisp bridge.  To avoid shadowing those existing entry
;; points, the wrappers live under `*-interactive' aliases (plus
;; `emacs-buffer-ui-*' canonical names).

;;; Code:

(defconst emacs-buffer-ui--load-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory that contains the buffer-ui shim and its sibling features.")

(defun emacs-buffer-ui--load-feature (feature)
  "Load FEATURE from the buffer-ui shim directory."
  (let ((file (expand-file-name (concat (symbol-name feature) ".el")
                                emacs-buffer-ui--load-directory)))
    (unless (load file nil t)
      (require feature))))

(require 'cl-lib)
(require 'emacs-buffer)
(emacs-buffer-ui--load-feature 'emacs-fileio-builtins)
(require 'emacs-minibuffer)
(require 'emacs-window)

(defconst emacs-buffer-ui--list-buffer-name "*Buffer List*"
  "Buffer name used by `emacs-buffer-ui-list-buffers'.")

(defun emacs-buffer-ui--find-buffer (name)
  "Return the live NeLisp buffer named NAME, or nil."
  (cl-find-if (lambda (buf)
                (equal name (nelisp-ec-buffer-name buf)))
              (emacs-buffer-buffer-list)))

;;;###autoload
(defun emacs-buffer-ui-find-buffer (name)
  "Return the live NeLisp buffer named NAME, or nil."
  (emacs-buffer-ui--find-buffer name))

(defun emacs-buffer-ui-switch-to-buffer-plan
    (input &optional buffer-exists-p)
  "Return a frontend-neutral switch-to-buffer plan for INPUT.
BUFFER-EXISTS-P is an optional predicate called with INPUT.  When omitted,
`emacs-buffer-ui-find-buffer' is used.  The result plist contains
`:status', `:message', and, on success, `:buffer-name' and
`:scroll-offset'."
  (cond
   ((or (null input) (string-empty-p input))
    (list :status 'empty
          :message "switch-to-buffer: empty"))
   ((not (if buffer-exists-p
             (funcall buffer-exists-p input)
           (emacs-buffer-ui-find-buffer input)))
    (list :status 'missing
          :buffer-name input
          :message (format "No buffer: %s" input)))
   (t
    (list :status 'ok
          :buffer-name input
          :scroll-offset 0
          :message (format "Switched: %s" input)))))

(defun emacs-buffer-ui-kill-buffer-plan
    (buffer-name &optional protected-name fallback-name)
  "Return a frontend-neutral kill-buffer plan for BUFFER-NAME.
PROTECTED-NAME defaults to \"*welcome*\" and is refused.  FALLBACK-NAME
defaults to PROTECTED-NAME.  On success the result contains
`:buffer-name', `:fallback-buffer', `:scroll-offset', and `:message'."
  (let ((protected (or protected-name "*welcome*")))
    (cond
     ((equal buffer-name protected)
      (list :status 'refused
            :buffer-name buffer-name
            :message (format "kill-buffer: refusing %s" protected)))
     (t
      (list :status 'ok
            :buffer-name buffer-name
            :fallback-buffer (or fallback-name protected)
            :scroll-offset 0
            :message (format "Killed: %s" buffer-name))))))

(defun emacs-buffer-ui-confirm-kill-buffer
    (buffer name read-confirmation-function)
  "Return non-nil when BUFFER named NAME may be killed.
Modified buffers call READ-CONFIRMATION-FUNCTION with the standard
\"modified; kill anyway\" prompt.  Accepted confirmations are \"yes\",
\"y\", \"YES\", and \"Y\"."
  (if (and (fboundp 'emacs-buffer-buffer-modified-p)
           (emacs-buffer-buffer-modified-p buffer))
      (let ((answer
             (funcall read-confirmation-function
                      (format "Buffer %s modified; kill anyway? " name))))
        (and answer (member answer '("yes" "y" "YES" "Y"))))
    t))

(defun emacs-buffer-ui-buffer-menu-entry (name &optional file modified-p)
  "Return one buffer-menu entry for NAME, FILE, and MODIFIED-P.
Hidden or empty buffer names return nil.  The entry shape is
`(LABEL . ACTION)', where ACTION is \"switch-to-buffer:NAME\"."
  (when (and (stringp name)
             (> (length name) 0)
             (not (eq (aref name 0) ?\s)))
    (let ((label (if file
                     (format "%s%s  (%s)"
                             (if modified-p "* " "  ")
                             name
                             file)
                   (format "%s%s" (if modified-p "* " "  ") name))))
      (cons label (concat "switch-to-buffer:" name)))))

(defun emacs-buffer-ui-buffer-menu-spec (buffers &rest plist)
  "Build a frontend-neutral buffer menu spec for BUFFERS.
PLIST accepts `:name-function', `:file-function', and
`:modified-function'.  Missing functions default to NeLisp buffer
accessors."
  (let ((name-fn (or (plist-get plist :name-function)
                     #'nelisp-ec-buffer-name))
        (file-fn (or (plist-get plist :file-function)
                     #'emacs-buffer-ui--buffer-file-name))
        (modified-fn (or (plist-get plist :modified-function)
                         #'emacs-buffer-buffer-modified-p))
        entries)
    (dolist (buf buffers)
      (let* ((name (and name-fn (funcall name-fn buf)))
             (file (and file-fn (funcall file-fn buf)))
             (modified-p (and modified-fn (funcall modified-fn buf)))
             (entry (emacs-buffer-ui-buffer-menu-entry
                     name file modified-p)))
        (when entry
          (push entry entries))))
    (nreverse entries)))

(defun emacs-buffer-ui--buffer-mode-name (buf)
  "Return a printable major-mode name for BUF."
  (condition-case _err
      (let ((mode (emacs-buffer-buffer-local-value 'major-mode buf)))
        (cond
         ((symbolp mode) (symbol-name mode))
         ((stringp mode) mode)
         (t "fundamental-mode")))
    (error "fundamental-mode")))

(defun emacs-buffer-ui--buffer-file-name (buf)
  "Return BUF's visited file path, or nil."
  (condition-case nil
      (and (fboundp 'buffer-file-name)
           (buffer-file-name buf))
    (error nil)))

(defun emacs-buffer-ui-current-buffer-name ()
  "Return the current NeLisp buffer name, or nil when unavailable."
  (let ((buffer (and (fboundp 'nelisp-ec-current-buffer)
                     (nelisp-ec-current-buffer))))
    (and buffer
         (fboundp 'nelisp-ec-buffer-name)
         (nelisp-ec-buffer-name buffer))))

(defun emacs-buffer-ui-get-or-create-text-buffer (name)
  "Return a writable text buffer named NAME.
This helper is intentionally frontend-neutral: GUI/TUI adapters may
display the returned buffer however they need."
  (or (and (boundp 'nelisp-ec--buffers)
           (cdr (assoc name nelisp-ec--buffers)))
      (and (fboundp 'nelisp-ec-generate-new-buffer)
           (nelisp-ec-generate-new-buffer name))
      (get-buffer-create name)))

(defun emacs-buffer-ui-move-to-buffer-start (&optional buffer)
  "Move point to `point-min' in BUFFER or the current buffer.
Return a frontend-neutral display-position plist containing `:point' and
`:scroll-offset'."
  (cond
   ((and buffer
         (fboundp 'nelisp-ec-buffer-p)
         (nelisp-ec-buffer-p buffer)
         (fboundp 'nelisp-ec-with-current-buffer))
    (nelisp-ec-with-current-buffer buffer
      (let ((point (nelisp-ec-point-min)))
        (nelisp-ec-goto-char point)
        (list :status 'moved
              :buffer buffer
              :point point
              :scroll-offset 0))))
   (buffer
    (with-current-buffer buffer
      (goto-char (point-min))
      (list :status 'moved
            :buffer buffer
            :point (point)
            :scroll-offset 0)))
   ((and (fboundp 'nelisp-ec-point-min)
         (fboundp 'nelisp-ec-goto-char))
    (let ((point (nelisp-ec-point-min)))
      (nelisp-ec-goto-char point)
      (list :status 'moved
            :buffer (and (fboundp 'nelisp-ec-current-buffer)
                         (nelisp-ec-current-buffer))
            :point point
            :scroll-offset 0)))
   (t
    (goto-char (point-min))
    (list :status 'moved
          :buffer (current-buffer)
          :point (point)
          :scroll-offset 0))))

(defun emacs-buffer-ui-replace-text-buffer
    (name text &optional ensure-final-newline)
  "Replace buffer NAME contents with TEXT and return a result plist.
When ENSURE-FINAL-NEWLINE is non-nil and TEXT does not end in a newline,
append one before insertion.  The result contains `:status', `:buffer',
`:buffer-name', `:text', and `:length'."
  (let* ((buffer (emacs-buffer-ui-get-or-create-text-buffer name))
         (body (if (and ensure-final-newline
                        (not (string-suffix-p "\n" text)))
                   (concat text "\n")
                 text)))
    (if (and (fboundp 'nelisp-ec-with-current-buffer)
             (fboundp 'nelisp-ec-buffer-p)
             (nelisp-ec-buffer-p buffer))
        (nelisp-ec-with-current-buffer buffer
          (nelisp-ec-erase-buffer)
          (nelisp-ec-insert body)
          (emacs-buffer-ui-move-to-buffer-start))
      (with-current-buffer buffer
        (when (fboundp 'erase-buffer)
          (erase-buffer))
        (insert body)))
    (list :status 'replaced
          :buffer buffer
          :buffer-name name
          :text body
          :length (length body))))

(defun emacs-buffer-ui--replacement-buffer (killed)
  "Return a live replacement buffer after KILLED is removed."
  (or (cl-find-if (lambda (buf) (not (eq buf killed)))
                  (emacs-buffer-buffer-list))
      (nelisp-ec-generate-new-buffer "*scratch*")))

(defun emacs-buffer-ui--retarget-windows (killed)
  "Replace KILLED in any displaying windows with a fallback buffer."
  (let ((wins (emacs-window-get-buffer-window-list killed)))
    (when wins
      (let ((replacement (emacs-buffer-ui--replacement-buffer killed)))
        (dolist (win wins)
          (emacs-window-set-window-buffer win replacement))
        (when (eq (nelisp-ec-current-buffer) killed)
          (nelisp-ec-set-buffer replacement))))))

;;;###autoload
(defun emacs-buffer-ui-switch-to-buffer (buffer-or-name)
  "Switch the selected window to BUFFER-OR-NAME.
When called interactively, prompt with minibuffer completion over the
current NeLisp buffer names.  Unknown names create a fresh buffer."
  (interactive
   (list (emacs-minibuffer-completing-read
          "Switch to buffer: "
          (mapcar #'nelisp-ec-buffer-name (emacs-buffer-buffer-list))
          nil nil nil nil)))
  (let* ((name (cond
                ((nelisp-ec-buffer-p buffer-or-name)
                 (nelisp-ec-buffer-name buffer-or-name))
                ((stringp buffer-or-name) buffer-or-name)
                (t (signal 'wrong-type-argument
                           (list '(or stringp nelisp-ec-buffer-p)
                                 buffer-or-name)))))
         (buf (or (emacs-buffer-ui-find-buffer name)
                  (nelisp-ec-generate-new-buffer name))))
    (emacs-window-set-window-buffer (emacs-window-selected-window) buf)
    (nelisp-ec-set-buffer buf)
    buf))

;;;###autoload
(defun emacs-buffer-ui-kill-buffer-interactive (&optional buffer)
  "Interactively kill BUFFER, defaulting to the current buffer.
Modified buffers require explicit `yes-or-no-p' confirmation."
  (interactive)
  (let ((buf (or buffer (nelisp-ec-current-buffer))))
    (unless (nelisp-ec-buffer-p buf)
      (signal 'wrong-type-argument (list 'nelisp-ec-buffer-p buf)))
    (when (and (emacs-buffer-buffer-modified-p buf)
               (not (emacs-minibuffer-yes-or-no-p
                     (format "Buffer %s modified; kill anyway? "
                             (nelisp-ec-buffer-name buf)))))
      (user-error "Kill buffer aborted"))
    (emacs-buffer-ui--retarget-windows buf)
    (nelisp-ec-kill-buffer buf)))

;;;###autoload
(defun emacs-buffer-ui-list-buffers ()
  "Render a plain-text *Buffer List* buffer and display it."
  (interactive)
  (let ((out (or (emacs-buffer-ui-find-buffer emacs-buffer-ui--list-buffer-name)
                 (nelisp-ec-generate-new-buffer emacs-buffer-ui--list-buffer-name))))
    (nelisp-ec-with-current-buffer out
      (nelisp-ec-erase-buffer)
      (nelisp-ec-insert "name\tsize\tmode\tfile\n")
      (dolist (buf (emacs-buffer-buffer-list))
        (nelisp-ec-insert
         (concat (nelisp-ec-buffer-name buf)
                 "\t"
                 (number-to-string (nelisp-ec-buffer-size buf))
                 "\t"
                 (emacs-buffer-ui--buffer-mode-name buf)
                 "\t"
                 (or (emacs-buffer-ui--buffer-file-name buf) "")
                 "\n")))
      (emacs-buffer-ui-move-to-buffer-start)
      (emacs-buffer-set-buffer-modified-p nil out))
    (emacs-window-set-window-buffer (emacs-window-selected-window) out)
    (nelisp-ec-set-buffer out)
    out))

(defun emacs-buffer-ui-run-switch-buffer-command (&rest plist)
  "Run a frontend-provided switch-buffer command.
PLIST accepts `:read-string', `:current-name', `:sync-window',
`:after-success', and `:message-function'.  The read function is called
with a prompt string and may return an empty string to select the current
buffer default."
  (let* ((read-string (plist-get plist :read-string))
         (current-name (or (plist-get plist :current-name)
                           #'emacs-buffer-ui-current-buffer-name))
         (sync-window (plist-get plist :sync-window))
         (after-success (plist-get plist :after-success))
         (message-function (plist-get plist :message-function))
         (default (and current-name (funcall current-name)))
         (prompt (if default
                     (format "Switch to buffer (default %s): " default)
                   "Switch to buffer: "))
         (name (and read-string (funcall read-string prompt)))
         (target (if (and name (> (length name) 0)) name default)))
    (when (and target (> (length target) 0))
      (condition-case err
          (let ((buffer (emacs-buffer-ui-switch-to-buffer target)))
            (when sync-window
              (funcall sync-window buffer))
            (when after-success
              (funcall after-success buffer))
            buffer)
        (error
         (when message-function
           (funcall message-function
                    "switch-to-buffer failed: %S" err))
         nil)))))

(defun emacs-buffer-ui-run-switch-existing-command (&rest plist)
  "Run a frontend switch command that refuses missing buffers.
PLIST accepts `:read-string', `:begin-prompt', `:prompt',
`:buffer-exists-p', `:apply-plan', and `:status-function'.  This helper
owns the reusable prompt/plan/status command shape while frontends keep
concrete buffer selection and display-state updates."
  (let* ((read-string (plist-get plist :read-string))
         (begin-prompt (plist-get plist :begin-prompt))
         (prompt (or (plist-get plist :prompt) "Switch to buffer: "))
         (buffer-exists-p (plist-get plist :buffer-exists-p))
         (apply-plan (plist-get plist :apply-plan))
         (status-function (plist-get plist :status-function)))
    (let ((finish
           (lambda (input)
             (let ((plan (emacs-buffer-ui-switch-to-buffer-plan
                          input buffer-exists-p)))
               (when (and (eq 'ok (plist-get plan :status))
                          apply-plan)
                 (funcall apply-plan plan))
               (when status-function
                 (funcall status-function (plist-get plan :message)))
               plan))))
      (cond
       (begin-prompt
        (funcall begin-prompt prompt (lambda (input) (funcall finish input))))
       (read-string
        (funcall finish (funcall read-string prompt)))
       (t
        (funcall finish nil))))))

(defun emacs-buffer-ui-run-list-buffers-command (&rest plist)
  "Run a frontend-provided list-buffers command.
PLIST accepts `:sync-window', `:emit-text', `:after-success', and
`:message-function'."
  (let ((sync-window (plist-get plist :sync-window))
        (emit-text (plist-get plist :emit-text))
        (after-success (plist-get plist :after-success))
        (message-function (plist-get plist :message-function)))
    (condition-case err
        (let ((buffer (emacs-buffer-ui-list-buffers)))
          (when sync-window
            (funcall sync-window buffer))
          (when (and (fboundp 'emacs-window-selected-window)
                     (fboundp 'emacs-window-set-window-start)
                     (fboundp 'nelisp-ec-with-current-buffer)
                     (fboundp 'nelisp-ec-point-min))
            (emacs-window-set-window-start
             (emacs-window-selected-window)
             (nelisp-ec-with-current-buffer buffer
               (nelisp-ec-point-min))))
          (when (and emit-text
                     (fboundp 'nelisp-ec-with-current-buffer)
                     (fboundp 'nelisp-ec-buffer-string))
            (funcall emit-text
                     (nelisp-ec-with-current-buffer buffer
                       (nelisp-ec-buffer-string))))
          (when after-success
            (funcall after-success buffer))
          buffer)
      (error
       (when message-function
         (funcall message-function "list-buffers failed: %S" err))
       nil))))

(defun emacs-buffer-ui-run-kill-buffer-command (&rest plist)
  "Run a frontend-provided kill-buffer command.
PLIST accepts `:read-string', `:current-name', `:sync-window',
`:after-success', and `:message-function'.  The read function is reused
for modified-buffer confirmation."
  (let* ((read-string (plist-get plist :read-string))
         (current-name (or (plist-get plist :current-name)
                           #'emacs-buffer-ui-current-buffer-name))
         (sync-window (plist-get plist :sync-window))
         (after-success (plist-get plist :after-success))
         (message-function (plist-get plist :message-function))
         (default (and current-name (funcall current-name)))
         (prompt (if default
                     (format "Kill buffer (default %s): " default)
                   "Kill buffer: "))
         (name (and read-string (funcall read-string prompt)))
         (target (if (and name (> (length name) 0)) name default)))
    (when (and target (> (length target) 0))
      (let ((buffer (emacs-buffer-ui-find-buffer target)))
        (cond
         ((not buffer)
          (when message-function
            (funcall message-function "No buffer named %s" target))
          nil)
         ((not (emacs-buffer-ui-confirm-kill-buffer
                buffer target read-string))
          nil)
         (t
          (condition-case err
              (let ((result
                     (cl-letf (((symbol-function
                                 'emacs-minibuffer-yes-or-no-p)
                                (lambda (&rest _) t)))
                       (emacs-buffer-ui-kill-buffer-interactive buffer))))
                (when sync-window
                  (funcall sync-window nil))
                (when after-success
                  (funcall after-success buffer))
                result)
            (error
             (when message-function
               (funcall message-function "kill-buffer failed: %S" err))
             nil))))))))

(defun emacs-buffer-ui-run-kill-buffer-plan-command (&rest plist)
  "Run a frontend kill-buffer command through `emacs-buffer-ui-kill-buffer-plan'.
PLIST accepts `:current-name', `:protected-name', `:fallback-name',
`:kill-function', `:apply-plan', and `:status-function'."
  (let* ((current-name (or (plist-get plist :current-name)
                           #'emacs-buffer-ui-current-buffer-name))
         (protected-name (plist-get plist :protected-name))
         (fallback-name (plist-get plist :fallback-name))
         (kill-function (plist-get plist :kill-function))
         (apply-plan (plist-get plist :apply-plan))
         (status-function (plist-get plist :status-function))
         (buffer-name (and current-name (funcall current-name)))
         (plan (emacs-buffer-ui-kill-buffer-plan
                buffer-name protected-name fallback-name)))
    (when (eq 'ok (plist-get plan :status))
      (when kill-function
        (funcall kill-function (plist-get plan :buffer-name)))
      (when apply-plan
        (funcall apply-plan plan)))
    (when status-function
      (funcall status-function (plist-get plan :message)))
    plan))

(defalias 'switch-to-buffer-interactive #'emacs-buffer-ui-switch-to-buffer)
(defalias 'kill-buffer-interactive #'emacs-buffer-ui-kill-buffer-interactive)
(defalias 'list-buffers-interactive #'emacs-buffer-ui-list-buffers)

(unless (fboundp 'switch-to-buffer)
  (defalias 'switch-to-buffer #'emacs-buffer-ui-switch-to-buffer))

(unless (fboundp 'list-buffers)
  (defalias 'list-buffers #'emacs-buffer-ui-list-buffers))

(provide 'emacs-buffer-ui)

;;; emacs-buffer-ui.el ends here
