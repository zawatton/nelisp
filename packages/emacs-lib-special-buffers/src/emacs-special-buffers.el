;;; emacs-special-buffers.el --- Scratch/messages/warnings buffers  -*- lexical-binding: t; -*-

;;; Commentary:

;; Runtime-level special buffer semantics shared by TUI/batch and GUI bridge
;; adapters.  The core path uses the normal NeLisp buffer substrate; adapters
;; may register a backend when their buffer state is serialized elsewhere.

;;; Code:

(require 'emacs-buffer-builtins)
(require 'emacs-buffer)

(defconst emacs-special-buffers-scratch-name "*scratch*")
(defconst emacs-special-buffers-messages-name "*Messages*")
(defconst emacs-special-buffers-warnings-name "*Warnings*")

(defconst emacs-special-buffers-scratch-initial-message
  ";; This buffer is for text that is not saved, and for Lisp evaluation.\n;; To create a file, visit it with C-x C-f and enter text in its buffer.\n\n")

(defvar emacs-special-buffers-backend nil
  "Plist backend for adapters with non-core buffer storage.
Supported keys:
`:ensure' function NAME -> buffer creation side effects.
`:append' function NAME TEXT -> append side effects.
`:switch' function NAME -> display/switch side effects.")

(defun emacs-special-buffers-register-backend (&rest backend)
  "Install BACKEND plist for adapter-owned special buffer storage."
  (setq emacs-special-buffers-backend backend))

(defun emacs-special-buffers-special-buffer-p (name)
  "Return non-nil when NAME is one of the standard special buffers."
  (member name (list emacs-special-buffers-scratch-name
                     emacs-special-buffers-messages-name
                     emacs-special-buffers-warnings-name)))

(defun emacs-special-buffers-default-text (name)
  "Return initial text for special buffer NAME."
  (if (equal name emacs-special-buffers-scratch-name)
      emacs-special-buffers-scratch-initial-message
    ""))

(defun emacs-special-buffers-read-only-p (name)
  "Return non-nil when special buffer NAME should be read-only by default."
  (not (equal name emacs-special-buffers-scratch-name)))

(defun emacs-special-buffers--backend-call (key &rest args)
  "Call backend function KEY with ARGS when installed."
  (let ((fn (plist-get emacs-special-buffers-backend key)))
    (and fn (apply fn args))))

(defun emacs-special-buffers--core-buffer-substrate-p ()
  "Return non-nil when special buffers should use NeLisp core buffers."
  (and (emacs-special-buffers--standalone-p)
       (fboundp 'nelisp-ec-generate-new-buffer)
       (fboundp 'nelisp-ec-buffer-string)))

(defun emacs-special-buffers--find-buffer (name)
  "Return buffer named NAME, or nil."
  (cond
   ((and (emacs-special-buffers--core-buffer-substrate-p)
         (boundp 'nelisp-ec--buffers)
         (assoc name nelisp-ec--buffers))
    (cdr (assoc name nelisp-ec--buffers)))
   ((fboundp 'get-buffer)
    (get-buffer name))
   (t nil)))

(defun emacs-special-buffers--ensure-core-buffer (name)
  "Ensure special buffer NAME exists in the core buffer substrate."
  (unless (emacs-special-buffers-special-buffer-p name)
    (signal 'wrong-type-argument (list 'emacs-special-buffer-name name)))
  (if (emacs-special-buffers--core-buffer-substrate-p)
      (let ((buf (or (emacs-special-buffers--find-buffer name)
                     (nelisp-ec-generate-new-buffer name))))
        (nelisp-ec-with-current-buffer buf
          (when (and (equal (nelisp-ec-buffer-string) "")
                     (not (equal (emacs-special-buffers-default-text name) "")))
            (nelisp-ec-insert (emacs-special-buffers-default-text name)))
          (when (boundp 'buffer-read-only)
            (setq buffer-read-only
                  (emacs-special-buffers-read-only-p name)))
          (when (fboundp 'emacs-buffer-set-buffer-modified-p)
            (emacs-buffer-set-buffer-modified-p nil buf)))
        buf)
    (let ((buf (or (emacs-special-buffers--find-buffer name)
                   (get-buffer-create name))))
      (with-current-buffer buf
        (when (and (equal (buffer-string) "")
                   (not (equal (emacs-special-buffers-default-text name) "")))
          (insert (emacs-special-buffers-default-text name)))
        (when (boundp 'buffer-read-only)
          (setq buffer-read-only
                (emacs-special-buffers-read-only-p name)))
        (when (fboundp 'set-buffer-modified-p)
          (set-buffer-modified-p nil)))
      buf)))

(defun emacs-special-buffers-ensure-buffer (name)
  "Ensure special buffer NAME exists and return it when possible."
  (or (emacs-special-buffers--backend-call :ensure name)
      (emacs-special-buffers--ensure-core-buffer name)))

(defun emacs-special-buffers-ensure-standard-buffers ()
  "Ensure the standard scratch/messages/warnings buffers exist."
  (dolist (name (list emacs-special-buffers-scratch-name
                      emacs-special-buffers-messages-name
                      emacs-special-buffers-warnings-name))
    (emacs-special-buffers-ensure-buffer name))
  t)

(defun emacs-special-buffers-display-plan (name &optional message)
  "Ensure special buffer NAME and return a frontend-neutral display plan.
The result contains `:status', `:buffer', `:buffer-name',
`:scroll-offset', and `:message'.  MESSAGE defaults to NAME."
  (let ((buffer (emacs-special-buffers-ensure-buffer name)))
    (list :status 'ok
          :buffer buffer
          :buffer-name name
          :scroll-offset 0
          :message (or message name))))

(defun emacs-special-buffers-append-to-buffer (name text)
  "Append TEXT to special buffer NAME."
  (or (emacs-special-buffers--backend-call :append name text)
      (let ((buf (emacs-special-buffers-ensure-buffer name)))
        (if (and (fboundp 'nelisp-ec-buffer-p)
                 (nelisp-ec-buffer-p buf))
            (nelisp-ec-with-current-buffer buf
              (nelisp-ec-goto-char (nelisp-ec-point-max))
              (nelisp-ec-insert text)
              (when (fboundp 'emacs-buffer-set-buffer-modified-p)
                (emacs-buffer-set-buffer-modified-p nil buf)))
          (with-current-buffer buf
            (let ((was-read-only (and (boundp 'buffer-read-only)
                                      buffer-read-only)))
              (when (boundp 'buffer-read-only)
                (setq buffer-read-only nil))
              (goto-char (point-max))
              (insert text)
              (when (boundp 'buffer-read-only)
                (setq buffer-read-only was-read-only))
              (when (fboundp 'set-buffer-modified-p)
                (set-buffer-modified-p nil)))))
        buf)))

(defun emacs-special-buffers-switch-to-buffer (name)
  "Switch to special buffer NAME."
  (or (emacs-special-buffers--backend-call :switch name)
      (let ((buf (emacs-special-buffers-ensure-buffer name)))
        (cond
         ((and (fboundp 'nelisp-ec-buffer-p)
               (nelisp-ec-buffer-p buf))
          (when (fboundp 'nelisp-ec-set-buffer)
            (nelisp-ec-set-buffer buf)))
         ((fboundp 'switch-to-buffer)
          (switch-to-buffer buf))
         ((fboundp 'set-buffer)
          (set-buffer buf)))
        buf)))

(defun emacs-special-buffers-message (format-string &rest args)
  "Record formatted message in `*Messages*' and return it."
  (when format-string
    (let ((text (apply #'format format-string args)))
      (emacs-special-buffers-append-to-buffer
       emacs-special-buffers-messages-name
       (concat text "\n"))
      text)))

(defun emacs-special-buffers-display-warning
    (type message &optional level buffer-name)
  "Record warning TYPE and MESSAGE in warnings/messages buffers."
  (ignore level)
  (let ((line (format "Warning [%s]: %s" type message)))
    (emacs-special-buffers-append-to-buffer
     (or buffer-name emacs-special-buffers-warnings-name)
     (concat line "\n"))
    (emacs-special-buffers-append-to-buffer
     emacs-special-buffers-messages-name
     (concat line "\n"))
    line))

(defun emacs-special-buffers-lwarn (type level message &rest args)
  "Compatibility implementation of `lwarn'."
  (emacs-special-buffers-display-warning
   type (apply #'format message args) level))

(defun emacs-special-buffers-warn (message &rest args)
  "Compatibility implementation of `warn'."
  (apply #'emacs-special-buffers-lwarn 'emacs 'warning message args))

(defun view-echo-area-messages ()
  "Display the persisted echo-area message history."
  (interactive)
  (emacs-special-buffers-switch-to-buffer
   emacs-special-buffers-messages-name))

(defun scratch-buffer ()
  "Switch to `*scratch*', creating it if needed."
  (interactive)
  (emacs-special-buffers-switch-to-buffer
   emacs-special-buffers-scratch-name))

(defun messages-buffer ()
  "Switch to `*Messages*', creating it if needed."
  (interactive)
  (emacs-special-buffers-switch-to-buffer
   emacs-special-buffers-messages-name))

(defun warnings-buffer ()
  "Switch to `*Warnings*', creating it if needed."
  (interactive)
  (emacs-special-buffers-switch-to-buffer
   emacs-special-buffers-warnings-name))

(defun get-scratch-buffer-create ()
  "Return the `*scratch*' buffer, creating it if needed."
  (emacs-special-buffers-ensure-buffer
   emacs-special-buffers-scratch-name))

(defun emacs-special-buffers--standalone-p ()
  "Return non-nil when running under the standalone NeLisp reader."
  (or (fboundp 'nl-write-file)
      (not (boundp 'emacs-version))))

(when (emacs-special-buffers--standalone-p)
  (fset 'message #'emacs-special-buffers-message)
  (fset 'display-warning #'emacs-special-buffers-display-warning)
  (fset 'lwarn #'emacs-special-buffers-lwarn)
  (fset 'warn #'emacs-special-buffers-warn))

(provide 'emacs-special-buffers)

;;; emacs-special-buffers.el ends here
