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

(require 'cl-lib)
(require 'emacs-buffer)
(require 'emacs-fileio-builtins)
(require 'emacs-minibuffer)
(require 'emacs-window)

(declare-function files--buffer-file-name "files-standalone-buffer"
                  (&optional buffer))

(defconst emacs-buffer-ui--list-buffer-name "*Buffer List*"
  "Buffer name used by `emacs-buffer-ui-list-buffers'.")

(defun emacs-buffer-ui--find-buffer (name)
  "Return the live NeLisp buffer named NAME, or nil."
  (cl-find-if (lambda (buf)
                (equal name (nelisp-ec-buffer-name buf)))
              (emacs-buffer-buffer-list)))

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
  (or (cdr (assq buf emacs-fileio--buffer-files))
      (and (fboundp 'files--buffer-file-name)
           (condition-case nil
               (files--buffer-file-name buf)
             (error nil)))))

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
         (buf (or (emacs-buffer-ui--find-buffer name)
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
  (let ((out (or (emacs-buffer-ui--find-buffer emacs-buffer-ui--list-buffer-name)
                 (nelisp-ec-generate-new-buffer emacs-buffer-ui--list-buffer-name))))
    (nelisp-ec-with-current-buffer out
      (nelisp-ec-erase-buffer)
      (nelisp-ec-insert (format "%-24s %-8s %-18s %s\n"
                                "name" "size" "mode" "file"))
      (dolist (buf (emacs-buffer-buffer-list))
        (nelisp-ec-insert
         (format "%-24s %-8d %-18s %s\n"
                 (nelisp-ec-buffer-name buf)
                 (nelisp-ec-buffer-size buf)
                 (emacs-buffer-ui--buffer-mode-name buf)
                 (or (emacs-buffer-ui--buffer-file-name buf) ""))))
      (emacs-buffer-set-buffer-modified-p nil out))
    (emacs-window-set-window-buffer (emacs-window-selected-window) out)
    (nelisp-ec-set-buffer out)
    out))

(defalias 'switch-to-buffer-interactive #'emacs-buffer-ui-switch-to-buffer)
(defalias 'kill-buffer-interactive #'emacs-buffer-ui-kill-buffer-interactive)
(defalias 'list-buffers-interactive #'emacs-buffer-ui-list-buffers)

(unless (fboundp 'switch-to-buffer)
  (defalias 'switch-to-buffer #'emacs-buffer-ui-switch-to-buffer))

(unless (fboundp 'list-buffers)
  (defalias 'list-buffers #'emacs-buffer-ui-list-buffers))

(provide 'emacs-buffer-ui)

;;; emacs-buffer-ui.el ends here
