;;; emacs-eshell.el --- minimal Emacs shell (eshell)  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Layer 2 (nelisp-emacs) eshell: an `*eshell*' buffer on the `emacs-comint'
;; machinery whose command dispatch follows eshell's hybrid model --
;;
;;   (LISP FORM)   a line starting with `(' is read and evaluated as Elisp
;;   eshell/CMD    a built-in Elisp command (cd / pwd / echo) is called
;;   external      anything else runs through `SHELL -c' via `call-process'
;;
;; This fits the standalone reader well: the built-ins and Lisp evaluation are
;; pure Elisp (no subprocess), and external commands use `call-process' (which
;; works on the reader, unlike a held-open interactive subprocess).  A
;; per-buffer working directory is tracked so `cd' persists across commands.
;;
;; Boundary (../nelisp-gui/docs/design/00-three-layer-architecture.org):
;;   - nelisp-emacs OWNS: the prompt/ring buffer, the command parser/dispatch,
;;     the built-ins, the cwd, and the external invocation.
;;   - nelisp-gui OWNS: rendering + key transport.

;;; Code:

(require 'emacs-comint)

(defvar eshell-buffer-name "*eshell*"
  "Buffer name used by `eshell'.")

(defvar eshell-prompt-string "$ "
  "Prompt inserted before each input line.")

(defvar eshell-file-name "/bin/sh"
  "Shell used to run external (non-built-in) command lines.")

(defvar eshell--cwd (make-hash-table :test 'equal)
  "Per-buffer working directory, keyed by buffer NAME.")

(defun eshell--cwd (&optional buffer)
  "Return BUFFER's working directory (default current)."
  (or (gethash (buffer-name (or buffer (current-buffer))) eshell--cwd)
      (and (boundp 'default-directory) default-directory)
      "/"))

(defun eshell--set-cwd (dir &optional buffer)
  "Set BUFFER's working directory to DIR (resolved against the current cwd)."
  (let ((d (file-name-as-directory
            (expand-file-name dir (eshell--cwd buffer)))))
    (puthash (buffer-name (or buffer (current-buffer))) d eshell--cwd)
    d))

(defun eshell--trim (string)
  "Trim leading/trailing whitespace from STRING (explicit class, reader-safe)."
  (replace-regexp-in-string "\\`[ \t\n\r\f\v]+\\|[ \t\n\r\f\v]+\\'" "" string))

(defun eshell--sh-quote (string)
  "Single-quote STRING for /bin/sh (adequate for path arguments)."
  (concat "'" string "'"))

;;;; --- built-in commands (eshell/NAME) ------------------------------

(defun eshell/pwd (&rest _args)
  "Built-in: print the working directory."
  (directory-file-name (eshell--cwd)))

(defun eshell/cd (&optional dir &rest _args)
  "Built-in: change the working directory to DIR (default \"/\")."
  (eshell--set-cwd (or dir "/"))
  "")

(defun eshell/echo (&rest args)
  "Built-in: echo ARGS joined by a space."
  (mapconcat #'identity args " "))

;;;; --- dispatch -----------------------------------------------------

(defun eshell--run-external (line)
  "Run LINE through `eshell-file-name' in the buffer's cwd; return output.
The cwd is set with an explicit `cd' (the reader's `call-process' does not
honor `default-directory' as the subprocess working directory)."
  (if (not (fboundp 'call-process))
      ""
    (let ((dir (eshell--cwd)))
      (with-temp-buffer
        (setq default-directory dir)
        (call-process eshell-file-name nil t nil "-c"
                      (concat "cd " (eshell--sh-quote dir) " 2>/dev/null; " line))
        (buffer-string)))))

(defun eshell--dispatch (line)
  "Run LINE per the eshell model; return the output string to insert."
  (cond
   ((string-prefix-p "(" line)
    (condition-case err
        (format "%S" (eval (car (read-from-string line)) t))
      (error (concat "error: " (error-message-string err)))))
   (t
    (let* ((parts (split-string line))
           (cmd (car parts))
           (args (cdr parts))
           (builtin (and cmd (intern-soft (concat "eshell/" cmd)))))
      (if (and builtin (fboundp builtin))
          (let ((out (condition-case err
                         (apply builtin args)
                       (error (concat "error: " (error-message-string err))))))
            (if (stringp out) out ""))
        (eshell--run-external line))))))

;;;; --- input + buffer -----------------------------------------------

(defun eshell--insert-prompt ()
  "Insert the prompt at point-max and set the comint mark after it."
  (goto-char (point-max))
  (insert eshell-prompt-string)
  (emacs-comint--set-mark (point-max)))

(defun eshell-send-input ()
  "Dispatch the pending input line and print its output + a new prompt."
  (interactive)
  (let* ((mark (emacs-comint--mark))
         (raw (buffer-substring-no-properties mark (point-max)))
         (line (eshell--trim raw)))
    (emacs-comint-add-to-input-history raw)
    (goto-char (point-max))
    (insert "\n")
    (unless (string= line "")
      (let ((out (eshell--dispatch line)))
        (when (and (stringp out) (> (length out) 0))
          (insert out)
          (unless (string-suffix-p "\n" out) (insert "\n")))))
    (eshell--insert-prompt)
    nil))

(defun eshell-mode ()
  "Put the current buffer into a minimal eshell mode over comint.
Resets the per-buffer working directory."
  (interactive)
  (emacs-comint-mode)
  (setq major-mode 'eshell-mode
        mode-name "Eshell")
  (remhash (buffer-name (current-buffer)) eshell--cwd)
  nil)

(defun eshell ()
  "Open or switch to the `*eshell*' buffer; return it."
  (interactive)
  (let ((buf (get-buffer-create eshell-buffer-name)))
    (with-current-buffer buf
      (unless (eq major-mode 'eshell-mode)
        (eshell-mode))
      (when (= (point-max) (point-min))
        (eshell--insert-prompt))
      (goto-char (point-max)))
    (if (fboundp 'switch-to-buffer)
        (switch-to-buffer buf)
      (set-buffer buf))
    buf))

;; The standard `eshell' / `eshell-mode' / `eshell/*' command names are defined
;; directly above (eshell, like ielm/isearch, owns its public names), so the
;; `eshell' facade loader only needs to `require' this module on standalone.

(provide 'emacs-eshell)

;;; emacs-eshell.el ends here
