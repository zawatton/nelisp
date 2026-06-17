;;; emacs-shell.el --- minimal comint-based command shell  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Layer 2 (nelisp-emacs) interactive shell: an `*shell*' buffer built on the
;; `emacs-comint' machinery (output mark, input ring, prompt).  Each submitted
;; line is run as a fresh `SHELL -c LINE' via the `call-process' substrate and
;; its output is inserted at the comint mark.
;;
;; Why one-shot per line rather than a persistent subprocess: the standalone
;; reader's `make-process' cannot yet hold an interactive subprocess open (its
;; stdin pipe is not maintained, so the shell exits immediately -- an L1
;; substrate gap, the same one `emacs-comint' documents).  `call-process' DOES
;; work on the reader, so a per-command shell runs end-to-end there.  Shell
;; state that a persistent process would keep (notably the working directory)
;; is tracked here: `cd' updates a per-buffer cwd that later commands inherit.
;;
;; Boundary (../nelisp-gui/docs/design/00-three-layer-architecture.org):
;;   - nelisp-emacs OWNS: the buffer/prompt/ring, the cwd tracking, and the
;;     command invocation.
;;   - nelisp-gui OWNS: rendering + key transport.
;;   - nelisp (L1) OWNS: the subprocess substrate.

;;; Code:

(require 'emacs-comint)

(defvar emacs-shell-buffer-name "*shell*"
  "Buffer name used by `emacs-shell'.")

(defvar emacs-shell-file-name "/bin/sh"
  "Shell program run for each submitted command line.")

(defvar emacs-shell-prompt-string "$ "
  "Prompt inserted before each input line.")

(defvar emacs-shell--cwd (make-hash-table :test 'equal)
  "Per-buffer working directory, keyed by buffer NAME.
Buffer-name keying mirrors `emacs-comint' (buffer objects are not usable
as `eq' hash keys on the standalone reader).")

(defun emacs-shell--cwd (&optional buffer)
  "Return BUFFER's working directory (default current)."
  (or (gethash (buffer-name (or buffer (current-buffer))) emacs-shell--cwd)
      (and (boundp 'default-directory) default-directory)
      "/"))

(defun emacs-shell--set-cwd (dir &optional buffer)
  "Set BUFFER's working directory to DIR (resolved against the current cwd)."
  (let ((d (file-name-as-directory
            (expand-file-name dir (emacs-shell--cwd buffer)))))
    (puthash (buffer-name (or buffer (current-buffer))) d emacs-shell--cwd)
    d))

(defun emacs-shell--trim (string)
  "Trim leading/trailing whitespace from STRING.
Uses explicit whitespace classes (the `[[:space:]]' POSIX class does not
match on the standalone reader)."
  (replace-regexp-in-string "\\`[ \t\n\r\f\v]+\\|[ \t\n\r\f\v]+\\'" "" string))

(defun emacs-shell--sh-quote (string)
  "Single-quote STRING for /bin/sh.
Adequate for path arguments; paths containing a single quote are not
supported by this minimal shell."
  (concat "'" string "'"))

(defun emacs-shell--run (command)
  "Run COMMAND through `emacs-shell-file-name' in the buffer's cwd.
Return the captured output string.  The cwd is set with an explicit `cd'
in the shell line: the standalone reader's `call-process' does not honor
`default-directory' as the subprocess working directory."
  (if (not (fboundp 'call-process))
      ""
    (let ((dir (emacs-shell--cwd)))
      (with-temp-buffer
        (setq default-directory dir)
        (call-process emacs-shell-file-name nil t nil "-c"
                      (concat "cd " (emacs-shell--sh-quote dir)
                              " 2>/dev/null; " command))
        (buffer-string)))))

(defun emacs-shell--insert-prompt ()
  "Insert the shell prompt at point-max and set the comint mark after it."
  (goto-char (point-max))
  (insert emacs-shell-prompt-string)
  (emacs-comint--set-mark (point-max)))

;;;; --- input -------------------------------------------------------

(defun emacs-shell-send-input ()
  "Run the pending input line as a shell command; print output + a new prompt.
`cd DIR' is handled in-process (it updates the per-buffer working
directory); every other line runs through `emacs-shell--run'."
  (interactive)
  (let* ((mark (emacs-comint--mark))
         (raw (buffer-substring-no-properties mark (point-max)))
         (command (emacs-shell--trim raw)))
    (emacs-comint-add-to-input-history raw)
    (goto-char (point-max))
    (insert "\n")
    (cond
     ((string= command "") nil)
     ((or (string= command "cd") (string-prefix-p "cd " command))
      (let ((target (emacs-shell--trim (substring command 2))))
        (emacs-shell--set-cwd (if (string= target "") "/" target))))
     (t
      (let ((out (emacs-shell--run command)))
        (insert out)
        (unless (or (string= out "") (string-suffix-p "\n" out))
          (insert "\n")))))
    (emacs-shell--insert-prompt)
    nil))

;;;; --- mode + buffer construction -----------------------------------

(defun emacs-shell-mode ()
  "Put the current buffer into a minimal shell mode over comint.
Resets the per-buffer working directory so a re-opened `*shell*' starts
from `default-directory' rather than a stale cwd."
  (interactive)
  (emacs-comint-mode)
  (setq major-mode 'shell-mode
        mode-name "Shell")
  (remhash (buffer-name (current-buffer)) emacs-shell--cwd)
  nil)

(defun emacs-shell ()
  "Open or switch to the `*shell*' command buffer; return it."
  (interactive)
  (let ((buf (get-buffer-create emacs-shell-buffer-name)))
    (with-current-buffer buf
      (unless (eq major-mode 'shell-mode)
        (emacs-shell-mode))
      (when (= (point-max) (point-min))
        (emacs-shell--insert-prompt))
      (goto-char (point-max)))
    (if (fboundp 'switch-to-buffer)
        (switch-to-buffer buf)
      (set-buffer buf))
    buf))

;;;; --- standard-name facade -----------------------------------------

(defun emacs-shell-install ()
  "Bind the standard `shell' / `shell-mode' command names to the
`emacs-shell' implementations.  Not run on `require' (keeps a bare load
from touching shared command symbols)."
  (defalias 'shell #'emacs-shell)
  (defalias 'shell-mode #'emacs-shell-mode))

(provide 'emacs-shell)

;;; emacs-shell.el ends here
