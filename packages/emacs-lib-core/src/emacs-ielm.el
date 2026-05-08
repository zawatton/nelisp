;;; emacs-ielm.el --- Minimal in-process ielm REPL for nelisp-emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 02 `docs/design/02-v01-daily-driver.org' §3.2.5 asks for the
;; smallest usable `ielm' REPL:
;;
;; - `M-x ielm' opens or switches to a `*ielm*' buffer
;; - prompt = "ELISP> "
;; - RET reads the current input, evaluates it in-process, prints the
;;   `prin1-to-string' result, and inserts the next prompt
;; - errors stay in-buffer and return to the prompt
;; - minimal comint-style history on M-p / M-n
;; - `C-c C-l' clears the buffer and resets history
;;
;; This is intentionally not a subprocess-backed comint clone.  The
;; entire loop runs against the current Lisp image via `read-from-string'
;; + `eval'.

;;; Code:

(require 'emacs-buffer-builtins)
(require 'emacs-eval)
(require 'emacs-keymap)
(require 'emacs-mode)

(defconst ielm-buffer-name "*ielm*"
  "Canonical buffer name used by `ielm'.")

(defconst ielm-prompt "ELISP> "
  "Prompt inserted before each input chunk.")

(defvar inferior-emacs-lisp-mode-map
  (let ((map (emacs-keymap-make-sparse-keymap)))
    (emacs-keymap-define-key map (kbd "RET") #'ielm-input-handler)
    (emacs-keymap-define-key map (kbd "M-p") #'ielm-previous-input)
    (emacs-keymap-define-key map (kbd "M-n") #'ielm-next-input)
    (emacs-keymap-define-key map (kbd "C-c C-l") #'ielm-clear-buffer)
    map)
  "Keymap for `inferior-emacs-lisp-mode'.")

(defvar emacs-ielm--state (make-hash-table :test 'eq :weakness nil)
  "Per-buffer ielm state.
Each value is a plist with keys:
- `:input-start'    point where editable user input begins
- `:history'        newest-first list of submitted input strings
- `:history-index'  nil or zero-based index into `:history' for M-p/M-n")

(defun emacs-ielm--buffer ()
  "Return the canonical ielm buffer, creating it if needed."
  (get-buffer-create ielm-buffer-name))

(defun emacs-ielm--state (buffer)
  "Return BUFFER's ielm state, creating a default record when needed."
  (or (gethash buffer emacs-ielm--state)
      (puthash buffer
               (list :input-start 1
                     :history nil
                     :history-index nil)
               emacs-ielm--state)))

(defun emacs-ielm--set-state (buffer key value)
  "Store VALUE under KEY in BUFFER's ielm state and return VALUE."
  (let ((state (copy-sequence (emacs-ielm--state buffer))))
    (setq state (plist-put state key value))
    (puthash buffer state emacs-ielm--state)
    value))

(defun emacs-ielm--set-input-start (buffer pos)
  "Record POS as BUFFER's editable input start."
  (emacs-ielm--set-state buffer :input-start pos))

(defun emacs-ielm--input-start (&optional buffer)
  "Return the current input start for BUFFER or current buffer."
  (plist-get (emacs-ielm--state (or buffer (current-buffer))) :input-start))

(defun emacs-ielm--history (&optional buffer)
  "Return the history list for BUFFER or current buffer."
  (plist-get (emacs-ielm--state (or buffer (current-buffer))) :history))

(defun emacs-ielm--history-index (&optional buffer)
  "Return the active history index for BUFFER or current buffer."
  (plist-get (emacs-ielm--state (or buffer (current-buffer))) :history-index))

(defun emacs-ielm--set-history (buffer history)
  "Replace BUFFER history with HISTORY."
  (emacs-ielm--set-state buffer :history history))

(defun emacs-ielm--set-history-index (buffer index)
  "Set BUFFER history navigation index to INDEX."
  (emacs-ielm--set-state buffer :history-index index))

(defun emacs-ielm--trim-right (string)
  "Drop trailing newline and space characters from STRING."
  (replace-regexp-in-string "[[:space:]\n\r]+\\'" "" string))

(defun emacs-ielm--whitespace-only-p (string)
  "Return non-nil when STRING is empty or all whitespace."
  (or (equal string "")
      (string-match-p "\\`[[:space:]\n\r\t]*\\'" string)))

(defun emacs-ielm--goto-input-end ()
  "Move point to the end of the current editable input."
  (goto-char (point-max)))

(defun emacs-ielm--replace-input (text)
  "Replace the current editable input with TEXT."
  (let ((start (emacs-ielm--input-start)))
    (delete-region start (point-max))
    (goto-char start)
    (insert text)))

(defun emacs-ielm--current-input ()
  "Return the current input from prompt to `point-max'."
  (buffer-substring-no-properties (emacs-ielm--input-start) (point-max)))

(defun emacs-ielm--append-newline-unless-present ()
  "Ensure point-max ends with a newline before output is printed."
  (unless (or (= (point-max) 1)
              (eq (char-before (point-max)) ?\n))
    (goto-char (point-max))
    (insert "\n")))

(defun emacs-ielm--insert-prompt ()
  "Insert a fresh prompt at point-max and record the new input start."
  (goto-char (point-max))
  (insert ielm-prompt)
  (emacs-ielm--set-input-start (current-buffer) (point)))

(defun emacs-ielm--ensure-prompt ()
  "Insert the first prompt when the current ielm buffer is empty."
  (when (= (point-max) 1)
    (emacs-ielm--insert-prompt)))

(defun emacs-ielm--reset-buffer-state (buffer)
  "Reset BUFFER history navigation and input start metadata."
  (emacs-ielm--set-history buffer nil)
  (emacs-ielm--set-history-index buffer nil)
  (emacs-ielm--set-input-start buffer 1))

(defun emacs-ielm--push-history (input)
  "Add INPUT to current buffer history unless it is blank."
  (unless (emacs-ielm--whitespace-only-p input)
    (emacs-ielm--set-history
     (current-buffer)
     (cons input (emacs-ielm--history)))
    (emacs-ielm--set-history-index (current-buffer) nil)))

(defun emacs-ielm--history-entry (index)
  "Return history entry at INDEX from the current buffer."
  (nth index (emacs-ielm--history)))

(defun emacs-ielm--read-one-form (input)
  "Read a single form from INPUT or signal an error on trailing junk."
  (let* ((read-result (read-from-string input))
         (form (car read-result))
         (end (cdr read-result))
         (rest (substring input end)))
    (unless (emacs-ielm--whitespace-only-p rest)
      (error "Trailing input after first form"))
    form))

(defun emacs-ielm--print-result (value)
  "Append VALUE to the REPL transcript."
  (emacs-ielm--append-newline-unless-present)
  (goto-char (point-max))
  (insert (prin1-to-string value) "\n"))

(defun emacs-ielm--print-error (err)
  "Append ERR's user-facing message to the REPL transcript."
  (emacs-ielm--append-newline-unless-present)
  (goto-char (point-max))
  (insert (error-message-string err) "\n"))

(defun emacs-ielm--open-buffer (buffer)
  "Display BUFFER and return it."
  (cond
   ((fboundp 'switch-to-buffer)
    (switch-to-buffer buffer))
   (t
    (set-buffer buffer)
    buffer)))

;;;###autoload
(defun inferior-emacs-lisp-mode ()
  "Major mode for the minimal in-process elisp REPL."
  (interactive)
  (emacs-mode-kill-all-local-variables)
  (emacs-mode-set-major-mode 'inferior-emacs-lisp-mode "IELM")
  (setq major-mode 'inferior-emacs-lisp-mode)
  (setq mode-name "IELM")
  (emacs-keymap-use-local-map inferior-emacs-lisp-mode-map)
  (emacs-ielm--state (current-buffer))
  (emacs-ielm--ensure-prompt)
  nil)

;;;###autoload
(defun ielm ()
  "Open or switch to the `*ielm*' buffer."
  (interactive)
  (let ((buffer (emacs-ielm--buffer)))
    (with-current-buffer buffer
      (unless (eq major-mode 'inferior-emacs-lisp-mode)
        (inferior-emacs-lisp-mode))
      (emacs-ielm--ensure-prompt)
      (emacs-ielm--goto-input-end))
    (emacs-ielm--open-buffer buffer)))

(defun ielm-input-handler ()
  "Read, evaluate, and print the current input in the ielm buffer."
  (interactive)
  (let ((input (emacs-ielm--trim-right (emacs-ielm--current-input))))
    (goto-char (point-max))
    (cond
     ((emacs-ielm--whitespace-only-p input)
      (emacs-ielm--append-newline-unless-present)
      (emacs-ielm--insert-prompt))
     (t
      (emacs-ielm--push-history input)
      (condition-case err
          (emacs-ielm--print-result
           (eval (emacs-ielm--read-one-form input) t))
        (error
         (emacs-ielm--print-error err)))
      (emacs-ielm--insert-prompt))))
  (emacs-ielm--goto-input-end)
  nil)

(defun ielm-previous-input ()
  "Replace the current input with the previous history item."
  (interactive)
  (let* ((history (emacs-ielm--history))
         (count (length history)))
    (when (> count 0)
      (let* ((current (emacs-ielm--history-index))
             (next-index (if current
                             (min (1+ current) (1- count))
                           0)))
        (emacs-ielm--set-history-index (current-buffer) next-index)
        (emacs-ielm--replace-input (or (emacs-ielm--history-entry next-index)
                                       "")))))
  (emacs-ielm--goto-input-end)
  nil)

(defun ielm-next-input ()
  "Replace the current input with the next history item."
  (interactive)
  (let ((current (emacs-ielm--history-index)))
    (cond
     ((null current) nil)
     ((<= current 0)
      (emacs-ielm--set-history-index (current-buffer) nil)
      (emacs-ielm--replace-input ""))
     (t
      (let ((next-index (1- current)))
        (emacs-ielm--set-history-index (current-buffer) next-index)
        (emacs-ielm--replace-input (or (emacs-ielm--history-entry next-index)
                                       ""))))))
  (emacs-ielm--goto-input-end)
  nil)

(defun ielm-clear-buffer ()
  "Clear the ielm buffer and reset history."
  (interactive)
  (erase-buffer)
  (emacs-ielm--reset-buffer-state (current-buffer))
  (emacs-ielm--insert-prompt)
  (emacs-ielm--goto-input-end)
  nil)

(provide 'emacs-ielm)

;;; emacs-ielm.el ends here
